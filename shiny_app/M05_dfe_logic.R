# =============================================================================
# M05_dfe_logic.R -- Logique DFE, collecte paramètres, contexte SymPy
# =============================================================================
# Dépendances : M01_constants.R, M02_validation_sympy.R,
#               M06_symbolic_ngm.R (build_symbolic_table, classify_params)
# Testé seul  : source("M01_constants.R")
#               source("M02_validation_sympy.R")
#               source("M06_symbolic_ngm.R")
#               source("M05_dfe_logic.R")
# =============================================================================


# =============================================================================
# SECTION 1 -- Détection de la démographie
# =============================================================================

#' Vérifie si le modèle contient des flux de naissance ou de mort
#'
#' @param flux_df data.frame
#' @return logical
has_demography <- function(flux_df) {
  if (nrow(flux_df) == 0) return(FALSE)
  # Un flux est demographique SEULEMENT si is_demographic = TRUE
  # Exclut les morts dues a la maladie (is_demographic=FALSE)
  any(flux_df$process_type %in% DEMOGRAPHIC_PROCESS_TYPES &
      vapply(flux_df$is_demographic, isTRUE, logical(1)))
}

#' Vérifie si une espèce donnée a de la démographie dans ses flux
#'
#' @param flux_df     data.frame
#' @param comp_df     data.frame
#' @param species_name character(1)
#' @return logical
has_demography_for_species <- function(flux_df, comp_df, species_name) {
  if (nrow(flux_df) == 0 || nrow(comp_df) == 0) return(FALSE)

  sp_ids <- comp_df$state_id[
    !is.na(comp_df$species) & comp_df$species == species_name
  ]
  if (length(sp_ids) == 0) return(FALSE)

  # Un flux est demographique SEULEMENT si is_demographic = TRUE
  # ET process_type in DEMOGRAPHIC_PROCESS_TYPES
  # Cela exclut les morts dues a la maladie (mu_tb, mort infectieux)
  # qui ont process_type="death" mais is_demographic=FALSE
  demo <- flux_df[
    flux_df$process_type %in% DEMOGRAPHIC_PROCESS_TYPES &
    vapply(flux_df$is_demographic, isTRUE, logical(1)),
  ]
  if (nrow(demo) == 0) return(FALSE)

  any(demo$from_state_id %in% sp_ids | demo$to_state_id %in% sp_ids)
}

#' Retourner la liste des espèces présentes dans comp_df
#'
#' @param comp_df data.frame
#' @return character -- au moins "human" si aucune espèce définie
get_species <- function(comp_df) {
  sp <- unique(comp_df$species)
  sp <- sp[!is.na(sp) & sp != ""]
  if (length(sp) == 0) sp <- "human"
  sp
}


# =============================================================================
# SECTION 2 -- Construction de la table DFE
# =============================================================================

#' Construire la table DFE automatiquement depuis comp_df et flux_df
#'
#' Pour chaque compartiment, propose une valeur DFE selon les règles :
#'   - dead       → 0 (exclu NGM)
#'   - infectieux → 0 (hypothèse standard NGM)
#'   - guéri/immun/traité → 0
#'   - susceptible sans démographie → Ntot (mono-espèce) ou N_<x> (multi)
#'   - susceptible avec démographie → Lambda_b / mu_d (suggestion)
#'   - autres → vide (à remplir manuellement)
#'
#' @param comp_df data.frame
#' @param flux_df data.frame
#' @return data.frame de schéma init_dfe
build_dfe_table <- function(comp_df, flux_df) {

  # Retourner un DFE vide si pas de compartiments
  if (nrow(comp_df) == 0)
    return(data.frame(
      state_id = character(0), label = character(0),
      state_type = character(0), species = character(0),
      dfe_expr = character(0), dfe_auto = logical(0),
      dfe_comment = character(0),
      stringsAsFactors = FALSE
    ))

  species   <- get_species(comp_df)
  multi_sp  <- length(species) > 1

  rows <- lapply(seq_len(nrow(comp_df)), function(i) {
    st  <- comp_df$state_type[i]
    sid <- comp_df$state_id[i]
    sp  <- if (!is.na(comp_df$species[i])) comp_df$species[i] else "human"

    has_demo_sp <- has_demography_for_species(flux_df, comp_df, sp)

    # ── Compartiment mort → toujours exclu ──────────────────────────────────
    if (st == "dead")
      return(data.frame(
        state_id = sid, label = comp_df$label[i],
        state_type = st, species = sp,
        dfe_expr = "0", dfe_auto = TRUE,
        dfe_comment = "Exclu du NGM -- toujours 0 au DFE",
        stringsAsFactors = FALSE
      ))

    # ── Compartiment infectieux → 0 au DFE ──────────────────────────────────
    if (st %in% INFECTED_STATE_TYPES)
      return(data.frame(
        state_id = sid, label = comp_df$label[i],
        state_type = st, species = sp,
        dfe_expr = "0", dfe_auto = TRUE,
        dfe_comment = "Infect\u00e9 \u2192 0 au DFE (hypoth\u00e8se standard NGM)",
        stringsAsFactors = FALSE
      ))

    # ── Guéri / immun / traité / hospitalisé → 0 au DFE ────────────────────
    if (st %in% c("recovered", "immune", "treated", "hospitalized"))
      return(data.frame(
        state_id = sid, label = comp_df$label[i],
        state_type = st, species = sp,
        dfe_expr = "0", dfe_auto = TRUE,
        dfe_comment = "Gu\u00e9ri/Immun \u2192 0 au DFE (pas de circulation virale)",
        stringsAsFactors = FALSE
      ))

    # ── Susceptible ──────────────────────────────────────────────────────────
    if (st == "susceptible") {

      if (!has_demo_sp) {
        # Sans demographie -- determiner le symbole de taille de population

        # Detecter patch du compartiment courant
        patch_col <- if ("patch" %in% names(comp_df)) comp_df$patch[i] else ""
        patch_col <- if (is.na(patch_col)) "" else trimws(as.character(patch_col))
        has_patch <- nchar(patch_col) > 0

        # Detecter si plusieurs susceptibles de meme espece (metapopulation)
        same_sp_susc <- comp_df$state_id[
          !is.na(comp_df$state_type) &
          comp_df$state_type == "susceptible" &
          !is.na(comp_df$species) &
          comp_df$species == sp]
        multi_patch_same_sp <- length(same_sp_susc) > 1

        if (!multi_sp && !multi_patch_same_sp) {
          # Mono-espece, un seul susceptible : S* = Ntot
          return(data.frame(
            state_id = sid, label = comp_df$label[i],
            state_type = st, species = sp,
            dfe_expr = "Ntot", dfe_auto = TRUE,
            dfe_comment = "Sans demographie : S* = Ntot (population totale)",
            stringsAsFactors = FALSE
          ))

        } else if (multi_patch_same_sp && has_patch) {
          # Metapopulation avec patch defini : S* = N<patch>
          ntot_sym <- paste0("N", patch_col)
          return(data.frame(
            state_id = sid, label = comp_df$label[i],
            state_type = st, species = sp,
            dfe_expr = ntot_sym, dfe_auto = FALSE,
            dfe_comment = paste0(
              "⚠️ Metapopulation patch ", patch_col,
              " : S* suggere = ", ntot_sym,
              " -- verifiez que ce nom correspond au parametre de taille du patch."
            ),
            stringsAsFactors = FALSE
          ))

        } else if (multi_patch_same_sp) {
          # Plusieurs susceptibles meme espece sans patch
          ntot_sym <- paste0("N_", sid)
          return(data.frame(
            state_id = sid, label = comp_df$label[i],
            state_type = st, species = sp,
            dfe_expr = ntot_sym, dfe_auto = FALSE,
            dfe_comment = paste0(
              "⚠️ Plusieurs susceptibles : S* suggere = ", ntot_sym,
              " -- definissez le patch dans les compartiments pour une suggestion ",
              "plus precise."
            ),
            stringsAsFactors = FALSE
          ))

        } else {
          # Multi-especes : suggerer N_<espece>
          sp_abbr  <- substr(sp, 1, 1)
          ntot_sym <- paste0("N_", sp_abbr)
          return(data.frame(
            state_id = sid, label = comp_df$label[i],
            state_type = st, species = sp,
            dfe_expr = ntot_sym, dfe_auto = FALSE,
            dfe_comment = paste0(
              "⚠️ Multi-especes : S* suggere = ", ntot_sym,
              " -- verifiez que ce nom correspond au parametre de taille ",
              "pour l'espece '", sp, "'"
            ),
            stringsAsFactors = FALSE
          ))
        }
      } else {
        # Avec démographie : S* = Lambda / mu
        birth_f <- flux_df[
          flux_df$process_type == "birth" &
          (flux_df$to_state_id %in%
             comp_df$state_id[comp_df$species == sp]), ]
        death_f <- flux_df[
          flux_df$process_type == "death" &
          flux_df$from_state_id == sid, ]

        bp <- if (nrow(birth_f) > 0 &&
                  !is.na(birth_f$rate_name[1]) &&
                  birth_f$rate_name[1] != "")
          birth_f$rate_name[1] else "Lambda_b"

        dp <- if (nrow(death_f) > 0 &&
                  !is.na(death_f$rate_name[1]) &&
                  death_f$rate_name[1] != "")
          death_f$rate_name[1] else "mu_d"

        sug <- paste0(bp, " / ", dp)
        return(data.frame(
          state_id = sid, label = comp_df$label[i],
          state_type = st, species = sp,
          dfe_expr = sug, dfe_auto = FALSE,
          dfe_comment = paste0(
            "\u26a0\ufe0f D\u00e9mographie d\u00e9tect\u00e9e pour '", sp,
            "' : S* = ", sug,
            " \u2014 v\u00e9rifiez et ajustez si n\u00e9cessaire"
          ),
          stringsAsFactors = FALSE
        ))
      }
    }

    # ── Autre type → valeur manuelle ─────────────────────────────────────────
    data.frame(
      state_id = sid, label = comp_df$label[i],
      state_type = st, species = sp,
      dfe_expr = "", dfe_auto = FALSE,
      dfe_comment = "Valeur DFE \u00e0 d\u00e9finir manuellement",
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}


# =============================================================================
# SECTION 3 -- Collecte et construction de la table des paramètres
# =============================================================================

#' Collecter tous les paramètres libres depuis flux_df et dfe_df
#'
#' @param flux_df data.frame
#' @param comp_df data.frame
#' @param dfe_df  data.frame ou NULL
#' @return character -- noms de paramètres uniques
collect_model_params <- function(flux_df, comp_df, dfe_df = NULL) {
  known  <- comp_df$state_id
  params <- character(0)

  # Scanner les flux
  if (nrow(flux_df) > 0) {
    for (i in seq_len(nrow(flux_df))) {
      expr <- flux_df$rate_expression[i]
      if (!is.na(expr) && trimws(expr) != "")
        params <- c(params, extract_free_params(expr, known))
    }
  }

  # Scanner les expressions DFE
  if (!is.null(dfe_df) && nrow(dfe_df) > 0) {
    for (i in seq_len(nrow(dfe_df))) {
      expr <- dfe_df$dfe_expr[i]
      if (!is.na(expr) && trimws(expr) != "")
        params <- c(params, extract_free_params(expr, known))
    }
  }

  unique(params)
}

#' Construire la table des paramètres
#'
#' Collecte les paramètres libres, récupère les descriptions déjà
#' saisies dans rate_param_desc et classe chaque paramètre.
#'
#' @param flux_df data.frame
#' @param comp_df data.frame
#' @param dfe_df  data.frame ou NULL
#' @return data.frame de schéma init_params
build_params_table <- function(flux_df, comp_df, dfe_df = NULL) {
  params <- collect_model_params(flux_df, comp_df, dfe_df)

  if (length(params) == 0)
    return(data.frame(
      param_name = character(0), description = character(0),
      units = character(0), category = character(0),
      stringsAsFactors = FALSE
    ))

  # Récupérer les descriptions déjà saisies dans les flux
  all_desc <- list()
  for (i in seq_len(nrow(flux_df))) {
    pd <- flux_df$rate_param_desc[i]
    if (!is.na(pd) && trimws(pd) != "") {
      d <- decode_param_desc(pd)
      for (nm in names(d))
        if (nm != "" && (is.null(all_desc[[nm]]) || all_desc[[nm]] == ""))
          all_desc[[nm]] <- d[[nm]]
    }
  }

  # Classifier
  classification <- classify_params(params, flux_df, comp_df, dfe_df)

  data.frame(
    param_name  = params,
    description = sapply(params, function(p) all_desc[[p]] %||% ""),
    units       = "",
    category    = classification$category,
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# SECTION 4 -- Construction du contexte SymPy
# =============================================================================

#' Construire le contexte envoyé au moteur Python (ngm_engine.py)
#'
#' Assemble les expressions F_i et V_i depuis la table symbolique,
#' les substitutions DFE, et les métadonnées du modèle.
#'
#' @param comp_df  data.frame
#' @param flux_df  data.frame
#' @param dfe_df   data.frame
#' @param params_df data.frame
#' @return list -- contexte JSON-serializable
build_sympy_context <- function(comp_df, flux_df, dfe_df, params_df) {
  infected  <- comp_df[comp_df$state_type %in% INFECTED_STATE_TYPES, ]
  dfe_subs  <- setNames(as.list(dfe_df$dfe_expr), dfe_df$state_id)
  param_vals <- list()   # valeurs numériques saisies dans Évaluation Numérique

  sym_df <- build_symbolic_table(comp_df, flux_df)

  fi_exprs <- list()
  vi_exprs <- list()

  for (sid in infected$state_id) {
    # F_i : flux is_F entrant dans sid
    is_F_vec <- vapply(sym_df$is_F, isTRUE, logical(1))
    f_in <- sym_df[is_F_vec & !is.na(sym_df$to_state_id) &
                   sym_df$to_state_id == sid, ]
    fi_exprs[[sid]] <- if (nrow(f_in) > 0)
      paste(f_in$rate_expression, collapse = " + ")
    else "0"

    # V_i : (sorties is_V depuis sid) - (entrees is_V dans sid)
    # isTRUE() sur chaque element pour eviter NA & condition = NA en R
    is_V_vec <- vapply(sym_df$is_V, isTRUE, logical(1))
    v_out <- sym_df[is_V_vec & !is.na(sym_df$from_state_id) &
                    sym_df$from_state_id == sid, ]
    v_in  <- sym_df[is_V_vec & !is.na(sym_df$to_state_id) &
                    sym_df$to_state_id   == sid, ]
    v_out_expr <- if (nrow(v_out) > 0)
      paste(v_out$rate_expression, collapse = " + ") else "0"
    v_in_expr  <- if (nrow(v_in) > 0)
      paste(v_in$rate_expression,  collapse = " + ") else "0"

    vi_exprs[[sid]] <- if (v_in_expr == "0") v_out_expr
    else paste0(v_out_expr, " - (", v_in_expr, ")")
  }

  # Log de diagnostic build_sympy_context
  message("[build_sympy_context] infected: ",
          paste(infected$state_id, collapse=", "))
  message("[build_sympy_context] sym_df nrow=", nrow(sym_df))
  for (i in seq_len(nrow(sym_df))) {
    message(sprintf("[sym_df row %d] %s->%s pt=%s is_F=%s is_V=%s expr=%s",
                    i,
                    sym_df$from_state_id[i] %||% "",
                    sym_df$to_state_id[i]   %||% "",
                    sym_df$process_type[i]  %||% "",
                    sym_df$is_F[i],
                    sym_df$is_V[i],
                    sym_df$rate_expression[i] %||% ""))
  }
  for (sid in names(fi_exprs)) {
    message(sprintf("[build_sympy_context] F_%s = %s", sid, fi_exprs[[sid]]))
    message(sprintf("[build_sympy_context] V_%s = %s", sid, vi_exprs[[sid]]))
  }

  list(
    infected_states = infected$state_id,
    all_states      = comp_df$state_id,
    dfe_subs        = dfe_subs,
    param_vals      = param_vals,
    fi_exprs        = fi_exprs,
    vi_exprs        = vi_exprs,
    has_demography  = has_demography(flux_df),
    species         = get_species(comp_df)
  )
}


# =============================================================================
# SECTION 5 -- Détection des dépendances inter-espèces
# =============================================================================

#' Détecter automatiquement les dépendances inter-espèces
#'
#' Analyse les rate_expressions pour trouver les compartiments infectieux
#' d'une espèce qui apparaissent dans les flux d'une autre espèce.
#'
#' @param flux_df data.frame
#' @param comp_df data.frame
#' @return list de dépendances (peut être vide)
detect_cross_species_dependencies <- function(flux_df, comp_df) {
  if (nrow(flux_df) == 0 || nrow(comp_df) == 0) return(list())

  deps <- list()

  for (i in seq_len(nrow(flux_df))) {
    flux <- flux_df[i, ]
    expr <- flux$rate_expression
    if (is.na(expr) || trimws(expr) == "") next

    # Compartiments mentionnés dans l'expression
    mentioned <- unique(unlist(regmatches(
      expr, gregexpr("[A-Za-z][A-Za-z0-9_]*", expr)
    )))
    mentioned_comps <- intersect(mentioned, comp_df$state_id)

    target_id <- flux$to_state_id
    if (is.na(target_id) || target_id == "") next

    target_comp <- comp_df[comp_df$state_id == target_id, ]
    if (nrow(target_comp) == 0) next

    target_species <- target_comp$species[1]

    for (comp_id in mentioned_comps) {
      if (comp_id == target_id) next

      source_comp <- comp_df[comp_df$state_id == comp_id, ]
      if (nrow(source_comp) == 0) next

      source_species <- source_comp$species[1]
      source_type    <- source_comp$state_type[1]

      # Dépendance inter-espèces si espèces différentes ET source infectieuse
      if (source_species != target_species &&
          source_type %in% c("infectious", "infected",
                             "carrier", "exposed")) {

        # Éviter les doublons
        already <- length(deps) > 0 && any(vapply(deps, function(d)
          isTRUE(d$from == comp_id) && isTRUE(d$to == target_id),
          logical(1)))
        if (already) next

        deps[[length(deps) + 1]] <- list(
          from         = comp_id,
          to           = target_id,
          from_species = source_species,
          to_species   = target_species,
          from_type    = source_type,
          to_type      = target_comp$state_type[1],
          via_flux     = flux$flux_id,
          mechanism    = sprintf(
            "%s (%s) influences transmission to %s (%s)",
            comp_id, source_species, target_id, target_species
          ),
          type    = "auto",
          visible = TRUE,
          is_cross_species = TRUE
        )
      }
    }
  }

  return(deps)
}


# =============================================================================
# SECTION 6 -- Helper UI : couleur de badge par state_type
# =============================================================================

#' Retourner la couleur de fond (hex) pour un badge state_type
#'
#' Cohérent avec NODE_COLORS de M01.
#'
#' @param st character(1) -- state_type
#' @return character(1) -- couleur hex
dfe_type_badge_color <- function(st) {
  switch(st,
    susceptible  = "#cce5ff",
    exposed      = "#fff3cd",
    infectious   = "#f8d7da",
    latent       = "#ffeeba",
    carrier      = "#f5c6cb",
    recovered    = "#d4edda",
    immune       = "#c3e6cb",
    treated      = "#d1ecf1",
    hospitalized = "#bee5eb",
    dead         = "#e2e3e5",
    "#e9ecef"
  )
}


# =============================================================================
# SECTION 7 -- Vérifications au chargement
# =============================================================================

stopifnot(
  exists("INFECTED_STATE_TYPES"),
  exists("DEMOGRAPHIC_PROCESS_TYPES"),
  exists("extract_free_params"),      # M02
  exists("build_symbolic_table"),     # M06
  exists("classify_params"),          # M06
  exists("decode_param_desc")         # M03
)

local({

  # ── has_demography ────────────────────────────────────────────────────────
  fd_demo <- data.frame(
    process_type = c("transmission", "birth", "death"),
    stringsAsFactors = FALSE
  )
  stopifnot(isTRUE(has_demography(fd_demo)))

  fd_no <- data.frame(
    process_type = c("transmission", "recovery"),
    stringsAsFactors = FALSE
  )
  stopifnot(!has_demography(fd_no))

  # ── get_species ───────────────────────────────────────────────────────────
  cd <- data.frame(
    species = c("human", "vector", "human"),
    stringsAsFactors = FALSE
  )
  sp <- get_species(cd)
  stopifnot(length(sp) == 2)
  stopifnot("human" %in% sp && "vector" %in% sp)

  # ── build_dfe_table : SIR sans démographie ────────────────────────────────
  comp <- data.frame(
    state_id   = c("S_h", "I_h", "R_h"),
    label      = c("Susceptible", "Infectious", "Recovered"),
    state_type = c("susceptible", "infectious", "recovered"),
    species    = c("human", "human", "human"),
    stringsAsFactors = FALSE
  )
  flux <- data.frame(
    flux_id         = c("F1","F2"),
    from_state_id   = c("S_h","I_h"),
    to_state_id     = c("I_h","R_h"),
    process_type    = c("transmission","recovery"),
    rate_name       = c("beta_c","gamma_r"),
    rate_expression = c("beta_c*S_h*I_h/Ntot","gamma_r*I_h"),
    stringsAsFactors = FALSE
  )

  dfe <- build_dfe_table(comp, flux)
  stopifnot(nrow(dfe) == 3)

  # S_h sans démographie mono-espèce → Ntot auto
  row_S <- dfe[dfe$state_id == "S_h", ]
  stopifnot(row_S$dfe_expr == "Ntot")
  stopifnot(isTRUE(row_S$dfe_auto))

  # I_h → 0 auto
  row_I <- dfe[dfe$state_id == "I_h", ]
  stopifnot(row_I$dfe_expr == "0")
  stopifnot(isTRUE(row_I$dfe_auto))

  # R_h → 0 auto
  row_R <- dfe[dfe$state_id == "R_h", ]
  stopifnot(row_R$dfe_expr == "0")
  stopifnot(isTRUE(row_R$dfe_auto))

  # ── collect_model_params ──────────────────────────────────────────────────
  p <- collect_model_params(flux, comp)
  stopifnot("beta_c" %in% p)
  stopifnot("gamma_r" %in% p)
  stopifnot("Ntot" %in% p)
  stopifnot(!"S_h" %in% p)  # compartiment → pas un paramètre

  # ── dfe_type_badge_color ──────────────────────────────────────────────────
  stopifnot(dfe_type_badge_color("susceptible") == "#cce5ff")
  stopifnot(dfe_type_badge_color("unknown_type") == "#e9ecef")

  # ── build_sympy_context ───────────────────────────────────────────────────
  dfe_df <- build_dfe_table(comp, flux)
  params_df <- data.frame(
    param_name = c("beta_c","gamma_r","Ntot"),
    description = c("","",""), units = c("","",""),
    category = c("flux_only","flux_only","flux_only"),
    stringsAsFactors = FALSE
  )
  ctx <- build_sympy_context(comp, flux, dfe_df, params_df)

  stopifnot("I_h" %in% ctx$infected_states)
  stopifnot(!is.null(ctx$fi_exprs[["I_h"]]))
  stopifnot(!is.null(ctx$vi_exprs[["I_h"]]))
  stopifnot(grepl("beta_c", ctx$fi_exprs[["I_h"]]))
  stopifnot(grepl("gamma_r", ctx$vi_exprs[["I_h"]]))
  stopifnot(ctx$dfe_subs[["S_h"]] == "Ntot")
  stopifnot(ctx$dfe_subs[["I_h"]] == "0")
})

# message("M05_dfe_logic.R charg\u00e9 -- OK")
