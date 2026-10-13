# =============================================================================
# M06_symbolic_ngm.R -- Table symbolique NGM, diagnostics, helpers sensibilité
# =============================================================================
# Dépendances : M01_constants.R, M02_validation_sympy.R
# Testé seul  : source("M01_constants.R")
#               source("M02_validation_sympy.R")
#               source("M06_symbolic_ngm.R")
# =============================================================================


# =============================================================================
# SECTION 1 -- Construction de la table symbolique (cœur du NGM)
# =============================================================================

#' Construire la table symbolique à partir des compartiments et des flux
#'
#' Pour chaque flux, détermine :
#'   is_F       : flux de nouvelle infection (vers compartiment infecté,
#'                process_type dans F_PROCESS_TYPES)
#'   is_V       : flux de transition depuis compartiment infecté (!is_F)
#'   is_entry   : entrée dans la classe infectée depuis non-infecté
#'   is_empty   : flux sans influence sur NGM (outflow non-infectieux)
#'   dfe_zero   : le flux est nul au DFE (dépend d'un infecté)
#'   dfe_positive: le flux est positif au DFE (démographique/constant)
#'
#' Note : dfe_zero et dfe_positive sont mutuellement exclusifs.
#'
#' @param comp data.frame -- table des compartiments (init_comp)
#' @param flux data.frame -- table des flux (init_flux)
#' @return data.frame avec toutes les colonnes de flux + colonnes NGM
build_symbolic_table <- function(comp, flux) {

  # ── Schéma vide retourné si aucun flux ────────────────────────────────────
  empty <- data.frame(
    flux_id        = character(0),
    from_state_id  = character(0),
    to_state_id    = character(0),
    process_type   = character(0),
    rate_name      = character(0),
    rate_expression= character(0),
    is_entry       = logical(0),
    is_F           = logical(0),
    is_V           = logical(0),
    is_empty       = logical(0),
    dfe_zero       = logical(0),
    dfe_positive   = logical(0),
    stringsAsFactors = FALSE
  )

  if (nrow(flux) == 0) return(empty)

  # ── Copier les colonnes utiles depuis flux ─────────────────────────────────
  result <- flux[, c(
    "flux_id", "from_state_id", "to_state_id",
    "process_type", "rate_name", "rate_expression"
  )]

  # Initialiser toutes les colonnes NGM à FALSE
  result$is_entry    <- FALSE
  result$is_F        <- FALSE
  result$is_V        <- FALSE
  result$is_empty    <- FALSE
  result$dfe_zero    <- FALSE
  result$dfe_positive<- FALSE

  if (nrow(comp) == 0) return(result)

  # ── Classifier chaque flux ─────────────────────────────────────────────────
  for (i in seq_len(nrow(result))) {

    fid <- result$from_state_id[i]
    tid <- result$to_state_id[i]
    pt  <- if (is.na(result$process_type[i])) "" else result$process_type[i]

    # Ignorer si source manquante (flux mal configuré)
    if (is.na(fid) || fid == "") next

    # ── Flux sortant (to = "" → mort, émigration, etc.) ───────────────────
    if (is.na(tid) || tid == "") {
      fr <- comp[comp$state_id == fid, ]
      if (nrow(fr) == 0) next
      fst <- fr$state_type[1]

      if (fst %in% INFECTED_STATE_TYPES) {
        # Sortie d'un compartiment infectieux → contribue à V
        result$is_V[i]       <- TRUE
        result$dfe_zero[i]   <- TRUE   # nul au DFE (infecté = 0)
        result$dfe_positive[i] <- FALSE
      } else {
        # Sortie d'un compartiment non-infectieux → hors NGM
        result$is_empty[i]    <- TRUE
        result$dfe_zero[i]    <- FALSE
        result$dfe_positive[i]<- FALSE
      }
      next
    }

    # ── Flux normal : from → to (les deux non vides) ──────────────────────
    fr <- comp[comp$state_id == fid, ]
    tr <- comp[comp$state_id == tid, ]
    if (nrow(fr) == 0 || nrow(tr) == 0) next

    fst <- fr$state_type[1]
    tst <- tr$state_type[1]

    # is_F : transmission VERS un compartiment infecté
    result$is_F[i] <- (tst %in% INFECTED_STATE_TYPES) && (pt %in% F_PROCESS_TYPES)

    # is_V : transition DEPUIS un compartiment infecté (pas is_F)
    result$is_V[i] <- (fst %in% INFECTED_STATE_TYPES) && !result$is_F[i]

    # is_entry : entrée dans la classe infectée depuis non-infecté
    result$is_entry[i] <- (tst %in% INFECTED_STATE_TYPES) &&
                          !(fst %in% INFECTED_STATE_TYPES)

    result$is_empty[i] <- FALSE

    # DFE : un flux est nul si from ou is_F dépend d'infectés
    flux_depends_on_infected <- (fst %in% INFECTED_STATE_TYPES) || result$is_F[i]
    result$dfe_zero[i]     <- flux_depends_on_infected
    result$dfe_positive[i] <- !flux_depends_on_infected  # mutuellement exclusif
  }

  return(result)
}


# =============================================================================
# SECTION 2 -- Hash de rv_flux() pour détecter les changements
# =============================================================================

#' Calculer un hash léger de la table des flux
#'
#' Utilisé pour détecter si rv_flux() a changé depuis le dernier
#' recalcul de la table symbolique (symbolic_stale reactive).
#'
#' @param fd data.frame -- table des flux
#' @return character(1) -- hash ou "" si table vide
flux_hash <- function(fd) {
  if (is.null(fd) || nrow(fd) == 0) return("")
  paste(
    apply(
      fd[, c("flux_id", "from_state_id", "to_state_id",
             "process_type", "rate_expression")],
      1, paste, collapse = "|"
    ),
    collapse = "||"
  )
}


# =============================================================================
# SECTION 3 -- Diagnostics du modèle
# =============================================================================

#' Lancer les diagnostics du modèle
#'
#' Analyse la cohérence structurelle : compartiments orphelins, flux
#' référençant des compartiments inexistants, expressions manquantes,
#' paramètres non documentés, etc.
#'
#' @param comp_df   data.frame -- compartiments
#' @param flux_df   data.frame -- flux
#' @param sym_df    data.frame -- table symbolique (ou NULL)
#' @param params_df data.frame -- paramètres
#' @param dfe_df    data.frame -- DFE (ou NULL)
#' @return list(issues, n_errors, n_warnings, n_infos, global_ok, summary)
run_diagnostics <- function(comp_df, flux_df, sym_df, params_df,
                             dfe_df = NULL) {

  issues <- list()  # chaque élément : list(level, code, title, detail, items)

  comp_ids    <- comp_df$state_id
  flux_from   <- flux_df$from_state_id
  flux_to     <- flux_df$to_state_id
  param_names <- if (nrow(params_df) > 0) params_df$param_name else character(0)

  # Toutes les expressions rate non vides
  all_exprs <- flux_df$rate_expression[
    !is.na(flux_df$rate_expression) &
    trimws(flux_df$rate_expression) != ""
  ]

  # ── Erreurs bloquantes ────────────────────────────────────────────────────

  # 1. Compartiments orphelins (aucun flux entrant ni sortant)
  involved <- unique(c(
    flux_from[!is.na(flux_from) & flux_from != ""],
    flux_to  [!is.na(flux_to)   & flux_to   != ""]
  ))
  orphans <- comp_ids[!comp_ids %in% involved]
  if (length(orphans) > 0)
    issues[[length(issues) + 1]] <- list(
      level  = "error",
      code   = "orphan_comp",
      title  = "Compartiments orphelins (aucun flux)",
      detail = paste0(
        "Ces compartiments ne sont connectés à aucun flux ",
        "-- ils n'influencent pas la dynamique."
      ),
      items  = orphans
    )

  # 2. Flux référençant un compartiment inexistant
  bad_from <- flux_df$flux_id[
    !is.na(flux_from) & flux_from != "" & !flux_from %in% comp_ids
  ]
  bad_to <- flux_df$flux_id[
    !is.na(flux_to) & flux_to != "" & !flux_to %in% comp_ids
  ]
  bad_refs <- unique(c(bad_from, bad_to))
  if (length(bad_refs) > 0)
    issues[[length(issues) + 1]] <- list(
      level  = "error",
      code   = "bad_ref",
      title  = "Flux avec compartiment inexistant",
      detail = paste0(
        "Ces flux pointent vers un compartiment qui n'existe plus ",
        "(probablement supprimé)."
      ),
      items  = bad_refs
    )

  # 3. Flux sans rate_expression
  no_expr <- flux_df$flux_id[
    is.na(flux_df$rate_expression) |
    trimws(flux_df$rate_expression) == ""
  ]
  if (length(no_expr) > 0)
    issues[[length(issues) + 1]] <- list(
      level  = "error",
      code   = "no_expr",
      title  = "Flux sans expression mathématique",
      detail = paste0(
        "Ces flux n'ont pas de rate_expression configurée ",
        "-- ils seront ignorés dans les ODE et le calcul NGM."
      ),
      items  = no_expr
    )

  # 4. Variables inconnues dans les expressions
  unknown_vars <- list()
  for (i in seq_len(nrow(flux_df))) {
    expr <- flux_df$rate_expression[i]
    if (is.na(expr) || trimws(expr) == "") next
    free <- extract_free_params(expr, c(comp_ids, param_names))
    # Exclure les noms de population courants (Ntot, N_h, N_v, N1…)
    free <- free[!grepl("^(Ntot|N_[a-z]+|N[0-9]+)$", free)]
    if (length(free) > 0)
      unknown_vars[[flux_df$flux_id[i]]] <- free
  }
  if (length(unknown_vars) > 0) {
    detail_items <- paste0(
      names(unknown_vars), " : {",
      sapply(unknown_vars, paste, collapse = ", "), "}"
    )
    issues[[length(issues) + 1]] <- list(
      level  = "error",
      code   = "unknown_vars",
      title  = "Variables non reconnues dans les expressions",
      detail = paste0(
        "Ces tokens ne sont ni des compartiments ni des paramètres ",
        "documentés. Faute de frappe probable."
      ),
      items  = detail_items
    )
  }

  # 5. Compartiments infectieux sans flux is_F entrant
  # Dans NGM, seuls certains infectieux recoivent des flux is_F (ex: E_h dans SEIR).
  # D'autres (ex: I_h) recoivent des progressions is_V -- c'est normal.
  # ERREUR : aucun flux is_F dans tout le modele (R0 sera forcement 0).
  # WARNING : un infectieux n'a ni is_F ni is_V entrant (vraiment isole).
  if (!is.null(sym_df) && nrow(sym_df) > 0) {
    inf_comp  <- comp_df$state_id[
      comp_df$state_type %in% INFECTED_STATE_TYPES]
    is_F_vec  <- vapply(sym_df$is_F, isTRUE, logical(1))
    is_V_vec  <- vapply(sym_df$is_V, isTRUE, logical(1))
    has_F_to  <- unique(sym_df$to_state_id[is_F_vec])
    has_V_to  <- unique(sym_df$to_state_id[is_V_vec])
    has_any_to <- union(has_F_to, has_V_to)

    # Erreur : aucun flux is_F dans tout le modele
    if (length(has_F_to) == 0 && length(inf_comp) > 0)
      issues[[length(issues) + 1]] <- list(
        level  = "error",
        code   = "no_F_at_all",
        title  = "Aucun flux de transmission (is_F) detecte",
        detail = paste0(
          "Le modele ne contient aucun flux de nouvelle infection. ",
          "R₀ sera 0. Verifiez que les flux de transmission ont ",
          "process_type = 'transmission'."),
        items  = character(0))

    # Warning : infectieux sans aucun flux entrant (ni F ni V)
    inf_isolated <- inf_comp[!inf_comp %in% has_any_to]
    if (length(inf_isolated) > 0)
      issues[[length(issues) + 1]] <- list(
        level  = "warning",
        code   = "inf_no_input",
        title  = "Compartiments infectieux sans flux entrant",
        detail = paste0(
          "Ces compartiments infectieux ne recoivent ni flux de transmission ",
          "(is_F) ni flux de progression (is_V). ",
          "Verifiez les process_type de leurs flux entrants."),
        items  = inf_isolated)
  }

  # ── Avertissements ────────────────────────────────────────────────────────

  # 6. Auto-flux (from == to)
  self_flux <- flux_df$flux_id[
    !is.na(flux_from) & !is.na(flux_to) &
    flux_from == flux_to & flux_from != ""
  ]
  if (length(self_flux) > 0)
    issues[[length(issues) + 1]] <- list(
      level  = "warning",
      code   = "self_flux",
      title  = "Auto-flux détectés (source = destination)",
      detail = paste0(
        "Ces flux ont le même compartiment source et destination. ",
        "Ils apparaîtront dans les deux membres de l'ODE. ",
        "Vérifiez que c'est intentionnel."
      ),
      items  = self_flux
    )

  # 7. Compartiments puits purs (sans flux entrant, hors types naturels)
  # "susceptible" est souvent sans flux entrant dans les modeles sans demographie
  # "recovered", "immune", "dead" sont naturellement des puits
  natural_sink_types <- c("dead", "recovered", "immune", "treated",
                          "susceptible")
  natural_sink_ids   <- comp_df$state_id[
    comp_df$state_type %in% natural_sink_types
  ]
  has_in    <- unique(flux_to[!is.na(flux_to) & flux_to != ""])
  sink_only <- comp_ids[
    !comp_ids %in% has_in & !comp_ids %in% natural_sink_ids
  ]
  if (length(sink_only) > 0)
    issues[[length(issues) + 1]] <- list(
      level  = "warning",
      code   = "sink_only",
      title  = "Compartiments sans flux entrant (puits purs)",
      detail = paste0(
        "Ces compartiments ne reçoivent aucun flux ",
        "(hors types 'dead','recovered','immune' qui sont naturellement ",
        "des puits)."
      ),
      items  = sink_only
    )

  # 8. Compartiments sources pures (sans flux sortant, hors types naturels)
  dead_ids             <- comp_df$state_id[
    !is.na(comp_df$state_type) & comp_df$state_type == "dead"
  ]
  # Types naturellement sans flux sortant (compartiments absorbants)
  natural_sink_out_types <- c("dead", "recovered", "immune")
  natural_sink_out_ids   <- comp_df$state_id[
    !is.na(comp_df$state_type) &
    comp_df$state_type %in% natural_sink_out_types
  ]
  natural_source_ids   <- comp_df$state_id[
    comp_df$state_type %in% c("susceptible")
  ]
  has_out     <- unique(flux_from[!is.na(flux_from) & flux_from != ""])
  source_only <- comp_ids[
    !comp_ids %in% has_out &
    !comp_ids %in% dead_ids &
    !comp_ids %in% natural_source_ids &
    !comp_ids %in% natural_sink_out_ids
  ]
  if (length(source_only) > 0)
    issues[[length(issues) + 1]] <- list(
      level  = "warning",
      code   = "source_only",
      title  = "Compartiments sans flux sortant (sources pures)",
      detail = paste0(
        "Ces compartiments n'ont aucun flux sortant ",
        "(hors types 'susceptible' et 'dead')."
      ),
      items  = source_only
    )

  # 9. Flux en doublon (même from + to + process_type)
  if (nrow(flux_df) > 1) {
    key      <- paste(flux_from, flux_to, flux_df$process_type, sep = "|")
    dup_keys <- key[
      duplicated(key) & !is.na(flux_from) & flux_from != ""
    ]
    if (length(dup_keys) > 0) {
      dup_flux <- flux_df$flux_id[key %in% dup_keys]
      issues[[length(issues) + 1]] <- list(
        level  = "warning",
        code   = "dup_flux",
        title  = "Flux en doublon (même source, destination et processus)",
        detail = paste0(
          "Ces paires de flux semblent redondantes. ",
          "Si intentionnel (deux mécanismes distincts), ignorez."
        ),
        items  = dup_flux
      )
    }
  }

  # ── Informations ──────────────────────────────────────────────────────────

  # 10. Paramètres libres utilisés mais non documentés dans rv_params
  all_free <- unique(unlist(lapply(all_exprs, function(e)
    extract_free_params(e, c(comp_ids, param_names))
  )))
  all_free <- all_free[!grepl("^(Ntot|N_[a-z]+|N[0-9]+)$", all_free)]
  if (length(all_free) > 0)
    issues[[length(issues) + 1]] <- list(
      level  = "info",
      code   = "undoc_params",
      title  = "Param\u00e8tres utilis\u00e9s non document\u00e9s",
      detail = paste0(
        "Ces param\u00e8tres apparaissent dans les expressions ",
        "mais n'ont pas de description dans l'onglet Param\u00e8tres."
      ),
      items  = all_free
    )

  # 11. Diagnostic intelligent des paramètres (avec dfe_df)
  if (length(param_names) > 0 && !is.null(dfe_df)) {
    classification <- classify_params(param_names, flux_df, comp_df, dfe_df)

    truly_unused <- classification$param_name[
      !classification$in_flux & !classification$in_dfe
    ]
    if (length(truly_unused) > 0)
      issues[[length(issues) + 1]] <- list(
        level  = "warning",
        code   = "truly_unused_params",
        title  = "Param\u00e8tres d\u00e9finis mais jamais utilis\u00e9s",
        detail = paste0(
          "Ces param\u00e8tres n'apparaissent ni dans les flux, ",
          "ni dans le DFE. Ils sont peut-\u00eatre inutiles ou oubli\u00e9s."
        ),
        items  = truly_unused
      )

    dfe_only_params <- classification$param_name[
      classification$category == "dfe_only"
    ]
    if (length(dfe_only_params) > 0)
      issues[[length(issues) + 1]] <- list(
        level  = "info",
        code   = "dfe_only_params",
        title  = "Param\u00e8tres d\u00e9finis uniquement au DFE",
        detail = paste0(
          "Ces param\u00e8tres apparaissent dans les expressions DFE ",
          "mais pas dans les flux. Normal pour N_v, N_h, Lambda, etc."
        ),
        items  = dfe_only_params
      )

  } else if (length(param_names) > 0 && is.null(dfe_df)) {
    # Fallback sans dfe_df
    all_tokens <- unique(unlist(lapply(all_exprs, function(e)
      unlist(regmatches(e, gregexpr("[A-Za-z_][A-Za-z0-9_]*", e)))
    )))
    unused_params <- param_names[!param_names %in% all_tokens]
    if (length(unused_params) > 0)
      issues[[length(issues) + 1]] <- list(
        level  = "info",
        code   = "unused_params",
        title  = "Param\u00e8tres document\u00e9s absents des flux",
        detail = "Ces param\u00e8tres ne sont pas dans les flux (peuvent \u00eatre dans le DFE).",
        items  = unused_params
      )
  }

  # ── Résumé ────────────────────────────────────────────────────────────────
  n_errors <- 0L; n_warnings <- 0L; n_infos <- 0L
  if (length(issues) > 0) {
    levels_vec <- vapply(issues, function(x) as.character(x$level), character(1))
    n_errors   <- sum(levels_vec == "error")
    n_warnings <- sum(levels_vec == "warning")
    n_infos    <- sum(levels_vec == "info")
  }

  n_species  <- if (nrow(comp_df) > 0)
    length(unique(comp_df$species[!is.na(comp_df$species)])) else 0L
  n_infected <- if (nrow(comp_df) > 0)
    sum(comp_df$state_type %in% INFECTED_STATE_TYPES) else 0L

  list(
    issues     = issues,
    n_errors   = n_errors,
    n_warnings = n_warnings,
    n_infos    = n_infos,
    global_ok  = n_errors == 0,
    summary    = list(
      n_comp     = nrow(comp_df),
      n_flux     = nrow(flux_df),
      n_params   = nrow(params_df),
      n_species  = n_species,
      n_infected = n_infected
    )
  )
}


# =============================================================================
# SECTION 4 -- Classifier les paramètres (flux / DFE / both)
# =============================================================================

#' Classifier les paramètres selon leur présence dans flux et/ou DFE
#'
#' @param params   character -- noms de paramètres à classifier
#' @param flux_df  data.frame -- table des flux
#' @param comp_df  data.frame -- table des compartiments
#' @param dfe_df   data.frame -- table DFE
#' @return data.frame(param_name, in_flux, in_dfe, category)
classify_params <- function(params, flux_df, comp_df, dfe_df) {

  # Présence dans les flux
  in_flux <- sapply(params, function(p) {
    if (nrow(flux_df) == 0) return(FALSE)
    pattern <- paste0(
      "\\b",
      gsub("([.|()\\^{}+$*?[])", "\\\\\\1", p),
      "\\b"
    )
    nrow(flux_df) > 0 && any(vapply(flux_df$rate_expression, function(e) {
      if (is.na(e)) return(FALSE)
      isTRUE(grepl(pattern, e))
    }, logical(1)))
  })

  # Présence dans les expressions DFE
  in_dfe <- sapply(params, function(p) {
    if (is.null(dfe_df) || nrow(dfe_df) == 0) return(FALSE)
    pattern <- paste0(
      "\\b",
      gsub("([.|()\\^{}+$*?[])", "\\\\\\1", p),
      "\\b"
    )
    nrow(dfe_df) > 0 && any(vapply(dfe_df$dfe_expr, function(e) {
      if (is.na(e)) return(FALSE)
      isTRUE(grepl(pattern, e))
    }, logical(1)))
  })

  category <- ifelse(
    in_flux & in_dfe, "both",
    ifelse(in_flux, "flux_only", "dfe_only")
  )

  data.frame(
    param_name = params,
    in_flux    = in_flux,
    in_dfe     = in_dfe,
    category   = category,
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# SECTION 5 -- Helpers analyse de sensibilité
# =============================================================================

#' Détecter le type d'effet mathématique depuis l'élasticité
#'
#' @param elasticity numeric -- valeur d'élasticité normalisée
#' @return character -- "quadratic" | "linear" | "square_root" |
#'                     "cubic" | "weak" | "other"
detect_effect_type <- function(elasticity) {
  abs_elas <- abs(elasticity)
  if      (abs_elas > 1.8 && abs_elas < 2.2) "quadratic"
  else if (abs_elas > 0.9 && abs_elas < 1.1) "linear"
  else if (abs_elas > 0.4 && abs_elas < 0.6) "square_root"
  else if (abs_elas > 2.8 && abs_elas < 3.2) "cubic"
  else if (abs_elas > 0.15 && abs_elas < 0.4) "weak"
  else "other"
}

#' Générer une interprétation mécanistique de l'élasticité
#'
#' @param param       character -- nom du paramètre
#' @param elasticity  numeric
#' @param effect_type character -- résultat de detect_effect_type()
#' @return character -- message d'interprétation
generate_interpretation <- function(param, elasticity, effect_type) {
  switch(effect_type,
    quadratic = sprintf(
      "Le param\u00e8tre '%s' a une **d\u00e9pendance quadratique** (\u03b5 \u2248 2). R\u2080 contient probablement %s\u00b2.",
      param, param
    ),
    linear = sprintf(
      "Le param\u00e8tre '%s' a une **d\u00e9pendance lin\u00e9aire** (\u03b5 \u2248 1). R\u2080 est directement proportionnel \u00e0 %s.",
      param, param
    ),
    square_root = sprintf(
      "Le param\u00e8tre '%s' a une **d\u00e9pendance en racine carr\u00e9e** (\u03b5 \u2248 0.5). R\u2080 contient probablement \u221a%s.",
      param, param
    ),
    cubic = sprintf(
      "Le param\u00e8tre '%s' a une **d\u00e9pendance cubique** (\u03b5 \u2248 3). R\u2080 contient probablement %s\u00b3.",
      param, param
    ),
    weak = sprintf(
      "Le param\u00e8tre '%s' a une **faible influence** (|\u03b5| < 0.4). Priorit\u00e9 basse pour l'intervention.",
      param
    ),
    sprintf("Le param\u00e8tre '%s' a une \u00e9lasticit\u00e9 \u03b5 = %.2f.", param, elasticity)
  )
}

#' Calculer l'impact en % sur R0 d'un changement en % du paramètre
#'
#' @param elasticity     numeric
#' @param percent_change numeric -- fraction (0.25 = 25%)
#' @return numeric -- impact en pourcentage sur R0
calculate_impact <- function(elasticity, percent_change = 0.25) {
  if (elasticity > 0) {
    impact <- 1 - (1 - percent_change)^abs(elasticity)
  } else {
    impact <- 1 - (1 + percent_change)^abs(elasticity)
  }
  impact * 100
}

#' Générer une recommandation d'intervention
#'
#' @param param       character -- nom du paramètre
#' @param elasticity  numeric
#' @param effect_type character (non utilisé mais conservé pour compatibilité)
#' @return character -- recommandation
generate_control_recommendation <- function(param, elasticity,
                                            effect_type = "") {
  action         <- if (elasticity > 0) "Réduire" else "Augmenter"
  direction_word <- if (elasticity > 0) "diminue" else "augmente"
  impact_25      <- calculate_impact(elasticity, 0.25)
  # paste0 au lieu de sprintf : evite la confusion avec les caracteres
  # Unicode (→, ₀) interpretes comme des specificateurs de format
  paste0(action, " '", param, "' de 25% → R₀ ",
         direction_word, " de ", round(impact_25, 1), "%")
}


# =============================================================================
# SECTION 6 -- Vérifications au chargement
# =============================================================================

stopifnot(
  exists("INFECTED_STATE_TYPES"),
  exists("F_PROCESS_TYPES"),
  exists("extract_free_params"),   # M02 chargé
  exists("init_comp"),             # M01 chargé
  exists("init_flux")
)

local({

  # ── Test build_symbolic_table : modèle SIR minimal ───────────────────────
  comp <- data.frame(
    state_id   = c("S", "I", "R"),
    state_type = c("susceptible", "infectious", "recovered"),
    stringsAsFactors = FALSE
  )
  flux <- data.frame(
    flux_id         = c("F1", "F2"),
    from_state_id   = c("S",  "I"),
    to_state_id     = c("I",  "R"),
    process_type    = c("transmission", "recovery"),
    rate_name       = c("beta", "gamma"),
    rate_expression = c("beta*S*I/N", "gamma*I"),
    stringsAsFactors = FALSE
  )

  sym <- build_symbolic_table(comp, flux)

  stopifnot(nrow(sym) == 2)

  # F1 : S→I transmission → is_F = TRUE
  f1 <- sym[sym$flux_id == "F1", ]
  stopifnot(isTRUE(f1$is_F), !isTRUE(f1$is_V))
  stopifnot(isTRUE(f1$dfe_zero))   # dépend de l'infecté

  # F2 : I→R recovery → is_V = TRUE
  f2 <- sym[sym$flux_id == "F2", ]
  stopifnot(!isTRUE(f2$is_F), isTRUE(f2$is_V))
  stopifnot(isTRUE(f2$dfe_zero))   # depuis infecté

  # ── Test build_symbolic_table : flux vide ─────────────────────────────────
  sym_empty <- build_symbolic_table(comp, init_flux)
  stopifnot(nrow(sym_empty) == 0)

  # ── Test flux_hash ─────────────────────────────────────────────────────────
  h1 <- flux_hash(flux)
  h2 <- flux_hash(flux)
  stopifnot(identical(h1, h2))       # reproductible
  stopifnot(flux_hash(init_flux) == "")  # vide → ""

  # ── Test detect_effect_type ───────────────────────────────────────────────
  stopifnot(detect_effect_type(1.0)  == "linear")
  stopifnot(detect_effect_type(2.0)  == "quadratic")
  stopifnot(detect_effect_type(0.5)  == "square_root")
  stopifnot(detect_effect_type(0.05) == "other")

  # ── Test calculate_impact ─────────────────────────────────────────────────
  # ε = 1 (linéaire) : -25% paramètre → -25% R0
  stopifnot(abs(calculate_impact(1, 0.25) - 25) < 0.01)
  # ε = 2 (quadratique) : -25% paramètre → ~44% R0
  stopifnot(abs(calculate_impact(2, 0.25) - 43.75) < 0.01)

  # ── Test run_diagnostics : modèle SIR propre ─────────────────────────────
  comp_full <- data.frame(
    state_id   = c("S_h", "I_h", "R_h"),
    state_type = c("susceptible", "infectious", "recovered"),
    species    = c("human", "human", "human"),
    stringsAsFactors = FALSE
  )
  flux_full <- data.frame(
    flux_id         = c("F1", "F2"),
    from_state_id   = c("S_h", "I_h"),
    to_state_id     = c("I_h", "R_h"),
    process_type    = c("transmission", "recovery"),
    rate_name       = c("beta_c", "gamma_r"),
    rate_expression = c("beta_c*S_h*I_h/Ntot", "gamma_r*I_h"),
    stringsAsFactors = FALSE
  )
  params_full <- data.frame(
    param_name = c("beta_c", "gamma_r", "Ntot"),
    description = c("", "", ""),
    units = c("", "", ""),
    category = c("flux_only", "flux_only", "flux_only"),
    stringsAsFactors = FALSE
  )
  sym_full <- build_symbolic_table(comp_full, flux_full)
  diag <- run_diagnostics(comp_full, flux_full, sym_full, params_full)

  stopifnot(diag$n_errors == 0)     # modèle SIR propre = pas d'erreur
  stopifnot(diag$global_ok)
  stopifnot(diag$summary$n_comp == 3)
  stopifnot(diag$summary$n_infected == 1)  # I_h seulement
})

# message("M06_symbolic_ngm.R charg\u00e9 -- OK")
