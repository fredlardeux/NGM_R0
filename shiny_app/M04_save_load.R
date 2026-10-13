# =============================================================================
# M04_save_load.R -- Sérialisation / désérialisation JSON des modèles
# =============================================================================
# Dépendances : M01_constants.R (init_comp, init_flux, init_dfe,
#                                init_params, APP_VERSION, %||%)
# Packages    : jsonlite (doit être installé)
# Testé seul  : source("M01_constants.R"); source("M04_save_load.R")
# =============================================================================


# =============================================================================
# SECTION 1 -- Conversion data.frame ↔ liste JSON
# =============================================================================

#' Convertir un data.frame en liste JSON-safe
#'
#' NA → NULL, logical → bool, numeric → numeric, autres → character.
#' Compatible avec jsonlite::toJSON(auto_unbox = TRUE).
#'
#' @param df data.frame
#' @return list de listes (une par ligne)
df_to_json_list <- function(df) {
  lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    lapply(row, function(v) {
      if (length(v) == 1 && is.na(v)) return(NULL)
      if (is.logical(v))  return(as.logical(v))
      if (is.numeric(v))  return(as.numeric(v))
      as.character(v)
    })
  })
}

#' Reconstruire un data.frame depuis une liste JSON parsée
#'
#' Force les types d'après un data.frame de référence (template).
#' Les colonnes absentes du JSON sont remplies avec la valeur par défaut
#' de la première ligne du template (ou NA si le template est vide).
#'
#' @param lst         list -- liste de listes (une par ligne)
#' @param template_df data.frame -- schéma de référence
#' @return data.frame
json_list_to_df <- function(lst, template_df) {
  if (length(lst) == 0) return(template_df[0, ])

  rows <- lapply(lst, function(row) {
    r <- as.data.frame(
      lapply(names(template_df), function(col) {
        v <- row[[col]]

        # Valeur absente ou NULL → valeur par défaut du template
        if (is.null(v)) {
          if (nrow(template_df) > 0) {
            return(template_df[[col]][1])
          } else {
            v <- NA
          }
        }

        # Forcer le type d'après le template
        if (is.logical(template_df[[col]])) return(isTRUE(as.logical(v)))
        if (is.numeric(template_df[[col]]))  return(as.numeric(v))
        as.character(ifelse(is.na(v), "", v))
      }),
      stringsAsFactors = FALSE
    )
    names(r) <- names(template_df)
    r
  })

  do.call(rbind, rows)
}


# =============================================================================
# SECTION 2 -- Construction de la liste modèle à sérialiser
# =============================================================================

#' Construire la liste complète du modèle pour sérialisation JSON
#'
#' @param model_name character(1) -- nom du modèle
#' @param comp_df    data.frame -- compartiments
#' @param flux_df    data.frame -- flux
#' @param sp_list    character  -- liste des espèces
#' @param dfe_df     data.frame -- DFE
#' @param params_df  data.frame -- paramètres
#' @return list -- structure complète prête pour jsonlite::toJSON()
build_model_list <- function(model_name, comp_df, flux_df,
                             sp_list, dfe_df, params_df) {
  list(
    meta = list(
      model_name  = model_name,
      saved_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      app_version = APP_VERSION
    ),
    species_list = as.list(sp_list),
    compartments = df_to_json_list(comp_df),
    fluxes       = df_to_json_list(flux_df),
    dfe          = df_to_json_list(dfe_df),
    params       = df_to_json_list(params_df)
  )
}


# =============================================================================
# SECTION 3 -- Chargement et validation depuis liste JSON parsée
# =============================================================================

#' Valider et charger un modèle depuis une liste JSON parsée
#'
#' Reconstruit les data.frames en forçant les types d'après les templates
#' de M01. Retourne une liste avec $ok = TRUE/FALSE.
#'
#' @param lst list -- résultat de jsonlite::fromJSON(..., simplifyVector=FALSE)
#' @return list(ok, errors, model_name, comp_df, flux_df,
#'              dfe_df, params_df, dfe_validated, species_list)
load_model_list <- function(lst) {
  errors <- character(0)

  # ── Vérifications minimales ───────────────────────────────────────────────
  if (is.null(lst$meta))
    errors <- c(errors, "Champ 'meta' manquant.")
  if (is.null(lst$compartments))
    errors <- c(errors, "Champ 'compartments' manquant.")
  if (is.null(lst$fluxes))
    errors <- c(errors, "Champ 'fluxes' manquant.")

  if (length(errors) > 0)
    return(list(ok = FALSE, errors = errors))

  # ── Reconstruction des data.frames ────────────────────────────────────────
  comp_df <- tryCatch(
    json_list_to_df(lst$compartments, init_comp),
    error = function(e) NULL
  )

  flux_df <- tryCatch(
    json_list_to_df(lst$fluxes, init_flux),
    error = function(e) NULL
  )

  dfe_df <- tryCatch({
    if (!is.null(lst$dfe) && length(lst$dfe) > 0)
      json_list_to_df(lst$dfe, init_dfe)
    else
      init_dfe[0, ]
  }, error = function(e) NULL)

  params_df <- tryCatch({
    if (!is.null(lst$params) && length(lst$params) > 0)
      json_list_to_df(lst$params, init_params)
    else
      init_params[0, ]
  }, error = function(e) NULL)

  # ── Erreurs de reconstruction ─────────────────────────────────────────────
  if (is.null(comp_df))
    errors <- c(errors,
      "Impossible de reconstruire la table Compartiments.")
  if (is.null(flux_df))
    errors <- c(errors,
      "Impossible de reconstruire la table Flux.")

  if (length(errors) > 0)
    return(list(ok = FALSE, errors = errors))

  # ── Espèces ───────────────────────────────────────────────────────────────
  sp_list <- unique(c(
    "human", "vector", "animal",
    unlist(lst$species_list %||% list())
  ))

  list(
    ok            = TRUE,
    model_name    = lst$meta$model_name %||% "Modèle chargé",
    comp_df       = comp_df,
    flux_df       = flux_df,
    dfe_df        = dfe_df,
    params_df     = params_df,
    dfe_validated = FALSE,   # toujours revalider après chargement
    species_list  = sp_list
  )
}


# =============================================================================
# SECTION 4 -- Vérifications au chargement
# =============================================================================

stopifnot(
  exists("APP_VERSION"),
  exists("init_comp"),
  exists("init_flux"),
  exists("init_dfe"),
  exists("init_params"),
  exists("%||%"),
  requireNamespace("jsonlite", quietly = TRUE)
)

local({

  # ── df_to_json_list ────────────────────────────────────────────────────────
  df <- data.frame(
    a = 1L, b = "hello", c = TRUE, d = NA_real_,
    stringsAsFactors = FALSE
  )
  lst <- df_to_json_list(df)
  stopifnot(length(lst) == 1)
  stopifnot(lst[[1]]$a == 1)
  stopifnot(lst[[1]]$b == "hello")
  stopifnot(isTRUE(lst[[1]]$c))
  stopifnot(is.null(lst[[1]]$d))  # NA → NULL

  # ── json_list_to_df ────────────────────────────────────────────────────────
  template <- data.frame(
    x = integer(0), y = character(0), z = logical(0),
    stringsAsFactors = FALSE
  )
  lst_in <- list(list(x = 3L, y = "foo", z = TRUE))
  df_out <- json_list_to_df(lst_in, template)
  stopifnot(nrow(df_out) == 1)
  stopifnot(df_out$y[1] == "foo")
  stopifnot(isTRUE(df_out$z[1]))

  # Colonne absente → valeur par défaut template (NA ici car template vide)
  template2 <- data.frame(
    x = 0L, y = "", z = FALSE,
    stringsAsFactors = FALSE
  )
  lst_in2 <- list(list(x = 5L))   # y et z absents
  df_out2 <- json_list_to_df(lst_in2, template2)
  stopifnot(nrow(df_out2) == 1)
  stopifnot(df_out2$x[1] == 5)

  # ── Cycle complet build → JSON → load ─────────────────────────────────────
  comp_test <- data.frame(
    state_id = "S_h", label = "Susceptible", state_type = "susceptible",
    state_comment = "", species = "human", strain = "",
    age_class = "", patch = "", description = "",
    verification = "ok", stringsAsFactors = FALSE
  )
  flux_test <- init_flux   # vide
  dfe_test  <- init_dfe[0, ]
  par_test  <- init_params[0, ]

  built <- build_model_list(
    "test_model", comp_test, flux_test,
    c("human"), dfe_test, par_test
  )

  stopifnot(!is.null(built$meta))
  stopifnot(built$meta$model_name == "test_model")
  stopifnot(built$meta$app_version == APP_VERSION)
  stopifnot(length(built$compartments) == 1)
  stopifnot(length(built$fluxes) == 0)

  # Sérialiser puis désérialiser
  json_str <- jsonlite::toJSON(built,
    pretty = TRUE, auto_unbox = TRUE,
    null = "null", na = "null", force = TRUE
  )
  lst_back <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
  result   <- load_model_list(lst_back)

  stopifnot(isTRUE(result$ok))
  stopifnot(result$model_name == "test_model")
  stopifnot(nrow(result$comp_df) == 1)
  stopifnot(result$comp_df$state_id[1] == "S_h")
  stopifnot(nrow(result$flux_df) == 0)
  stopifnot(!isTRUE(result$dfe_validated))

  # ── load_model_list : erreur si champs manquants ──────────────────────────
  r_bad <- load_model_list(list(meta = list(model_name = "x")))
  stopifnot(!isTRUE(r_bad$ok))
  stopifnot(length(r_bad$errors) > 0)
})

# message("M04_save_load.R chargé -- OK")
