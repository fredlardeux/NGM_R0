# =============================================================================
# M03_rate_templates.R -- Templates de taux épidémiologiques
# =============================================================================
# Dépendances : M01_constants.R
# Testé seul  : source("M01_constants.R"); source("M03_rate_templates.R")
# =============================================================================


# =============================================================================
# SECTION 1 -- Table des templates
# =============================================================================

RATE_TEMPLATES <- data.frame(
  template_id = c(
    # A. Transmission directe
    "trans_direct_freq", "trans_direct_dens", "trans_direct_power",
    "trans_direct_sat",  "trans_direct_hollingII",
    # B. Transmission vectorielle
    "trans_vector_simple", "trans_vector_ross",
    "trans_vector_dens",   "trans_vector_sat",
    # C. Transmission environnementale
    "trans_env_linear", "trans_env_sat", "trans_env_dose",
    # D. Progression
    "prog_linear", "prog_multi_stage",
    # E. Recovery
    "rec_linear", "rec_treat",
    # F. Death
    "death_natural", "death_disease",
    # G. Birth
    "birth_const", "birth_prop",
    # H. Waning
    "waning_linear",
    # I. Vaccination
    "vacc_mass", "vacc_pulse",
    # J. Relapse
    "react_linear",
    # K. Superinfection
    "superinfection",
    # L. Treatment
    "treat_start", "treat_success"
  ),

  process_type = c(
    "transmission","transmission","transmission","transmission","transmission",
    "transmission","transmission","transmission","transmission",
    "transmission","transmission","transmission",
    "progression","progression",
    "recovery","recovery",
    "death","death",
    "birth","birth",
    "waning_immunity",
    "vaccination","vaccination",
    "relapse",
    "superinfection",
    "treatment","treatment"
  ),

  transmission_type = c(
    "direct","direct","direct","direct","direct",
    "vector","vector","vector","vector",
    "environmental","environmental","environmental",
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
  ),

  density_type = c(
    "frequency","density","frequency","frequency","frequency",
    "frequency","frequency","density","frequency",
    NA, NA, NA,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
  ),

  # Notes syntaxe :
  #   ^ remplacé par ** (Python/SymPy)
  #   N → Ntot  (N est réservé SymPy)
  #   beta  → beta_c   (beta  est fonction SymPy)
  #   sigma → sigma_p  (sigma est fonction SymPy)
  #   gamma → gamma_r  (gamma est fonction SymPy)
  expression_template = c(
    "beta_c * FROM * TO / Ntot",
    "beta_c * FROM * TO",
    "beta_c * FROM * TO**p / Ntot",
    "beta_c * FROM * TO / (1 + alpha * TO)",
    "beta_c * FROM * TO / (K + TO)",
    "a * b * TO / N_vec",
    "a**2 * b * c * TO / N_vec",
    "a * b * TO",
    "a * b * TO / (1 + alpha * TO)",
    "beta_env * FROM * Env",
    "beta_env * FROM * Env / (K + Env)",
    "beta_env * FROM * (1 - exp(-k_env * Env))",
    "sigma_p * FROM",
    "k_stage * FROM",
    "gamma_r * FROM",
    "gamma_T * FROM",
    "mu_d * FROM",
    "alpha_d * FROM",
    "Lambda",
    "b_birth * Ntot",
    "omega * FROM",
    "nu * FROM",
    "nu_t * FROM",
    "rho * FROM",
    "beta_c2 * FROM * INFECTIOUS",
    "tau * FROM",
    "gamma_T * FROM"
  ),

  label = c(
    "Direct (frequency-dependent)",
    "Direct (density-dependent)",
    "Direct (power law)",
    "Direct (saturating)",
    "Direct (Holling type II)",
    "Vector (simple FOI)",
    "Vector (Ross-MacDonald)",
    "Vector (density-dependent)",
    "Vector (saturating)",
    "Environmental (linear)",
    "Environmental (saturating)",
    "Environmental (dose-response)",
    "Linear progression",
    "Multi-stage (Erlang)",
    "Linear recovery",
    "Treatment recovery",
    "Natural death",
    "Disease death",
    "Constant birth",
    "Proportional birth",
    "Linear waning",
    "Mass vaccination",
    "Pulse vaccination",
    "Linear relapse/reactivation",
    "Superinfection",
    "Treatment initiation",
    "Treatment success"
  ),

  parameters = c(
    "beta_c, Ntot",
    "beta_c",
    "beta_c, p, Ntot",
    "beta_c, alpha",
    "beta_c, K",
    "a, b, N_vec",
    "a, b, c, N_vec",
    "a, b",
    "a, b, alpha",
    "beta_env, Env",
    "beta_env, Env, K",
    "beta_env, k_env, Env",
    "sigma_p",
    "k_stage",
    "gamma_r",
    "gamma_T",
    "mu_d",
    "alpha_d",
    "Lambda",
    "b_birth, Ntot",
    "omega",
    "nu",
    "nu_t",
    "rho",
    "beta_c2, INFECTIOUS",
    "tau",
    "gamma_T"
  ),

  stringsAsFactors = FALSE
)


# =============================================================================
# SECTION 2 -- Filtrage et génération
# =============================================================================

#' Filtrer les templates par type de processus / transmission / densité
#'
#' @param process_type      character(1)
#' @param transmission_type character(1) ou NA
#' @param density_type      character(1) ou NA
#' @return data.frame -- sous-ensemble de RATE_TEMPLATES
filter_templates <- function(process_type,
                             transmission_type = NA,
                             density_type = NA) {
  t <- RATE_TEMPLATES
  t <- t[t$process_type == process_type, ]

  if (!is.na(transmission_type) && transmission_type != "")
    t <- t[is.na(t$transmission_type) |
           t$transmission_type == transmission_type, ]

  if (!is.na(density_type) && density_type != "")
    t <- t[is.na(t$density_type) |
           t$density_type == density_type, ]

  return(t)
}

#' Générer une expression de taux depuis un template
#'
#' Remplace les placeholders FROM, TO, INFECTIOUS par les vrais IDs.
#'
#' @param template_id   character(1)
#' @param from_id       character(1) -- compartiment source
#' @param to_id         character(1) -- compartiment cible
#' @param infectious_ids character   -- compartiments infectieux (pour INFECTIOUS)
#' @return character(1) -- expression générée, ou "" si template inconnu
generate_rate_expression <- function(template_id, from_id, to_id,
                                     infectious_ids = character(0)) {
  tmpl <- RATE_TEMPLATES[RATE_TEMPLATES$template_id == template_id, ]
  if (nrow(tmpl) == 0) return("")

  expr <- tmpl$expression_template[1]
  expr <- gsub("FROM", from_id, expr)
  expr <- gsub("TO",   to_id,   expr)

  if (length(infectious_ids) > 0)
    expr <- gsub("INFECTIOUS", infectious_ids[1], expr)

  return(expr)
}


# =============================================================================
# SECTION 3 -- Sérialisation des descriptions de paramètres
# =============================================================================

#' Encoder une liste nommée param→description en chaîne compacte
#'
#' Format : "param1=desc1||param2=desc2"
#' Stocké dans la colonne rate_param_desc de init_flux.
#'
#' @param lst list nommée character
#' @return character(1)
encode_param_desc <- function(lst) {
  if (length(lst) == 0) return("")
  paste(
    paste0(names(lst), "=",
           gsub("\\|\\|", " ", unlist(lst))),
    collapse = "||"
  )
}

#' Décoder une chaîne compacte en liste nommée param→description
#'
#' @param s character(1) -- chaîne encodée par encode_param_desc()
#' @return list nommée
decode_param_desc <- function(s) {
  if (is.null(s) || is.na(s) || trimws(s) == "") return(list())

  parts  <- strsplit(s, "\\|\\|")[[1]]
  result <- list()

  for (p in parts) {
    idx <- regexpr("=", p)
    if (idx > 0) {
      k <- trimws(substr(p, 1, idx - 1))
      v <- trimws(substr(p, idx + 1, nchar(p)))
      if (k != "") result[[k]] <- v
    }
  }
  result
}


# =============================================================================
# SECTION 4 -- Vérifications au chargement
# =============================================================================

stopifnot(exists("PROCESS_TYPES"))   # M01 chargé

local({

  # Cohérence interne de la table
  stopifnot(is.data.frame(RATE_TEMPLATES))
  stopifnot(nrow(RATE_TEMPLATES) == 27)
  stopifnot(all(c("template_id", "process_type",
                  "expression_template", "label",
                  "parameters") %in% names(RATE_TEMPLATES)))

  # Tous les process_type de la table sont dans PROCESS_TYPES
  valid_pt <- unique(RATE_TEMPLATES$process_type)
  stopifnot(all(valid_pt %in% PROCESS_TYPES))

  # IDs uniques
  stopifnot(!any(duplicated(RATE_TEMPLATES$template_id)))

  # filter_templates
  t <- filter_templates("transmission", "direct", "frequency")
  stopifnot(nrow(t) >= 1)
  stopifnot(all(t$process_type == "transmission"))

  t_all <- filter_templates("recovery")
  stopifnot(nrow(t_all) >= 1)

  t_none <- filter_templates("nonexistent_type")
  stopifnot(nrow(t_none) == 0)

  # generate_rate_expression
  expr <- generate_rate_expression(
    "trans_direct_freq", "S_h", "I_h"
  )
  stopifnot(grepl("beta_c", expr))
  stopifnot(grepl("S_h",   expr))
  stopifnot(grepl("I_h",   expr))
  stopifnot(grepl("Ntot",  expr))

  expr_bad <- generate_rate_expression("nonexistent", "S", "I")
  stopifnot(expr_bad == "")

  # encode / decode param_desc
  lst  <- list(beta_c = "taux de contact", gamma_r = "taux de guérison")
  enc  <- encode_param_desc(lst)
  dec  <- decode_param_desc(enc)
  stopifnot(dec[["beta_c"]] == "taux de contact")
  stopifnot(dec[["gamma_r"]] == "taux de guérison")

  stopifnot(encode_param_desc(list()) == "")
  stopifnot(length(decode_param_desc("")) == 0)
  stopifnot(length(decode_param_desc(NA)) == 0)
})

# message("M03_rate_templates.R chargé -- OK")
