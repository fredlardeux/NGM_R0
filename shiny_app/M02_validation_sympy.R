# =============================================================================
# M02_validation_sympy.R -- Validation SymPy : identifiants et expressions
# =============================================================================
# Dépendances : M01_constants.R (SYMPY_RESERVED_BLOCK, SYMPY_RESERVED_WARN)
# Testé seul  : source("M01_constants.R"); source("M02_validation_sympy.R")
# =============================================================================


# =============================================================================
# SECTION 1 -- Validation d'un identifiant (state_id, rate_name, param_name)
# =============================================================================

#' Valider un identifiant SymPy
#'
#' Vérifie qu'un nom peut être utilisé sans ambiguïté comme symbole SymPy.
#' Retourne une liste avec :
#'   $valid    : logical -- FALSE si une erreur bloquante est détectée
#'   $errors   : character vector -- messages d'erreur bloquants
#'   $warnings : character vector -- avertissements non bloquants
#'
#' @param id character(1) -- identifiant à valider
validate_sympy_id <- function(id) {

  errors   <- character(0)
  warnings <- character(0)

  # Garde : vide ou NULL
  if (is.null(id) || trimws(id) == "") {
    errors <- c(errors, "L'identifiant ne peut pas être vide.")
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }
  id <- trimws(id)

  # 1. Doit commencer par une lettre ou _
  if (!grepl("^[A-Za-z_]", id))
    errors <- c(errors, "Doit commencer par une lettre ou '_'.")

  # 2. Caractères autorisés : lettres, chiffres, _
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", id))
    errors <- c(errors,
      "Caractères invalides -- autorisé : lettres, chiffres, '_'.")

  # 3. Nom exactement réservé (niveau BLOCK = erreur bloquante)
  if (id %in% SYMPY_RESERVED_BLOCK)
    errors <- c(errors, paste0(
      "'", id, "' est un mot réservé Python/SymPy -- ",
      "il sera interprété comme une constante ou un opérateur, ",
      "pas comme une variable. ",
      "Choisissez un nom différent ",
      "(ex: '", id, "_c' ou '", id, "_state')."))

  # 4. Cas particulier : E1, E2… ressemblent à une notation scientifique
  if (grepl("^E[0-9]+$", id))
    warnings <- c(warnings, paste0(
      "'", id, "' ressemble à une notation scientifique ",
      "(ex : E1 = exp(1) en SymPy). ",
      "Préférez 'E_", substr(id, 2, nchar(id)),
      "' ou 'Exp_", substr(id, 2, nchar(id)), "'."))

  # 5. Homonyme de fonction SymPy (niveau WARN = avertissement)
  if (id %in% SYMPY_RESERVED_WARN)
    warnings <- c(warnings, paste0(
      "'", id, "' est aussi le nom d'une fonction SymPy -- ",
      "préférez '", id, "_r' ou '", id, "_state' ",
      "pour éviter toute ambiguïté."))

  # 6. Longueur excessive (> 20 caractères)
  if (nchar(id) > 20)
    warnings <- c(warnings, paste0(
      "Identifiant très long (", nchar(id), " caractères) -- ",
      "les noms courts améliorent la lisibilité des expressions."))

  list(valid = length(errors) == 0, errors = errors, warnings = warnings)
}


# =============================================================================
# SECTION 2 -- Validation d'une expression mathématique pour SymPy
# =============================================================================

#' Valider une expression mathématique SymPy
#'
#' Effectue des vérifications syntaxiques sans appel Python.
#' Retourne une liste avec :
#'   $valid       : logical
#'   $errors      : character vector
#'   $warnings    : character vector
#'   $suggestions : character vector (correction proposée si ^ détecté)
#'   $tokens_found: character vector (tous les identifiants trouvés)
#'
#' @param expr         character(1) -- expression à valider
#' @param known_symbols character   -- noms connus (compartiments, paramètres)
validate_sympy_expression <- function(expr, known_symbols = character(0)) {

  errors      <- character(0)
  warnings    <- character(0)
  suggestions <- character(0)

  # Garde : vide ou NULL
  if (is.null(expr) || trimws(expr) == "")
    return(list(
      valid        = FALSE,
      errors       = "L'expression ne peut pas être vide.",
      warnings     = warnings,
      suggestions  = suggestions,
      tokens_found = character(0)
    ))

  expr_trimmed <- trimws(expr)

  # 1. Caractères interdits : ; { } \
  if (grepl("[;{}]", expr_trimmed) || grepl("\\\\", expr_trimmed))
    errors <- c(errors,
      "Caractères interdits : ';', '{', '}', '\\'.")

  # 2. ^ invalide → ** (syntaxe Python)
  if (grepl("\\^", expr_trimmed)) {
    errors      <- c(errors,
      "Utiliser ** pour les puissances (pas ^). Ex: x**2 au lieu de x^2.")
    suggestions <- c(suggestions, gsub("\\^", "**", expr_trimmed))
  }

  # 3. // non supporté
  if (grepl("//", expr_trimmed, fixed = TRUE))
    errors <- c(errors,
      "Opérateur // non supporté. Utiliser / pour la division.")

  # 4. Parenthèses équilibrées
  n_open  <- nchar(gsub("[^(]", "", expr_trimmed))
  n_close <- nchar(gsub("[^)]", "", expr_trimmed))
  if (n_open != n_close)
    errors <- c(errors, paste0(
      "Parenthèses non équilibrées : ",
      n_open, " ouvrante(s), ", n_close, " fermante(s)."))

  # 5. Opérateurs consécutifs
  # On remplace ** par un placeholder ¶ pour ne pas le confondre avec *
  expr_no_pow <- gsub("\\*\\*", "\u00b6", expr_trimmed)
  if (grepl("[-+*/]{2,}", expr_no_pow))
    warnings <- c(warnings,
      "Opérateurs consécutifs détectés -- vérifiez la syntaxe.")

  # 6. Commence par un opérateur invalide
  if (grepl("^[+*/]", expr_trimmed))
    errors <- c(errors,
      "L'expression commence par un opérateur invalide.")
  if (grepl("^-", expr_trimmed))
    warnings <- c(warnings,
      "L'expression commence par '-' (signe unaire) -- valide, mais vérifiez.")

  # 7. Se termine par un opérateur
  expr_no_pow_trimmed <- trimws(gsub("\\*\\*", "\u00b6", expr_trimmed))
  if (grepl("[-+*/]$", expr_no_pow_trimmed))
    errors <- c(errors,
      "L'expression se termine par un opérateur.")

  # 8. Extraire tous les tokens identifiants
  all_tokens <- unlist(regmatches(
    expr_trimmed,
    gregexpr("[A-Za-z_][A-Za-z0-9_]*", expr_trimmed)
  ))

  # 9. Noms réservés dangereux dans l'expression (BLOCK)
  dangerous <- all_tokens[all_tokens %in% SYMPY_RESERVED_BLOCK]
  if (length(dangerous) > 0)
    errors <- c(errors, paste0(
      "Noms réservés Python/SymPy détectés : ",
      paste(unique(dangerous), collapse = ", ")))

  # 10. Homonymes de fonctions SymPy (WARN)
  warn_toks <- all_tokens[all_tokens %in% SYMPY_RESERVED_WARN]
  if (length(warn_toks) > 0)
    warnings <- c(warnings, paste0(
      "Symboles homonymes de fonctions SymPy (à vérifier) : ",
      paste(unique(warn_toks), collapse = ", ")))

  # 11. Symboles libres non reconnus → interprétés comme paramètres
  math_funcs <- c(
    "exp", "log", "sqrt", "sin", "cos", "tan",
    "asin", "acos", "atan", "sinh", "cosh", "tanh",
    "Abs", "abs", "sign", "floor", "ceiling", "factorial",
    "Min", "Max", "Piecewise", "Heaviside",
    "log10", "log2", "re", "im", "conjugate"
  )
  unknown <- all_tokens[
    !all_tokens %in% c(
      known_symbols, math_funcs,
      SYMPY_RESERVED_BLOCK, SYMPY_RESERVED_WARN
    )
  ]
  if (length(unknown) > 0)
    warnings <- c(warnings, paste0(
      "Symboles interprétés comme paramètres libres : ",
      paste(unique(unknown), collapse = ", ")))

  list(
    valid        = length(errors) == 0,
    errors       = errors,
    warnings     = warnings,
    suggestions  = suggestions,
    tokens_found = all_tokens
  )
}


# =============================================================================
# SECTION 3 -- Extraction des paramètres libres d'une expression
# =============================================================================

#' Extraire les paramètres libres d'une expression
#'
#' Retourne les tokens qui ne sont ni des compartiments connus,
#' ni des fonctions mathématiques standards, ni des réservés SymPy.
#'
#' @param expr         character(1) -- expression mathématique
#' @param known_symbols character   -- noms connus (compartiments)
#' @return character vector des paramètres libres (peut être vide)
extract_free_params <- function(expr, known_symbols = character(0)) {

  if (is.null(expr) || trimws(expr) == "") return(character(0))

  math_funcs <- c(
    "exp", "log", "sqrt", "sin", "cos", "tan",
    "asin", "acos", "atan", "sinh", "cosh", "tanh",
    "Abs", "abs", "sign", "floor", "ceiling", "factorial",
    "Min", "Max", "Piecewise", "Heaviside",
    "log10", "log2", "re", "im", "conjugate"
  )

  all_block <- c(SYMPY_RESERVED_BLOCK, SYMPY_RESERVED_WARN)

  tokens <- unique(unlist(regmatches(
    expr,
    gregexpr("[A-Za-z_][A-Za-z0-9_]*", expr)
  )))

  tokens[!tokens %in% c(known_symbols, math_funcs, all_block)]
}


# =============================================================================
# SECTION 4 -- Helpers pour l'interface Shiny
# =============================================================================

#' Formater le résultat d'une validation pour affichage Shiny (tags HTML)
#'
#' @param vr list -- résultat de validate_sympy_id() ou validate_sympy_expression()
#' @return tagList Shiny
format_validation_ui <- function(vr) {
  tags$div(
    if (length(vr$errors) > 0)
      tags$div(
        style = paste0(
          "background:#f8d7da;border:1px solid #f5c6cb;",
          "padding:8px;border-radius:4px;margin-top:5px;"
        ),
        tags$strong("\u274c Erreurs :"),
        tags$ul(lapply(vr$errors, tags$li))
      ),
    if (length(vr$warnings) > 0)
      tags$div(
        style = paste0(
          "background:#fff3cd;border:1px solid #ffc107;",
          "padding:8px;border-radius:4px;margin-top:5px;"
        ),
        tags$strong("\u26a0\ufe0f Avertissements :"),
        tags$ul(lapply(vr$warnings, tags$li))
      ),
    if (length(vr$suggestions) > 0)
      tags$div(
        style = paste0(
          "background:#d1ecf1;border:1px solid #bee5eb;",
          "padding:8px;border-radius:4px;margin-top:5px;"
        ),
        tags$strong("\U0001f4a1 Suggestion : "),
        tags$code(vr$suggestions[1])
      ),
    if (length(vr$errors) == 0 && length(vr$warnings) == 0)
      tags$div(
        style = paste0(
          "background:#d4edda;border:1px solid #c3e6cb;",
          "padding:8px;border-radius:4px;margin-top:5px;"
        ),
        "\u2705 Expression valide pour SymPy."
      )
  )
}

#' Retourner les problèmes SymPy d'un identifiant sous forme de liste simple
#'
#' Wrapper léger autour de validate_sympy_id() pour les banners de validation.
#'
#' @param name character(1)
#' @return list(has_errors, has_warnings, errors, warnings)
get_sympy_issues <- function(name) {
  v <- validate_sympy_id(name)
  list(
    has_errors   = !v$valid,
    has_warnings = length(v$warnings) > 0,
    errors       = v$errors,
    warnings     = v$warnings
  )
}

#' Suggérer des noms alternatifs pour un identifiant réservé
#'
#' @param name character(1)
#' @return character(1) -- suggestions séparées par des virgules
get_sympy_suggestion <- function(name) {
  suggestions <- list(
    S     = "S_h, Sus, Susc",
    I     = "I_h, Inf, Infec",
    E     = "E_h, Exp, Expo",
    R     = "R_h, Rec, Recov",
    N     = "Ntot, N_pop, Npop",
    Q     = "Q_h, Quar",
    C     = "C_h, Case",
    D     = "D_h, Dead",
    P     = "P_h, Prev",
    T     = "T_h, Treat",
    K     = "K_h, Const",
    beta  = "beta_c, beta_t, beta_rate",
    gamma = "gamma_r, gamma_rec, gamma_rate",
    sigma = "sigma_p, sigma_prog, sigma_rate",
    delta = "delta_d, delta_rate",
    mu    = "mu_d, mu_mort, mu_rate",
    alpha = "alpha_v, alpha_rate"
  )
  name_lower <- tolower(name)
  if (name_lower %in% names(suggestions)) {
    suggestions[[name_lower]]
  } else {
    paste0(name, "_c, ", name, "_rate, ", name, "_h")
  }
}

#' Valider une expression de taux en vérifiant que tous les symboles
#' sont connus (compartiments ou paramètres déclarés)
#'
#' Utilisé dans les banners de l'onglet Flux.
#'
#' @param rate_expr     character(1)
#' @param compartments_df data.frame avec colonne state_id
#' @param params_df       data.frame avec colonne param_name (ou NULL)
#' @return list(valid, unknown_symbols, suggestions)
validate_rate_expression <- function(rate_expr,
                                     compartments_df,
                                     params_df = NULL) {

  if (is.null(rate_expr) || trimws(rate_expr) == "")
    return(list(valid = TRUE,
                unknown_symbols = character(0),
                suggestions = list()))

  # Extraire tous les identifiants
  symbols_found <- unique(unlist(regmatches(
    rate_expr,
    gregexpr("\\b[A-Za-z_][A-Za-z0-9_]*\\b", rate_expr)
  )))

  known_comps   <- compartments_df$state_id
  known_params  <- if (!is.null(params_df)) params_df$param_name else character(0)
  math_constants <- c(
    "pi", "e", "exp", "log", "ln", "sqrt",
    "sin", "cos", "tan", "sinh", "cosh", "tanh",
    "abs", "floor", "ceiling"
  )
  known_all <- c(known_comps, known_params, math_constants)
  unknown   <- symbols_found[!symbols_found %in% known_all]

  if (length(unknown) == 0)
    return(list(valid = TRUE,
                unknown_symbols = character(0),
                suggestions = list()))

  # Suggestions par correspondance approchée
  suggestions <- list()
  for (sym in unknown) {
    sym_lower <- tolower(sym)

    # Correspondance exacte (insensible à la casse) dans les compartiments
    comp_matches <- known_comps[tolower(known_comps) == sym_lower]
    if (length(comp_matches) == 0) {
      # Correspondance partielle après suppression de suffixe courant
      sym_base <- gsub("_[hvrcsped]$", "", sym, ignore.case = TRUE)
      possible <- known_comps[grepl(sym_base, known_comps, ignore.case = TRUE)]
      comp_matches <- possible[seq_len(min(3, length(possible)))]
    }

    # Correspondance dans les paramètres
    param_matches <- known_params[tolower(known_params) == sym_lower]
    if (length(param_matches) == 0) {
      sym_base <- gsub("_[crtpdv]$", "", sym, ignore.case = TRUE)
      possible <- known_params[grepl(sym_base, known_params, ignore.case = TRUE)]
      param_matches <- possible[seq_len(min(3, length(possible)))]
    }

    all_matches <- c(comp_matches, param_matches)
    if (length(all_matches) > 0)
      suggestions[[sym]] <- all_matches
  }

  list(valid = FALSE, unknown_symbols = unknown, suggestions = suggestions)
}


# =============================================================================
# SECTION 5 -- Vérification au chargement
# =============================================================================

# Vérifier que les dépendances M01 sont bien chargées
stopifnot(
  exists("SYMPY_RESERVED_BLOCK"),
  exists("SYMPY_RESERVED_WARN"),
  is.character(SYMPY_RESERVED_BLOCK),
  is.character(SYMPY_RESERVED_WARN)
)

# Tests internes rapides (sans shiny)
local({
  # validate_sympy_id
  r <- validate_sympy_id("S_h")
  stopifnot(isTRUE(r$valid), length(r$errors) == 0)

  r <- validate_sympy_id("S")          # réservé SymPy
  stopifnot(!r$valid)

  r <- validate_sympy_id("1bad")       # commence par chiffre
  stopifnot(!r$valid)

  r <- validate_sympy_id("beta_c")     # ok
  stopifnot(isTRUE(r$valid))

  r <- validate_sympy_id("gamma")      # warn seulement
  stopifnot(isTRUE(r$valid), length(r$warnings) > 0)

  # validate_sympy_expression
  r <- validate_sympy_expression("beta_c * S_h * I_h / Ntot",
                                  c("S_h", "I_h", "Ntot"))
  stopifnot(isTRUE(r$valid))

  r <- validate_sympy_expression("x^2")   # ^ invalide
  stopifnot(!r$valid, length(r$suggestions) > 0)

  r <- validate_sympy_expression("(a + b")  # parenthèse non fermée
  stopifnot(!r$valid)

  # extract_free_params
  p <- extract_free_params("beta_c * S_h * I_h / Ntot", c("S_h", "I_h"))
  stopifnot("beta_c" %in% p, "Ntot" %in% p, !"S_h" %in% p)

  # get_sympy_suggestion
  s <- get_sympy_suggestion("beta")
  stopifnot(nchar(s) > 0)
})

# message("M02_validation_sympy.R chargé -- OK")
