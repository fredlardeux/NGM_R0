# =============================================================================
# M01_constants.R -- Constantes globales, types, palettes, data.frames vides
# =============================================================================
# Dépendances : aucune
# Testé seul  : source("M01_constants.R") doit s'exécuter sans erreur
# =============================================================================

APP_VERSION <- "2.0"

# ── Opérateur null-coalesce ───────────────────────────────────────────────────
# (comme %||% de rlang, sans dépendance externe)
`%||%` <- function(a, b) if (!is.null(a)) a else b


# =============================================================================
# SECTION 1 -- Mots réservés Python / SymPy
# =============================================================================

PYTHON_KEYWORDS <- c(
  "False", "None", "True",
  "and", "as", "assert", "async", "await",
  "break", "class", "continue", "def", "del",
  "elif", "else", "except", "finally", "for",
  "from", "global", "if", "import", "in",
  "is", "lambda", "nonlocal", "not", "or",
  "pass", "raise", "return", "try", "while",
  "with", "yield"
)

# Niveau 1 : erreur bloquante (seront interprétés comme constantes/opérateurs)
SYMPY_CONSTANTS_BLOCK <- c(
  "I",           # Unité imaginaire
  "E",           # Constante d'Euler
  "S",           # Singleton factory SymPy
  "N",           # Fonction évaluation numérique
  "C",           # Constante de Catalan
  "O",           # Ordre de grandeur (Landau)
  "Q",           # Rationnels SymPy
  "Lambda",      # Fonction Lambda SymPy
  "pi",          # π
  "oo",          # Infini positif
  "zoo",         # Infini complexe
  "nan",         # Not a Number
  "GoldenRatio", # φ
  "EulerGamma"   # γ d'Euler-Mascheroni
)

SYMPY_RESERVED_BLOCK <- c(PYTHON_KEYWORDS, SYMPY_CONSTANTS_BLOCK)

# Niveau 2 : avertissement seulement (non bloquant)
SYMPY_RESERVED_WARN <- c(
  "exp", "log", "sqrt", "Abs", "sign",
  "sin", "cos", "tan", "asin", "acos", "atan",
  "sinh", "cosh", "tanh",
  "floor", "ceiling", "factorial",
  "Min", "Max", "Piecewise",
  "re", "im", "conjugate",
  "beta",    # Fonction beta d'Euler B(x,y)
  "gamma",   # Fonction gamma Γ(x)
  "sigma",   # Heaviside
  "delta",   # Dirac delta
  "Ei",      # Intégrale exponentielle
  "li",      # Logarithme intégral
  "Symbol", "symbols", "Function", "Matrix",
  "Rational", "Float", "Integer",
  "Eq", "solve", "diff", "integrate",
  "lambdify", "simplify", "expand", "factor"
)


# =============================================================================
# SECTION 2 -- Types de compartiments et processus
# =============================================================================

STATE_TYPES <- c(
  "susceptible",                       # non infecte
  "exposed",                           # infecte non encore infectieux (latence)
  "infectious",                        # infectieux symptomatiques
  "asymptomatic",                      # infectieux asymptomatiques
  "carrier",                           # porteur chronique
  "latent",                            # latence longue (TB)
  "treated",                           # sous traitement, encore infectieux
  "hospitalized",                      # hospitalise, encore infectieux
  "recovered",                         # gueri immunise
  "immune",                            # immunise sans infection prealable
  "vaccinated",                        # vaccine (susceptible reduit)
  "dead"                               # deces -- hors NGM
)

STATE_COMMENTS <- c(
  susceptible  = "Susceptible - can be infected",
  exposed      = "Exposed - infected but not yet infectious",
  infectious   = "Infectious - can transmit",
  asymptomatic = "Asymptomatic infectious - reduced transmissibility",
  recovered    = "Recovered - immune",
  immune       = "Immune - protected",
  latent       = "Latent - dormant infection",
  carrier      = "Carrier - chronic infectious",
  latent       = "Latent - long-term latency (e.g. TB)",
  treated      = "Treated - infectious but under treatment",
  hospitalized = "Hospitalized - infectious in hospital setting",
  vaccinated   = "Vaccinated - reduced susceptibility",
  treated      = "Treated - under treatment",
  hospitalized = "Hospitalized - in hospital",
  dead         = "Dead - removed from system"
)

# Compartiments qui entrent dans le calcul NGM (vecteur infecté)
INFECTED_STATE_TYPES <- c(
  "exposed",        # infecte non encore infectieux -- contribue a V (progression)
  "infectious",     # infectieux symptomatiques -- contribue a F et V
  "asymptomatic",   # infectieux asymptomatiques -- contribue a F et V
  "carrier",        # porteur chronique -- contribue a F et V
  "latent",         # latence longue (TB) -- contribue a V
  "treated",        # sous traitement encore infectieux -- contribue a V
  "hospitalized"    # hospitalise encore infectieux (Ebola) -- contribue a V
)

# Process types qui génèrent des flux is_F (nouvelles infections)
F_PROCESS_TYPES <- c("transmission", "superinfection", "relapse")

# Tous les types de processus disponibles dans l'interface
PROCESS_TYPES <- c(
  "transmission", "progression", "recovery", "waning_immunity",
  "birth", "death", "migration", "treatment", "vaccination",
  "relapse", "superinfection"
)

TRANSMISSION_TYPES <- c("direct", "vector", "environmental", "vertical", "sexual")

DENSITY_TYPES <- c("frequency", "density")

# Process types considérés comme démographiques (naissance / mort)
DEMOGRAPHIC_PROCESS_TYPES <- c("birth", "death")


# =============================================================================
# SECTION 3 -- Palettes visuelles (diagramme visNetwork)
# =============================================================================

# Couleurs des nœuds par state_type
NODE_COLORS <- list(
  susceptible  = list(
    background = "#cce5ff", border = "#4a90d9",
    highlight  = list(background = "#a8d4ff", border = "#1a5fa8")
  ),
  exposed      = list(
    background = "#fff3cd", border = "#e6a817",
    highlight  = list(background = "#ffe8a0", border = "#b87e00")
  ),
  infectious   = list(
    background = "#f8d7da", border = "#d9534f",
    highlight  = list(background = "#f5b8bc", border = "#a02020")
  ),
  asymptomatic = list(
    background = "#ffe4b5", border = "#ffa500",
    highlight  = list(background = "#ffd080", border = "#cc8000")
  ),
  latent       = list(
    background = "#ffeeba", border = "#d4a017",
    highlight  = list(background = "#ffd97a", border = "#9a7200")
  ),
  carrier      = list(
    background = "#f5c6cb", border = "#c0392b",
    highlight  = list(background = "#f0a0a8", border = "#8b1a1a")
  ),
  recovered    = list(
    background = "#d4edda", border = "#28a745",
    highlight  = list(background = "#b0dbb8", border = "#1a7a30")
  ),
  immune       = list(
    background = "#c3e6cb", border = "#20a050",
    highlight  = list(background = "#98d4a8", border = "#106030")
  ),
  treated      = list(
    background = "#d1ecf1", border = "#17a2b8",
    highlight  = list(background = "#a8dce8", border = "#0d6e80")
  ),
  hospitalized = list(
    background = "#bee5eb", border = "#138496",
    highlight  = list(background = "#90d0dc", border = "#0a5a68")
  ),
  vaccinated   = list(
    background = "#b0e0e6", border = "#4682b4",
    highlight  = list(background = "#87ceeb", border = "#2060a0")
  ),
  dead         = list(
    background = "#e2e3e5", border = "#6c757d",
    highlight  = list(background = "#c8cacf", border = "#4a5560")
  ),
  default      = list(
    background = "#f0f0f0", border = "#999999",
    highlight  = list(background = "#dcdcdc", border = "#666666")
  )
)

# Couleur de bordure d'arête par process_type
EDGE_COLORS <- list(
  transmission   = "#d9534f",  # rouge  -- flux épidémique central
  superinfection = "#e07020",  # orange foncé
  relapse        = "#e08040",  # orange
  progression    = "#5bc0de",  # bleu clair
  recovery       = "#28a745",  # vert
  waning_immunity= "#8e44ad",  # violet
  treatment      = "#17a2b8",  # cyan
  vaccination    = "#20c997",  # vert-cyan
  birth          = "#6f42c1",  # violet foncé
  death          = "#6c757d",  # gris
  migration      = "#fd7e14",  # orange vif
  default        = "#aaaaaa"   # gris neutre
)


# =============================================================================
# SECTION 4 -- Data.frames vides de référence (templates pour JSON load/save)
# =============================================================================

init_comp <- data.frame(
  state_id      = character(0),
  label         = character(0),
  state_type    = character(0),
  state_comment = character(0),
  species       = character(0),
  strain        = character(0),
  age_class     = character(0),
  patch         = character(0),
  description   = character(0),
  verification  = character(0),
  stringsAsFactors = FALSE
)

init_flux <- data.frame(
  flux_id              = character(0),
  from_state_id        = character(0),
  to_state_id          = character(0),
  process_type         = character(0),
  transmission_type    = character(0),
  rate_name            = character(0),
  rate_expression      = character(0),
  density_type         = character(0),
  description          = character(0),
  is_demographic       = logical(0),
  is_progression       = logical(0),
  is_recovery          = logical(0),
  verification         = character(0),
  rate_template_id     = character(0),
  rate_template_label  = character(0),
  rate_expression_auto = character(0),
  rate_source          = character(0),
  rate_validated       = logical(0),
  rate_units           = character(0),
  rate_comment         = character(0),
  rate_param_desc      = character(0),
  stringsAsFactors = FALSE
)

init_dfe <- data.frame(
  state_id    = character(0),
  label       = character(0),
  state_type  = character(0),
  species     = character(0),
  dfe_expr    = character(0),
  dfe_auto    = logical(0),
  dfe_comment = character(0),
  stringsAsFactors = FALSE
)

init_params <- data.frame(
  param_name  = character(0),
  description = character(0),
  units       = character(0),
  category    = character(0),
  stringsAsFactors = FALSE
)


# =============================================================================
# SECTION 5 -- Vérification au chargement (optionnelle, silencieuse)
# =============================================================================

stopifnot(
  # Vérifier cohérence INFECTED_STATE_TYPES ⊂ STATE_TYPES
  all(INFECTED_STATE_TYPES %in% STATE_TYPES),
  # Vérifier que F_PROCESS_TYPES ⊂ PROCESS_TYPES
  all(F_PROCESS_TYPES %in% PROCESS_TYPES),
  # Vérifier que DEMOGRAPHIC_PROCESS_TYPES ⊂ PROCESS_TYPES
  all(DEMOGRAPHIC_PROCESS_TYPES %in% PROCESS_TYPES),
  # Vérifier que NODE_COLORS couvre tous les STATE_TYPES + "default"
  all(STATE_TYPES %in% names(NODE_COLORS)),
  # Vérifier que STATE_COMMENTS couvre tous les STATE_TYPES
  all(STATE_TYPES %in% names(STATE_COMMENTS)),
  # Vérifier les colonnes critiques de init_comp
  all(c("state_id", "label", "state_type", "species",
        "verification") %in% names(init_comp)),
  # Vérifier les colonnes critiques de init_flux
  all(c("flux_id", "from_state_id", "to_state_id",
        "process_type", "rate_expression",
        "verification") %in% names(init_flux)),
  # Vérifier les colonnes de init_dfe
  all(c("state_id", "dfe_expr", "dfe_auto") %in% names(init_dfe)),
  # Vérifier les colonnes de init_params
  all(c("param_name", "description", "units",
        "category") %in% names(init_params))
)

# message("M01_constants.R chargé -- OK")
