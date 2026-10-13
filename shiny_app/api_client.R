# =============================================================================
# api_client.R -- Client HTTP pour le backend FastAPI NGM
# =============================================================================
# Remplace tous les appels reticulate/ngm_py$... par des requetes HTTP.
# Compatible httr2 >= 1.0.0 (syntaxe moderne avec req_perform).
#
# Usage dans les modules server :
#   source("api_client.R")
#   result <- api_compute_ngm(ctx, base_url)
#
# Le base_url est fourni par app.R apres demarrage du serveur :
#   base_url <- start_ngm_server()
# =============================================================================

library(httr2)
library(jsonlite)


# =============================================================================
# SECTION 0 -- Helper : extraction robuste de scalaires depuis JSON parse
# =============================================================================

#' Extraire un scalaire depuis une valeur JSON parsee par jsonlite.
#' jsonlite peut retourner list(TRUE), c(TRUE), TRUE, NULL selon les options.
#' scal() normalise tout ca en un scalaire R propre.
#'
#' @param x  valeur brute depuis fromJSON
#' @param default valeur par defaut si NULL ou vide
#' @return scalaire R (logical, numeric ou character)
scal <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0) return(default)
  if (is.list(x)) x <- x[[1]]
  if (length(x) > 1) x <- x[[1]]
  x
}

#' Version logique : toujours retourner TRUE/FALSE
scal_lgl <- function(x, default = FALSE) {
  isTRUE(scal(x, default))
}

#' Version caractere
scal_chr <- function(x, default = "") {
  v <- scal(x, default)
  if (is.null(v)) return(default)
  as.character(v)
}

#' Version entiere
scal_int <- function(x, default = 0L) {
  v <- scal(x, default)
  if (is.null(v)) return(default)
  as.integer(v)
}


# =============================================================================
# SECTION 1 -- Utilitaires internes
# =============================================================================

#' Faire une requete POST JSON au backend et retourner le resultat parse
#'
#' @param endpoint  character -- ex: "/compute_ngm"
#' @param body      list      -- sera serialise en JSON
#' @param base_url  character -- ex: "http://127.0.0.1:8765"
#' @param timeout   numeric   -- secondes avant timeout (defaut 120)
#' @return list -- reponse parsee depuis JSON
.api_post <- function(endpoint, body, base_url, timeout = 120) {
  url <- paste0(base_url, endpoint)

  # Normaliser les listes vides : list() -> named list pour forcer {}
  # jsonlite serialise list() en [] (tableau) mais Pydantic attend {} (objet)
  body <- rapply(body, function(x) {
    if (is.list(x) && length(x) == 0) setNames(list(), character(0))
    else x
  }, how = "replace")

  # Normaliser les champs qui doivent TOUJOURS etre des tableaux JSON
  # meme quand ils ont un seul element (auto_unbox les convertirait en scalaire)
  array_fields <- c("infected_states", "all_states", "species",
                    "type_names", "K_entries")
  for (field in array_fields) {
    if (!is.null(body[[field]]) && !is.list(body[[field]])) {
      body[[field]] <- as.list(body[[field]])
    }
  }

  resp <- tryCatch({
    request(url) |>
      req_method("POST") |>
      req_headers("Content-Type" = "application/json") |>
      req_body_raw(
        jsonlite::toJSON(body, auto_unbox = TRUE,
                         null = "null", na = "null", force = TRUE)
      ) |>
      req_timeout(timeout) |>
      req_error(is_error = function(resp) FALSE) |>  # on gere les erreurs manuellement
      req_perform()
  }, error = function(e) {
    stop(paste0("Connexion au serveur FastAPI impossible : ", e$message,
                "\n  URL : ", url,
                "\n  Verifiez que le serveur est demarre (voir console RStudio)."))
  })

  status <- resp_status(resp)
  body_raw <- resp_body_string(resp)

  # Log brut pour diagnostic
  message("[api_post] endpoint=", endpoint,
          " status=", status,
          " body_preview=", substr(body_raw, 1, 200))

  if (status >= 500) {
    detail <- tryCatch(
      jsonlite::fromJSON(body_raw)$detail,
      error = function(e) body_raw
    )
    stop(paste0("Erreur serveur (HTTP ", status, ") : ", detail))
  }

  tryCatch(
    jsonlite::fromJSON(body_raw, simplifyVector = FALSE),
    error = function(e)
      stop(paste0("Reponse non parsable depuis ", endpoint, " : ", e$message))
  )
}


#' Verifier que le serveur repond (health check)
#'
#' @param base_url character
#' @return logical
api_health_check <- function(base_url) {
  tryCatch({
    resp <- request(paste0(base_url, "/health")) |>
      req_timeout(3) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
    resp_status(resp) == 200L
  }, error = function(e) FALSE)
}


# =============================================================================
# SECTION 2 -- Endpoints principaux
# =============================================================================

#' Calcul NGM symbolique complet (etapes 1-7)
#'
#' Remplace : ngm_py$compute_ngm_json(ctx_json)
#'
#' @param ctx      list    -- contexte produit par build_sympy_context()
#' @param base_url character
#' @return list -- result avec ok, steps, error, etc.
api_compute_ngm <- function(ctx, base_url) {
  .api_post("/compute_ngm", ctx, base_url, timeout = 120)
}


#' Evaluation numerique de R0
#'
#' Remplace : ngm_py$evaluate_R0_numeric(R0_expr_str, param_vals)
#'
#' @param R0_expr_str    character -- expression symbolique de R0
#' @param param_vals_dict list     -- {param_name: valeur_numerique}
#' @param base_url       character
#' @return list -- {ok, R0_numeric, warnings}
api_evaluate_r0 <- function(R0_expr_str, param_vals_dict, base_url) {
  .api_post(
    "/evaluate_r0",
    list(R0_expr_str     = R0_expr_str,
         param_vals_dict = param_vals_dict),
    base_url,
    timeout = 30
  )
}


#' Validation symbolique du DFE
#'
#' Remplace : ngm_py$validate_dfe(ctx_json)
#'
#' @param ctx      list -- contexte avec fi_exprs, vi_exprs, dfe_subs
#' @param base_url character
#' @return list -- {ok, valid, level1, level2, all_F_zero, V_nonsingular, issues}
api_validate_dfe <- function(ctx, base_url) {
  # Debug : afficher le JSON envoye
  json_body <- jsonlite::toJSON(ctx, auto_unbox = TRUE,
                                null = "null", na = "null", force = TRUE)
  message("[api_validate_dfe] JSON envoye (extrait):")
  message("  fi_exprs: ", jsonlite::toJSON(ctx$fi_exprs, auto_unbox=TRUE))
  message("  vi_exprs: ", jsonlite::toJSON(ctx$vi_exprs, auto_unbox=TRUE))
  message("  all_states: ", paste(ctx$all_states, collapse=", "))
  .api_post("/validate_dfe", ctx, base_url, timeout = 60)
}


#' Sensibilite locale (elasticites)
#'
#' Remplace : ngm_py$sensitivity_analysis(R0_expr_str, param_vals)
#'
#' @param R0_expr_str    character
#' @param param_vals_dict list
#' @param base_url       character
#' @return list -- {ok, R0_value, sensitivities}
api_sensitivity_local <- function(R0_expr_str, param_vals_dict, base_url) {
  .api_post(
    "/sensitivity_local",
    list(R0_expr_str     = R0_expr_str,
         param_vals_dict = param_vals_dict),
    base_url,
    timeout = 60
  )
}


#' Sensibilite globale LHS + PRCC
#'
#' Remplace : ngm_py$global_sensitivity_lhs_prcc(R0_expr_str, param_bounds, N)
#'
#' @param R0_expr_str       character
#' @param param_bounds_dict list -- {param: c(min, max)}
#' @param N                 integer -- taille echantillon LHS
#' @param base_url          character
#' @return list -- {ok, N_actual, prcc, scatter_data, R0_stats}
api_sensitivity_lhs_prcc <- function(R0_expr_str, param_bounds_dict,
                                      N = 500, base_url) {
  # Convertir les bornes en listes (jsonlite serialise c(a,b) en [a,b])
  bounds <- lapply(param_bounds_dict, function(b) as.list(b))
  .api_post(
    "/sensitivity_lhs_prcc",
    list(R0_expr_str       = R0_expr_str,
         param_bounds_dict = bounds,
         N                 = as.integer(N)),
    base_url,
    timeout = 300   # peut etre long pour N eleve
  )
}


# =============================================================================
# SECTION 3 -- Endpoints Hartemink (futur)
# =============================================================================

#' Calcul R0 depuis matrice K (approche Hartemink)
#'
#' @param K_entries  list of lists -- matrice NxN d'expressions
#' @param type_names character     -- noms des types infectieux
#' @param param_vals list          -- valeurs numeriques optionnelles
#' @param base_url   character
#' @return list -- {ok, R0_expr_str, R0_numeric, method}
api_hartemink_compute_r0 <- function(K_entries, type_names,
                                      param_vals = NULL, base_url) {
  body <- list(
    K_entries  = K_entries,
    type_names = type_names
  )
  if (!is.null(param_vals)) body$param_vals <- param_vals
  .api_post("/hartemink/compute_r0", body, base_url, timeout = 60)
}


# =============================================================================
# SECTION 4 -- Gestion du serveur FastAPI
# =============================================================================

# Variable globale pour stocker le process du serveur
.ngm_server_process <- NULL
.ngm_base_url       <- NULL

#' Demarrer le serveur FastAPI si pas deja en cours
#'
#' Cherche main.py dans backend/ relatif au dossier de app.R.
#' Lance uvicorn en arriere-plan (non bloquant).
#'
#' @param app_dir  character -- dossier contenant app.R
#' @param port     integer   -- port (defaut 8765)
#' @return character -- base_url (ex: "http://127.0.0.1:8765")
start_ngm_server <- function(app_dir = getwd(), port = 8765L) {
  base_url <- paste0("http://127.0.0.1:", port)

  # 1. Verifier si le serveur repond deja
  if (api_health_check(base_url)) {
    message("[FastAPI] Serveur deja en cours sur ", base_url)
    .ngm_base_url <<- base_url
    return(base_url)
  }

  # 2. Trouver main.py
  main_py <- file.path(app_dir, "backend", "main.py")
  if (!file.exists(main_py)) {
    # Chercher dans le dossier parent
    main_py <- file.path(dirname(app_dir), "backend", "main.py")
    if (!file.exists(main_py)) {
      stop(paste0(
        "[FastAPI] main.py introuvable.\n",
        "  Cherche dans : ", file.path(app_dir, "backend", "main.py"), "\n",
        "  Verifiez la structure du projet v2_fastapi/."))
    }
  }

  backend_dir <- dirname(main_py)
  message("[FastAPI] Demarrage du serveur depuis : ", backend_dir)

  # 3. Lancer uvicorn en arriere-plan
  # On utilise processx si disponible, sinon system2
  if (requireNamespace("processx", quietly = TRUE)) {
    .ngm_server_process <<- processx::process$new(
      command = "python",
      args    = c("-m", "uvicorn", "main:app",
                  "--host", "127.0.0.1",
                  "--port", as.character(port),
                  "--log-level", "warning"),
      wd      = backend_dir,
      stdout  = "|",
      stderr  = "|"
    )
    message("[FastAPI] Serveur demarre via processx (PID: ",
            .ngm_server_process$get_pid(), ")")
  } else {
    # Fallback : system2 (Windows compatible)
    if (.Platform$OS.type == "windows") {
      shell(
        paste0('start /B python -m uvicorn main:app',
               ' --host 127.0.0.1 --port ', port,
               ' --log-level warning'),
        intern = FALSE, wait = FALSE,
        translate = TRUE
      )
    } else {
      system2(
        "python",
        args = c("-m", "uvicorn", "main:app",
                 "--host", "127.0.0.1",
                 "--port", as.character(port),
                 "--log-level", "warning"),
        wait   = FALSE,
        stdout = FALSE, stderr = FALSE,
        env    = paste0("PYTHONPATH=", backend_dir)
      )
    }
    message("[FastAPI] Serveur demarre via system2")
  }

  # 4. Attendre que le serveur soit pret (max 15 secondes)
  message("[FastAPI] Attente demarrage", appendLF = FALSE)
  for (i in seq_len(30L)) {
    Sys.sleep(0.5)
    message(".", appendLF = FALSE)
    if (api_health_check(base_url)) {
      message(" OK!")
      .ngm_base_url <<- base_url
      return(base_url)
    }
  }

  stop(paste0(
    "\n[FastAPI] Le serveur n'a pas demarre en 15 secondes.\n",
    "  Solutions :\n",
    "  1. Ouvrez un terminal et lancez manuellement :\n",
    "     cd ", backend_dir, "\n",
    "     python -m uvicorn main:app --host 127.0.0.1 --port ", port,
    " --log-level info\n",
    "  2. Installez processx : install.packages('processx')\n",
    "  3. Verifiez que Python et uvicorn sont dans le PATH."))
}


#' Arreter le serveur FastAPI (a appeler en fin de session si besoin)
stop_ngm_server <- function() {
  if (!is.null(.ngm_server_process) &&
      requireNamespace("processx", quietly = TRUE)) {
    if (.ngm_server_process$is_alive()) {
      .ngm_server_process$kill()
      message("[FastAPI] Serveur arrete.")
    }
  } else {
    message("[FastAPI] Arret manuel : fermez le terminal uvicorn.")
  }
  .ngm_server_process <<- NULL
  .ngm_base_url       <<- NULL
}


# =============================================================================
# SECTION 5 -- Verification au chargement
# =============================================================================

if (!requireNamespace("httr2", quietly = TRUE))
  stop("Package 'httr2' requis. Installez avec : install.packages('httr2')")

httr2_ver <- as.numeric_version(packageVersion("httr2"))
if (httr2_ver < "1.0.0")
  stop(paste0("httr2 >= 1.0.0 requis. Version installee : ", httr2_ver,
              "\nMettez a jour : install.packages('httr2')"))

message("[api_client] Charge -- httr2 ", packageVersion("httr2"), " OK")
