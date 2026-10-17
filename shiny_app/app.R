# =============================================================================
# app.R -- NGM R0 Calculator v2 (FastAPI backend)
# =============================================================================
# Architecture :
#   - Backend Python : FastAPI + uvicorn (demarre automatiquement)
#   - Frontend R     : Shiny (httr2 au lieu de reticulate)
#
# Structure du projet v2_fastapi/ :
#   backend/
#     main.py          <- FastAPI app
#     ngm_engine.py    <- calculs Python (inchange)
#   shiny_app/
#     app.R            <- CE FICHIER
#     api_client.R     <- wrapper httr2
#     M01...M13        <- modules (quasi inchanges)
# =============================================================================

# ── Packages ------------------------------------------------------------------
library(shiny)
library(DT)
library(jsonlite)
library(visNetwork)
library(digest)
library(plotly)
# Note : reticulate N'EST PLUS necessaire

# ── API client (httr2) --------------------------------------------------------
source("api_client.R")

# ── Couche 1 -- Core pur ------------------------------------------------------
source("M01_constants.R")
source("M02_validation_sympy.R")
source("M03_rate_templates.R")
source("M04_save_load.R")

# ── Couche 2 -- Logique pure --------------------------------------------------
source("M06_symbolic_ngm.R")
source("M05_dfe_logic.R")

# ── Couche 3 -- Builder visuel ------------------------------------------------
source("M07_diagram_builder.R")

# ── UI ------------------------------------------------------------------------
source("M12_ui.R")


# =============================================================================
# DEMARRAGE DU SERVEUR FASTAPI
# =============================================================================

# Determiner le dossier de l'app (robust : RStudio + Rscript + shinyapps.io)
.app_dir <- tryCatch({
  frames <- sys.frames()
  found  <- NULL
  for (i in rev(seq_along(frames))) {
    f <- frames[[i]]$ofile
    if (!is.null(f) && nzchar(f)) {
      found <- dirname(normalizePath(f, mustWork = FALSE))
      break
    }
  }
  found %||% getwd()
}, error = function(e) getwd())

# Port du serveur FastAPI (modifiable ici si conflit)
NGM_API_PORT <- 8765L
NGM_BASE_URL <- NULL

# Demarrage (une seule fois au lancement de l'app)
# PRIORITE : detecter un serveur deja en cours avant d'en lancer un nouveau.
# Attendre jusqu'a 3 secondes que le serveur reponde (cas processx lent).
NGM_BASE_URL <- tryCatch({
  base_url <- paste0("http://127.0.0.1:", NGM_API_PORT)

  # Verifier plusieurs fois (le serveur peut demarrer lentement)
  # On attend jusqu'a 20s pour laisser le warmup sympy se terminer
  already_up <- FALSE
  for (attempt in 1:40) {
    if (api_health_check(base_url)) {
      already_up <- TRUE
      break
    }
    Sys.sleep(0.5)
    if (attempt %% 4 == 0)
      message("[FastAPI] Attente demarrage... ", attempt/2, "s")
  }

  if (already_up) {
    message("[FastAPI] Serveur deja en cours sur ", base_url, " -- demarrage auto ignore.")
    base_url
  } else {
    message("[FastAPI] Aucun serveur detecte sur ", base_url,
            " -- demarrage automatique...")
    start_ngm_server(app_dir = .app_dir, port = NGM_API_PORT)
  }
}, error = function(e) {
  message("\n[WARN] Serveur FastAPI non demarre.")
  message(e$message)
  message("\nSolution : lancez manuellement dans un terminal :")
  message("  cd v2_fastapi/backend")
  message("  python -m uvicorn main:app --host 127.0.0.1 --port 8765")
  NULL
})

if (!is.null(NGM_BASE_URL)) {
  message("[NGM] Backend disponible : ", NGM_BASE_URL)
  message("[NGM] Documentation API : ", NGM_BASE_URL, "/docs")
} else {
  message("[NGM] Mode degrade -- certaines fonctions indisponibles.")
}


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # -- URL du backend (reactive, pour pouvoir changer si besoin) --------------
  api_url <- reactive({ NGM_BASE_URL })

  # Helper : verifier que le backend est disponible
  check_api <- function() {
    url <- api_url()
    if (is.null(url)) {
      showNotification(
        tags$div(
          tags$strong("\u274c Serveur FastAPI non disponible"),
          tags$br(),
          tags$span(
            "Lancez manuellement : ",
            tags$code(paste0("cd backend && python -m uvicorn main:app",
                             " --port ", NGM_API_PORT))
          )
        ),
        type = "error", duration = 15
      )
      return(FALSE)
    }
    if (!api_health_check(url)) {
      showNotification(
        "\u274c Le serveur FastAPI ne repond pas. Relancez l'app.",
        type = "error", duration = 10
      )
      return(FALSE)
    }
    TRUE
  }

  # -- ReactiveVals partagees -------------------------------------------------
  rv_comp   <- reactiveVal(init_comp)
  rv_flux   <- reactiveVal(init_flux)
  rv_dfe    <- reactiveVal(init_dfe)
  rv_params <- reactiveVal(init_params)

  rv_symbolic          <- reactiveVal(NULL)
  rv_flux_hash         <- reactiveVal(NULL)
  rv_dfe_validated     <- reactiveVal(FALSE)
  rv_infected_order    <- reactiveVal(character(0))
  rv_infected_locked   <- reactiveVal(FALSE)
  rv_ngm_result        <- reactiveVal(NULL)
  rv_sym_df_ngm        <- reactiveVal(NULL)
  rv_ngm_computed_hash <- reactiveVal(NULL)
  rv_numeric_params    <- reactiveVal(list())
  rv_R0_numeric        <- reactiveVal(NULL)
  rv_dependencies         <- reactiveVal(list())
  rv_dependencies_synced  <- reactiveVal(TRUE)
  rv_last_flux_hash_deps  <- reactiveVal(NULL)
  rv_edit_dep_index       <- reactiveVal(NULL)
  rv_delete_dep_index     <- reactiveVal(NULL)
  rv_last_autosave     <- reactiveVal(Sys.time())
  rv_autosave_enabled  <- reactiveVal(TRUE)
  rv_flux_suggestions  <- reactiveVal(list())
  rv_load_status       <- reactiveVal(NULL)
  species_list         <- reactiveVal(c("human", "vector", "animal"))

  # Flag de chargement : TRUE pendant le chargement d un JSON
  # Bloque les observers reactifs qui recalculent le DFE/params
  rv_loading           <- reactiveVal(FALSE)

  # DFE/params en attente de re-forcage apres chargement JSON
  # M09 surveille ces valeurs et les applique apres ses propres recalculs
  rv_pending_dfe       <- reactiveVal(NULL)
  rv_pending_params    <- reactiveVal(NULL)

  # -- Rendre api_url et check_api disponibles pour les modules --------------
  # (passes via l'environnement local = TRUE)

  # -- Modules server ---------------------------------------------------------
  source("M08_server_comp_flux.R",   local = TRUE)
  source("M09_server_dfe.R",         local = TRUE)
  source("M10_server_ngm_numeric.R", local = TRUE)
  source("M11_server_sensitivity.R", local = TRUE)
  source("M12_server.R",             local = TRUE)
  source("M13_server_scenarios.R",   local = TRUE)

  # -- Nettoyage en fin de session --------------------------------------------
  session$onSessionEnded(function() {
    # Ne pas arreter le serveur si d'autres sessions sont actives
    # Le serveur s'arrete quand RStudio est ferme
    message("[NGM] Session terminee.")
  })
}


# =============================================================================
# LANCEMENT
# =============================================================================

in_rstudio <- Sys.getenv("RSTUDIO") == "1"

if (in_rstudio) {
  launch_browser <- function(url) {
    sysname <- Sys.info()["sysname"]
    if (.Platform$OS.type == "windows") shell.exec(url)
    else if (sysname == "Darwin") system(paste0("open '", url, "'"))
    else system(paste0("xdg-open '", url, "'"))
  }
  cat("\nLancement NGM R0 Calculator v2 (FastAPI)...\n\n")
  shinyApp(ui, server, options = list(launch.browser = launch_browser))
} else {
  shinyApp(ui, server, options = list(launch.browser = TRUE))
}
