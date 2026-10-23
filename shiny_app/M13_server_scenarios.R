# =============================================================================
# M13_server_scenarios.R -- Comparaison de scenarios
# =============================================================================
# Architecture : UN SEUL observer global qui lit tous les inputs scenario.
# Pas d'observers dynamiques par scenario => pas de boucle infinie,
# pas de corruption de liste.
#
# Generique : lit uniquement R0_expr_str + param_names depuis rv_ngm_result().
# Fonctionne pour n'importe quel modele, du SIR au plus complexe.
#
# Dependances : rv_ngm_result, rv_params, rv_sensitivity_result, ngm_py
# =============================================================================


# =============================================================================
# SECTION 1 -- ReactiveVals
# =============================================================================

# Liste de scenarios. Chaque element est une liste :
#   list(id, name, color, params = list(p1=val, p2=val, ...))
rv_scenarios        <- reactiveVal(list())
rv_scenarios_result <- reactiveVal(NULL)

# Compteur simple pour les IDs (evite les problemes POSIXt)
.sc_counter <- local({ n <- 0L; function() { n <<- n + 1L; n } })


# =============================================================================
# SECTION 2 -- Helpers internes
# =============================================================================

# Palette de couleurs pour les scenarios
.scenario_color <- function(n) {
  palette <- c(
    "#4a90d9", "#e74c3c", "#2ecc71", "#f39c12",
    "#9b59b6", "#1abc9c", "#e67e22", "#34495e",
    "#e91e63", "#00bcd4"
  )
  palette[((n - 1L) %% length(palette)) + 1L]
}

# Convertir hex en rgba(r,g,b,alpha)
.hex_alpha <- function(hex, alpha = 0.08) {
  hex <- gsub("#", "", hex)
  r   <- strtoi(substr(hex, 1, 2), 16L)
  g   <- strtoi(substr(hex, 3, 4), 16L)
  b   <- strtoi(substr(hex, 5, 6), 16L)
  paste0("rgba(", r, ",", g, ",", b, ",", alpha, ")")
}

# Badge statut epidemique
.epi_badge <- function(R0_val) {
  if (is.na(R0_val))
    return(tags$span(
      style = "background:#6c757d;color:white;padding:2px 10px;
               border-radius:12px;font-size:11px;", "N/A"))
  if (R0_val < 1)
    tags$span(style = "background:#28a745;color:white;padding:2px 10px;
               border-radius:12px;font-size:11px;font-weight:600;",
              "\u2b07\ufe0f R\u2080 < 1")
  else if (R0_val < 2)
    tags$span(style = "background:#ffc107;color:#333;padding:2px 10px;
               border-radius:12px;font-size:11px;font-weight:600;",
              "\u26a0\ufe0f 1 \u2264 R\u2080 < 2")
  else
    tags$span(style = "background:#dc3545;color:white;padding:2px 10px;
               border-radius:12px;font-size:11px;font-weight:600;",
              "\u2b06\ufe0f R\u2080 \u2265 2")
}

# Construire un scenario vide (valeurs NA)
.new_scenario <- function(name, params, color) {
  list(
    id     = paste0("sc", .sc_counter()),
    name   = name,
    color  = color,
    params = setNames(
      lapply(params, function(p) NA_real_),
      params
    )
  )
}


# =============================================================================
# SECTION 3 -- UI principale
# =============================================================================

output$scenarios_content <- renderUI({
  r <- rv_ngm_result()

  if (is.null(r) || !scal_lgl(r$ok))
    return(tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;border-radius:6px;
               padding:40px;text-align:center;margin-top:20px;",
      tags$h4(style = "color:#856404;", "\U0001f4ca Comparaison de Scenarios"),
      tags$p("Calculez d'abord R\u2080 symbolique dans l'onglet ",
             strong("Symbolic / NGM"), "."),
      tags$p(style = "font-size:13px;color:#666;",
             "Definissez plusieurs jeux de parametres et comparez leurs R\u2080.")
    ))

  params   <- r$steps$`1_symbols`$param_names
  scenarios <- rv_scenarios()

  tagList(
    tags$h3("\U0001f4ca Comparaison de Scenarios"),
    tags$p(style = "color:#666;font-size:13px;margin-bottom:16px;",
           "Definissez plusieurs jeux de parametres et comparez leurs R\u2080. ",
           "Utile pour evaluer l'impact d'interventions."),

    # Expression R0
    tags$div(
      style = "background:#f0f7ff;border:1px solid #4a90d9;border-radius:6px;
               padding:10px 16px;margin-bottom:16px;",
      tags$span(style = "font-weight:600;color:#1a5fa8;", "R\u2080 = "),
      tags$code(style = "font-size:13px;color:#333;",
                if (identical(trimws(as.character(
                      r$steps$`7_eigenvalues`$R0_expr_str)), "?"))
                  "methode numerique : rayon spectral de K, calcule pour chaque scenario"
                else r$steps$`7_eigenvalues`$R0_expr_str)
    ),

    # Boutons
    tags$div(
      style = "display:flex;gap:10px;flex-wrap:wrap;margin-bottom:16px;",
      actionButton("btn_add_scenario",
                   "\u2795 Nouveau scenario", class = "btn-outline-primary"),
      actionButton("btn_add_baseline",
                   "\U0001f3af Baseline", class = "btn-outline-secondary"),
      if (length(scenarios) > 0)
        actionButton("btn_clear_scenarios",
                     "\U0001f5d1 Tout effacer", class = "btn-outline-danger"),
      if (length(scenarios) >= 2)
        actionButton("btn_run_scenarios",
                     "\u2699\ufe0f Calculer R\u2080 pour tous",
                     class = "btn-success",
                     style = "margin-left:auto;")
    ),

    # Table de saisie
    uiOutput("scenarios_input_ui"),

    # Resultats
    uiOutput("scenarios_results_ui")
  )
})


# =============================================================================
# SECTION 4 -- Table de saisie des scenarios (UI)
# =============================================================================

output$scenarios_input_ui <- renderUI({
  r <- rv_ngm_result()
  if (is.null(r)) return(NULL)
  params    <- r$steps$`1_symbols`$param_names
  scenarios <- rv_scenarios()
  pd        <- rv_params()

  if (length(scenarios) == 0)
    return(tags$div(
      style = "background:#f8f9fa;border:2px dashed #dee2e6;border-radius:6px;
               padding:30px;text-align:center;color:#6c757d;margin-bottom:20px;",
      tags$p(style = "font-size:15px;margin-bottom:6px;",
             "\u2b06\ufe0f Aucun scenario"),
      tags$p(style = "font-size:13px;",
             "Cliquez sur ", tags$strong("\u2795 Nouveau scenario"),
             " ou ", tags$strong("\U0001f3af Baseline"), ".")
    ))

  # En-tetes : une colonne par parametre
  header_cells <- lapply(params, function(p) {
    idx   <- which(pd$param_name == p)
    units <- if (length(idx) > 0 && !is.na(pd$units[idx[1]])) pd$units[idx[1]] else ""
    tags$th(
      style = "padding:8px;min-width:130px;",
      tags$code(style = "color:#80d4ff;font-size:12px;", p),
      if (nchar(units) > 0)
        tags$div(style = "font-size:10px;color:#bbb;font-weight:normal;", units)
    )
  })

  # Lignes : un textInput pour le nom + un numericInput par parametre
  rows <- lapply(seq_along(scenarios), function(si) {
    sc <- scenarios[[si]]

    name_cell <- tags$td(
      style = "padding:6px 8px;",
      tags$div(
        style = "display:flex;align-items:center;gap:6px;",
        tags$div(style = paste0(
          "width:14px;height:14px;border-radius:50%;flex-shrink:0;",
          "background:", sc$color, ";border:2px solid rgba(0,0,0,0.15);")),
        textInput(paste0("sc_nm_", sc$id), NULL,
                  value = sc$name, width = "130px")
      )
    )

    param_cells <- lapply(params, function(p) {
      cur <- sc$params[[p]]
      tags$td(
        style = "padding:4px 6px;",
        numericInput(
          paste0("sc_val_", sc$id, "_", p),
          label = NULL,
          value = if (!is.null(cur) && !is.na(cur)) cur else NA,
          min   = 0, step = 0.001, width = "110px"
        )
      )
    })

    del_cell <- tags$td(
      style = "padding:6px 8px;text-align:center;",
      actionButton(
        paste0("sc_del_", sc$id), "\u00d7",
        class = "btn-sm btn-outline-danger",
        style = "padding:2px 8px;font-size:13px;"
      )
    )

    tags$tr(
      style = paste0("border-bottom:1px solid #dee2e6;background:",
                     .hex_alpha(sc$color), ";"),
      name_cell,
      del_cell,
      param_cells
    )
  })

  tagList(
    tags$div(
      style = "overflow-x:auto;margin-bottom:16px;",
      tags$table(
        style = "width:100%;border-collapse:collapse;font-size:13px;",
        tags$thead(tags$tr(
          style = "background:#343a40;color:white;",
          tags$th(style = "padding:8px;min-width:160px;", "Scenario"),
          tags$th(style = "padding:8px;width:50px;", ""),
          header_cells
        )),
        tags$tbody(rows)
      )
    ),
    # Legende descriptions parametres
    local({
      pd_f <- pd[pd$param_name %in% params &
                   !is.na(pd$description) & pd$description != "", ]
      if (nrow(pd_f) > 0)
        tags$p(style = "font-size:11px;color:#888;margin-bottom:4px;",
               paste(paste0(pd_f$param_name, " : ", pd_f$description),
                     collapse = " | "))
    })
  )
})


# =============================================================================
# SECTION 5 -- Pas d'observer global automatique
# =============================================================================
# Les valeurs des inputs sont capturees UNIQUEMENT au moment d'une action
# explicite de l'utilisateur (ajout, suppression, calcul) via
# .flush_inputs_to_scenarios(). Cela evite la boucle renderUI <-> observe
# qui causait la reinitialisation des cellules.


# =============================================================================
# SECTION 6 -- Observers boutons (ajout, suppression, baseline, calcul)
# =============================================================================

# Helper : lire les valeurs courantes des inputs AVANT de modifier rv_scenarios
# Ceci est indispensable : quand rv_scenarios() change, renderUI regenère
# toute la table, detruisant les inputs et reinitialisant leurs valeurs.
# On capture donc les valeurs AVANT la modification.
.flush_inputs_to_scenarios <- function(sc_list, params, input) {
  lapply(sc_list, function(sc) {
    # Nom
    nm_key <- paste0("sc_nm_", sc$id)
    nm_val <- input[[nm_key]]
    if (!is.null(nm_val) && nchar(trimws(nm_val)) > 0)
      sc$name <- nm_val
    # Valeurs des parametres
    for (p in params) {
      val_key <- paste0("sc_val_", sc$id, "_", p)
      val     <- input[[val_key]]
      if (!is.null(val) && length(val) == 1 && !is.na(val))
        sc$params[[p]] <- as.numeric(val)
    }
    sc
  })
}

# Ajouter scenario vide
observeEvent(input$btn_add_scenario, {
  r <- rv_ngm_result()
  if (is.null(r) || !scal_lgl(r$ok)) return()
  params <- r$steps$`1_symbols`$param_names
  sc     <- rv_scenarios()

  # ETAPE 1 : sauvegarder les valeurs courantes des inputs existants
  sc <- .flush_inputs_to_scenarios(sc, params, input)

  # ETAPE 2 : numerotation (hors baseline)
  # vapply garantit un vecteur character(1), meme sur liste vide
  if (length(sc) > 0) {
    noms <- vapply(sc, function(s) {
      nm <- s$name
      if (is.null(nm) || length(nm) == 0) "" else as.character(nm[1])
    }, character(1))
    n_sc <- sum(tolower(trimws(noms)) != "baseline") + 1L
  } else {
    n_sc <- 1L
  }
  n_col <- length(sc) + 1L

  # ETAPE 3 : ajouter le nouveau scenario
  rv_scenarios(c(sc, list(
    .new_scenario(paste0("Scenario ", n_sc), params, .scenario_color(n_col))
  )))
  rv_scenarios_result(NULL)
})

# Ajouter baseline
observeEvent(input$btn_add_baseline, {
  r <- rv_ngm_result()
  if (is.null(r) || !scal_lgl(r$ok)) return()
  params <- r$steps$`1_symbols`$param_names
  sc     <- rv_scenarios()

  # ETAPE 1 : sauvegarder les valeurs courantes des inputs existants
  sc <- .flush_inputs_to_scenarios(sc, params, input)

  n <- length(sc) + 1L

  # Tenter de pre-remplir depuis l'analyse locale
  sens <- tryCatch(rv_sensitivity_result(), error = function(e) NULL)
  param_vals <- setNames(lapply(params, function(p) {
    if (!is.null(sens) && !is.null(sens$sensitivities)) {
      s <- if (is.data.frame(sens$sensitivities)) sens$sensitivities
           else do.call(rbind, lapply(sens$sensitivities, as.data.frame))
      j <- which(s$parameter == p)
      if (length(j) > 0) return(as.numeric(s$value[j[1]]))
    }
    NA_real_
  }), params)

  baseline <- list(
    id     = paste0("sc", .sc_counter()),
    name   = "Baseline",
    color  = .scenario_color(n),
    params = param_vals
  )
  rv_scenarios(c(sc, list(baseline)))
  rv_scenarios_result(NULL)
})

# Effacer tous les scenarios
observeEvent(input$btn_clear_scenarios, {
  rv_scenarios(list())
  rv_scenarios_result(NULL)
})

# Supprimer un scenario specifique
observe({
  scenarios <- rv_scenarios()
  if (length(scenarios) == 0) return()
  r <- isolate(rv_ngm_result())
  if (is.null(r) || !scal_lgl(r$ok)) return()
  params <- r$steps$`1_symbols`$param_names
  for (sc in scenarios) {
    local({
      sc_id  <- sc$id
      del_id <- paste0("sc_del_", sc_id)
      observeEvent(input[[del_id]], {
        # Sauvegarder les valeurs courantes avant de modifier la liste
        scs <- rv_scenarios()
        scs <- .flush_inputs_to_scenarios(scs, params, input)
        idx <- which(sapply(scs, function(s) s$id) == sc_id)
        if (length(idx) > 0)
          rv_scenarios(scs[-idx])
        rv_scenarios_result(NULL)
      }, ignoreInit = TRUE, once = TRUE)
    })
  }
})


# =============================================================================
# SECTION 7 -- Calcul R0 pour tous les scenarios
# =============================================================================

observeEvent(input$btn_run_scenarios, {
  r <- rv_ngm_result()
  if (is.null(r) || !scal_lgl(r$ok)) {
    showNotification("Calculez d'abord R\u2080 symbolique.", type = "error")
    return()
  }

  # Capturer les valeurs courantes des inputs et les persister
  scenarios <- rv_scenarios()
  params    <- r$steps$`1_symbols`$param_names
  scenarios <- .flush_inputs_to_scenarios(scenarios, params, input)
  rv_scenarios(scenarios)

  R0_expr_str <- r$steps$`7_eigenvalues`$R0_expr_str

  if (length(scenarios) < 2) {
    showNotification("Definissez au moins 2 scenarios.", type = "warning")
    return()
  }

  # Valider completude
  errors <- character(0)
  for (sc in scenarios) {
    missing_p <- params[sapply(params, function(p) {
      v <- sc$params[[p]]
      is.null(v) || is.na(v)
    })]
    if (length(missing_p) > 0)
      errors <- c(errors, paste0(
        "'", sc$name, "' : manquant pour ",
        paste(missing_p, collapse = ", ")))
  }
  if (length(errors) > 0) {
    showNotification(paste(errors, collapse = " | "),
                     type = "error", duration = 10)
    return()
  }

  url <- NGM_BASE_URL
  if (is.null(url) || !api_health_check(url)) {
    showNotification("Serveur FastAPI non disponible.", type = "error")
    return()
  }

  # Modeles en methode numerique (K >= 5x5) : R0_expr_str vaut "?", il n'y a
  # pas de formule a evaluer. On recalcule K avec les valeurs de chaque
  # scenario via /compute_ngm (meme principe que M10 et M11).
  numeric_method <- is.null(R0_expr_str) ||
    identical(trimws(as.character(R0_expr_str)), "?") ||
    identical(scal_chr(r$steps$`7_eigenvalues`$method, ""), "numeric")
  ctx_base <- NULL
  if (numeric_method) {
    ctx_base <- build_sympy_context(rv_comp(), rv_flux(), rv_dfe(), rv_params())
    ctx_base$infected_states <- rv_infected_order()
  }
  eval_scenario <- function(params_sc) {
    if (!numeric_method)
      return(api_evaluate_r0(R0_expr_str, params_sc, url))
    ctx <- ctx_base
    ctx$param_vals <- params_sc
    rr <- api_compute_ngm(ctx, url)
    if (!scal_lgl(rr$ok))
      return(list(ok = FALSE,
                  error = scal_chr(rr$error, "Erreur de calcul NGM")))
    s7  <- rr$steps$`7_eigenvalues`
    val <- suppressWarnings(as.numeric(scal(s7$R0_numeric_direct)))
    if (length(val) == 0 || is.na(val))
      val <- suppressWarnings(as.numeric(scal(s7$R0_expr_str)))
    if (length(val) == 0 || is.na(val))
      return(list(ok = FALSE, error = "R0 numerique non disponible"))
    list(ok = TRUE, R0_numeric = val)
  }

  results <- list()
  withProgress(message = "Calcul des scenarios...", value = 0, {
    for (i in seq_along(scenarios)) {
      sc <- scenarios[[i]]
      incProgress(1 / length(scenarios),
                  detail = paste0(sc$name, " (", i, "/",
                                  length(scenarios), ")"))
      res <- tryCatch(
        eval_scenario(sc$params),
        error = function(e)
          list(ok = FALSE, error = e$message)
      )
      results[[i]] <- list(
        id    = sc$id,
        name  = sc$name,
        color = sc$color,
        params= sc$params,
        ok    = scal_lgl(res$ok),
        R0    = if (scal_lgl(res$ok)) round(as.numeric(scal(res$R0_numeric)), 10)
                else NA_real_,
        error = if (!scal_lgl(res$ok)) res$error else NULL
      )
    }
  })

  rv_scenarios_result(results)
  n_ok  <- if (length(results) > 0) sum(vapply(results, function(x) isTRUE(x$ok), logical(1))) else 0L
  n_err <- length(results) - n_ok
  msg   <- paste0("\u2705 ", n_ok, " scenario(s) calcule(s)")
  if (n_err > 0) msg <- paste0(msg, " | \u274c ", n_err, " erreur(s)")
  showNotification(msg, type = "message", duration = 4)
})


# =============================================================================
# SECTION 8 -- Affichage des resultats
# =============================================================================

output$scenarios_results_ui <- renderUI({
  results <- rv_scenarios_result()
  if (is.null(results)) return(NULL)

  ok_res  <- Filter(function(x) x$ok,  results)
  err_res <- Filter(function(x) !x$ok, results)

  if (length(ok_res) == 0)
    return(tags$div(
      style = "background:#f8d7da;border:1px solid #f5c6cb;border-radius:6px;
               padding:16px;margin-top:16px;",
      tags$h5("\u274c Tous les calculs ont echoue"),
      tags$ul(lapply(err_res, function(x)
        tags$li(tags$strong(x$name), " : ", x$error)))
    ))

  # Reference : "Baseline" ou premier scenario
  ref_idx <- which(tolower(sapply(ok_res, function(x) x$name)) == "baseline")
  if (length(ref_idx) == 0) ref_idx <- 1L
  ref_R0  <- ok_res[[ref_idx[1]]]$R0
  ref_nm  <- ok_res[[ref_idx[1]]]$name

  all_params <- names(ok_res[[1]]$params)

  tagList(
    tags$hr(),
    tags$h4("\U0001f4ca Resultats"),

    # Erreurs partielles
    if (length(err_res) > 0)
      tags$div(
        style = "background:#fff3cd;border:1px solid #ffc107;
                 border-radius:5px;padding:8px 14px;margin-bottom:12px;",
        tags$strong("\u26a0\ufe0f Echecs : "),
        paste(sapply(err_res, function(x)
          paste0(x$name, " (", x$error, ")")), collapse = " | ")
      ),

    # Graphique
    plotlyOutput("scenarios_bar_plot", height = "340px"),
    br(),

    # Tableau
    tags$h5("Tableau comparatif"),
    tags$p(style = "font-size:12px;color:#666;margin-bottom:6px;",
           paste0("Reference : ", ref_nm, " (R\u2080 = ", ref_R0, ")")),
    tags$div(
      style = "overflow-x:auto;",
      tags$table(
        class = "table table-striped table-bordered",
        style = "width:100%;font-size:13px;",
        tags$thead(tags$tr(
          style = "background:#343a40;color:white;",
          tags$th("Scenario"),
          tags$th("R\u2080"),
          tags$th("Statut"),
          tags$th(paste0("vs ", ref_nm)),
          tags$th("Reduction necessaire"),
          lapply(all_params, function(p)
            tags$th(tags$code(style = "color:#80d4ff;font-size:11px;", p)))
        )),
        tags$tbody(lapply(seq_along(ok_res), function(i) {
          res    <- ok_res[[i]]
          R0_val <- res$R0
          is_ref <- i == ref_idx[1]

          delta_pct <- if (!is.na(ref_R0) && ref_R0 != 0)
            round((R0_val - ref_R0) / ref_R0 * 100, 1) else NA
          delta_str <- if (!is.na(delta_pct))
            paste0(if (delta_pct > 0) "+" else "", delta_pct, "%")
          else "--"
          delta_col <- if (!is.na(delta_pct) && delta_pct < 0) "#28a745"
                       else if (!is.na(delta_pct) && delta_pct > 0) "#dc3545"
                       else "#333"

          reduc <- if (!is.na(R0_val) && R0_val > 1)
            paste0(round((1 - 1/R0_val) * 100, 1), "%")
          else "\u2705 Sous le seuil"

          tags$tr(
            style = if (is_ref) "background:rgba(0,0,0,0.04);" else "",
            tags$td(
              tags$div(style = "display:flex;align-items:center;gap:8px;",
                tags$div(style = paste0(
                  "width:12px;height:12px;border-radius:50%;",
                  "background:", res$color, ";")),
                tags$strong(res$name),
                if (is_ref) tags$span(
                  style = "font-size:10px;background:#6c757d;color:white;
                           padding:1px 5px;border-radius:8px;", "ref")
              )
            ),
            tags$td(style = "text-align:right;font-weight:700;font-size:15px;",
                    if (!is.na(R0_val)) format(round(R0_val, 4), nsmall = 4) else "Err"),
            tags$td(style = "text-align:center;", .epi_badge(R0_val)),
            tags$td(style = paste0("text-align:right;font-weight:600;",
                                   "color:", delta_col, ";"), delta_str),
            tags$td(style = "text-align:right;color:#555;", reduc),
            lapply(all_params, function(p) {
              v <- res$params[[p]]
              tags$td(style = "text-align:right;font-family:monospace;
                      font-size:12px;",
                      if (!is.null(v) && !is.na(v)) signif(v, 4) else "--")
            })
          )
        }))
      )
    ),

    # Interpretation
    uiOutput("scenarios_interp_ui")
  )
})

# Graphique a barres
output$scenarios_bar_plot <- renderPlotly({
  results <- rv_scenarios_result()
  req(results)
  ok_res <- Filter(function(x) x$ok, results)
  if (length(ok_res) == 0) return(NULL)

  nms  <- sapply(ok_res, function(x) x$name)
  R0s  <- sapply(ok_res, function(x) x$R0)
  bars <- sapply(ok_res, function(x) {
    if (x$R0 < 1)      "rgba(40,167,69,0.8)"
    else if (x$R0 < 2) "rgba(255,193,7,0.9)"
    else                "rgba(220,53,69,0.85)"
  })
  borders <- sapply(ok_res, function(x) x$color)

  hovers <- sapply(ok_res, function(x) {
    pstr <- paste(sapply(names(x$params), function(p) {
      v <- x$params[[p]]
      if (!is.null(v) && !is.na(v)) paste0(p, " = ", signif(v, 4))
      else paste0(p, " = ?")
    }), collapse = "<br>")
    paste0("<b>", x$name, "</b><br>R\u2080 = ", round(x$R0, 4), "<br><br>", pstr)
  })

  plot_ly(x = nms, y = R0s, type = "bar",
          marker   = list(color = bars,
                          line  = list(color = borders, width = 2)),
          text     = round(R0s, 4), textposition = "outside",
          hovertext = hovers, hoverinfo = "text") %>%
    layout(
      xaxis  = list(title = ""),
      yaxis  = list(title = "R\u2080", rangemode = "tozero",
                    gridcolor = "#e9ecef"),
      title  = list(text = "R\u2080 par scenario", x = 0.5),
      shapes = list(list(
        type = "line", x0 = -0.5, x1 = length(nms) - 0.5,
        y0 = 1, y1 = 1,
        line = list(color = "#dc3545", width = 2, dash = "dash")
      )),
      annotations = list(list(
        x = length(nms) - 0.5, y = 1, text = "Seuil R\u2080 = 1",
        xanchor = "right", yanchor = "bottom", showarrow = FALSE,
        font = list(color = "#dc3545", size = 11)
      )),
      plot_bgcolor  = "white", paper_bgcolor = "white",
      margin = list(t = 60, b = 40)
    ) %>%
    config(displayModeBar = FALSE)
})

# Interpretation automatique
output$scenarios_interp_ui <- renderUI({
  results <- rv_scenarios_result()
  req(results)
  ok_res <- Filter(function(x) x$ok, results)
  if (length(ok_res) < 2) return(NULL)

  R0s  <- sapply(ok_res, function(x) x$R0)
  nms  <- sapply(ok_res, function(x) x$name)
  best <- which.min(R0s)
  worst<- which.max(R0s)

  ref_idx <- which(tolower(nms) == "baseline")
  if (length(ref_idx) == 0) ref_idx <- 1L

  under  <- nms[R0s < 1]

  tags$div(
    style = "background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;
             padding:14px;margin-top:12px;",
    tags$h5(style = "margin-top:0;", "\U0001f4a1 Interpretation"),
    tags$ul(style = "font-size:13px;margin-bottom:0;line-height:1.9;",
      tags$li("Meilleur scenario : ", tags$strong(style = "color:#28a745;",
        nms[best]), paste0(" (R\u2080 = ", R0s[best], ")",
        if (R0s[best] < 1) " \u2705 sous le seuil" else "")),
      tags$li("Pire scenario : ", tags$strong(style = "color:#dc3545;",
        nms[worst]), paste0(" (R\u2080 = ", R0s[worst], ")")),
      if (ref_idx != best) tags$li(
        paste0("Reduction de R\u2080 par rapport a ",
               nms[ref_idx], " : ",
               round((R0s[ref_idx] - R0s[best]) / R0s[ref_idx] * 100, 1),
               "% (", round(R0s[ref_idx], 4), " \u2192 ",
               round(R0s[best], 4), ").")),
      if (length(under) > 0)
        tags$li(paste(length(under), "scenario(s) sous le seuil : "),
                tags$strong(paste(under, collapse = ", ")), ".")
      else
        tags$li(style = "color:#dc3545;",
                tags$strong("Aucun scenario"), " ne passe sous R\u2080 = 1. ",
                paste0("Reduction minimale necessaire : ",
                       round((1 - 1/min(R0s)) * 100, 1), "%."))
    )
  )
})
