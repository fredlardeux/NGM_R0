# =============================================================================
# M11_server_sensitivity.R -- Server : Analyse de sensibilité
# =============================================================================
# Dépendances : M01, M06 (detect_effect_type, calculate_impact, etc.)
# Helper : extraire les params qui apparaissent reellement dans R0_expr_str
.params_in_R0 <- function(r) {
  if (is.null(r) || !scal_lgl(r$ok)) return(character(0))
  all_p   <- r$steps$`1_symbols`$param_names %||% character(0)
  R0_expr <- r$steps$`7_eigenvalues`$R0_expr_str %||% ""
  if (grepl("^[0-9.e+\\-]+$", trimws(R0_expr))) return(all_p)
  in_R0 <- Filter(function(p)
    isTRUE(grepl(paste0("\\b", p, "\\b"), R0_expr, perl = TRUE)), all_p)
  if (length(in_R0) == 0) all_p else in_R0
}

# Helper : detecter si R0 est en methode numerique (pas de formule symbolique)
.is_numeric_method <- function(r) {
  if (is.null(r) || !scal_lgl(r$ok)) return(FALSE)
  R0_expr <- r$steps$`7_eigenvalues`$R0_expr_str %||% ""
  trimws(R0_expr) == "?" || r$steps$`7_eigenvalues`$method == "numeric"
}

# Helper : evaluer R0 numeriquement via compute_ngm (pour methode numerique)
.eval_R0_numeric_ngm <- function(r, param_vals, url) {
  ctx <- build_sympy_context(rv_comp(), rv_flux(), rv_dfe(), rv_params())
  ctx$infected_states <- rv_infected_order()
  ctx$param_vals      <- param_vals
  res <- api_compute_ngm(ctx, url)
  if (!scal_lgl(res$ok)) return(list(ok=FALSE, error=res$error))
  R0_val <- res$steps$`7_eigenvalues`$R0_numeric_direct %||%
            as.numeric(res$steps$`7_eigenvalues`$R0_expr_str)
  list(ok=TRUE, R0_numeric=R0_val)
}

# ReactiveVals attendues : rv_ngm_result, rv_params, ngm_py
# ReactiveVals déclarées ici : rv_sensitivity_params, rv_sensitivity_result,
#                               rv_heatmap_result
# =============================================================================


# =============================================================================
# SECTION 1 -- ReactiveVals locaux
# =============================================================================

rv_sensitivity_params <- reactiveVal(list())
rv_sensitivity_result <- reactiveVal(NULL)
rv_heatmap_result     <- reactiveVal(NULL)


# =============================================================================
# SECTION 2 -- Onglet Sensitivity Analysis
# =============================================================================

output$sensitivity_content <- renderUI({
  r <- rv_ngm_result()
  if (is.null(r) || !scal_lgl(r$ok))
    return(tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;border-radius:6px;
               padding:30px;text-align:center;margin-top:40px;",
      tags$h4(style = "color:#856404;",
              "\U0001f3af Sensitivity Analysis"),
      tags$p("Calculez d'abord R\u2080 symbolique dans ",
             strong("Symbolic / NGM"), ".")))

  R0_latex     <- r$steps$`7_eigenvalues`$R0_latex
  params_in_R0 <- .params_in_R0(r)

  if (length(params_in_R0) == 0)
    return(tags$div(
      style = "background:#d4edda;border:1px solid #c3e6cb;border-radius:6px;
               padding:30px;text-align:center;margin-top:40px;",
      tags$h4(style = "color:#155724;", "\u2705 R\u2080 est constant"),
      tags$p("Aucun param\u00e8tre \u2014 pas d'analyse de sensibilit\u00e9 n\u00e9cessaire.")))

  tagList(
    tags$h3("\U0001f3af Analyse de Sensibilit\u00e9 de R\u2080"),
    tags$div(
      style = "background:#f0f7ff;border:1px solid #4a90d9;border-radius:6px;
               padding:14px;margin-bottom:20px;",
      tags$h5(style = "margin-top:0;color:#1a5fa8;", "Expression R\u2080"),
      tags$div(style = "background:white;padding:16px;border-radius:4px;
               overflow-x:auto;text-align:center;font-size:18px;",
               withMathJax(paste0("\\[ R_0 = ", R0_latex, " \\]")))),

    tabsetPanel(
      id   = "sensitivity_subtabs",
      type = "pills",
      tabPanel("\u2460 \u00c9lasticit\u00e9",   br(), uiOutput("elasticity_content")),
      tabPanel("\u2461 Tornado Plot",          br(), uiOutput("tornado_content")),
      tabPanel("\u2462 Heatmap 2D",            br(), uiOutput("heatmap2d_content")),
      tabPanel("④ Sensibilité Globale",  br(), uiOutput("global_sens_content")))
  )
})


# =============================================================================
# SECTION 3 -- Onglet Élasticité
# =============================================================================

output$elasticity_content <- renderUI({
  r <- rv_ngm_result()
  if (is.null(r)) return(NULL)
  params_in_R0 <- .params_in_R0(r)

  tagList(
    tags$h4("Valeurs de r\u00e9f\u00e9rence (sc\u00e9nario baseline)"),
    tags$p(style = "color:#666;font-size:13px;margin-bottom:4px;",
           "Fournissez les valeurs num\u00e9riques pour le calcul local des \u00e9lasticit\u00e9s."),
    tags$div(
      style = "background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;
               padding:16px;margin-bottom:20px;",
      tags$table(
        style = "width:100%;border-collapse:collapse;",
        tags$thead(tags$tr(
          tags$th(style = "text-align:left;padding:8px;border-bottom:2px solid #ddd;",
                  "Param\u00e8tre"),
          tags$th(style = "text-align:left;padding:8px;border-bottom:2px solid #ddd;",
                  "Valeur"),
          tags$th(style = "text-align:left;padding:8px;border-bottom:2px solid #ddd;",
                  "Unit\u00e9s"),
          tags$th(style = "text-align:left;padding:8px;border-bottom:2px solid #ddd;",
                  "Description"))),
        tags$tbody(lapply(params_in_R0, function(pname) {
          pt    <- rv_params()
          desc  <- if (nrow(pt) > 0 && pname %in% pt$param_name)
            pt$description[pt$param_name == pname] else ""
          units <- if (nrow(pt) > 0 && pname %in% pt$param_name)
            pt$units[pt$param_name == pname] else ""
          tags$tr(
            tags$td(style = "padding:8px;font-weight:600;", pname),
            tags$td(style = "padding:8px;",
                    numericInput(paste0("sens_param_", pname),
                                 label = NULL, value = 1,
                                 min = 0.001, step = 0.1,
                                 width = "150px")),
            tags$td(style = "padding:8px;color:#666;font-size:12px;",
                    units),
            tags$td(style = "padding:8px;color:#666;font-size:12px;",
                    desc))
        })))),

    tags$div(
      style = "background:#e7f3ff;border-left:3px solid #2196F3;
               padding:12px 16px;margin-bottom:20px;border-radius:0;",
      tags$p(style = "margin:0;font-size:13px;color:#0d47a1;",
             tags$strong("\U0001f4cd Analyse locale : "),
             "\u00c9lasticit\u00e9s calcul\u00e9es au point de r\u00e9f\u00e9rence ci-dessus.")),

    tags$div(
      style = "display:flex;gap:12px;align-items:center;margin-bottom:20px;",
      actionButton("btn_compute_sensitivity",
                   "\u2699\ufe0f Calculer l'\u00e9lasticit\u00e9",
                   class = "btn-primary btn-lg"),
      actionButton("btn_reset_sensitivity", "\u21ba R\u00e9initialiser",
                   class = "btn-outline-secondary")),
    uiOutput("sensitivity_results"))
})


# =============================================================================
# SECTION 4 -- Calcul de la sensibilité
# =============================================================================

observeEvent(input$btn_compute_sensitivity, {
  r <- rv_ngm_result()
  if (is.null(r) || !scal_lgl(r$ok)) {
    showNotification("Calculez d'abord R\u2080 symbolique.", type = "error")
    return()
  }

  R0_expr_str  <- r$steps$`7_eigenvalues`$R0_expr_str
  params_in_R0 <- .params_in_R0(r)

  param_vals <- list()
  missing    <- character(0)
  for (pname in params_in_R0) {
    val <- input[[paste0("sens_param_", pname)]]
    if (is.null(val) || is.na(val) || val <= 0)
      missing <- c(missing, pname)
    else
      param_vals[[pname]] <- val
  }
  if (length(missing) > 0) {
    showNotification(paste0("Valeurs manquantes ou invalides : ",
                             paste(missing, collapse = ", ")),
                     type = "error", duration = 8)
    return()
  }
  rv_sensitivity_params(param_vals)
  tryCatch({
    showNotification("\u2699\ufe0f Calcul en cours...",
                     id = "sens_compute", type = "message", duration = NULL)
  url <- NGM_BASE_URL
  if (is.null(url)) { showNotification("Serveur FastAPI non disponible.", type="error"); return(NULL) }

    # Pour les grands modeles (methode numerique), utiliser differences finies
    if (.is_numeric_method(r)) {
      result <- tryCatch({
        # Calculer R0 baseline
        r0_base <- .eval_R0_numeric_ngm(r, param_vals, url)
        if (!scal_lgl(r0_base$ok))
          return(list(ok=FALSE, error=r0_base$error %||% "Erreur calcul R0 baseline"))
        R0_val <- r0_base$R0_numeric
        eps    <- 0.001  # 0.1% de perturbation
        sensitivities_list <- lapply(seq_along(params_in_R0), function(ki) {
          pname   <- params_in_R0[ki]
          pv_orig <- as.numeric(param_vals[[pname]])
          # R0 avec param + eps
          pv_h <- param_vals; pv_h[[pname]] <- pv_orig * (1 + eps)
          r0_h <- .eval_R0_numeric_ngm(r, pv_h, url)
          # R0 avec param - eps
          pv_l <- param_vals; pv_l[[pname]] <- pv_orig * (1 - eps)
          r0_l <- .eval_R0_numeric_ngm(r, pv_l, url)
          if (!scal_lgl(r0_h$ok) || !scal_lgl(r0_l$ok)) return(NULL)
          deriv     <- (r0_h$R0_numeric - r0_l$R0_numeric) / (2 * eps * pv_orig)
          elasticity <- deriv * pv_orig / R0_val
          list(rank=ki, parameter=pname, value=pv_orig,
               derivative=deriv, elasticity=elasticity,
               sensitivity_index=abs(elasticity),
               direction=if(elasticity>=0) "positive" else "negative")
        })
        sensitivities_list <- Filter(Negate(is.null), sensitivities_list)
        # Trier par |elasticite|
        ord <- order(sapply(sensitivities_list,
                            function(x) abs(x$sensitivity_index)),
                     decreasing=TRUE)
        sensitivities_list <- sensitivities_list[ord]
        for (ki in seq_along(sensitivities_list))
          sensitivities_list[[ki]]$rank <- ki
        list(ok=TRUE, R0_value=R0_val,
             sensitivities=do.call(rbind, lapply(sensitivities_list, as.data.frame)),
             method="numeric_fd")
      }, error=function(e) list(ok=FALSE, error=e$message))
    } else {
      result <- api_sensitivity_local(R0_expr_str, param_vals, url)
    }
    removeNotification("sens_compute")

    if (scal_lgl(result$ok)) {
      rv_sensitivity_result(result)
      showNotification("\u2705 Analyse de sensibilit\u00e9 termin\u00e9e.",
                       type = "message", duration = 3)
    } else {
      showNotification(paste0("\u274c ", result$error),
                       type = "error", duration = 10)
      rv_sensitivity_result(NULL)
    }
  }, error = function(e) {
    removeNotification("sens_compute")
    showNotification(paste0("\u274c Erreur Python : ", e$message),
                     type = "error", duration = 10)
  })
})

observeEvent(input$btn_reset_sensitivity, {
  r <- rv_ngm_result()
  if (is.null(r) || !scal_lgl(r$ok)) return()
  params_in_R0 <- .params_in_R0(r)
  rv_sensitivity_params(list())
  rv_sensitivity_result(NULL)
  for (pname in params_in_R0)
    updateNumericInput(session, paste0("sens_param_", pname), value = 1)
  showNotification("Valeurs r\u00e9initialis\u00e9es \u00e0 1.", type = "message", duration = 3)
})


# =============================================================================
# SECTION 5 -- Affichage des résultats de sensibilité
# =============================================================================

output$sensitivity_results <- renderUI({
  sens_data <- rv_sensitivity_result()
  if (is.null(sens_data)) return(NULL)

  R0_value      <- sens_data$R0_value
  sensitivities <- sens_data$sensitivities

  # fromJSON peut retourner data.frame ou liste selon les cas
  if (is.data.frame(sensitivities)) {
    sens_df <- sensitivities
  } else {
    sens_df <- do.call(rbind, lapply(sensitivities, function(s)
      data.frame(
        rank              = s$rank,
        parameter         = s$parameter,
        value             = s$value,
        derivative        = s$derivative,
        elasticity        = s$elasticity,
        sensitivity_index = s$sensitivity_index,
        direction         = s$direction,
        stringsAsFactors  = FALSE)))
  }

  top_param      <- sens_df$parameter[1]
  top_elasticity <- sens_df$elasticity[1]

  tagList(
    # R0 baseline
    tags$div(
      style = "background:#d4edda;border:1px solid #c3e6cb;border-radius:6px;
               padding:20px;margin-bottom:20px;",
      tags$h4(style = "color:#155724;margin-top:0;", "\u2705 Valeur R\u2080"),
      tags$div(style = "font-size:28px;font-weight:700;color:#155724;
               font-family:monospace;text-align:center;",
               paste0("R\u2080 = ", round(R0_value, 4)))),

    # Table élasticités
    tags$h4("\u00c9lasticit\u00e9s"),
    tags$div(
      style = "overflow-x:auto;margin-bottom:20px;",
      tags$table(
        class = "table table-striped table-bordered",
        style = "width:100%;",
        tags$thead(tags$tr(
          style = "background:#f8f9fa;",
          tags$th("Rang"), tags$th("Param\u00e8tre"),
          tags$th("Valeur"), tags$th("\u2202R\u2080/\u2202p"),
          tags$th("\u00c9lasticit\u00e9 (\u03b5)"),
          tags$th("Impact"), tags$th("Direction"))),
        tags$tbody(lapply(seq_len(nrow(sens_df)), function(i) {
          row <- sens_df[i, ]
          impact_badge <- if (row$sensitivity_index > 1)
            tags$span(style = "background:#dc3545;color:white;padding:3px 8px;
                      border-radius:12px;font-size:11px;font-weight:600;", "HIGH")
          else if (row$sensitivity_index > 0.5)
            tags$span(style = "background:#ffc107;color:#000;padding:3px 8px;
                      border-radius:12px;font-size:11px;font-weight:600;", "MEDIUM")
          else
            tags$span(style = "background:#28a745;color:white;padding:3px 8px;
                      border-radius:12px;font-size:11px;font-weight:600;", "LOW")
          dir_icon <- if (row$direction == "positive") "\u2b06\ufe0f" else "\u2b07\ufe0f"
          tags$tr(
            tags$td(style = "text-align:center;font-weight:600;", row$rank),
            tags$td(style = "font-family:monospace;font-weight:600;", row$parameter),
            tags$td(style = "text-align:right;", round(row$value, 4)),
            tags$td(style = "text-align:right;", round(row$derivative, 4)),
            tags$td(style = "text-align:right;font-weight:600;",
                    round(row$elasticity, 4)),
            tags$td(style = "text-align:center;", impact_badge),
            tags$td(style = "text-align:center;font-size:18px;", dir_icon))
        })))),

    # Interprétation mécanistique
    tags$h4("Interpr\u00e9tation m\u00e9canistique"),
    tags$div(
      style = "background:#fff9e6;border-left:4px solid #ffc107;
               padding:16px;border-radius:0;margin-bottom:16px;",
      local({
        top_1       <- sens_df[1, ]
        effect_type <- detect_effect_type(top_1$elasticity)
        interp      <- generate_interpretation(top_1$parameter,
                                               top_1$elasticity, effect_type)
        tags$p(style = "margin:0;font-size:14px;line-height:1.6;",
               tags$strong("Param\u00e8tre le plus influent : "),
               tags$code(top_1$parameter),
               tags$br(),
               HTML(gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", interp)))
      })),

    # Stratégies de contrôle
    tags$h4("Strat\u00e9gies de contr\u00f4le"),
    tags$div(
      style = "background:#e7f3ff;border-left:4px solid #2196F3;
               padding:16px;border-radius:0;margin-bottom:20px;",
      tags$div(
        style = "font-family:monospace;font-size:13px;line-height:1.8;",
        lapply(seq_len(min(3, nrow(sens_df))), function(i) {
          row <- sens_df[i, ]
          rec <- generate_control_recommendation(row$parameter,
                                                 row$elasticity)
          tags$div(sprintf("%d. ", i),
                   tags$strong(style = "color:#0066cc;", row$parameter),
                   sprintf(" (\u03b5 = %.2f) : ", row$elasticity),
                   rec)
        })))
  )
})


# =============================================================================
# =============================================================================
# SECTION 6 \u2014 Tornado Plot interactif avec curseur de variation
# =============================================================================

# ReactiveVal pour la variation (%) choisie par l'utilisateur
rv_tornado_pct <- reactiveVal(20)

output$tornado_content <- renderUI({
  sens_data <- rv_sensitivity_result()
  if (is.null(sens_data))
    return(tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;padding:20px;
               border-radius:6px;text-align:center;margin-top:20px;",
      tags$p(style = "color:#856404;font-size:14px;",
             "Calculez l'\u00e9lasticit\u00e9 d'abord dans l'onglet ",
             strong("\u2460 \u00c9lasticit\u00e9"), ".")))

  tagList(
    tags$h4("\U0001f32a\ufe0f Tornado Diagram interactif"),
    tags$p(style = "color:#666;font-size:13px;",
           "Faites glisser le curseur pour changer la plage de variation.",
           " Les barres se mettent \u00e0 jour en temps r\u00e9el."),

    # ── Curseur de variation ────────────────────────────────────────────────
    tags$div(
      style = "background:#f0f7ff;border:1px solid #4a90d9;border-radius:6px;
               padding:14px;margin-bottom:16px;",
      tags$div(style = "display:flex;align-items:center;gap:16px;flex-wrap:wrap;",
        tags$span(style = "font-weight:600;color:#1a5fa8;white-space:nowrap;",
                  "Plage de variation :"),
        tags$div(style = "flex:1;min-width:200px;",
          sliderInput("tornado_variation_pct",
                      label = NULL,
                      min = 5, max = 75, value = 20, step = 5,
                      post = "%", width = "100%")),
        uiOutput("tornado_r0_display")
      )
    ),

    # ── Graphe ──────────────────────────────────────────────────────────────
    plotlyOutput("tornado_plot", height = "500px"),
    br(),

    # ── Interpr\u00e9tation ────────────────────────────────────────────────────────
    uiOutput("tornado_interpretation")
  )
})

# Affichage R0 baseline
output$tornado_r0_display <- renderUI({
  sens_data <- rv_sensitivity_result()
  if (is.null(sens_data)) return(NULL)
  tags$div(
    style = "background:#d4edda;border:1px solid #c3e6cb;border-radius:6px;
             padding:6px 14px;white-space:nowrap;",
    tags$span(style = "font-weight:600;color:#155724;",
              paste0("Baseline R\u2080 = ", round(sens_data$R0_value, 4)))
  )
})

# ── Calcul et rendu du tornado ────────────────────────────────────────────────
output$tornado_plot <- renderPlotly({
  sens_data <- rv_sensitivity_result()
  req(sens_data)

  pct <- (input$tornado_variation_pct %||% 20) / 100

  R0_value <- sens_data$R0_value
  sensitivities <- if (is.data.frame(sens_data$sensitivities))
    sens_data$sensitivities
  else
    do.call(rbind, lapply(sens_data$sensitivities, as.data.frame))

  param_vals  <- rv_sensitivity_params()
  r           <- rv_ngm_result()
  R0_expr_str <- r$steps$`7_eigenvalues`$R0_expr_str
  url <- NGM_BASE_URL
  if (is.null(url)) { showNotification("Serveur FastAPI non disponible.", type="error"); return(NULL) }
  # Calculer impact pour chaque param\u00e8tre
  tornado_data <- lapply(seq_len(nrow(sensitivities)), function(i) {
    param    <- sensitivities$parameter[i]
    base_val <- sensitivities$value[i]
    tryCatch({
      ph <- param_vals; ph[[param]] <- base_val * (1 + pct)
      pl <- param_vals; pl[[param]] <- base_val * (1 - pct)
      rh <- if (.is_numeric_method(r))
        .eval_R0_numeric_ngm(r, ph, url)
      else
        api_evaluate_r0(R0_expr_str, ph, url)
      rl <- if (.is_numeric_method(r))
        .eval_R0_numeric_ngm(r, pl, url)
      else
        api_evaluate_r0(R0_expr_str, pl, url)
      if (!scal_lgl(rh$ok) || !scal_lgl(rl$ok)) return(NULL)
      imp_h <- (rh$R0_numeric - R0_value) / R0_value * 100
      imp_l <- (rl$R0_numeric - R0_value) / R0_value * 100
      list(parameter   = param,
           impact_high = imp_h,
           impact_low  = imp_l,
           total_range = abs(imp_h) + abs(imp_l),
           R0_high     = rh$R0_numeric,
           R0_low      = rl$R0_numeric)
    }, error = function(e) NULL)
  })

  tornado_data <- Filter(Negate(is.null), tornado_data)
  if (length(tornado_data) == 0)
    return(plot_ly() %>% layout(title = "Calcul \u00e9chou\u00e9"))

  # Trier par range total (plus influent en haut)
  tornado_data <- tornado_data[
    order(sapply(tornado_data, function(x) x$total_range), decreasing = FALSE)
  ]

  params      <- sapply(tornado_data, function(x) x$parameter)
  impact_high <- sapply(tornado_data, function(x) x$impact_high)
  impact_low  <- sapply(tornado_data, function(x) x$impact_low)
  R0_high     <- sapply(tornado_data, function(x) x$R0_high)
  R0_low      <- sapply(tornado_data, function(x) x$R0_low)
  pct_label   <- paste0("+", round(pct * 100), "%")
  mct_label   <- paste0("-", round(pct * 100), "%")

  plot_ly() %>%
    add_trace(
      y = params, x = impact_low,
      type = "bar", orientation = "h",
      name = mct_label,
      marker = list(color = "rgba(55,128,191,0.75)",
                    line  = list(color = "rgba(55,128,191,1)", width = 1)),
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        mct_label, " param\u00e8tre<br>",
        "Impact R\u2080 : %{x:+.2f}%<br>",
        "R\u2080 = ", round(R0_low, 4), "<extra></extra>")
    ) %>%
    add_trace(
      y = params, x = impact_high,
      type = "bar", orientation = "h",
      name = pct_label,
      marker = list(color = "rgba(219,64,82,0.75)",
                    line  = list(color = "rgba(219,64,82,1)", width = 1)),
      hovertemplate = paste0(
        "<b>%{y}</b><br>",
        pct_label, " param\u00e8tre<br>",
        "Impact R\u2080 : %{x:+.2f}%<br>",
        "R\u2080 = ", round(R0_high, 4), "<extra></extra>")
    ) %>%
    layout(
      barmode  = "overlay",
      xaxis    = list(
        title      = paste0("Impact sur R\u2080 (%) pour \u00b1",
                            round(pct * 100), "% de variation"),
        zeroline      = TRUE,
        zerolinewidth = 2,
        zerolinecolor = "#333"
      ),
      yaxis  = list(title = "", categoryorder = "trace"),
      shapes = list(list(
        type    = "line",
        xref    = "x", yref = "paper",
        x0      = 0,   x1 = 0,
        y0      = 0,   y1 = 1,
        line    = list(color = "#333333", width = 2, dash = "dot")
      )),
      title    = list(
        text = paste0(
          "\U0001f32a\ufe0f Tornado Plot interactif<br>",
          "<sub>Baseline R\u2080 = ", round(R0_value, 4),
          "  |  Variation : \u00b1", round(pct * 100), "%</sub>"),
        x = 0.5),
      showlegend = TRUE,
      legend     = list(x = 0.85, y = 0.05,
                        bgcolor = "rgba(255,255,255,0.8)",
                        bordercolor = "#ccc", borderwidth = 1),
      margin     = list(l = 110, r = 50, t = 90, b = 60)
    ) %>%
    config(displayModeBar = TRUE, displaylogo = FALSE,
           modeBarButtonsToRemove = c("select2d", "lasso2d"))
})

# ── Interpr\u00e9tation automatique du tornado ────────────────────────────────────
output$tornado_interpretation <- renderUI({
  sens_data <- rv_sensitivity_result()
  req(sens_data)
  pct <- (input$tornado_variation_pct %||% 20) / 100

  sensitivities <- if (is.data.frame(sens_data$sensitivities))
    sens_data$sensitivities
  else
    do.call(rbind, lapply(sens_data$sensitivities, as.data.frame))

  if (nrow(sensitivities) == 0) return(NULL)

  top1 <- sensitivities[1, ]
  top2 <- if (nrow(sensitivities) >= 2) sensitivities[2, ] else NULL

  impact_top1 <- round(calculate_impact(top1$elasticity, pct), 1)

  tags$div(
    style = "background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;
             padding:14px;",
    tags$h5(style = "margin-top:0;color:#495057;",
            "\U0001f4a1 Lecture du graphe"),
    tags$ul(style = "margin-bottom:0;font-size:13px;",
      tags$li(
        tags$strong(style = "color:#dc3545;", top1$parameter),
        sprintf(" est le param\u00e8tre le plus influent : une variation de \u00b1%d%%",
                round(pct * 100)),
        sprintf(" change R\u2080 d'environ \u00b1%.1f%%.", impact_top1)
      ),
      if (!is.null(top2)) {
        ratio <- if (top2$sensitivity_index > 0)
          round(top1$sensitivity_index / top2$sensitivity_index, 1) else NA
        tags$li(
          tags$strong(style = "color:#3498db;", top1$parameter),
          if (!is.na(ratio) && ratio > 1)
            paste0(" est ", ratio, "x plus influent que "),
          if (!is.na(ratio) && ratio > 1)
            tags$strong(top2$parameter),
          "."
        )
      },
      tags$li(
        "La ligne pointill\u00e9e verticale repr\u00e9sente R\u2080 baseline (variation = 0%).",
        " Les barres ",
        tags$span(style = "color:#dc3545;font-weight:600;", "rouges"),
        " = hausse du param\u00e8tre, ",
        tags$span(style = "color:#3498db;font-weight:600;", "bleues"),
        " = baisse."
      )
    )
  )
})


# =============================================================================
# SECTION 7 -- Heatmap 2D
# =============================================================================

output$heatmap2d_content <- renderUI({
  r <- rv_ngm_result()
  if (is.null(r)) return(NULL)
  params_in_R0 <- .params_in_R0(r)

  if (length(params_in_R0) < 2)
    return(tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;padding:20px;
               border-radius:6px;text-align:center;margin-top:20px;",
      tags$h4(style = "color:#856404;",
              "\u26a0\ufe0f Au moins 2 param\u00e8tres n\u00e9cessaires"),
      tags$p("R\u2080 ne contient que ", length(params_in_R0), " param\u00e8tre(s).")))

  tagList(
    tags$h4("Heatmap 2D des param\u00e8tres"),
    tags$div(
      style = "background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;
               padding:20px;margin-bottom:20px;",
      fluidRow(
        column(6,
               tags$h5(style = "margin-top:0;", "Axe X"),
               selectInput("heatmap_param_x", "Param\u00e8tre :",
                           choices  = c("-- S\u00e9lectionner --" = "",
                                        params_in_R0),
                           selected = ""),
               uiOutput("heatmap_x_range_ui")),
        column(6,
               tags$h5(style = "margin-top:0;", "Axe Y"),
               uiOutput("heatmap_param_y_ui"),
               uiOutput("heatmap_y_range_ui"))),
      uiOutput("heatmap_fixed_params_ui"),
      tags$div(style = "margin-top:20px;",
               actionButton("btn_generate_heatmap",
                            "\U0001f525 G\u00e9n\u00e9rer la Heatmap",
                            class = "btn-primary btn-lg"),
               tags$span(style = "margin-left:12px;color:#666;font-size:13px;",
                         "\u23f1\ufe0f 10-30 secondes selon la grille"))),
    uiOutput("heatmap2d_results"))
})

output$heatmap_x_range_ui <- renderUI({
  if (is.null(input$heatmap_param_x) || input$heatmap_param_x == "")
    return(NULL)
  fluidRow(
    column(4, numericInput("heatmap_x_min",   "Min :", value = NULL, step = 0.1)),
    column(4, numericInput("heatmap_x_max",   "Max :", value = NULL, step = 0.1)),
    column(4, numericInput("heatmap_x_steps", "Pts :", value = 20, min = 5, max = 50)))
})

output$heatmap_param_y_ui <- renderUI({
  r <- rv_ngm_result(); if (is.null(r)) return(NULL)
  params_in_R0 <- .params_in_R0(r)
  param_x      <- input$heatmap_param_x
  available    <- if (!is.null(param_x) && param_x != "")
    setdiff(params_in_R0, param_x) else params_in_R0
  selectInput("heatmap_param_y", "Param\u00e8tre :",
              choices  = c("-- S\u00e9lectionner --" = "", available),
              selected = "")
})

output$heatmap_y_range_ui <- renderUI({
  if (is.null(input$heatmap_param_y) || input$heatmap_param_y == "")
    return(NULL)
  fluidRow(
    column(4, numericInput("heatmap_y_min",   "Min :", value = NULL, step = 0.01)),
    column(4, numericInput("heatmap_y_max",   "Max :", value = NULL, step = 0.1)),
    column(4, numericInput("heatmap_y_steps", "Pts :", value = 20, min = 5, max = 50)))
})

output$heatmap_fixed_params_ui <- renderUI({
  r <- rv_ngm_result(); if (is.null(r)) return(NULL)
  params_in_R0 <- .params_in_R0(r)
  param_x <- input$heatmap_param_x %||% ""
  param_y <- input$heatmap_param_y %||% ""
  fixed   <- setdiff(params_in_R0, c(param_x, param_y))
  fixed   <- fixed[fixed != ""]
  if (length(fixed) == 0) return(NULL)
  tagList(
    tags$hr(),
    tags$h5("Param\u00e8tres fixes"),
    tags$div(
      style = "display:grid;grid-template-columns:repeat(auto-fill,
               minmax(200px,1fr));gap:12px;",
      lapply(fixed, function(pname)
        tags$div(numericInput(paste0("heatmap_fixed_", pname),
                              label = pname, value = NULL,
                              min = 0.001, step = 0.1)))))
})

observeEvent(input$btn_generate_heatmap, {
  r <- rv_ngm_result()
  if (is.null(r) || !scal_lgl(r$ok)) {
    showNotification("Calculez d'abord R\u2080 symbolique.", type = "error")
    return()
  }

  param_x <- input$heatmap_param_x
  param_y <- input$heatmap_param_y
  if (is.null(param_x) || is.null(param_y) ||
      param_x == "" || param_y == "" || param_x == param_y) {
    showNotification("S\u00e9lectionnez deux param\u00e8tres diff\u00e9rents.",
                     type = "error"); return()
  }

  x_min   <- as.numeric(input$heatmap_x_min)
  x_max   <- as.numeric(input$heatmap_x_max)
  x_steps <- as.integer(input$heatmap_x_steps)
  y_min   <- as.numeric(input$heatmap_y_min)
  y_max   <- as.numeric(input$heatmap_y_max)
  y_steps <- as.integer(input$heatmap_y_steps)

  if (any(is.na(c(x_min, x_max, y_min, y_max, x_steps, y_steps)))) {
    showNotification("Remplissez tous les min/max/pts.", type = "error")
    return()
  }
  if (x_min >= x_max || y_min >= y_max) {
    showNotification("Min doit \u00eatre < Max.", type = "error"); return()
  }

  R0_expr_str  <- r$steps$`7_eigenvalues`$R0_expr_str
  params_in_R0 <- .params_in_R0(r)

  # Paramètres fixes
  fixed_params  <- list()
  missing_fixed <- character(0)
  for (pname in setdiff(params_in_R0, c(param_x, param_y))) {
    val <- input[[paste0("heatmap_fixed_", pname)]]
    if (!is.null(val) && !is.na(val) && val != "")
      fixed_params[[pname]] <- as.numeric(val)
    else
      missing_fixed <- c(missing_fixed, pname)
  }
  if (length(missing_fixed) > 0) {
    showNotification(paste0("Valeurs manquantes pour : ",
                             paste(missing_fixed, collapse = ", ")),
                     type = "error", duration = 10); return()
  }

  showNotification("\U0001f504 G\u00e9n\u00e9ration heatmap...",
                   duration = NULL, id = "heatmap_progress")

  x_values  <- seq(x_min, x_max, length.out = x_steps)
  y_values  <- seq(y_min, y_max, length.out = y_steps)
  R0_matrix <- matrix(NA_real_, nrow = y_steps, ncol = x_steps)
  tryCatch({
  url <- NGM_BASE_URL
  if (is.null(url)) { showNotification("Serveur FastAPI non disponible.", type="error"); return(NULL) }

    for (i in seq_along(y_values)) {
      for (j in seq_along(x_values)) {
        pv <- fixed_params
        pv[[param_x]] <- x_values[j]
        pv[[param_y]] <- y_values[i]
        res <- if (.is_numeric_method(r))
          .eval_R0_numeric_ngm(r, pv, url)
        else
          api_evaluate_r0(R0_expr_str, pv, url)
        if (scal_lgl(res$ok) && !is.null(res$R0_numeric) &&
            is.finite(as.numeric(scal(res$R0_numeric))))
          R0_matrix[i, j] <- as.numeric(scal(res$R0_numeric))
      }
    }

    removeNotification(id = "heatmap_progress")
    na_count <- sum(is.na(R0_matrix))
    total    <- length(R0_matrix)
    if (na_count == total) {
      showNotification("\u274c Tous les calculs ont \u00e9chou\u00e9.",
                       type = "error", duration = 15)
      rv_heatmap_result(NULL); return()
    }
    if (na_count > 0)
      showNotification(paste0("\u26a0\ufe0f ", na_count, "/", total,
                               " cellules invalides."),
                       type = "warning", duration = 8)

    rv_heatmap_result(list(
      ok = TRUE, param_x = param_x, param_y = param_y,
      x_values = x_values, y_values = y_values,
      R0_matrix = R0_matrix, fixed_params = fixed_params))
    showNotification("\u2705 Heatmap g\u00e9n\u00e9r\u00e9e.", type = "message", duration = 3)

  }, error = function(e) {
    removeNotification(id = "heatmap_progress")
    showNotification(paste0("\u274c ", e$message), type = "error", duration = 10)
    rv_heatmap_result(NULL)
  })
})

output$heatmap2d_results <- renderUI({
  hd <- rv_heatmap_result()
  if (is.null(hd) || !scal_lgl(hd$ok))
    return(tags$div(
      style = "background:#e7f3ff;border:1px solid #2196F3;padding:20px;
               border-radius:6px;margin-top:20px;text-align:center;",
      tags$p("Configurez les param\u00e8tres et cliquez ",
             strong("\U0001f525 G\u00e9n\u00e9rer"), ".")))
  tagList(
    tags$hr(), tags$h4("R\u00e9sultats Heatmap"),
    plotlyOutput("heatmap_plot", height = "600px"), br(),
    tags$div(
      style = "background:#f0f7ff;border:1px solid #4a90d9;border-radius:6px;
               padding:16px;margin-top:20px;",
      tags$h5(style = "margin-top:0;color:#1a5fa8;",
              "\U0001f4a1 Interpr\u00e9tation"),
      tags$p(style = "font-size:13px;",
             tags$strong("\u00c9chelle : "),
             "\U0001f534 Rouge = R\u2080 \u00e9lev\u00e9 | ",
             "\U0001f7e1 Jaune = mod\u00e9r\u00e9 | ",
             "\U0001f7e2 Vert = faible")))
})

output$heatmap_plot <- renderPlotly({
  hd <- rv_heatmap_result()
  if (is.null(hd)) return(NULL)
  plot_ly(
    x = hd$x_values, y = hd$y_values, z = hd$R0_matrix,
    type      = "heatmap",
    colorscale= list(c(0, "green"), c(0.2, "yellow"),
                     c(0.4, "orange"), c(1, "red")),
    hovertemplate = paste0(
      hd$param_x, ": %{x:.3f}<br>",
      hd$param_y, ": %{y:.3f}<br>",
      "R\u2080: %{z:.3f}<extra></extra>"),
    colorbar = list(title = "R\u2080")) %>%
    layout(
      title = paste0("Heatmap R\u2080 : ", hd$param_x, " \u00d7 ", hd$param_y),
      xaxis = list(title = paste0(hd$param_x, " \u2192")),
      yaxis = list(title = paste0(hd$param_y, " \u2192")),
      font  = list(size = 14)) %>%
    config(displayModeBar = TRUE, displaylogo = FALSE)
})


# ==============================================================================
# SECTION 8 -- Sensibilité Globale : LHS + PRCC
# ==============================================================================

# ReactiveVals locaux
rv_global_sens_result <- reactiveVal(NULL)
rv_global_sens_N      <- reactiveVal(500)

# ── UI du sous-onglet ─────────────────────────────────────────────────────────
output$global_sens_content <- renderUI({
  r <- rv_ngm_result()
  if (is.null(r) || !scal_lgl(r$ok))
    return(tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;border-radius:6px;
               padding:30px;text-align:center;margin-top:20px;",
      tags$p(style = "color:#856404;",
             "Calculez d'abord R₀ symbolique dans ",
             strong("Symbolic / NGM"), ".")))

  params_in_R0 <- .params_in_R0(r)
  if (length(params_in_R0) == 0)
    return(tags$div(
      style = "background:#d4edda;border:1px solid #c3e6cb;border-radius:6px;
               padding:20px;text-align:center;",
      tags$p("R₀ est constant -- pas de sensibilité à calculer.")))

  tagList(
    tags$h4("🌍 Sensibilité Globale -- LHS + PRCC"),

    # ── Notice méthodologique ──────────────────────────────────────────────────
    tags$div(
      style = "background:#e7f3ff;border-left:4px solid #2196F3;
               padding:14px;border-radius:0;margin-bottom:20px;",
      tags$p(style = "margin:0 0 8px 0;font-size:13px;line-height:1.6;",
             tags$strong("📐 Méthode : "),
             "Latin Hypercube Sampling (LHS) + Partial Rank Correlation ",
             "Coefficient (PRCC). Contrairement à l'élasticité locale ",
             "(calculée en un seul point), le PRCC explore ",
             tags$strong("l'ensemble de la plage de variation"),
             " de chaque paramètre. Il est robuste aux non-linéarités ",
             "et aux interactions entre paramètres."),
      tags$p(style = "margin:0;font-size:12px;color:#555;",
             "📚 Référence : Marino et al. (2008), ",
             tags$em("A methodology for performing global uncertainty and ",
                     "sensitivity analysis in systems biology."),
             " J Theor Biol.")
    ),

    # ── Configuration ─────────────────────────────────────────────────────────
    tags$div(
      style = "background:#f8f9fa;border:1px solid #dee2e6;border-radius:6px;
               padding:20px;margin-bottom:20px;",
      tags$h5(style = "margin-top:0;", "Configuration"),

      # Taille echantillon + avertissement
      fluidRow(
        column(4,
               numericInput("global_sens_N",
                            "Taille d'echantillon LHS (N) :",
                            value = 500, min = 100, max = 5000, step = 100)),
        column(8,
               tags$div(style = "margin-top:25px;",
                        uiOutput("global_sens_cost_badge")))
      ),

      tags$hr(),
      tags$h5("Plages de variation des paramètres"),
      tags$p(style = "color:#666;font-size:13px;margin-bottom:12px;",
             "Définissez les plages biologiquement plausibles pour chaque ",
             "paramètre. Les bornes doivent être strictement positives. ",
             "Le LHS couvrira uniformément ces plages."),
      uiOutput("global_sens_param_table"),

      tags$div(style = "margin-top:20px;",
               actionButton("btn_run_global_sens",
                            "🚀 Lancer l'analyse LHS + PRCC",
                            class = "btn-primary btn-lg"),
               uiOutput("global_sens_run_info"))
    ),

    # ── Résultats ──────────────────────────────────────────────────────────────
    uiOutput("global_sens_results")
  )
})

# Badge de cout calcul selon N
output$global_sens_cost_badge <- renderUI({
  N <- input$global_sens_N %||% 500
  if (is.null(N) || is.na(N)) N <- 500
  if (N <= 300) {
    tags$span(style = "background:#28a745;color:white;padding:6px 14px;
              border-radius:20px;font-size:13px;font-weight:600;",
              "🟢 Rapide (~1-3s)")
  } else if (N <= 1000) {
    tags$span(style = "background:#ffc107;color:#333;padding:6px 14px;
              border-radius:20px;font-size:13px;font-weight:600;",
              "🟡 Modéré (~5-15s)")
  } else {
    tags$span(style = "background:#dc3545;color:white;padding:6px 14px;
              border-radius:20px;font-size:13px;font-weight:600;",
              "🔴 Intensif (~30s+) -- réduire N si l'app se fige")
  }
})

# Table de saisie des plages de paramètres
output$global_sens_param_table <- renderUI({
  r <- rv_ngm_result()
  if (is.null(r)) return(NULL)
  params_in_R0 <- .params_in_R0(r)
  pd <- rv_params()

  tags$div(
    style = "overflow-x:auto;",
    tags$table(
      style = "width:100%;border-collapse:collapse;font-size:13px;",
      tags$thead(tags$tr(
        style = "background:#495057;color:white;",
        tags$th(style = "padding:8px;min-width:120px;", "Paramètre"),
        tags$th(style = "padding:8px;",                 "Description"),
        tags$th(style = "padding:8px;min-width:160px;", "Borne inférieure"),
        tags$th(style = "padding:8px;min-width:160px;", "Borne supérieure"),
        tags$th(style = "padding:8px;min-width:100px;", "Unités")
      )),
      tags$tbody(lapply(params_in_R0, function(pname) {
        # Récupérer infos depuis rv_params
        idx   <- which(pd$param_name == pname)
        desc  <- if (length(idx) > 0) pd$description[idx[1]] else ""
        units <- if (length(idx) > 0) pd$units[idx[1]] else ""
        # Valeur baseline depuis l'analyse locale si disponible
        sens_local <- rv_sensitivity_result()
        base_val <- NULL
        if (!is.null(sens_local)) {
          s <- sens_local$sensitivities
          if (is.data.frame(s)) {
            j <- which(s$parameter == pname)
            if (length(j) > 0) base_val <- s$value[j[1]]
          }
        }
        # Proposer borne = [base/5, base*5] si baseline connue, sinon [0.01, 10]
        lo_default <- if (!is.null(base_val)) round(base_val / 5, 4) else 0.01
        hi_default <- if (!is.null(base_val)) round(base_val * 5, 4) else 10

        tags$tr(
          style = "border-bottom:1px solid #dee2e6;",
          tags$td(style = "padding:6px 8px;font-family:monospace;font-weight:600;",
                  tags$code(style = "color:#d63384;", pname)),
          tags$td(style = "padding:6px 8px;color:#666;font-size:12px;", desc),
          tags$td(style = "padding:4px 6px;",
                  numericInput(paste0("gs_lo_", pname), NULL,
                               value = lo_default, min = 0, step = 0.001,
                               width = "100%")),
          tags$td(style = "padding:4px 6px;",
                  numericInput(paste0("gs_hi_", pname), NULL,
                               value = hi_default, min = 0, step = 0.001,
                               width = "100%")),
          tags$td(style = "padding:6px 8px;color:#999;font-size:12px;", units)
        )
      }))
    )
  )
})

# Info sous le bouton
output$global_sens_run_info <- renderUI({
  res <- rv_global_sens_result()
  if (is.null(res)) return(NULL)
  if (scal_lgl(res$ok))
    tags$span(style = "margin-left:12px;color:#28a745;font-weight:600;",
              paste0("✅ Terminé -- ", res$N_actual, " points valides"))
  else
    tags$span(style = "margin-left:12px;color:#dc3545;font-weight:600;",
              paste0("❌ ", res$error))
})

# ── Lancement du calcul ───────────────────────────────────────────────────────
observeEvent(input$btn_run_global_sens, {
  r <- rv_ngm_result()
  if (is.null(r) || !scal_lgl(r$ok)) {
    showNotification("Calculez d'abord R₀ symbolique.", type = "error")
    return()
  }

  R0_expr_str  <- r$steps$`7_eigenvalues`$R0_expr_str
  params_in_R0 <- .params_in_R0(r)
  N            <- as.integer(input$global_sens_N %||% 500)

  # Collecter les bornes
  param_bounds <- list()
  errors_bounds <- character(0)
  for (pname in params_in_R0) {
    lo <- input[[paste0("gs_lo_", pname)]]
    hi <- input[[paste0("gs_hi_", pname)]]
    if (is.null(lo) || is.null(hi) || is.na(lo) || is.na(hi)) {
      errors_bounds <- c(errors_bounds,
                         paste0("Bornes manquantes pour '", pname, "'"))
    } else if (hi <= lo) {
      errors_bounds <- c(errors_bounds,
                         paste0("Borne max <= borne min pour '", pname, "'"))
    } else {
      param_bounds[[pname]] <- c(as.numeric(lo), as.numeric(hi))
    }
  }

  if (length(errors_bounds) > 0) {
    showNotification(paste(errors_bounds, collapse = " | "),
                     type = "error", duration = 10)
    return()
  }

  # Lancer le calcul
  url <- NGM_BASE_URL
  if (is.null(url)) { showNotification("Serveur FastAPI non disponible.", type="error"); return(NULL) }

  withProgress(
    message = paste0("LHS + PRCC en cours (N=", N, ")..."),
    detail  = "Cela peut prendre quelques secondes.",
    value   = 0.3, {
      result <- tryCatch({
        # Pour methode numerique, utiliser compute_ngm au lieu de evaluate_r0
        ctx_global <- if (.is_numeric_method(r)) {
          ctx_tmp <- build_sympy_context(rv_comp(), rv_flux(),
                                         rv_dfe(), rv_params())
          ctx_tmp$infected_states <- rv_infected_order()
          ctx_tmp
        } else NULL
        result_tmp <- api_sensitivity_lhs_prcc(
          R0_expr_str, param_bounds, N, url,
          ngm_ctx = ctx_global)
        incProgress(0.6)
        result_tmp
      }, error = function(e) {
        list(ok = FALSE, error = e$message)
      })
    })

  rv_global_sens_result(result)

  if (scal_lgl(result$ok)) {
    # Afficher les warnings Python s'il y en a
    warns <- result$warnings
    if (!is.null(warns) && length(warns) > 0)
      showNotification(paste(unlist(warns), collapse = " | "),
                       type = "warning", duration = 8)
    showNotification(
      paste0("✅ Analyse terminée : ", result$N_actual,
             " points LHS valides."),
      type = "message", duration = 4)
  } else {
    showNotification(paste0("❌ ", result$error),
                     type = "error", duration = 15)
  }
})

# ── Affichage des résultats ─────────────────────────────────────────────────
output$global_sens_results <- renderUI({
  res <- rv_global_sens_result()
  if (is.null(res)) return(NULL)
  if (!scal_lgl(res$ok)) return(NULL)

  # Convertir PRCC list en data.frame
  prcc_list <- res$prcc
  prcc_df <- do.call(rbind, lapply(prcc_list, function(x) {
    data.frame(
      rank        = x$rank,
      parameter   = x$parameter,
      prcc        = x$prcc,
      abs_prcc    = x$abs_prcc,
      p_value     = x$p_value,
      significant = x$significant,
      direction   = x$direction,
      stringsAsFactors = FALSE
    )
  }))

  tagList(
    tags$hr(),

    # ── Statistiques de R₀ ────────────────────────────────────────────────
    tags$h4("Distribution de R₀ sur l'espace des paramètres"),
    tags$div(
      style = "display:flex;gap:12px;flex-wrap:wrap;margin-bottom:20px;",
      .stat_badge("Min",    res$R0_stats$min,    "#6c757d"),
      .stat_badge("Q5%",    res$R0_stats$q05,    "#17a2b8"),
      .stat_badge("Médiane", res$R0_stats$median, "#28a745"),
      .stat_badge("Moyenne",res$R0_stats$mean,   "#ffc107"),
      .stat_badge("Q95%",   res$R0_stats$q95,    "#fd7e14"),
      .stat_badge("Max",    res$R0_stats$max,    "#dc3545"),
      .stat_badge("σ",       res$R0_stats$std,    "#6f42c1")
    ),

    # ── Tableau PRCC ──────────────────────────────────────────────────────────
    tags$h4("Coefficients PRCC"),
    tags$p(style = "color:#666;font-size:13px;margin-bottom:12px;",
           "|PRCC| ≥ 0.5 : influence forte | ",
           "0.2 ≤ |PRCC| < 0.5 : influence modérée | ",
           "|PRCC| < 0.2 : influence faible. ",
           "* = significatif (p < 0.05)."),
    tags$div(
      style = "overflow-x:auto;margin-bottom:20px;",
      tags$table(
        class = "table table-striped",
        style = "width:100%;font-size:13px;",
        tags$thead(tags$tr(
          style = "background:#343a40;color:white;",
          tags$th("Rang"), tags$th("Paramètre"),
          tags$th("PRCC"), tags$th("Barre"), tags$th("|PRCC|"),
          tags$th("p-value"), tags$th("Influence"), tags$th("Direction")
        )),
        tags$tbody(lapply(seq_len(nrow(prcc_df)), function(i) {
          row <- prcc_df[i, ]
          # Barre de progression visuelle
          bar_width <- round(abs(row$prcc) * 100)
          bar_color <- if (row$prcc > 0) "#dc3545" else "#3498db"
          bar_html  <- tags$div(
            style = paste0("background:#e9ecef;border-radius:3px;height:14px;"),
            tags$div(style = paste0(
              "width:", bar_width, "%;height:14px;",
              "background:", bar_color, ";border-radius:3px;",
              "float:", if (row$prcc > 0) "left" else "right", ";"))
          )
          # Badge influence
          influence_badge <- if (abs(row$prcc) >= 0.5)
            tags$span(style = "background:#dc3545;color:white;padding:2px 8px;
                      border-radius:10px;font-size:11px;", "Forte")
          else if (abs(row$prcc) >= 0.2)
            tags$span(style = "background:#ffc107;color:#333;padding:2px 8px;
                      border-radius:10px;font-size:11px;", "Modérée")
          else
            tags$span(style = "background:#28a745;color:white;padding:2px 8px;
                      border-radius:10px;font-size:11px;", "Faible")

          tags$tr(
            tags$td(style = "text-align:center;font-weight:600;", row$rank),
            tags$td(style = "font-family:monospace;font-weight:600;",
                    tags$code(row$parameter)),
            tags$td(style = paste0("text-align:right;font-weight:700;color:",
                                   if (row$prcc > 0) "#dc3545" else "#3498db", ";"),
                    sprintf("%+.4f", row$prcc)),
            tags$td(style = "min-width:120px;padding:4px 8px;", bar_html),
            tags$td(style = "text-align:right;", round(row$abs_prcc, 4)),
            tags$td(style = "text-align:right;color:#666;font-size:12px;",
                    if (row$significant)
                      tags$strong(sprintf("%.4f *", row$p_value))
                    else
                      sprintf("%.4f", row$p_value)),
            tags$td(style = "text-align:center;", influence_badge),
            tags$td(style = "text-align:center;",
                    if (row$direction == "positive") "⬆️" else "⬇️")
          )
        }))
      )
    ),

    # ── Graphiques scatter PRCC ───────────────────────────────────────────────
    tags$h4("Scatter plots : paramètres vs R₀"),
    tags$p(style = "color:#666;font-size:13px;margin-bottom:12px;",
           "Chaque point = un scénario LHS. La pente de la tendance ",
           "reflète le signe du PRCC. ",
           "Sélectionnez le paramètre à visualiser :"),
    selectInput("gs_scatter_param",
                label   = NULL,
                choices = prcc_df$parameter,
                width   = "200px"),
    plotlyOutput("gs_scatter_plot", height = "380px"),

    # ── Interprétation ─────────────────────────────────────────────────────────
    tags$div(
      style = "background:#fff9e6;border-left:4px solid #ffc107;
               padding:16px;border-radius:0;margin-top:20px;",
      tags$h5(style = "margin-top:0;", "💡 Interprétation"),
      tags$ul(style = "font-size:13px;margin-bottom:0;",
        local({
          top <- prcc_df[1, ]
          items <- list(tags$li(
            tags$strong(style = "color:#dc3545;", top$parameter),
            " est le paramètre le plus influent",
            paste0(" (PRCC = ", sprintf("%+.4f", top$prcc),
                   ", p = ", sprintf("%.4f", top$p_value),
                   if (isTRUE(top$significant)) " *" else "", ")."),
            " Une augmentation de ce paramètre ",
            if (top$direction == "positive") "augmente" else "réduit",
            " R₀."))
          non_sig <- prcc_df[!prcc_df$significant, "parameter"]
          if (length(non_sig) > 0)
            items <- c(items, list(tags$li(
              "Paramètres non significatifs (p ≥ 0.05) : ",
              tags$strong(paste(non_sig, collapse = ", ")),
              ". Leur influence sur R₀ n'est pas démontrée sur cette plage.")))
          items <- c(items, list(tags$li(
            "N = ", tags$strong(res$N_actual), " points LHS utilisés. ",
            "Augmentez N pour plus de précision sur les modèles complexes.")))
          tagList(items)
        })
      )
    )
  )
})

# ── Scatter plot interactif ───────────────────────────────────────────────────
output$gs_scatter_plot <- renderPlotly({
  res   <- rv_global_sens_result()
  req(res, scal_lgl(res$ok))
  param <- input$gs_scatter_param
  req(param)

  sd <- res$scatter_data[[param]]
  if (is.null(sd)) return(NULL)

  x_vals <- unlist(sd$x)
  y_vals <- unlist(sd$y)

  # PRCC du param selectionne
  prcc_val <- NA
  for (pr in res$prcc) {
    if (pr$parameter == param) { prcc_val <- pr$prcc; break }
  }

  # Couleur des points selon R0 (gradient)
  plot_ly(
    x    = x_vals,
    y    = y_vals,
    type = "scatter",
    mode = "markers",
    marker = list(
      color     = y_vals,
      colorscale= list(c(0,"#2ecc71"), c(0.5,"#f39c12"), c(1,"#e74c3c")),
      size      = 5,
      opacity   = 0.65,
      colorbar  = list(title = "R₀", len = 0.6)
    ),
    hovertemplate = paste0(
      param, " = %{x:.4f}<br>R₀ = %{y:.4f}<extra></extra>")
  ) %>%
    layout(
      title  = list(
        text = paste0(param, " vs R₀",
                      if (!is.na(prcc_val))
                        paste0("  |  PRCC = ", sprintf("%+.4f", prcc_val))
                      else ""),
        x = 0.5),
      xaxis  = list(title = param),
      yaxis  = list(title = "R₀"),
      margin = list(t = 60)
    ) %>%
    config(displayModeBar = FALSE)
})

# ── Helper badge statistique ──────────────────────────────────────────────────
.stat_badge <- function(label, value, color) {
  tags$div(
    style = paste0("background:white;border:2px solid ", color, ";",
                   "border-radius:8px;padding:8px 16px;text-align:center;",
                   "min-width:90px;"),
    tags$div(style = paste0("font-size:11px;color:", color,
                             ";font-weight:600;"), label),
    tags$div(style = "font-size:16px;font-weight:700;color:#333;",
             round(value, 3))
  )
}
