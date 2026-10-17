# =============================================================================
# M12_server.R -- Server : Diagram, Diagnostics, Exports, Help, Save/Load
# =============================================================================
# Sourcé DANS server() via source("M12_server.R", local = TRUE).
# Tous les output$, observeEvent, reactive sont ici.
# =============================================================================


# =============================================================================
# SECTION 0 -- Tableau de bord R₀ en temps réel
# =============================================================================
# Affiche un widget compact en permanence sous la barre Save/Load.
# Sources de données (par priorité décroissante) :
#   1. rv_R0_numeric()     -- évaluation numérique (valeur exacte)
#   2. rv_ngm_result()     -- expression symbolique seulement (pas de valeur)
#   3. NULL                -- pas encore calculé
# =============================================================================

output$r0_dashboard <- renderUI({

  R0_num  <- rv_R0_numeric()
  R0_sym  <- rv_ngm_result()
  dfe_ok  <- rv_dfe_validated()
  ver     <- all_verified()

  # ---- État 1 : valeur numérique disponible ----------------------------------------------------
  if (!is.null(R0_num) && !is.null(R0_num$value)) {
    val <- R0_num$value

    # Couleur selon seuil épidémique
    cfg <- if (val < 1)
      list(bg = "#d4edda", border = "#28a745", text = "#155724",
           icon = "⬇️", label = "R₀ < 1 -- épidémie s'éteint")
    else if (val < 2)
      list(bg = "#fff3cd", border = "#ffc107", text = "#856404",
           icon = "⚠️", label = "R₀ ∈ [1,2[ -- croissance lente")
    else
      list(bg = "#f8d7da", border = "#dc3545", text = "#721c24",
           icon = "⬆️", label = paste0("R₀ ≥ 2 -- croissance rapide"))

    return(tags$div(
      style = paste0(
        "background:", cfg$bg, ";border:1px solid ", cfg$border, ";",
        "border-radius:6px;padding:10px 18px;margin-bottom:14px;",
        "display:flex;align-items:center;gap:16px;flex-wrap:wrap;"
      ),
      # Icône + valeur
      tags$div(
        style = "display:flex;align-items:center;gap:10px;",
        tags$span(style = "font-size:22px;", cfg$icon),
        tags$span(
          style = paste0("font-size:22px;font-weight:700;",
                         "font-family:monospace;color:", cfg$text, ";"),
          paste0("R₀ = ", round(val, 4))
        )
      ),
      # Label interprétatif
      tags$span(
        style = paste0("font-size:13px;color:", cfg$text, ";"),
        cfg$label
      ),
      # Contexte : expression symbolique si dispo
      if (!is.null(R0_sym) && isTRUE(R0_sym$ok)) {
        expr_str <- R0_sym$steps$`7_eigenvalues`$R0_expr_str
        tags$span(
          style = "margin-left:auto;font-size:11px;color:#666;
                   font-family:monospace;background:rgba(0,0,0,0.05);
                   padding:3px 8px;border-radius:3px;",
          paste0("R₀ = ", expr_str)
        )
      },
      # Bouton mise à jour rapide (renvoie vers évaluation numérique)
      tags$div(
        style = "margin-left:auto;",
        actionButton("dashboard_goto_numeric",
                     "↻ Mettre à jour",
                     class = "btn-sm btn-outline-secondary",
                     style = "font-size:11px;padding:3px 10px;")
      )
    ))
  }

  # ---- État 2 : R₀ symbolique calculé mais pas encore évalué ----------------------
  if (!is.null(R0_sym) && isTRUE(R0_sym$ok)) {
    expr_str <- R0_sym$steps$`7_eigenvalues`$R0_expr_str
    return(tags$div(
      style = "background:#e7f3ff;border:1px solid #4a90d9;
               border-radius:6px;padding:10px 18px;margin-bottom:14px;
               display:flex;align-items:center;gap:14px;flex-wrap:wrap;",
      tags$span(style = "font-size:18px;", "🧮"),
      tags$span(
        style = "font-weight:600;color:#1a5fa8;font-size:14px;",
        "R₀ symbolique calculé"
      ),
      tags$span(
        style = "font-family:monospace;font-size:13px;color:#333;
                 background:rgba(0,0,0,0.05);padding:3px 8px;border-radius:3px;",
        paste0("R₀ = ", expr_str)
      ),
      tags$div(
        style = "margin-left:auto;",
        actionButton("dashboard_goto_numeric",
                     "🔢 Évaluer numériquement",
                     class = "btn-sm btn-primary",
                     style = "font-size:11px;padding:3px 10px;")
      )
    ))
  }

  # ---- État 3 : DFE validé mais NGM pas encore calculé --------------------------
  if (dfe_ok && ver$all_ok) {
    return(tags$div(
      style = "background:#f8f9fa;border:1px solid #dee2e6;
               border-radius:6px;padding:10px 18px;margin-bottom:14px;
               display:flex;align-items:center;gap:14px;",
      tags$span(style = "font-size:16px;", "⏳"),
      tags$span(style = "color:#6c757d;font-size:13px;",
                "R₀ non encore calculé -- allez dans l'onglet ",
                tags$strong("Symbolic / NGM"), "."),
      tags$div(
        style = "margin-left:auto;",
        actionButton("dashboard_goto_symbolic",
                     "⚙️ Calculer R₀",
                     class = "btn-sm btn-outline-primary",
                     style = "font-size:11px;padding:3px 10px;")
      )
    ))
  }

  # ---- État 4 : modèle incomplet --------------------------------------------------------
  # Ne rien afficher -- ne pas surcharger l'interface au démarrage
  NULL
})

# Navigation depuis le dashboard
observeEvent(input$dashboard_goto_numeric, {
  updateTabsetPanel(session, "main_tabs",
                    selected = "📊 Évaluation Numérique")
})
observeEvent(input$dashboard_goto_symbolic, {
  updateTabsetPanel(session, "main_tabs", selected = "Symbolic / NGM")
})

# -- Save / Load
# =============================================================================

output$btn_save_model <- downloadHandler(
  filename = function() {
    raw  <- trimws(input$model_name %||% "modele_NGM")
    safe <- gsub("[^A-Za-z0-9_\\-]", "_", raw)
    safe <- gsub("_+", "_", safe)
    if (nchar(safe) == 0) safe <- "modele_NGM"
    paste0(safe, ".json")
  },
  content = function(file) {
    model_list <- build_model_list(
      model_name = trimws(input$model_name %||% ""),
      comp_df    = rv_comp(),
      flux_df    = rv_flux(),
      sp_list    = species_list(),
      dfe_df     = rv_dfe(),
      params_df  = rv_params()
    )
    json_str <- jsonlite::toJSON(
      model_list,
      pretty = TRUE, auto_unbox = TRUE,
      null = "null", na = "null", force = TRUE
    )
    writeLines(json_str, file)
  }
)

output$load_status_badge <- renderUI({
  s <- rv_load_status()
  if (is.null(s)) return(NULL)
  style <- switch(s$type,
    success = "background:#d4edda; border:1px solid #c3e6cb; color:#155724;",
    warning = "background:#fff3cd; border:1px solid #ffc107; color:#856404;",
    error   = "background:#f8d7da; border:1px solid #f5c6cb; color:#721c24;"
  )
  tags$div(
    style = paste0(style, "padding:4px 10px; border-radius:4px; font-size:12px;"),
    s$message
  )
})

observeEvent(input$btn_load_model, {
  req(input$btn_load_model)
  lst <- tryCatch(
    jsonlite::fromJSON(input$btn_load_model$datapath, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(lst)) {
    rv_load_status(list(ok = FALSE, type = "error",
                        message = "\u274c JSON invalide ou illisible."))
    return()
  }
  result <- load_model_list(lst)
  if (!result$ok) {
    rv_load_status(list(ok = FALSE, type = "error",
                        message = paste0("\u274c ",
                                         paste(result$errors, collapse = " | "))))
    return()
  }
  # Activer le flag de chargement AVANT de modifier rv_comp/rv_flux
  # Cela bloque l'observer M09 qui recalculerait le DFE automatiquement
  rv_loading(TRUE)

  rv_comp(result$comp_df)
  rv_flux(result$flux_df)
  if (!is.null(result$dfe_df))    rv_dfe(result$dfe_df)
  if (!is.null(result$params_df)) rv_params(result$params_df)
  rv_dfe_validated(isTRUE(result$dfe_validated))
  rv_dfe_sym_result(NULL)     # reset badge DFE
  species_list(result$species_list)
  updateTextInput(session, "model_name", value = result$model_name)
  rv_symbolic(build_symbolic_table(result$comp_df, result$flux_df))
  rv_flux_hash(flux_hash(result$flux_df))

  # -- Log : verifier le DFE charge ------------------------------------------
  dfe_loaded <- rv_dfe()
  message("[M12 load] DFE apres chargement:")
  for (i in seq_len(nrow(dfe_loaded))) {
    message("  ", dfe_loaded$state_id[i], " = ", dfe_loaded$dfe_expr[i])
  }

  # -- Reset complet de tous les calculs du modele precedent ----------------
  rv_ngm_result(NULL)         # efface R0 symbolique
  rv_ngm_computed_hash(NULL)  # force recalcul NGM
  rv_infected_order(character(0))
  rv_infected_locked(FALSE)
  rv_numeric_params(list())   # efface valeurs numeriques
  rv_R0_numeric(NULL)         # efface R0 numerique
  rv_scenarios(list())        # efface scenarios
  rv_scenarios_result(NULL)
  tryCatch(rv_sensitivity_result(NULL), error = function(e) NULL)
  tryCatch(rv_heatmap_result(NULL),      error = function(e) NULL)
  tryCatch(rv_global_sens_result(NULL),  error = function(e) NULL)
  tryCatch(rv_diag(NULL),               error = function(e) NULL)

  # Desactiver le flag de chargement -- les observers peuvent reprendre
  rv_loading(FALSE)

  # Re-forcer le DFE et les params APRES rv_loading(FALSE)
  # L'observer M09 peut se declencher juste apres rv_loading(FALSE)
  # et ecraser le DFE. On re-force une derniere fois apres un delai.
  if (!is.null(result$dfe_df))    rv_dfe(result$dfe_df)
  if (!is.null(result$params_df)) rv_params(result$params_df)

  # Second re-forcage : stocker le DFE correct pour le reappliquer
  # apres que tous les observers reactifs se soient executes
  rv_pending_dfe(result$dfe_df)
  rv_pending_params(result$params_df)
  rv_load_status(list(
    ok = TRUE, type = "success",
    message = paste0("\u2705 '", result$model_name, "' \u2014 ",
                     nrow(result$comp_df), " comp., ",
                     nrow(result$flux_df), " flux")
  ))
  showNotification(
    paste0("Mod\u00e8le '", result$model_name, "' charg\u00e9."),
    type = "message", duration = 4
  )
})


# =============================================================================
# SECTION 2 -- Diagramme visNetwork
# =============================================================================

diagram_data <- reactive({
  current_deps <- rv_dependencies()
  result <- build_diagram_data(
    rv_comp(), rv_flux(), rv_symbolic(),
    dependencies = current_deps
  )
  if (length(current_deps) == 0 && length(result$dependencies) > 0)
    rv_dependencies(result$dependencies)
  list(nodes = result$nodes, edges = result$edges)
})

output$diagram_content <- renderUI({
  cd <- rv_comp()
  if (nrow(cd) == 0)
    return(tags$div(
      style = "background:#f8f9fa; border:1px solid #dee2e6;
               border-radius:6px; padding:40px; text-align:center;
               margin-top:20px;",
      tags$h4(style = "color:#6c757d;",
              "\U0001f5fa Aucun compartiment d\u00e9fini"),
      tags$p("Ajoutez des compartiments dans l'onglet ",
             strong("Compartiments"), ".")
    ))
  tagList(
    tags$div(
      style = "background:#f8f9fa; border:1px solid #dee2e6;
               border-radius:6px; padding:10px 14px; margin-bottom:12px;
               display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
      actionButton("diag_reset_layout", "\u21ba Reset layout",
                   class = "btn-outline-secondary btn-sm"),
      actionButton("diag_fit", "\u229e Fit view",
                   class = "btn-outline-secondary btn-sm"),
      tags$div(style = "margin-left:auto;",
               uiOutput("diag_selection_info"))
    ),
    tags$div(
      style = "border:1px solid #dee2e6; border-radius:6px; overflow:hidden;",
      visNetworkOutput("vis_diagram", height = "580px")
    ),
    uiOutput("cross_species_dependencies_section"),
    uiOutput("diag_edit_panel")
  )
})

output$vis_diagram <- renderVisNetwork({
  dd <- diagram_data()
  if (nrow(dd$nodes) == 0)
    return(visNetwork(
      data.frame(id = 1, label = "Aucun\ncompartiment",
                 color.background = "#f8f9fa",
                 color.border     = "#dee2e6"),
      data.frame()
    ))
  multi_sp <- length(unique(dd$nodes$group)) > 1
  visNetwork(dd$nodes, dd$edges) %>%
    visOptions(highlightNearest = list(enabled = TRUE, degree = 1,
                                       hover = TRUE)) %>%
    visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(
        gravitationalConstant = -80, centralGravity  = 0.01,
        springLength          = 220, springConstant  = 0.04,
        damping               = 0.9
      ),
      stabilization = list(enabled = TRUE, iterations = 300, fit = TRUE)
    ) %>%
    visEdges(
      arrows = list(to = list(enabled = TRUE, scaleFactor = 1.2)),
      font   = list(size = 12, align = "middle"),
      smooth = list(enabled = TRUE, type = "curvedCW", roundness = 0.2)
    ) %>%
    visNodes(font = list(size = 16), size = 25, margin = 12) %>%
    visInteraction(hover = TRUE, tooltipDelay = 150,
                   dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>%
    visGroups(groupname = "human",  color = list(border = "#4a90d9")) %>%
    visGroups(groupname = "vector", color = list(border = "#e6a817")) %>%
    visGroups(groupname = "animal", color = list(border = "#28a745")) %>%
    visLegend(enabled = multi_sp, useGroups = TRUE,
              position = "right", width = 0.15) %>%
    visEvents(
      selectNode   = "function(p){
        Shiny.setInputValue('diag_selected_node', p.nodes[0],
                            {priority:'event'}); }",
      selectEdge   = "function(p){ if(p.edges.length>0)
        Shiny.setInputValue('diag_selected_edge', p.edges[0],
                            {priority:'event'}); }",
      deselectNode = "function(p){
        Shiny.setInputValue('diag_selected_node', null); }",
      deselectEdge = "function(p){
        Shiny.setInputValue('diag_selected_edge', null); }"
    )
})

observeEvent(list(rv_comp(), rv_flux(), rv_symbolic()), {
  dd <- diagram_data()
  if (nrow(dd$nodes) == 0) return()
  visNetworkProxy("vis_diagram") %>%
    visUpdateNodes(dd$nodes) %>%
    visUpdateEdges(dd$edges)
}, ignoreInit = TRUE)

observeEvent(input$diag_reset_layout, {
  visNetworkProxy("vis_diagram") %>%
    visPhysics(enabled = TRUE) %>%
    visStabilize(iterations = 200)
})
observeEvent(input$diag_fit, {
  visNetworkProxy("vis_diagram") %>% visFit()
})

# ── Info sélection ────────────────────────────────────────────────────────────
output$diag_selection_info <- renderUI({
  node_id <- input$diag_selected_node
  edge_id <- input$diag_selected_edge
  if (!is.null(node_id) && !is.na(node_id)) {
    row <- rv_comp()[rv_comp()$state_id == node_id, ]
    if (nrow(row) == 0) return(NULL)
    tags$span(
      style = "font-size:12px; color:#555;",
      "S\u00e9lection : ", tags$strong(node_id),
      tags$span(
        style = paste0("background:", dfe_type_badge_color(row$state_type[1]),
                       "; border-radius:3px; padding:1px 6px;",
                       " margin-left:4px; font-size:11px;"),
        row$state_type[1]
      )
    )
  } else if (!is.null(edge_id) && !is.na(edge_id)) {
    row <- rv_flux()[rv_flux()$flux_id == edge_id, ]
    if (nrow(row) == 0) return(NULL)
    tags$span(
      style = "font-size:12px; color:#555;",
      "S\u00e9lection : ", tags$strong(edge_id),
      tags$span(style = "color:#666; margin-left:4px;",
                paste0(row$from_state_id[1], " \u2192 ", row$to_state_id[1]))
    )
  } else NULL
})

# ── Panneau édition depuis diagramme ─────────────────────────────────────────
output$diag_edit_panel <- renderUI({
  node_id <- input$diag_selected_node
  edge_id <- input$diag_selected_edge
  if (!is.null(node_id) && !is.na(node_id)) {
    row <- rv_comp()[rv_comp()$state_id == node_id, ]
    if (nrow(row) == 0) return(NULL)
    r <- row[1, ]
    tags$div(
      style = "background:#f0f7ff; border:1px solid #4a90d9;
               border-radius:6px; padding:14px; margin-top:12px;",
      tags$h5(paste0("\U0001f535 ", node_id), style = "margin:0 0 8px 0;"),
      tags$p(tags$strong("Type : "), r$state_type,
             tags$strong("  Esp\u00e8ce : "), r$species),
      tags$div(
        style = "margin-top:10px;",
        actionButton("diag_edit_comp_btn",
                     "\u270f\ufe0f Modifier (Compartiments)",
                     class = "btn-primary btn-sm"),
        actionButton("diag_del_comp_btn", "\U0001f5d1 Supprimer",
                     class = "btn-danger btn-sm",
                     style = "margin-left:8px;")
      )
    )
  } else if (!is.null(edge_id) && !is.na(edge_id)) {
    row <- rv_flux()[rv_flux()$flux_id == edge_id, ]
    if (nrow(row) == 0) return(NULL)
    r <- row[1, ]
    tags$div(
      style = "background:#f0fff4; border:1px solid #28a745;
               border-radius:6px; padding:14px; margin-top:12px;",
      tags$h5(paste0("\u27a1\ufe0f Flux : ", edge_id),
              style = "margin:0 0 8px 0;"),
      tags$p(tags$strong("Trajet : "),
             paste0(r$from_state_id, " \u2192 ", r$to_state_id)),
      if (!is.na(r$rate_expression) && nchar(trimws(r$rate_expression)) > 0)
        tags$p(tags$strong("Expression : "), tags$code(r$rate_expression))
      else
        tags$p(style = "color:#dc3545; font-weight:600;",
               "\u26a0\ufe0f Expression manquante"),
      tags$div(
        style = "margin-top:10px;",
        actionButton("diag_edit_flux_btn",
                     "\u270f\ufe0f Modifier (Flux)",
                     class = "btn-primary btn-sm"),
        actionButton("diag_del_flux_btn", "\U0001f5d1 Supprimer",
                     class = "btn-danger btn-sm",
                     style = "margin-left:8px;")
      )
    )
  } else NULL
})

# ── Navigation depuis diagramme ───────────────────────────────────────────────
observeEvent(input$diag_edit_comp_btn, {
  updateTabsetPanel(session, "main_tabs", selected = "Compartiments")
  node_id <- isolate(input$diag_selected_node)
  if (!is.null(node_id)) {
    idx <- which(rv_comp()$state_id == node_id)
    if (length(idx) > 0) dataTableProxy("comp_table") %>% selectRows(idx[1])
  }
})
observeEvent(input$diag_edit_flux_btn, {
  updateTabsetPanel(session, "main_tabs", selected = "Flux")
  edge_id <- isolate(input$diag_selected_edge)
  if (!is.null(edge_id)) {
    idx <- which(rv_flux()$flux_id == edge_id)
    if (length(idx) > 0) dataTableProxy("flux_table") %>% selectRows(idx[1])
  }
})

# ── Suppression depuis diagramme ──────────────────────────────────────────────
observeEvent(input$diag_del_comp_btn, {
  node_id <- isolate(input$diag_selected_node)
  if (is.null(node_id)) return()
  cd  <- rv_comp()
  idx <- which(cd$state_id == node_id)
  if (length(idx) == 0 || nrow(cd) <= 1) {
    showNotification("Impossible de supprimer.", type = "warning"); return()
  }
  showModal(modalDialog(
    title = paste0("Supprimer '", node_id, "' ?"),
    tags$p("Compartiment : ", tags$strong(node_id)),
    footer = tagList(modalButton("Annuler"),
                     actionButton("diag_del_comp_confirm", "Supprimer",
                                  class = "btn-danger")),
    easyClose = TRUE
  ))
})
observeEvent(input$diag_del_comp_confirm, {
  node_id <- isolate(input$diag_selected_node); removeModal()
  if (is.null(node_id)) return()
  cd  <- rv_comp()
  idx <- which(cd$state_id == node_id)
  if (length(idx) > 0 && nrow(cd) > 1) {
    rv_comp(cd[-idx, , drop = FALSE])
    # Supprimer les flux associés
    fd <- rv_flux()
    if (nrow(fd) > 0) {
      keep <- !(
        (!is.na(fd$from_state_id) & fd$from_state_id == node_id) |
        (!is.na(fd$to_state_id)   & fd$to_state_id   == node_id)
      )
      n_removed <- sum(!keep)
      rv_flux(fd[keep, , drop = FALSE])
      showNotification(
        paste0("'", node_id, "' supprim\u00e9",
               if (n_removed > 0)
                 paste0(" -- ", n_removed, " flux associ\u00e9(s) supprim\u00e9(s).")
               else "."),
        type = "message", duration = 3)
    } else {
      showNotification(paste0("'", node_id, "' supprim\u00e9."),
                       type = "message", duration = 3)
    }
  }
})
observeEvent(input$diag_del_flux_btn, {
  edge_id <- isolate(input$diag_selected_edge)
  if (is.null(edge_id)) return()
  fd <- rv_flux(); idx <- which(fd$flux_id == edge_id)
  if (length(idx) == 0) return()
  showModal(modalDialog(
    title = paste0("Supprimer flux '", edge_id, "' ?"),
    tags$p("Flux : ", tags$strong(
      paste0(fd$from_state_id[idx], " \u2192 ", fd$to_state_id[idx]))),
    footer = tagList(modalButton("Annuler"),
                     actionButton("diag_del_flux_confirm", "Supprimer",
                                  class = "btn-danger")),
    easyClose = TRUE
  ))
})
observeEvent(input$diag_del_flux_confirm, {
  edge_id <- isolate(input$diag_selected_edge); removeModal()
  if (is.null(edge_id)) return()
  fd <- rv_flux(); idx <- which(fd$flux_id == edge_id)
  if (length(idx) > 0) {
    rv_flux(fd[-idx, , drop = FALSE])
    showNotification(paste0("Flux '", edge_id, "' supprim\u00e9."),
                     type = "message", duration = 3)
  }
})

# ── Dépendances inter-espèces ─────────────────────────────────────────────────
output$cross_species_dependencies_section <- renderUI({
  deps <- rv_dependencies()
  n    <- length(deps)
  tags$div(
    style = "margin-top:20px;",
    tags$div(
      style = "background:#f8f9fa; border:1px solid #dee2e6;
               border-radius:6px; padding:12px 16px;
               display:flex; align-items:center; gap:10px;",
      tags$span(style = "font-size:15px; font-weight:600;",
                "\U0001f517 D\u00e9pendances inter-esp\u00e8ces"),
      if (n > 0)
        tags$span(
          style = "background:#e67e22; color:white; border-radius:12px;
                   padding:2px 10px; font-size:12px;",
          paste0(n, " d\u00e9tect\u00e9e(s)")
        ),
      tags$div(
        style = "margin-left:auto; display:flex; gap:8px;",
        actionButton("btn_recalc_dependencies", "\U0001f504 Recalculer",
                     class = "btn-primary btn-sm"),
        actionButton("btn_add_manual_dep", "+ Manuel",
                     class = "btn-success btn-sm")
      )
    )
  )
})

observeEvent(input$btn_recalc_dependencies, {
  current_deps <- rv_dependencies()
  manual_deps  <- Filter(function(d) d$type == "manual", current_deps)
  result       <- build_diagram_data(rv_comp(), rv_flux(), rv_symbolic())
  new_auto     <- result$dependencies
  rv_dependencies(c(new_auto, manual_deps))
  rv_dependencies_synced(TRUE)
  rv_last_flux_hash_deps(digest::digest(rv_flux()))
  showNotification(
    paste0("\u2705 ", length(new_auto),
           " auto + ", length(manual_deps), " manuelles."),
    type = "message", duration = 5
  )
})

observe({
  current_hash <- digest::digest(rv_flux())
  last_hash    <- rv_last_flux_hash_deps()
  if (!is.null(last_hash) && current_hash != last_hash &&
      length(rv_dependencies()) > 0)
    rv_dependencies_synced(FALSE)
})

observeEvent(input$btn_add_manual_dep, {
  comp_df <- rv_comp()
  inf_c   <- comp_df[comp_df$state_type %in% INFECTED_STATE_TYPES, ]
  sus_c   <- comp_df[comp_df$state_type == "susceptible", ]
  if (nrow(inf_c) == 0 || nrow(sus_c) == 0) {
    showNotification("Besoin d'au moins un infectieux et un susceptible.",
                     type = "warning"); return()
  }
  showModal(modalDialog(
    title = paste0("\u2795 D\u00e9pendance manuelle (",
                   generate_dependency_id(rv_dependencies()), ")"),
    selectInput("modal_dep_from", "De (infectieux) :",
                choices = setNames(inf_c$state_id,
                                   paste0(inf_c$state_id,
                                          " [", inf_c$species, "]"))),
    selectInput("modal_dep_to", "Vers (susceptible) :",
                choices = setNames(sus_c$state_id,
                                   paste0(sus_c$state_id,
                                          " [", sus_c$species, "]"))),
    textInput("modal_dep_mechanism", "M\u00e9canisme (optionnel) :"),
    footer = tagList(
      modalButton("Annuler"),
      actionButton("confirm_add_manual_dep", "Ajouter",
                   class = "btn-success")
    )
  ))
})
observeEvent(input$confirm_add_manual_dep, {
  removeModal()
  from_id <- input$modal_dep_from
  to_id   <- input$modal_dep_to
  if (is.null(from_id) || is.null(to_id)) return()
  comp_df <- rv_comp()
  from_sp <- comp_df$species[comp_df$state_id == from_id]
  to_sp   <- comp_df$species[comp_df$state_id == to_id]
  from_sp <- if (length(from_sp) > 0) from_sp[1] else "unknown"
  to_sp   <- if (length(to_sp)   > 0) to_sp[1]   else "unknown"
  deps    <- rv_dependencies()
  rv_dependencies(c(deps, list(list(
    dependency_id    = generate_dependency_id(deps),
    from             = from_id,   to  = to_id,
    from_species     = from_sp,   to_species = to_sp,
    via_flux         = "",        mechanism  = input$modal_dep_mechanism %||% "",
    type             = "manual",  visible    = TRUE,
    is_cross_species = from_sp != to_sp
  ))))
  rv_dependencies_synced(FALSE)
  showNotification(
    paste0("\u2705 ", from_id, " \u2192 ", to_id, " ajout\u00e9e."),
    type = "message", duration = 4
  )
})


# =============================================================================
# SECTION 3 -- Diagnostics
# =============================================================================

rv_diag <- reactive({
  run_diagnostics(
    rv_comp(), rv_flux(),
    rv_symbolic() %||% build_symbolic_table(rv_comp(), rv_flux()),
    rv_params(), rv_dfe()
  )
})

output$diag_tab_title <- renderUI({
  d <- rv_diag()
  if (d$n_errors > 0)
    tagList(span("\U0001f534 Diagnostics"),
            tags$sup(tags$span(
              style = "background:#dc3545; color:white; border-radius:10px;
                       padding:1px 6px; font-size:10px; margin-left:4px;",
              d$n_errors)))
  else if (d$n_warnings > 0)
    tagList(span("\U0001f7e0 Diagnostics"),
            tags$sup(tags$span(
              style = "background:#ffc107; color:#333; border-radius:10px;
                       padding:1px 6px; font-size:10px; margin-left:4px;",
              d$n_warnings)))
  else span("\u2705 Diagnostics")
})

output$diag_content <- renderUI({
  d  <- rv_diag()
  cd <- rv_comp()

  diag_card <- function(issue) {
    cfg <- switch(issue$level,
      error   = list(bg = "#f8d7da", border = "#f5c6cb",
                     icon = "\U0001f534", label = "ERREUR"),
      warning = list(bg = "#fff3cd", border = "#ffc107",
                     icon = "\U0001f7e0", label = "AVERTISSEMENT"),
      info    = list(bg = "#d1ecf1", border = "#bee5eb",
                     icon = "\U0001f7e1", label = "INFO"),
      list(bg = "#f8f9fa", border = "#dee2e6",
           icon = "\u2139\ufe0f", label = "INFO")
    )
    tags$div(
      style = paste0("background:", cfg$bg, "; border:1px solid ", cfg$border,
                     "; border-radius:6px; padding:12px 16px;",
                     " margin-bottom:10px;"),
      tags$div(
        style = "display:flex; align-items:center; gap:8px; margin-bottom:6px;",
        tags$span(cfg$icon), tags$strong(issue$title)
      ),
      tags$p(style = "margin:0 0 6px 0; font-size:13px; color:#555;",
             issue$detail),
      if (length(issue$items) > 0)
        tags$div(
          style = "background:rgba(0,0,0,0.05); border-radius:4px;
                   padding:6px 10px; font-family:monospace; font-size:13px;",
          paste(issue$items, collapse = "  \u2022  ")
        )
    )
  }

  sm <- d$summary
  tc <- table(cd$state_type)

  tagList(
    # Résumé
    tags$div(
      style = "background:#f0f7ff; border:1px solid #4a90d9;
               border-radius:6px; padding:12px 16px; margin-bottom:16px;",
      tags$h5(style = "margin:0 0 10px 0; color:#1a4a7a;",
              "\U0001f4ca Structure du mod\u00e8le"),
      fluidRow(
        column(3, tags$div(style = "text-align:center;",
          tags$div(style = "font-size:28px; font-weight:bold; color:#1a4a7a;",
                   sm$n_comp),
          tags$div(style = "font-size:12px;", "compartiments"))),
        column(3, tags$div(style = "text-align:center;",
          tags$div(style = "font-size:28px; font-weight:bold; color:#1a4a7a;",
                   sm$n_flux),
          tags$div(style = "font-size:12px;", "flux"))),
        column(3, tags$div(style = "text-align:center;",
          tags$div(style = "font-size:28px; font-weight:bold; color:#1a4a7a;",
                   sm$n_infected),
          tags$div(style = "font-size:12px;", "infect\u00e9s"))),
        column(3, tags$div(style = "text-align:center;",
          tags$div(style = "font-size:28px; font-weight:bold; color:#1a4a7a;",
                   sm$n_species),
          tags$div(style = "font-size:12px;", "esp\u00e8ces")))
      )
    ),

    # Bandeau global
    if (d$n_errors == 0 && d$n_warnings == 0)
      tags$div(
        style = "background:#d4edda; border:1px solid #c3e6cb;
                 border-radius:6px; padding:12px 16px; margin-bottom:16px;",
        tags$strong("\u2705 Mod\u00e8le valide \u2014 "),
        "Aucune erreur ni avertissement."
      )
    else
      tags$div(
        style = "background:#f8f9fa; border:1px solid #dee2e6;
                 border-radius:6px; padding:12px 16px; margin-bottom:16px;",
        tags$div(style = "display:flex; gap:16px;",
          if (d$n_errors > 0)
            tags$span(style = "color:#dc3545; font-weight:bold;",
                      paste0("\U0001f534 ", d$n_errors, " erreur(s)")),
          if (d$n_warnings > 0)
            tags$span(style = "color:#856404; font-weight:bold;",
                      paste0("\U0001f7e0 ", d$n_warnings, " avertissement(s)")),
          if (d$n_infos > 0)
            tags$span(style = "color:#0c5460;",
                      paste0("\U0001f7e1 ", d$n_infos, " info(s)"))
        )
      ),

    # Issues
    {
      errors   <- Filter(function(x) x$level == "error",   d$issues)
      warnings <- Filter(function(x) x$level == "warning", d$issues)
      infos    <- Filter(function(x) x$level == "info",    d$issues)
      tagList(
        if (length(errors) > 0) tagList(
          tags$h5(style = "color:#dc3545; margin:16px 0 8px;",
                  paste0("\U0001f534 Erreurs (", length(errors), ")")),
          lapply(errors, diag_card)),
        if (length(warnings) > 0) tagList(
          tags$h5(style = "color:#856404; margin:16px 0 8px;",
                  paste0("\U0001f7e0 Avertissements (", length(warnings), ")")),
          lapply(warnings, diag_card)),
        if (length(infos) > 0) tagList(
          tags$h5(style = "color:#0c5460; margin:16px 0 8px;",
                  paste0("\U0001f7e1 Informations (", length(infos), ")")),
          lapply(infos, diag_card))
      )
    }
  )
})


# =============================================================================
# SECTION 4 -- Exports
# =============================================================================

output$exports_content <- renderUI({
  has_ngm <- !is.null(rv_ngm_result()) && isTRUE(rv_ngm_result()$ok)

  tagList(
    tags$h3("\U0001f4e4 Exports"),

    # CSV
    tags$div(
      style = "background:#f8f9fa; border:1px solid #dee2e6;
               border-radius:8px; padding:20px; margin-bottom:20px;",
      tags$h4(style = "margin-top:0;", "\u2460 Tables CSV"),
      tags$div(style = "display:flex; gap:12px; flex-wrap:wrap;",
        downloadButton("export_compartments_csv", "\U0001f4ca Compartiments",
                       class = "btn-sm btn-outline-primary"),
        downloadButton("export_fluxes_csv",       "\U0001f4ca Flux",
                       class = "btn-sm btn-outline-primary"),
        downloadButton("export_parameters_csv",   "\U0001f4ca Param\u00e8tres",
                       class = "btn-sm btn-outline-primary"),
        downloadButton("export_dfe_csv",          "\U0001f4ca DFE",
                       class = "btn-sm btn-outline-primary")
      )
    ),

    # Script Python
    tags$div(
      style = "background:#fff9e6; border:1px solid #ffc107;
               border-radius:8px; padding:20px; margin-bottom:20px;",
      tags$h4(style = "margin-top:0;", "\u2461 Script Python"),
      if (has_ngm)
        downloadButton("export_python_script",
                       "\U0001f40d Script Python standalone",
                       class = "btn-sm btn-outline-warning")
      else
        tags$p(style = "color:#856404;",
               "\u26a0\ufe0f Calculez R\u2080 d'abord.")
    ),

    # PDF
    tags$div(
      style = "background:#e7f3ff; border:1px solid #2196F3;
               border-radius:8px; padding:20px;",
      tags$h4(style = "margin-top:0;", "\u2462 Rapport PDF"),
      if (has_ngm)
        downloadButton("btn_export_pdf_main",
                       "\U0001f4e5 T\u00e9l\u00e9charger rapport PDF",
                       class = "btn-primary")
      else
        tags$p(style = "color:#0d47a1;",
               "\u2139\ufe0f Calculez R\u2080 pour d\u00e9bloquer le PDF.")
    )
  )
})

# CSV handlers
# ── Export Compartiments ──────────────────────────────────────────────────────
output$export_compartments_csv <- downloadHandler(
  filename = function()
    paste0("compartiments_", format(Sys.Date(), "%Y%m%d"), ".csv"),
  content = function(file) {
    df <- rv_comp()
    if (is.null(df) || nrow(df) == 0) {
      showNotification("Aucun compartiment \u00e0 exporter.", type = "warning")
      return()
    }
    cols <- intersect(c("state_id", "label", "state_type", "species",
                        "strain", "description", "verification"), names(df))
    write.csv(df[, cols, drop = FALSE], file, row.names = FALSE)
  }
)

# ── Export Flux ───────────────────────────────────────────────────────────────
output$export_fluxes_csv <- downloadHandler(
  filename = function()
    paste0("flux_", format(Sys.Date(), "%Y%m%d"), ".csv"),
  content = function(file) {
    df <- rv_flux()
    if (is.null(df) || nrow(df) == 0) {
      showNotification("Aucun flux \u00e0 exporter.", type = "warning")
      return()
    }
    cols <- intersect(c("flux_id", "from_state_id", "to_state_id",
                        "process_type", "transmission_type", "density_type",
                        "rate_name", "rate_expression",
                        "rate_units", "rate_comment", "verification"),
                      names(df))
    write.csv(df[, cols, drop = FALSE], file, row.names = FALSE)
  }
)

# \u2500\u2500 Export Parametres \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
# Collecte les parametres directement depuis les expressions de flux + DFE.
# N'utilise PAS rv_params() qui peut etre vide si l'onglet DFE n'a pas ete visite.
output$export_parameters_csv <- downloadHandler(
  filename = function()
    paste0("parametres_", format(Sys.Date(), "%Y%m%d"), ".csv"),
  content = function(file) {
    # 1. Collecter TOUS les parametres libres depuis flux ET dfe
    all_param_names <- collect_model_params(rv_flux(), rv_comp(), rv_dfe())
    if (length(all_param_names) == 0) {
      showNotification("Aucun param\u00e8tre d\u00e9tect\u00e9 dans les expressions.",
                       type = "warning")
      return()
    }
    # 2. Recuperer descriptions/unites depuis rv_params() si disponibles
    old_p <- rv_params()
    description <- sapply(all_param_names, function(p) {
      j <- which(old_p$param_name == p)
      if (length(j) > 0 && !is.na(old_p$description[j[1]])) old_p$description[j[1]] else ""
    })
    units <- sapply(all_param_names, function(p) {
      j <- which(old_p$param_name == p)
      if (length(j) > 0 && !is.na(old_p$units[j[1]])) old_p$units[j[1]] else ""
    })
    # 3. Classifier (flux / dfe / both)
    classification <- classify_params(all_param_names, rv_flux(), rv_comp(), rv_dfe())
    # 4. Construire le data.frame final
    df_export <- data.frame(
      param_name  = all_param_names,
      description = description,
      units       = units,
      category    = classification$category,
      in_flux     = classification$in_flux,
      in_dfe      = classification$in_dfe,
      stringsAsFactors = FALSE
    )
    write.csv(df_export, file, row.names = FALSE)
  }
)

# ── Export DFE ────────────────────────────────────────────────────────────────
output$export_dfe_csv <- downloadHandler(
  filename = function()
    paste0("dfe_", format(Sys.Date(), "%Y%m%d"), ".csv"),
  content = function(file) {
    df <- rv_dfe()
    if (is.null(df) || nrow(df) == 0) {
      showNotification("Aucune valeur DFE \u00e0 exporter.", type = "warning")
      return()
    }
    cols <- intersect(c("state_id", "label", "state_type", "species",
                        "dfe_expr", "dfe_auto", "dfe_comment"), names(df))
    write.csv(df[, cols, drop = FALSE], file, row.names = FALSE)
  }
)

# Script Python -- BUG-11 corrigé : handler unique
output$export_python_script <- downloadHandler(
  filename = function() {
    safe <- gsub("[^A-Za-z0-9_]", "_",
                 trimws(input$model_name %||% "model"))
    paste0("ngm_", safe, ".py")
  },
  content = function(file) {
    r <- rv_ngm_result()
    if (is.null(r) || !isTRUE(r$ok)) {
      showNotification("Calculez R\u2080 d'abord.", type = "error"); return()
    }
    ctx      <- build_sympy_context(rv_comp(), rv_flux(), rv_dfe(), rv_params())
    ctx$infected_states <- rv_infected_order()
    ctx_json <- jsonlite::toJSON(ctx, auto_unbox = TRUE,
                                 null = "null", na = "null", force = TRUE)
    # Export Python : genere un script standalone depuis le contexte
    url <- NGM_BASE_URL
    script_lines <- c(
      "#!/usr/bin/env python3",
      paste0('"""Script NGM genere par NGM R0 Calculator v2."""'),
      "",
      "from sympy import *",
      "",
      paste0("# R0 = ", r$steps$`7_eigenvalues`$R0_expr_str),
      paste0("# Expression LaTeX : ", r$steps$`7_eigenvalues`$R0_latex),
      "",
      "# Pour reevaluer, installez sympy et executez ce script.",
      "# Parametres infectes : " ,
      paste0("#   ", paste(r$steps$`1_symbols`$infected_vector, collapse=", ")),
      "",
      paste0("R0_expr = '", r$steps$`7_eigenvalues`$R0_expr_str, "'"),
      "print('R0 =', R0_expr)"
    )
    writeLines(script_lines, file)
  }
)

# PDF -- BUG-10 corrigé : handler unique via fonction
.rmd_template <- function() {
'---
title: "NGM Report"
date: "`r Sys.Date()`"
output:
  pdf_document:
    latex_engine: xelatex
params:
  model_name: "Model"
  compartments: NULL
  fluxes: NULL
  dfe: NULL
  params_table: NULL
  ngm_result: NULL
  sensitivity_result: NULL
# ---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)
library(knitr)
```

# `r params$model_name`

## Compartiments
```{r}
if(!is.null(params$compartments) && nrow(params$compartments)>0)
  kable(params$compartments[,c("state_id","label","state_type","species")])
```

## R0
```{r, results="asis"}
if(!is.null(params$ngm_result) && isTRUE(params$ngm_result$ok)){
  R0 <- params$ngm_result$steps[["7_eigenvalues"]]$R0_latex
  cat("$$R_0 =", R0, "$$")
} else { cat("R0 non calcule.") }
```
'
}

output$btn_export_pdf_main <- downloadHandler(
  filename = function() {
    safe <- gsub("[^A-Za-z0-9_]", "_",
                 trimws(input$model_name %||% "model"))
    paste0("ngm_report_", safe, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  content = function(file) {
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      showNotification("Package 'rmarkdown' manquant.", type = "error")
      return()
    }
    temp_rmd <- tempfile(fileext = ".Rmd")
    writeLines(.rmd_template(), temp_rmd)
    params_list <- list(
      model_name         = input$model_name %||% "Mod\u00e8le",
      compartments       = rv_comp(),
      fluxes             = rv_flux(),
      dfe                = rv_dfe(),
      params_table       = rv_params(),
      ngm_result         = rv_ngm_result(),
      sensitivity_result = rv_sensitivity_result()
    )
    tryCatch({
      showNotification("\U0001f504 G\u00e9n\u00e9ration PDF...",
                       type = "message", duration = NULL, id = "pdf_prog")
      rmarkdown::render(temp_rmd, output_file = file,
                        params = params_list, envir = new.env(), quiet = TRUE)
      removeNotification("pdf_prog")
      showNotification("\u2705 PDF g\u00e9n\u00e9r\u00e9.", type = "message", duration = 5)
    }, error = function(e) {
      removeNotification("pdf_prog")
      showNotification(paste0("\u274c PDF : ", e$message),
                       type = "error", duration = 10)
    })
    unlink(temp_rmd)
  }
)


# =============================================================================
# SECTION 5 -- Aide
# =============================================================================

output$help_content <- renderUI({
  tagList(
    tags$h2("\u2753 Guide \u2014 NGM R\u2080 Calculator"),

    tags$div(
      style = "background:#e7f3ff; border-left:4px solid #2196F3;
               padding:16px; margin:16px 0;",
      tags$h3(style = "margin-top:0; color:#1976D2;",
              "\U0001f680 D\u00e9marrage Rapide"),
      tags$ol(
        tags$li(tags$strong("Compartiments"), " : ajouter S, I, R"),
        tags$li(tags$strong("Flux"), " : S\u2192I (transmission), I\u2192R (recovery)"),
        tags$li(tags$strong("DFE"), " : valider S*=Ntot, I*=0, R*=0"),
        tags$li(tags$strong("Symbolic / NGM"), " : calculer R\u2080"),
        tags$li(tags$strong("\u00c9valuation"), " : saisir les valeurs num\u00e9riques")
      )
    ),

    tags$h3("Concepts"),
    tags$div(
      style = "background:white; border:1px solid #ddd; border-radius:6px;
               padding:16px; margin:16px 0;",
      tags$ul(
        tags$li(tags$strong("R\u2080 < 1"), " \u2192 \u00c9pid\u00e9mie s'\u00e9teint"),
        tags$li(tags$strong("R\u2080 = 1"), " \u2192 Seuil critique"),
        tags$li(tags$strong("R\u2080 > 1"), " \u2192 \u00c9pid\u00e9mie se propage"),
        tags$li(tags$strong("is_F"), " : flux de nouvelle infection (S\u2192I)"),
        tags$li(tags$strong("is_V"), " : sortie d'un infect\u00e9 (I\u2192R, mort...)")
      )
    ),

    tags$h3("Exemple SIR"),
    tags$pre(style = "background:#f5f5f5; padding:12px; border-radius:4px;",
             paste0(
               "S_h \u2192 I_h : beta_c * S_h * I_h / Ntot\n",
               "I_h \u2192 R_h : gamma_r * I_h\n",
               "R\u2080 = beta_c / gamma_r\n",
               "Num\u00e9rique : beta_c=0.3, gamma_r=0.1 \u2192 R\u2080 = 3.0"
             )),

    tags$div(
      style = "margin-top:32px; padding-top:16px; border-top:1px solid #ddd;
               text-align:center; color:#999;",
      tags$p("NGM R\u2080 Calculator \u2014 v2.0")
    )
  )
})
