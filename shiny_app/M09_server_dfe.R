# =============================================================================
# M09_server_dfe.R -- Server : onglet DFE / Paramètres
# =============================================================================
# Dépendances : M01, M02, M05
# ReactiveVals attendues : rv_comp, rv_flux, rv_dfe, rv_params,
#                          rv_dfe_validated, all_verified()
# =============================================================================


# =============================================================================
# SECTION 1 -- Mise à jour automatique DFE + Paramètres
# =============================================================================

# Observer : reconstruit le DFE et la table params à chaque changement
# de comp ou flux, en préservant les éditions manuelles de l'utilisateur.
observe({
  cd <- rv_comp()
  fd <- rv_flux()
  if (nrow(cd) == 0) return()
  # Ne pas reconstruire le DFE pendant un chargement JSON
  # (rv_loading=TRUE signifie que M12 est en train de charger le fichier)
  if (isTRUE(rv_loading())) return()

  new_dfe <- build_dfe_table(cd, fd)

  # Preserver les expressions modifiees manuellement
  # MAIS seulement si le modele est le meme (memes state_ids)
  # Si les state_ids ont change (nouveau modele charge), on utilise
  # les valeurs calculees par build_dfe_table sans preservation
  old <- rv_dfe()
  same_model <- nrow(old) > 0 &&
    length(intersect(old$state_id, new_dfe$state_id)) == nrow(new_dfe) &&
    length(intersect(old$state_id, new_dfe$state_id)) == nrow(old)

  if (same_model) {
    # Meme modele : preserver les valeurs editees manuellement
    for (i in seq_len(nrow(new_dfe))) {
      j <- which(old$state_id == new_dfe$state_id[i])
      if (length(j) > 0 && !isTRUE(old$dfe_auto[j[1]])) {
        new_dfe$dfe_expr[i]    <- old$dfe_expr[j[1]]
        new_dfe$dfe_auto[i]    <- FALSE
        new_dfe$dfe_comment[i] <- old$dfe_comment[j[1]]
      }
    }
  }
  # Si nouveau modele (same_model=FALSE) : utiliser build_dfe_table tel quel
  # Le chargement JSON mettra ensuite rv_dfe() avec les valeurs correctes
  rv_dfe(new_dfe)
  rv_dfe_validated(FALSE)
  rv_dfe_sym_result(NULL)   # reset badge -> "En attente"

  # Reconstruire la table des parametres
  current_dfe <- rv_dfe()
  new_params  <- build_params_table(fd, cd, current_dfe)

  # Préserver descriptions et unités déjà saisies
  old_p <- rv_params()
  if (nrow(old_p) > 0) {
    for (i in seq_len(nrow(new_params))) {
      j <- which(old_p$param_name == new_params$param_name[i])
      if (length(j) > 0) {
        if (!is.na(old_p$description[j[1]]) &&
            old_p$description[j[1]] != "")
          new_params$description[i] <- old_p$description[j[1]]
        if (!is.na(old_p$units[j[1]]) &&
            old_p$units[j[1]] != "")
          new_params$units[i] <- old_p$units[j[1]]
      }
    }
  }
  rv_params(new_params)
})


# =============================================================================
# SECTION 2 -- Rendu de l'onglet DFE
# =============================================================================

output$dfe_tab_content <- renderUI({
  v <- all_verified()

  # Onglet verrouillé si comp/flux non vérifiés
  if (!v$all_ok) {
    raisons <- character(0)
    if (!v$comp_ok) raisons <- c(raisons,
      paste0(v$n_comp_pending, " compartiment(s) non v\u00e9rifi\u00e9s"))
    if (!v$flux_ok) raisons <- c(raisons,
      paste0(v$n_flux_pending, " flux non v\u00e9rifi\u00e9s"))
    return(tags$div(
      style = "background:#f8d7da;border:1px solid #f5c6cb;padding:20px;
               border-radius:6px;margin-top:20px;text-align:center;",
      tags$h4("\U0001f512 Onglet verrouill\u00e9"),
      tags$p("Accessible uniquement quand tous les statuts sont ",
             strong("'ok'"), "."),
      tags$p(strong("En attente : "), paste(raisons, collapse = " et "))))
  }

  dfe_df  <- rv_dfe()
  par_df  <- rv_params()
  has_dem <- has_demography(rv_flux())
  species <- get_species(rv_comp())

  tagList(
    # Bandeau démographie
    if (has_dem)
      tags$div(
        style = "background:#fff3cd;border:1px solid #ffc107;padding:10px;
                 border-radius:5px;margin-bottom:12px;",
        tags$strong("\u26a0\ufe0f D\u00e9mographie d\u00e9tect\u00e9e"),
        tags$p(style = "margin:4px 0 0 0;font-size:13px;",
               "Les valeurs DFE en orange n\u00e9cessitent votre v\u00e9rification. ",
               "Pour un mod\u00e8le avec d\u00e9mographie : S* = \u039b/\u03bc."))
    else
      tags$div(
        style = "background:#d4edda;border:1px solid #c3e6cb;padding:8px;
                 border-radius:5px;margin-bottom:12px;",
        "\u2705 Sans d\u00e9mographie \u2014 les susceptibles sont initialis\u00e9s
         \u00e0 la taille de population de leur esp\u00e8ce."),

    # Bandeau multi-espèces
    if (length(species) > 1)
      tags$div(
        style = "background:#d1ecf1;border:1px solid #bee5eb;padding:8px;
                 border-radius:5px;margin-bottom:12px;",
        paste0("\U0001f52c Mod\u00e8le multi-esp\u00e8ces : ",
               paste(species, collapse = ", "),
               ". Tailles de population : N_h, N_v, N_a, etc.")),

    h4("Table DFE \u2014 Valeurs au DFE"),
    tags$p(style = "color:#666;font-size:13px;",
           "D\u00e9finissez la valeur de chaque compartiment au DFE. ",
           "Vert = calcul\u00e9 auto. Orange = \u00e0 v\u00e9rifier. ",
           "Modifiez dans la colonne 'Valeur au DFE'."),

    # Légende
    tags$div(
      style = "display:flex;gap:16px;margin-bottom:10px;
               font-size:12px;flex-wrap:wrap;",
      tags$span(
        tags$span(style = "background:#eaffea;border:1px solid #c3e6cb;
                  padding:2px 8px;border-radius:3px;", "\u2705 Auto"),
        " calcul\u00e9 automatiquement"),
      tags$span(
        tags$span(style = "background:#fff3cd;border:1px solid #ffc107;
                  padding:2px 8px;border-radius:3px;", "\u26a0\ufe0f \u00c0 v\u00e9rifier"),
        " hypoth\u00e8se d\u00e9mographique"),
      tags$span(
        tags$span(style = "background:#f8d7da;border:1px solid #f5c6cb;
                  padding:2px 8px;border-radius:3px;", "\u270f\ufe0f Manuel"),
        " valeur saisie manuellement"),
      tags$span(
        tags$span(style = "background:#e9ecef;border:1px solid #dee2e6;
                  padding:2px 8px;border-radius:3px;", "\u26d4 Exclu"),
        " hors NGM (dead)")),

    uiOutput("dfe_table_ui"),
    br(),

    h4("Param\u00e8tres du mod\u00e8le"),
    tags$p(style = "color:#666;font-size:13px;",
           "Tous les symboles libres d\u00e9tect\u00e9s dans les expressions de taux."),

    if (nrow(par_df) == 0)
      tags$p(style = "color:#999;font-style:italic;",
             "Aucun param\u00e8tre libre d\u00e9tect\u00e9.")
    else
      uiOutput("params_table_ui"),

    br(),

    # Bouton validation
    tags$div(
      style = "background:#f8f9fa;border:1px solid #dee2e6;padding:14px;
               border-radius:6px;",
      fluidRow(
        column(5, actionButton("btn_validate_dfe", "\u2705 Valider le DFE",
                               class = "btn-success btn-lg",
                               style = "width:100%;")),
        column(7, uiOutput("dfe_validation_status"))),
      tags$p(style = "color:#666;font-size:12px;margin-top:8px;margin-bottom:0;",
             "\u26a0\ufe0f V\u00e9rifiez toutes les valeurs DFE avant de valider. ",
             "La validation d\u00e9verrouillera l'onglet Symbolic / NGM.")),
    br(),

    # Aperçu contexte SymPy (debug)
    tags$details(
      tags$summary(style = "cursor:pointer;font-weight:600;color:#495057;",
                   "\U0001f50d Aper\u00e7u du contexte SymPy"),
      br(),
      verbatimTextOutput("sympy_context_preview"))
  )
})


# =============================================================================
# SECTION 3 -- Rendu table DFE
# =============================================================================

output$dfe_table_ui <- renderUI({
  dfe_df <- rv_dfe()
  if (nrow(dfe_df) == 0)
    return(tags$p("Aucun compartiment d\u00e9fini."))

  row_bg <- function(auto, expr, state_type) {
    if (!is.na(state_type) && state_type == "dead") return("#f0f0f0")
    if (isTRUE(auto) && expr == "0")  return("#f5f5f5")
    if (isTRUE(auto) && expr != "")   return("#eaffea")
    if (!isTRUE(auto) && expr == "")  return("#f8d7da")
    if (!isTRUE(auto))                return("#fff3cd")
    "#ffffff"
  }

  tags$div(
    style = "overflow-x:auto;",
    tags$table(
      style = "width:100%;border-collapse:collapse;font-size:13px;",
      tags$thead(tags$tr(
        style = "background:#343a40;color:white;",
        tags$th(style = "padding:8px;min-width:80px;",  "ID"),
        tags$th(style = "padding:8px;min-width:120px;", "Label"),
        tags$th(style = "padding:8px;min-width:100px;", "Type"),
        tags$th(style = "padding:8px;min-width:70px;",  "Esp\u00e8ce"),
        tags$th(style = "padding:8px;min-width:220px;", "Valeur au DFE"),
        tags$th(style = "padding:8px;",                 "Commentaire")
      )),
      tags$tbody(lapply(seq_len(nrow(dfe_df)), function(i) {
        row     <- dfe_df[i, ]
        bg      <- row_bg(row$dfe_auto, row$dfe_expr, row$state_type)
        is_dead <- !is.na(row$state_type) && row$state_type == "dead"
        tags$tr(
          style = paste0("background:", bg,
                         ";border-bottom:1px solid #dee2e6;"),
          tags$td(style = "padding:6px 8px;font-weight:600;
                  font-family:monospace;", row$state_id),
          tags$td(style = "padding:6px 8px;", row$label),
          tags$td(style = "padding:4px 8px;",
                  tags$span(
                    style = paste0("background:",
                                   dfe_type_badge_color(row$state_type),
                                   ";padding:2px 6px;border-radius:3px;",
                                   "font-size:11px;font-weight:600;"),
                    row$state_type)),
          tags$td(style = "padding:6px 8px;color:#666;font-size:12px;",
                  row$species),
          tags$td(style = "padding:4px 6px;",
                  if (is_dead)
                    tags$span(style = "color:#999;font-style:italic;
                              font-size:12px;", "0 (exclu NGM)")
                  else
                    textInput(
                      inputId = paste0("dfe_val_", row$state_id),
                      label   = NULL,
                      value   = row$dfe_expr,
                      placeholder = "ex: Ntot, Lambda_b/mu_d, 0",
                      width   = "100%")),
          tags$td(style = "padding:6px 8px;font-size:12px;color:#555;",
                  row$dfe_comment))
      }))
    )
  )
})


# =============================================================================
# SECTION 4 -- Rendu table Paramètres
# =============================================================================

output$params_table_ui <- renderUI({
  par_df <- rv_params()
  if (nrow(par_df) == 0) return(NULL)

  banners <- list()

  # Avertissement symboles réservés dans les paramètres
  issues <- lapply(par_df$param_name, get_sympy_issues)
  problematic <- par_df$param_name[
    sapply(issues, function(x) x$has_errors || x$has_warnings)
  ]
  if (length(problematic) > 0) {
    sugg_html <- lapply(problematic, function(name) {
      iss <- get_sympy_issues(name)
      msg <- if (iss$has_errors) paste(iss$errors, collapse = "; ")
             else paste(iss$warnings, collapse = "; ")
      tags$div(style = "margin-left:20px;margin-top:6px;",
               tags$code(style = "background:#856404;color:white;
                          padding:2px 6px;border-radius:3px;", name),
               tags$span(style = "margin-left:8px;font-size:12px;", msg),
               tags$br(),
               tags$span(style = "margin-left:8px;color:#856404;
                          font-size:12px;",
                         "\u2192 Try: ", get_sympy_suggestion(name)))
    })
    banners[[1]] <- tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;padding:12px;
               border-radius:4px;margin-bottom:12px;",
      tags$strong("\u26a0\ufe0f Symboles SymPy r\u00e9serv\u00e9s dans les param\u00e8tres"),
      tags$div(sugg_html))
  }

  tagList(
    banners,

    # Légende badges
    tags$div(
      style = "background:#f8f9fa;border:1px solid #dee2e6;border-radius:4px;
               padding:10px;margin-bottom:12px;font-size:12px;",
      tags$strong("L\u00e9gende : "),
      tags$span(style = "background:#28a745;color:white;padding:2px 6px;
                border-radius:3px;margin-left:8px;font-size:10px;
                font-weight:600;", "Flux"),
      tags$span(style = "margin-left:4px;color:#666;",
                "= dans les flux"),
      tags$span(style = "background:#17a2b8;color:white;padding:2px 6px;
                border-radius:3px;margin-left:12px;font-size:10px;
                font-weight:600;", "DFE"),
      tags$span(style = "margin-left:4px;color:#666;",
                "= dans le DFE uniquement"),
      tags$span(style = "background:#6c757d;color:white;padding:2px 6px;
                border-radius:3px;margin-left:12px;font-size:10px;
                font-weight:600;", "Flux+DFE"),
      tags$span(style = "margin-left:4px;color:#666;",
                "= dans les flux ET le DFE")),

    # Table paramètres
    tags$div(
      style = "overflow-x:auto;",
      tags$table(
        style = "width:100%;border-collapse:collapse;font-size:13px;",
        tags$thead(tags$tr(
          style = "background:#495057;color:white;",
          tags$th(style = "padding:8px;min-width:110px;", "Param\u00e8tre"),
          tags$th(style = "padding:8px;",                 "Description"),
          tags$th(style = "padding:8px;min-width:110px;", "Unit\u00e9s")
        )),
        tags$tbody(lapply(seq_len(nrow(par_df)), function(i) {
          p <- par_df[i, ]

          badge <- switch(
            p$category %||% "",
            dfe_only = tags$span(
              style = "background:#17a2b8;color:white;font-size:9px;
                       padding:2px 5px;border-radius:3px;margin-left:4px;",
              "DFE"),
            flux_only = tags$span(
              style = "background:#28a745;color:white;font-size:9px;
                       padding:2px 5px;border-radius:3px;margin-left:4px;",
              "Flux"),
            both = tags$span(
              style = "background:#6c757d;color:white;font-size:9px;
                       padding:2px 5px;border-radius:3px;margin-left:4px;",
              "Flux+DFE"),
            NULL
          )

          tags$tr(
            style = if (i %% 2 == 0)
              "background:#f8f9fa;border-bottom:1px solid #dee2e6;"
            else
              "background:white;border-bottom:1px solid #dee2e6;",
            tags$td(style = "padding:4px 8px;font-family:monospace;
                    font-weight:600;background:#f0f0f0;",
                    tags$code(p$param_name), badge),
            tags$td(style = "padding:4px 6px;",
                    textInput(paste0("par_desc_", p$param_name), NULL,
                              value = p$description,
                              placeholder = "Description",
                              width = "100%")),
            tags$td(style = "padding:4px 6px;",
                    textInput(paste0("par_units_", p$param_name), NULL,
                              value = p$units,
                              placeholder = "ex: 1/jour",
                              width = "100%")))
        }))
      )
    )
  )
})


# =============================================================================
# SECTION 5 -- Observers inputs DFE et Paramètres
# =============================================================================

# Mise à jour dfe_expr depuis les textInputs
observe({
  dfe_df <- rv_dfe()
  if (nrow(dfe_df) == 0) return()
  for (i in seq_len(nrow(dfe_df))) {
    local({
      sid <- dfe_df$state_id[i]
      observeEvent(input[[paste0("dfe_val_", sid)]], {
        df2 <- rv_dfe()
        j   <- which(df2$state_id == sid)
        if (length(j) > 0) {
          df2$dfe_expr[j]    <- trimws(input[[paste0("dfe_val_", sid)]])
          df2$dfe_auto[j]    <- FALSE
          df2$dfe_comment[j] <- "\u270f\ufe0f Valeur modifi\u00e9e manuellement"
          rv_dfe(df2)
          rv_dfe_validated(FALSE)
          rv_dfe_sym_result(NULL)   # reset badge -> "En attente"
        }
      }, ignoreInit = TRUE)
    })
  }
})

# Mise à jour descriptions et unités des paramètres
observe({
  par_df <- rv_params()
  if (nrow(par_df) == 0) return()
  for (i in seq_len(nrow(par_df))) {
    local({
      pn <- par_df$param_name[i]
      observeEvent(input[[paste0("par_desc_", pn)]], {
        df2 <- rv_params()
        j   <- which(df2$param_name == pn)
        if (length(j) > 0) {
          df2$description[j] <- input[[paste0("par_desc_", pn)]]
          rv_params(df2)
        }
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0("par_units_", pn)]], {
        df2 <- rv_params()
        j   <- which(df2$param_name == pn)
        if (length(j) > 0) {
          df2$units[j] <- input[[paste0("par_units_", pn)]]
          rv_params(df2)
        }
      }, ignoreInit = TRUE)
    })
  }
})


# =============================================================================
# SECTION 6 -- Validation DFE
# =============================================================================

# ReactiveVal pour stocker le resultat de validation symbolique
rv_dfe_sym_result <- reactiveVal(NULL)

observeEvent(input$btn_validate_dfe, {
  dfe_df <- rv_dfe()
  par_df <- rv_params()
  errors <- character(0)

  # -- Niveau 1a : expressions non-dead doivent etre remplies ----------------
  missing <- dfe_df[
    !is.na(dfe_df$state_type) &
    dfe_df$state_type != "dead" &
    trimws(dfe_df$dfe_expr) == "", ]
  if (nrow(missing) > 0)
    errors <- c(errors, paste0(
      "Valeur DFE manquante pour : ",
      paste(missing$state_id, collapse = ", ")))

  # -- Niveau 1b : syntaxe SymPy de chaque expression DFE -------------------
  known_in_dfe <- c(dfe_df$state_id, par_df$param_name)
  for (i in seq_len(nrow(dfe_df))) {
    expr <- trimws(dfe_df$dfe_expr[i])
    if (expr == "" || expr == "0") next
    vr <- validate_sympy_expression(expr, known_in_dfe)
    if (!vr$valid)
      errors <- c(errors, paste0(
        dfe_df$state_id[i], " : ",
        paste(vr$errors, collapse = "; ")))
  }

  if (length(errors) > 0) {
    rv_dfe_validated(FALSE)
    rv_dfe_sym_result(NULL)
    showNotification(
      paste0("\u274c ", paste(errors, collapse = " | ")),
      type = "error", duration = 10)
    return()
  }

  # -- Niveau 2 : validation symbolique via Python ---------------------------
  # Reconstruction fi_exprs/vi_exprs DIRECTEMENT depuis les flux
  # (sans dependre de rv_symbolic() qui peut ne pas etre encore calcule)

  url <- NGM_BASE_URL
  if (is.null(url)) {
    rv_dfe_validated(TRUE)
    rv_dfe_sym_result(NULL)
    showNotification(
      "\u2705 DFE valide (syntaxe OK). Serveur FastAPI indisponible.",
      type = "warning", duration = 5)
    return()
  }

  # Construire le contexte complet depuis M05 (inclut fi_exprs et vi_exprs)
  # build_sympy_context() appelle classify_fluxes() qui calcule is_F/is_V
  # directement depuis les flux et les types de compartiments
  ctx <- build_sympy_context(rv_comp(), rv_flux(), dfe_df, par_df)

  # infected_states : ordre depuis rv_infected_order() ou depuis les compartiments
  inf_order <- rv_infected_order()
  if (length(inf_order) == 0)
    inf_order <- rv_comp()$state_id[
      rv_comp()$state_type %in% INFECTED_STATE_TYPES]
  ctx$infected_states <- inf_order

  # Log de diagnostic
  message("[validate_dfe] infected_states: ",
          paste(ctx$infected_states, collapse=", "))
  for (sid in ctx$infected_states) {
    message("[validate_dfe] F_", sid, " = ",
            ctx$fi_exprs[[sid]] %||% "ABSENT")
    message("[validate_dfe] V_", sid, " = ",
            ctx$vi_exprs[[sid]] %||% "ABSENT")
  }
  message("[validate_dfe] dfe_subs: ",
          paste(names(ctx$dfe_subs), unlist(ctx$dfe_subs),
                sep = "=", collapse = ", "))

  withProgress(message = "Validation symbolique du DFE...", value = 0.5, {
    sym_result <- tryCatch({
      api_validate_dfe(ctx, url)
    }, error = function(e) {
      list(ok = FALSE, error = e$message)
    })
  })

  # Log : voir exactement ce que R recoit du serveur
  message("[sym_result] class=", class(sym_result))
  message("[sym_result] ok=",           scal_lgl(sym_result$ok))
  message("[sym_result] valid=",        scal_lgl(sym_result$valid))
  message("[sym_result] V_nonsingular=",scal_lgl(sym_result$V_nonsingular))
  message("[sym_result] n_errors=",     scal_int(sym_result$n_errors))
  l2 <- sym_result$level2
  if (!is.null(l2)) {
    message("[sym_result] V_det=",      l2$V_det)
    message("[sym_result] V_nonsing2=", l2$V_nonsingular)
  }

  rv_dfe_sym_result(sym_result)

  if (!scal_lgl(sym_result$ok)) {
    rv_dfe_validated(TRUE)  # syntaxe OK, juste validation sym failed
    showNotification(
      paste0("\u26a0\ufe0f Validation symbolique echouee : ", sym_result$error),
      type = "warning", duration = 8)
    return()
  }

  if (scal_lgl(sym_result$valid)) {
    rv_dfe_validated(TRUE)
    showNotification(
      "\u2705 DFE valide -- F_i = 0 et V inversible au DFE.",
      type = "message", duration = 5)
  } else {
    # Des erreurs symboliques mais on laisse l'utilisateur continuer
    # avec un avertissement (il peut avoir des raisons valides)
    rv_dfe_validated(TRUE)
    n_err <- sym_result$n_errors %||% 0L
    showNotification(
      paste0("\u26a0\ufe0f DFE accepte avec ", n_err,
             " probleme(s) symbolique(s). Voir le rapport ci-dessous."),
      type = "warning", duration = 6)
  }
})

output$dfe_validation_status <- renderUI({
  validated  <- rv_dfe_validated()
  sym_result <- rv_dfe_sym_result()

  # -- Badge principal -------------------------------------------------------
  # Etat initial : rv_dfe_validated() = FALSE mais rien encore soumis
  # On distingue "pas encore valide" (NULL sym_result ET validated=FALSE)
  # de "valide et echoue" (validated=FALSE apres tentative)
  if (!validated && is.null(sym_result)) {
    # Etat neutre initial -- aucune validation encore tentee
    badge <- tags$span(
      style = "background:#e9ecef;border:1px solid #ced4da;
               padding:8px 16px;border-radius:6px;
               font-weight:600;color:#6c757d;font-size:13px;",
      "\u23f3 En attente de validation")
    return(tags$div(style = "padding:6px;", badge))
  }

  if (!validated) {
    badge <- tags$span(
      style = "background:#f8d7da;border:1px solid #f5c6cb;
               padding:8px 16px;border-radius:6px;
               font-weight:600;color:#721c24;font-size:13px;",
      "\u274c Non valide")
    return(tags$div(style = "padding:6px;", badge))
  }

  if (is.null(sym_result)) {
    badge <- tags$span(
      style = "background:#d4edda;border:1px solid #c3e6cb;
               padding:8px 16px;border-radius:6px;
               font-weight:600;color:#155724;font-size:13px;",
      "\u2705 DFE valide (syntaxe)")
    return(tags$div(style = "padding:6px;", badge))
  }

  # -- Rapport detaille de validation symbolique ----------------------------
  n_err  <- scal_int(sym_result$n_errors,  0L)
  n_warn <- scal_int(sym_result$n_warnings, 0L)
  all_F  <- scal_lgl(sym_result$all_F_zero)
  V_ok   <- scal_lgl(sym_result$V_nonsingular)
  # Extraire level2 comme liste (simplifyVector peut en faire un data.frame)
  l2 <- sym_result$level2
  if (!is.list(l2) || is.data.frame(l2)) l2 <- as.list(l2)
  # Extraire V_nonsingular depuis level2 si top-level est NULL
  if (!scal_lgl(sym_result$V_nonsingular) && !is.null(l2$V_nonsingular))
    V_ok <- scal_lgl(l2$V_nonsingular)

  is_valid   <- scal_lgl(sym_result$valid)
  main_style <- if (is_valid)
    "background:#d4edda;border:1px solid #28a745;"
  else
    "background:#fff3cd;border:1px solid #ffc107;"
  main_color <- if (is_valid) "#155724" else "#856404"
  main_icon  <- if (is_valid) "\u2705" else "\u26a0\ufe0f"
  main_label <- if (is_valid)
    "DFE mathematiquement valide" else
    paste0("DFE accepte avec ", n_err, " probleme(s)")

  tags$div(
    style = "padding:6px;",
    # Badge principal
    tags$div(
      style = paste0(main_style,
                     "border-radius:6px;padding:8px 16px;",
                     "margin-bottom:10px;"),
      tags$span(style = paste0("font-weight:600;color:", main_color,
                               ";font-size:13px;"),
                main_icon, " ", main_label)
    ),

    # Rapport F_i
    tags$div(
      style = "background:#f8f9fa;border:1px solid #dee2e6;
               border-radius:5px;padding:10px 14px;margin-bottom:8px;",
      tags$strong("Verification F_i = 0 au DFE :"),
      tags$div(style = "margin-top:6px;",
        lapply(names(if (!is.null(l2)) (l2$F_at_dfe %||% list()) else list()),
               function(sid) {
          is_zero <- isTRUE(if (!is.null(l2)) l2$F_is_zero[[sid]] else FALSE)
          expr    <- if (!is.null(l2)) (l2$F_at_dfe[[sid]] %||% "?") else "?"
          tags$div(
            style = "display:flex;align-items:center;gap:8px;
                     margin-bottom:4px;font-size:13px;",
            tags$span(if (is_zero) "\u2705" else "\u274c"),
            tags$code(paste0("F_", sid)),
            tags$span(style = "color:#666;", paste0("= ", expr, " au DFE")),
            if (!is_zero) tags$span(
              style = "background:#f8d7da;color:#721c24;font-size:11px;
                       padding:1px 6px;border-radius:3px;",
              "Doit etre 0 !")
          )
        })
      )
    ),

    # Rapport V
    tags$div(
      style = "background:#f8f9fa;border:1px solid #dee2e6;
               border-radius:5px;padding:10px 14px;margin-bottom:8px;",
      tags$strong("Verification matrice V au DFE :"),
      tags$div(style = "display:flex;align-items:center;gap:8px;margin-top:6px;",
        tags$span(if (V_ok) "\u2705" else "\u274c"),
        tags$span(style = "font-size:13px;",
          if (V_ok)
            paste0("V inversible (det = ",
                   as.character(if (!is.null(l2)) (l2$V_det %||% "?") else "?"),
                   ")")
          else
            paste0("V singuliere (det = 0) -- calcul NGM impossible")
        )
      )
    ),

    # Issues symboliques detaillees
    if (n_err > 0 || n_warn > 0) {
      all_issues <- c(
        if (is.list(sym_result$issues))   sym_result$issues   else list(),
        if (is.list(sym_result$warnings)) sym_result$warnings else list()
      )
      tags$div(
        style = "background:#fff3cd;border:1px solid #ffc107;
                 border-radius:5px;padding:10px 14px;",
        tags$strong(paste0("\u26a0\ufe0f Problemes detectes (", n_err + n_warn, ") :")),
        tags$ul(style = "margin:6px 0 0 0;font-size:12px;",
          lapply(all_issues, function(iss) {
            col <- if (iss$level == "error") "#721c24" else "#856404"
            tags$li(style = paste0("color:", col, ";margin-bottom:4px;"),
                    iss$message %||% "")
          })
        )
      )
    }
  )
})


# =============================================================================
# SECTION 7 -- Aperçu contexte SymPy (debug)
# =============================================================================

output$sympy_context_preview <- renderPrint({
  dfe_df <- rv_dfe()
  par_df <- rv_params()
  if (nrow(dfe_df) == 0) { cat("DFE non encore disponible.\n"); return() }
  ctx <- build_sympy_context(rv_comp(), rv_flux(), dfe_df, par_df)
  cat("=== CONTEXTE SYMPY ===\n\n")
  cat("Compartiments infect\u00e9s :\n")
  cat(" ", paste(ctx$infected_states, collapse = ", "), "\n\n")
  cat("Substitutions DFE :\n")
  for (nm in names(ctx$dfe_subs))
    cat(sprintf("  %-15s = %s\n", nm, ctx$dfe_subs[[nm]]))
  cat("\nExpressions F_i :\n")
  for (nm in names(ctx$fi_exprs))
    cat(sprintf("  F[%-10s] = %s\n", nm, ctx$fi_exprs[[nm]]))
  cat("\nExpressions V_i :\n")
  for (nm in names(ctx$vi_exprs))
    cat(sprintf("  V[%-10s] = %s\n", nm, ctx$vi_exprs[[nm]]))
  cat(sprintf("\nD\u00e9mographie : %s\n", ctx$has_demography))
  cat(sprintf("Esp\u00e8ces    : %s\n", paste(ctx$species, collapse = ", ")))
})
