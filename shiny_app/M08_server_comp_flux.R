# =============================================================================
# M08_server_comp_flux.R -- Server : CRUD Compartiments & Flux, modal ⚙️ Rate
# =============================================================================
# Dépendances : M01, M02, M03
# ReactiveVals attendues dans l'environnement parent :
#   rv_comp, rv_flux, rv_params, rv_flux_suggestions, species_list
# =============================================================================


# =============================================================================
# SECTION 1 -- Helpers HTML pour les tables DT
# =============================================================================

#' Construire le HTML de la table Compartiments pour DT
create_comp_html <- function(df) {
  if (!"verification" %in% names(df)) df$verification <- "pending"

  do.call(rbind, lapply(seq_len(nrow(df)), function(i) {

    # Dropdown state_type
    st_opts <- paste0(sapply(STATE_TYPES, function(st) {
      sel <- if (!is.na(df$state_type[i]) && df$state_type[i] == st)
        " selected" else ""
      sprintf('<option value="%s"%s>%s</option>', st, sel,
              STATE_COMMENTS[[st]])
    }), collapse = "")
    state_type_sel <- sprintf(
      '<select class="compact-select" id="comp_st_%d"
           onchange="Shiny.setInputValue(\'comp_st_%d\',this.value)">
           %s</select>',
      i, i, st_opts)

    # Input espèce avec datalist
    dl_id   <- paste0("species_list_", i)
    dl_opts <- paste0(sapply(species_list(), function(sp)
      sprintf('<option value="%s">', sp)), collapse = "")
    species_inp <- sprintf(
      '<input type="text" class="editable-input" id="comp_species_%d"
           list="%s" value="%s"
           onchange="Shiny.setInputValue(\'comp_species_%d\',this.value)">
       <datalist id="%s">%s</datalist>',
      i, dl_id,
      if (!is.na(df$species[i])) df$species[i] else "",
      i, dl_id, dl_opts)

    # Dropdown verification
    ver_val <- if (is.na(df$verification[i]) ||
                   df$verification[i] == "") "pending"
               else df$verification[i]
    ver_sel <- sprintf(
      '<select class="compact-select" id="comp_ver_%d"
           onchange="Shiny.setInputValue(\'comp_ver_%d\',this.value)">
           <option value="pending"%s>pending</option>
           <option value="ok"%s>ok</option></select>',
      i, i,
      if (ver_val == "pending") " selected" else "",
      if (ver_val == "ok")      " selected" else "")

    data.frame(
      state_id      = df$state_id[i],
      label         = df$label[i],
      state_type    = state_type_sel,
      state_comment = df$state_comment[i],
      species       = species_inp,
      strain        = df$strain[i],
      description   = df$description[i],
      verification  = ver_sel,
      stringsAsFactors = FALSE)
  }))
}

#' Réinitialiser les champs rate et verification d'une ligne flux
reset_rate_and_ver <- function(fd, idx) {
  fd$rate_name[idx]            <- ""
  fd$rate_expression[idx]      <- ""
  fd$rate_template_id[idx]     <- ""
  fd$rate_template_label[idx]  <- ""
  fd$rate_expression_auto[idx] <- ""
  fd$rate_source[idx]          <- ""
  fd$rate_validated[idx]       <- FALSE
  fd$rate_units[idx]           <- ""
  fd$rate_comment[idx]         <- ""
  fd$rate_param_desc[idx]      <- ""
  fd$verification[idx]         <- "pending"
  fd
}

#' Construire le HTML de la table Flux pour DT
create_flux_html <- function(flux_df, comp_df) {
  if (nrow(flux_df) == 0)
    return(data.frame(
      flux_id = character(0), from_state_id = character(0),
      to_state_id = character(0), process_type = character(0),
      transmission_type = character(0), density_type = character(0),
      rate_name = character(0), rate_expression = character(0),
      rate_config = character(0), verification = character(0),
      stringsAsFactors = FALSE))

  if (!"verification" %in% names(flux_df))
    flux_df$verification <- "pending"

  sids <- comp_df$state_id

  mk_sid <- function(input_id, cur) {
    opts <- paste0(
      '<option value="">--</option>',
      paste0(sapply(sids, function(s) {
        sel <- if (!is.na(cur) && cur == s) " selected" else ""
        sprintf('<option value="%s"%s>%s</option>', s, sel, s)
      }), collapse = ""))
    sprintf(
      '<select class="compact-select" id="%s"
           onchange="Shiny.setInputValue(\'%s\',this.value)">%s</select>',
      input_id, input_id, opts)
  }

  do.call(rbind, lapply(seq_len(nrow(flux_df)), function(i) {
    f <- flux_df[i, ]

    # process_type
    proc_opts <- paste0(sapply(PROCESS_TYPES, function(pt) {
      sel <- if (!is.na(f$process_type) && f$process_type == pt)
        " selected" else ""
      sprintf('<option value="%s"%s>%s</option>', pt, sel, pt)
    }), collapse = "")
    proc_sel <- sprintf(
      '<select class="compact-select" id="flux_process_%d"
           onchange="Shiny.setInputValue(\'flux_process_%d\',this.value)">
           <option value=""%s>--</option>%s</select>',
      i, i,
      if (is.na(f$process_type) || f$process_type == "") " selected" else "",
      proc_opts)

    is_trans <- !is.na(f$process_type) && f$process_type == "transmission"

    # transmission_type
    if (is_trans) {
      tr_opts <- paste0(sapply(TRANSMISSION_TYPES, function(tt) {
        sel <- if (!is.na(f$transmission_type) &&
                   f$transmission_type == tt) " selected" else ""
        sprintf('<option value="%s"%s>%s</option>', tt, sel, tt)
      }), collapse = "")
      trans_sel <- sprintf(
        '<select class="compact-select" id="flux_trans_%d"
             onchange="Shiny.setInputValue(\'flux_trans_%d\',this.value)">
             <option value="">--</option>%s</select>', i, i, tr_opts)
    } else {
      trans_sel <- '<span style="color:#999;">N/A</span>'
    }

    # density_type
    if (is_trans) {
      dn_opts <- paste0(sapply(DENSITY_TYPES, function(dt) {
        sel <- if (!is.na(f$density_type) &&
                   f$density_type == dt) " selected" else ""
        sprintf('<option value="%s"%s>%s</option>', dt, sel, dt)
      }), collapse = "")
      dens_sel <- sprintf(
        '<select class="compact-select" id="flux_dens_%d"
             onchange="Shiny.setInputValue(\'flux_dens_%d\',this.value)">
             <option value="">--</option>%s</select>', i, i, dn_opts)
    } else {
      dens_sel <- '<span style="color:#999;">N/A</span>'
    }

    # Bouton Rate
    rate_btn <- sprintf(
      '<button class="btn btn-sm btn-primary"
           onclick="Shiny.setInputValue(\'rate_btn_%d\',Math.random())"
           style="padding:2px 8px;font-size:12px;">\u2699\ufe0f Rate</button>',
      i)

    # verification
    ver_val <- if (is.na(f$verification) || f$verification == "") "pending"
               else f$verification
    ver_sel <- sprintf(
      '<select class="compact-select" id="flux_ver_%d"
           onchange="Shiny.setInputValue(\'flux_ver_%d\',this.value)">
           <option value="pending"%s>pending</option>
           <option value="ok"%s>ok</option></select>',
      i, i,
      if (ver_val == "pending") " selected" else "",
      if (ver_val == "ok")      " selected" else "")

    data.frame(
      flux_id           = f$flux_id,
      from_state_id     = mk_sid(paste0("flux_from_", i), f$from_state_id),
      to_state_id       = mk_sid(paste0("flux_to_",   i), f$to_state_id),
      process_type      = proc_sel,
      transmission_type = trans_sel,
      density_type      = dens_sel,
      rate_name         = if (!is.na(f$rate_name)) f$rate_name else "",
      rate_expression   = if (!is.na(f$rate_expression)) f$rate_expression else "",
      rate_config       = rate_btn,
      verification      = ver_sel,
      stringsAsFactors  = FALSE)
  }))
}


# =============================================================================
# SECTION 2 -- Onglet Compartiments
# =============================================================================

# ── Rendu table ───────────────────────────────────────────────────────────────
output$comp_table <- renderDT({
  datatable(
    create_comp_html(rv_comp()),
    escape = FALSE, rownames = FALSE, selection = "single",
    editable = list(target = "cell", columns = c(0, 1, 5, 6)),
    options  = list(dom = 't', paging = FALSE,
                    ordering = FALSE, scrollX = TRUE))
}, server = FALSE)

# ── Édition cellule ───────────────────────────────────────────────────────────
observeEvent(input$comp_table_cell_edit, {
  info <- input$comp_table_cell_edit
  df   <- rv_comp()

  if (info$col == 0) {
    new_id <- trimws(info$value)
    v      <- validate_sympy_id(new_id)

    if (!v$valid) {
      showNotification(
        tags$div(tags$strong("\u274c state_id invalide"),
                 tags$br(),
                 tags$span(paste(v$errors, collapse = " | "))),
        type = "error", duration = 10)
      df$state_id[info$row]     <- paste0("id_", info$row)
      df$verification[info$row] <- "pending"
      rv_comp(df)
      return()
    }
    if (new_id %in% df$state_id[-info$row]) {
      showNotification(
        tags$div(tags$strong("\u274c state_id dupliqu\u00e9"),
                 tags$br(),
                 tags$span(paste0("'", new_id, "' existe d\u00e9j\u00e0."))),
        type = "error", duration = 8)
      df$state_id[info$row]     <- paste0("id_", info$row)
      df$verification[info$row] <- "pending"
      rv_comp(df)
      return()
    }
    if (length(v$warnings) > 0)
      showNotification(
        tags$div(tags$strong("\u26a0\ufe0f Avertissement state_id"),
                 tags$br(),
                 tags$span(paste(v$warnings, collapse = " | "))),
        type = "warning", duration = 8)

    df$state_id[info$row] <- new_id

  } else if (info$col == 1) { df$label[info$row]       <- info$value
  } else if (info$col == 5) { df$strain[info$row]      <- info$value
  } else if (info$col == 6) { df$description[info$row] <- info$value
  }

  df$verification[info$row] <- "pending"
  rv_comp(df)
})

# ── Observers dropdowns ───────────────────────────────────────────────────────
observe({
  df <- rv_comp()
  for (i in seq_len(nrow(df))) local({
    ii <- i

    observeEvent(input[[paste0("comp_st_", ii)]], {
      df2 <- rv_comp(); if (ii > nrow(df2)) return()
      st  <- input[[paste0("comp_st_", ii)]]
      if (!is.null(st)) {
        df2$state_type[ii]    <- st
        df2$state_comment[ii] <- STATE_COMMENTS[[st]] %||% ""
        df2$verification[ii]  <- "pending"
        rv_comp(df2)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observeEvent(input[[paste0("comp_species_", ii)]], {
      df2 <- rv_comp(); if (ii > nrow(df2)) return()
      sp  <- input[[paste0("comp_species_", ii)]]
      if (!is.null(sp)) {
        df2$species[ii] <- sp
        if (sp != "" && !sp %in% species_list())
          species_list(c(species_list(), sp))
        df2$verification[ii] <- "pending"
        rv_comp(df2)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observeEvent(input[[paste0("comp_ver_", ii)]], {
      df2 <- rv_comp(); if (ii > nrow(df2)) return()
      val <- input[[paste0("comp_ver_", ii)]]
      if (!is.null(val)) {
        df2$verification[ii] <- val
        rv_comp(df2)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  })
})

# ── Add / Delete / Validate all ───────────────────────────────────────────────
observeEvent(input$add_comp, {
  df <- rv_comp(); i <- 1L
  repeat { nid <- paste0("C", i); if (!nid %in% df$state_id) break; i <- i + 1L }
  rv_comp(rbind(df, data.frame(
    state_id = nid, label = "", state_type = "", state_comment = "",
    species = "", strain = "", age_class = "", patch = "",
    description = "", verification = "pending",
    stringsAsFactors = FALSE)))
})

observeEvent(input$del_comp, {
  sel <- input$comp_table_rows_selected
  if (length(sel) == 1) {
    df <- rv_comp()
    if (nrow(df) <= 1) {
      showNotification("Impossible de supprimer le dernier compartiment.",
                       type = "warning")
      return()
    }
    # ID du compartiment supprimé
    deleted_id <- df$state_id[sel]

    # Supprimer le compartiment
    rv_comp(df[-sel, , drop = FALSE])

    # Supprimer les flux qui référencent ce compartiment
    # (from_state_id OU to_state_id correspond à l'ID supprimé)
    fd <- rv_flux()
    if (nrow(fd) > 0) {
      keep <- !(
        (!is.na(fd$from_state_id) & fd$from_state_id == deleted_id) |
        (!is.na(fd$to_state_id)   & fd$to_state_id   == deleted_id)
      )
      n_removed <- sum(!keep)
      if (n_removed > 0) {
        rv_flux(fd[keep, , drop = FALSE])
        showNotification(
          paste0("Compartiment '", deleted_id, "' supprim\u00e9 -- ",
                 n_removed, " flux associ\u00e9(s) \u00e9galement supprim\u00e9(s)."),
          type = "message", duration = 4)
      } else {
        showNotification(
          paste0("Compartiment '", deleted_id, "' supprim\u00e9."),
          type = "message", duration = 3)
      }
    }
  }
})

observeEvent(input$validate_all_comp, {
  df <- rv_comp()
  if (nrow(df) > 0) {
    df$verification <- "ok"
    rv_comp(df)
    showNotification(paste0("\u2705 ", nrow(df), " compartiment(s) valid\u00e9(s)."),
                     type = "message", duration = 3)
  }
})

# ── Bannière validation ───────────────────────────────────────────────────────
output$comp_validation_banner <- renderUI({
  v  <- all_verified()
  cd <- rv_comp()
  banners <- list()

  # Vérifier les symboles SymPy réservés
  # vapply garantit logical(0) sur vecteur vide, contrairement a sapply
  if (nrow(cd) > 0 && length(cd$state_id) > 0) {
    issues           <- lapply(cd$state_id, get_sympy_issues)
    problematic_comp <- cd$state_id[
      vapply(issues, function(x) isTRUE(x$has_errors) || isTRUE(x$has_warnings),
             logical(1))
    ]
  } else {
    problematic_comp <- character(0)
  }
  if (length(problematic_comp) > 0) {
    suggestions_html <- lapply(problematic_comp, function(name) {
      iss <- get_sympy_issues(name)
      msg <- if (iss$has_errors) paste(iss$errors, collapse = "; ")
             else paste(iss$warnings, collapse = "; ")
      tags$div(style = "margin-left:20px;margin-top:6px;",
               tags$code(style = "background:#856404;color:white;
                          padding:2px 6px;border-radius:3px;", name),
               tags$span(style = "margin-left:8px;font-size:12px;", msg),
               tags$br(),
               tags$span(style = "margin-left:8px;color:#856404;font-size:12px;",
                         "\u2192 Try: ", get_sympy_suggestion(name)))
    })
    banners[[length(banners) + 1]] <- tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;padding:12px;
               border-radius:4px;margin-bottom:10px;",
      tags$strong("\u26a0\ufe0f Symboles SymPy r\u00e9serv\u00e9s"),
      tags$div(suggestions_html))
  }

  # Statut global
  if (v$comp_ok)
    banners[[length(banners) + 1]] <- tags$div(
      style = "background:#d4edda;border:1px solid #c3e6cb;padding:8px;
               border-radius:4px;margin-bottom:10px;",
      "\u2705 Tous les compartiments sont v\u00e9rifi\u00e9s.")
  else
    banners[[length(banners) + 1]] <- tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;padding:8px;
               border-radius:4px;margin-bottom:10px;",
      paste0("\u26a0\ufe0f ", v$n_comp_pending,
             " compartiment(s) en attente (statut 'ok' requis)."))

  tagList(banners)
})

output$comp_debug <- renderPrint({
  cat("Compartiments:\n"); print(rv_comp())
})


# =============================================================================
# SECTION 3 -- Onglet Flux
# =============================================================================

# ── Rendu table ───────────────────────────────────────────────────────────────
output$flux_table <- renderDT({
  datatable(
    create_flux_html(rv_flux(), rv_comp()),
    escape = FALSE, rownames = FALSE, selection = "single",
    editable = list(target = "cell", columns = c(6, 7)),
    options  = list(dom = 't', paging = FALSE,
                    ordering = FALSE, scrollX = TRUE))
}, server = FALSE)

observeEvent(input$flux_table_cell_edit, {
  info <- input$flux_table_cell_edit
  fd   <- rv_flux()
  if (info$col == 6) {
    fd$rate_name[info$row]       <- info$value
    fd$verification[info$row]    <- "pending"
  }
  if (info$col == 7) {
    fd$rate_expression[info$row] <- info$value
    fd$verification[info$row]    <- "pending"
  }
  rv_flux(fd)
})

# ── Observers dropdowns flux ──────────────────────────────────────────────────
observe({
  fd <- rv_flux()
  for (i in seq_len(nrow(fd))) local({
    ii <- i

    observeEvent(input[[paste0("flux_from_", ii)]], {
      fd2 <- rv_flux(); if (ii > nrow(fd2)) return()
      val <- input[[paste0("flux_from_", ii)]]
      if (!is.null(val) && !is.na(val)) {
        fd2$from_state_id[ii] <- val
        rv_flux(reset_rate_and_ver(fd2, ii))
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observeEvent(input[[paste0("flux_to_", ii)]], {
      fd2 <- rv_flux(); if (ii > nrow(fd2)) return()
      val <- input[[paste0("flux_to_", ii)]]
      if (!is.null(val) && !is.na(val)) {
        fd2$to_state_id[ii] <- val
        rv_flux(reset_rate_and_ver(fd2, ii))

        # Suggestion outflow
        if (val == "") {
          from_id <- fd2$from_state_id[ii]
          if (!is.na(from_id) && from_id != "") {
            from_comp <- rv_comp()[rv_comp()$state_id == from_id, ]
            if (nrow(from_comp) > 0) {
              from_type <- from_comp$state_type[1]
              if (from_type %in% INFECTED_STATE_TYPES) {
                rv_flux_suggestions(list(
                  flux_row = ii, flux_id = fd2$flux_id[ii],
                  from_id  = from_id,
                  message  = sprintf(
                    "\U0001f4a1 Flux %s (%s \u2192 \u2205) : outflow infectieux. Sugg\u00e9r\u00e9 : is_V = TRUE",
                    fd2$flux_id[ii], from_id),
                  suggested_class = "is_V",
                  reason = "Sortie d'un compartiment infectieux \u2192 matrice V"))
              } else {
                rv_flux_suggestions(list(
                  flux_row = ii, flux_id = fd2$flux_id[ii],
                  from_id  = from_id,
                  message  = sprintf(
                    "\U0001f4a1 Flux %s (%s \u2192 \u2205) : outflow non-infectieux. Sugg\u00e9r\u00e9 : is_empty = TRUE",
                    fd2$flux_id[ii], from_id),
                  suggested_class = "is_empty",
                  reason = "Sortie non-infectieuse \u2192 hors NGM"))
              }
            }
          }
        } else {
          rv_flux_suggestions(list())
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observeEvent(input[[paste0("flux_process_", ii)]], {
      fd2 <- rv_flux(); if (ii > nrow(fd2)) return()
      val <- input[[paste0("flux_process_", ii)]]
      if (!is.null(val) && !is.na(val)) {
        fd2$process_type[ii] <- val
        rv_flux(reset_rate_and_ver(fd2, ii))
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observeEvent(input[[paste0("flux_trans_", ii)]], {
      fd2 <- rv_flux(); if (ii > nrow(fd2)) return()
      val <- input[[paste0("flux_trans_", ii)]]
      if (!is.null(val) && !is.na(val)) {
        fd2$transmission_type[ii] <- val
        rv_flux(reset_rate_and_ver(fd2, ii))
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observeEvent(input[[paste0("flux_dens_", ii)]], {
      fd2 <- rv_flux(); if (ii > nrow(fd2)) return()
      val <- input[[paste0("flux_dens_", ii)]]
      if (!is.null(val) && !is.na(val)) {
        fd2$density_type[ii] <- val
        rv_flux(reset_rate_and_ver(fd2, ii))
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    observeEvent(input[[paste0("flux_ver_", ii)]], {
      fd2 <- rv_flux(); if (ii > nrow(fd2)) return()
      val <- input[[paste0("flux_ver_", ii)]]
      if (!is.null(val) && !is.na(val)) {
        fd2$verification[ii] <- val
        rv_flux(fd2)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ── Modal ⚙️ Rate ────────────────────────────────────────────────────────
    observeEvent(input[[paste0("rate_btn_", ii)]], {
      fd_current <- isolate(rv_flux())
      if (ii > nrow(fd_current)) {
        showNotification("Ligne flux introuvable.", type = "warning")
        return()
      }
      cf <- fd_current[ii, ]

      templates <- filter_templates(
        cf$process_type,
        if (is.na(cf$transmission_type)) NA else cf$transmission_type,
        if (is.na(cf$density_type))      NA else cf$density_type)

      tpl_choices <- if (nrow(templates) > 0)
        c("-- None --" = "none", "-- Manuel --" = "",
          setNames(templates$template_id,
                   paste0(templates$label, " \u2014 ", templates$parameters)))
      else
        c("-- None --" = "none", "-- Manuel --" = "")

      cur_tpl <- if (!is.na(cf$rate_template_id) && cf$rate_template_id != "")
        cf$rate_template_id else ""

      showModal(modalDialog(
        title = paste0("\u2699\ufe0f Rate \u2014 flux ", cf$flux_id),
        size  = "l",
        h4("Flux"),
        tags$p(
          strong("De : "),
          if (is.na(cf$from_state_id) || cf$from_state_id == "")
            "(vide)" else cf$from_state_id,
          strong("  \u2192  \u00c0 : "),
          if (is.na(cf$to_state_id) || cf$to_state_id == "")
            "(vide/outflow)" else cf$to_state_id),
        tags$p(
          strong("Process : "),
          if (is.na(cf$process_type)) "" else cf$process_type),
        hr(), h4("Template"),
        if (nrow(templates) == 0)
          tags$p("Aucun template pour ce type.", style = "color:orange;"),
        selectInput("modal_template", "Choisir :",
                    choices = tpl_choices, selected = cur_tpl),
        conditionalPanel(
          "input.modal_template !== '' && input.modal_template !== 'none'",
          uiOutput("modal_template_preview")),
        hr(), h4("Expression rate"),
        textInput("modal_rate_name", "Nom du param\u00e8tre :",
                  value = if (!is.na(cf$rate_name)) cf$rate_name else "",
                  placeholder = "ex : beta_c, gamma_r"),
        textAreaInput("modal_rate_expression",
                      "Expression math\u00e9matique :",
                      value = if (!is.na(cf$rate_expression))
                        cf$rate_expression else "",
                      placeholder = "ex : beta_c * S_h * I_h / Ntot",
                      rows = 3),
        uiOutput("modal_expr_empty_warning"),
        uiOutput("modal_expression_validation"),
        uiOutput("modal_params_section"),
        hr(), h4("M\u00e9tadonn\u00e9es (optionnelles)"),
        textInput("modal_rate_units", "Unit\u00e9s :",
                  value = if (!is.na(cf$rate_units)) cf$rate_units else "",
                  placeholder = "ex : 1/jour"),
        textAreaInput("modal_rate_comment", "Commentaire :",
                      value = if (!is.na(cf$rate_comment))
                        cf$rate_comment else "", rows = 2),
        tags$div(
          style = "background:#fff3cd;padding:10px;border-radius:5px;margin-top:10px;",
          checkboxInput("modal_rate_validated",
                        strong("\u2713 Valid\u00e9 (requis pour sauvegarder)"),
                        value = isTRUE(cf$rate_validated)),
          tags$small("Cochez apr\u00e8s v\u00e9rification de l'expression.")),
        footer = tagList(
          actionButton("modal_save", "\U0001f4be Sauvegarder",
                       class = "btn-primary"),
          modalButton("Annuler"))
      ))
      session$userData$current_flux_idx <- ii
    }, ignoreInit = TRUE)
  })
})

# ── Add / Delete / Validate all flux ─────────────────────────────────────────
observeEvent(input$add_flux, {
  fd <- rv_flux(); i <- 1L
  repeat { nid <- paste0("F", i); if (!nid %in% fd$flux_id) break; i <- i + 1L }
  rv_flux(rbind(fd, data.frame(
    flux_id = nid, from_state_id = "", to_state_id = "",
    process_type = "", transmission_type = "", density_type = "",
    rate_name = "", rate_expression = "", description = "",
    is_demographic = FALSE, is_progression = FALSE, is_recovery = FALSE,
    verification = "pending", rate_template_id = "",
    rate_template_label = "", rate_expression_auto = "",
    rate_source = "", rate_validated = FALSE,
    rate_units = "", rate_comment = "", rate_param_desc = "",
    stringsAsFactors = FALSE)))
})

observeEvent(input$del_flux, {
  sel <- input$flux_table_rows_selected
  if (length(sel) == 1) rv_flux(rv_flux()[-sel, , drop = FALSE])
})

observeEvent(input$validate_all_flux, {
  df <- rv_flux()
  if (nrow(df) > 0) {
    df$verification <- "ok"
    rv_flux(df)
    showNotification(paste0("\u2705 ", nrow(df), " flux valid\u00e9(s)."),
                     type = "message", duration = 3)
  }
})

# ── Bannière validation flux ──────────────────────────────────────────────────
output$flux_validation_banner <- renderUI({
  v  <- all_verified()
  fd <- rv_flux()
  cd <- rv_comp()
  pd <- rv_params()
  banners <- list()

  # Statut vérification
  if (v$flux_ok)
    banners[[1]] <- tags$div(
      style = "background:#d4edda;border:1px solid #c3e6cb;padding:8px;
               border-radius:4px;margin-bottom:6px;",
      "\u2705 Tous les flux sont v\u00e9rifi\u00e9s.")
  else
    banners[[1]] <- tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;padding:8px;
               border-radius:4px;margin-bottom:6px;",
      paste0("\u26a0\ufe0f ", v$n_flux_pending,
             " flux en attente (statut 'ok' requis)."))

  # Flux sans expression
  if (length(v$flux_no_expr) > 0)
    banners[[length(banners) + 1]] <- tags$div(
      style = "background:#f8d7da;border:1px solid #f5c6cb;padding:8px;
               border-radius:4px;margin-bottom:6px;",
      tags$strong("\u274c Expression manquante : "),
      paste0(length(v$flux_no_expr), " flux sans rate_expression : "),
      tags$code(paste(v$flux_no_expr, collapse = ", ")))

  # Auto-flux
  if (length(v$flux_self) > 0)
    banners[[length(banners) + 1]] <- tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;padding:8px;
               border-radius:4px;margin-bottom:6px;",
      tags$strong("\u26a0\ufe0f Auto-flux : "),
      tags$code(paste(v$flux_self, collapse = ", ")))

  # Suggestion outflow
  suggestion <- rv_flux_suggestions()
  if (length(suggestion) > 0 && !is.null(suggestion$message))
    banners[[length(banners) + 1]] <- tags$div(
      style = "background:#e7f3ff;border-left:4px solid #2196F3;
               padding:12px;border-radius:4px;margin-bottom:6px;",
      tags$p(style = "margin:0 0 6px 0;font-weight:600;",
             suggestion$message),
      tags$p(style = "margin:0;font-size:12px;color:#666;",
             suggestion$reason),
      tags$div(style = "margin-top:10px;",
               actionButton("btn_apply_flux_suggestion", "\u2713 Appliquer",
                            class = "btn-sm btn-primary",
                            style = "margin-right:8px;"),
               actionButton("btn_dismiss_flux_suggestion", "\u2717 Ignorer",
                            class = "btn-sm btn-outline-secondary")))

  do.call(tagList, banners)
})

output$flux_debug <- renderPrint({ cat("Flux:\n"); print(rv_flux()) })


# =============================================================================
# SECTION 4 -- Modal Rate : observers internes
# =============================================================================

output$modal_template_preview <- renderUI({
  req(input$modal_template,
      input$modal_template != "",
      input$modal_template != "none")
  idx <- session$userData$current_flux_idx
  fd  <- rv_flux()
  if (is.null(idx) || idx > nrow(fd)) return(NULL)
  cf      <- fd[idx, ]
  inf_ids <- rv_comp()$state_id[
    rv_comp()$state_type %in% c("infectious", "carrier")
  ]
  expr  <- generate_rate_expression(
    input$modal_template, cf$from_state_id, cf$to_state_id, inf_ids)
  tinfo <- RATE_TEMPLATES[RATE_TEMPLATES$template_id == input$modal_template, ]
  tags$div(
    style = "background:#f0f8ff;padding:10px;border-radius:5px;margin-top:8px;",
    tags$p(strong("Template : "), tinfo$label[1]),
    tags$p(strong("Forme : "), tags$code(tinfo$expression_template[1])),
    tags$p(strong("G\u00e9n\u00e9r\u00e9e : "), tags$code(expr)),
    tags$p(strong("Param\u00e8tres : "), tinfo$parameters[1]),
    actionButton("modal_apply_template", "\u2713 Appliquer",
                 class = "btn-sm btn-success"))
})

observeEvent(input$modal_apply_template, {
  req(input$modal_template,
      input$modal_template != "",
      input$modal_template != "none")
  idx <- session$userData$current_flux_idx
  fd  <- rv_flux()
  if (is.null(idx) || idx > nrow(fd)) return()
  cf      <- fd[idx, ]
  inf_ids <- rv_comp()$state_id[
    rv_comp()$state_type %in% c("infectious", "carrier")
  ]
  expr  <- generate_rate_expression(
    input$modal_template, cf$from_state_id, cf$to_state_id, inf_ids)
  tinfo <- RATE_TEMPLATES[RATE_TEMPLATES$template_id == input$modal_template, ]
  updateTextAreaInput(session, "modal_rate_expression", value = expr)
  if (is.null(input$modal_rate_name) || trimws(input$modal_rate_name) == "") {
    suggested <- trimws(strsplit(tinfo$parameters[1], ",")[[1]][1])
    updateTextInput(session, "modal_rate_name", value = suggested)
  }
  updateCheckboxInput(session, "modal_rate_validated", value = FALSE)
  showNotification("Template appliqu\u00e9.", type = "message")
})

# Reset validated si expression change
observeEvent(input$modal_rate_name,
  updateCheckboxInput(session, "modal_rate_validated", value = FALSE),
  ignoreInit = TRUE)
observeEvent(input$modal_rate_expression,
  updateCheckboxInput(session, "modal_rate_validated", value = FALSE),
  ignoreInit = TRUE)

observeEvent(input$modal_template, {
  updateCheckboxInput(session, "modal_rate_validated", value = FALSE)
  if (isTRUE(input$modal_template == "none")) {
    updateTextInput(session, "modal_rate_name", value = "")
    updateTextAreaInput(session, "modal_rate_expression", value = "")
    updateTextInput(session, "modal_rate_units", value = "")
    updateTextAreaInput(session, "modal_rate_comment", value = "")
  }
}, ignoreInit = TRUE)

output$modal_expr_empty_warning <- renderUI({
  expr <- trimws(input$modal_rate_expression %||% "")
  if (expr == "")
    tags$div(
      style = "background:#f8d7da;border:1px solid #f5c6cb;padding:8px 12px;
               border-radius:4px;margin-top:4px;font-size:13px;",
      tags$strong("\u26a0\ufe0f Expression obligatoire"))
  else NULL
})

output$modal_expression_validation <- renderUI({
  expr <- input$modal_rate_expression
  if (is.null(expr) || trimws(expr) == "") return(NULL)
  format_validation_ui(
    validate_sympy_expression(expr, rv_comp()$state_id))
})

output$modal_params_section <- renderUI({
  expr <- input$modal_rate_expression
  if (is.null(expr) || trimws(expr) == "") return(NULL)
  known  <- rv_comp()$state_id
  params <- extract_free_params(expr, known)
  if (length(params) == 0) return(NULL)
  idx <- session$userData$current_flux_idx
  fd  <- rv_flux()
  saved_desc <- if (!is.null(idx) && idx <= nrow(fd))
    decode_param_desc(fd$rate_param_desc[idx]) else list()
  tagList(
    hr(), h4("Param\u00e8tres libres d\u00e9tect\u00e9s"),
    tags$div(
      style = "background:#f8f9fa;border:1px solid #dee2e6;
               border-radius:4px;padding:12px;",
      lapply(params, function(p) {
        saved_val <- if (!is.null(saved_desc[[p]])) saved_desc[[p]] else ""
        tags$div(
          style = "display:flex;align-items:center;margin-bottom:8px;gap:10px;",
          tags$div(style = "min-width:100px;",
                   tags$code(style = "font-size:13px;background:#e9ecef;
                              padding:3px 7px;border-radius:3px;", p)),
          tags$div(style = "flex:1;",
                   textInput(paste0("param_desc_", p), NULL,
                             value = saved_val,
                             placeholder = paste0("Description de ", p))))
      })))
})

# ── Sauvegarde du modal ───────────────────────────────────────────────────────
observeEvent(input$modal_save, {
  fd  <- rv_flux()
  idx <- session$userData$current_flux_idx
  if (is.null(idx) || idx > nrow(fd)) return()

  expr <- trimws(input$modal_rate_expression %||% "")

  if (expr == "") {
    showNotification(
      tags$div(tags$strong("\u274c Expression manquante"),
               tags$br(),
               tags$span("Une expression est obligatoire.")),
      type = "error", duration = 8)
    return()
  }
  if (!isTRUE(input$modal_rate_validated)) {
    showNotification(
      tags$div(tags$strong("\u274c Validation requise"),
               tags$br(),
               tags$span("Cochez '\u2713 Valid\u00e9'.")),
      type = "error", duration = 5)
    return()
  }
  vr <- validate_sympy_expression(expr, rv_comp()$state_id)
  if (!vr$valid) {
    showNotification(
      tags$div(tags$strong("\u274c Expression invalide"),
               tags$br(),
               tags$span(paste(vr$errors, collapse = " | "))),
      type = "error", duration = 8)
    return()
  }

  fd$rate_name[idx]       <- input$modal_rate_name
  fd$rate_expression[idx] <- expr
  fd$rate_units[idx]      <- input$modal_rate_units
  fd$rate_comment[idx]    <- input$modal_rate_comment
  fd$rate_validated[idx]  <- TRUE

  # Descriptions paramètres libres
  known  <- rv_comp()$state_id
  params <- extract_free_params(expr, known)
  if (length(params) > 0) {
    desc_list <- setNames(
      lapply(params, function(p) {
        v <- input[[paste0("param_desc_", p)]]
        if (is.null(v)) "" else trimws(v)
      }), params)
    fd$rate_param_desc[idx] <- encode_param_desc(desc_list)
  } else {
    fd$rate_param_desc[idx] <- ""
  }

  tpl <- input$modal_template
  if (is.null(tpl) || tpl == "" || tpl == "none") {
    fd$rate_template_id[idx]    <- ""
    fd$rate_template_label[idx] <- ""
    fd$rate_source[idx]         <- "manual"
  } else {
    tinfo <- RATE_TEMPLATES[RATE_TEMPLATES$template_id == tpl, ]
    fd$rate_template_id[idx]    <- tpl
    fd$rate_template_label[idx] <- tinfo$label[1]
    fd$rate_source[idx]         <- "template"
  }

  rv_flux(fd)
  removeModal()
  showNotification("\u2705 Rate sauvegard\u00e9.", type = "message", duration = 3)
})


# =============================================================================
# SECTION 5 -- Suggestions outflow
# =============================================================================

observeEvent(input$btn_apply_flux_suggestion, {
  suggestion <- rv_flux_suggestions()
  if (length(suggestion) == 0 || is.null(suggestion$flux_row)) return()
  fd <- rv_flux()
  if (suggestion$flux_row > nrow(fd)) return()
  fd$verification[suggestion$flux_row] <- "ok"
  rv_flux(fd)
  rv_flux_suggestions(list())
  showNotification(
    paste0("\u2713 Suggestion appliqu\u00e9e pour ", suggestion$flux_id),
    type = "message", duration = 3)
})

observeEvent(input$btn_dismiss_flux_suggestion, {
  rv_flux_suggestions(list())
  showNotification("Suggestion ignor\u00e9e.", type = "message", duration = 2)
})
