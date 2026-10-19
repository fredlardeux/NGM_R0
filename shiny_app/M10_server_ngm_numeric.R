# =============================================================================
# M10_server_ngm_numeric.R -- Server : NGM symbolique + Évaluation numérique
# =============================================================================
# Dépendances : M01, M02, M05, M06
# ReactiveVals attendues : rv_comp, rv_flux, rv_dfe, rv_params,
#   rv_symbolic, rv_flux_hash, rv_dfe_validated,
#   rv_infected_order, rv_infected_locked,
#   rv_ngm_result, rv_sym_df_ngm, rv_ngm_computed_hash,
#   rv_numeric_params, rv_R0_numeric,
#   rv_dependencies, rv_dependencies_synced,
#   rv_last_autosave, rv_autosave_enabled,
#   rv_load_status, species_list, ngm_py
# =============================================================================


# =============================================================================
# SECTION 1 -- Réactives globales partagées
# =============================================================================

#' Statut de vérification global (comp + flux)
all_verified <- reactive({
  cd <- rv_comp()
  fd <- rv_flux()

  comp_ok        <- nrow(cd) > 0 && all(cd$verification == "ok")
  flux_ok        <- nrow(fd) == 0 || all(fd$verification == "ok")
  n_comp_pending <- sum(cd$verification != "ok")
  n_flux_pending <- if (nrow(fd) > 0) sum(fd$verification != "ok") else 0L

  flux_no_expr <- if (nrow(fd) > 0)
    fd$flux_id[is.na(fd$rate_expression) |
               trimws(fd$rate_expression) == ""]
  else character(0)

  flux_self <- if (nrow(fd) > 0)
    fd$flux_id[!is.na(fd$from_state_id) & !is.na(fd$to_state_id) &
               fd$from_state_id == fd$to_state_id &
               fd$from_state_id != ""]
  else character(0)

  list(comp_ok = comp_ok, flux_ok = flux_ok,
       all_ok  = comp_ok && flux_ok,
       n_comp_pending = n_comp_pending,
       n_flux_pending = n_flux_pending,
       flux_no_expr   = flux_no_expr,
       flux_self      = flux_self)
})

#' Hash global du modèle -- détecte tout changement influençant R₀
current_model_hash <- reactive({
  fd  <- rv_flux()
  dfe <- rv_dfe()
  sym <- rv_symbolic()
  ord <- rv_infected_order()

  flux_str <- if (nrow(fd) > 0)
    paste(fd$flux_id, fd$from_state_id, fd$to_state_id,
          fd$rate_expression, sep = "|", collapse = "||")
  else ""

  dfe_str <- if (nrow(dfe) > 0)
    paste(dfe$state_id, dfe$dfe_expr, sep = "=", collapse = ",")
  else ""

  sym_str <- if (!is.null(sym) && nrow(sym) > 0)
    paste(sym$flux_id, sym$is_F, sym$is_V, sym$dfe_zero,
          sep = "|", collapse = "||")
  else ""

  paste(flux_str, dfe_str, sym_str,
        paste(ord, collapse = ","), sep = "###")
})

# Effacer résultats si le modèle change après un calcul
observe({
  h    <- current_model_hash()
  last <- isolate(rv_ngm_computed_hash())
  if (!is.null(last) && !identical(h, last)) {
    rv_ngm_result(NULL)
    rv_sym_df_ngm(NULL)
  }
})

# Reactive : table symbolique désynchronisée ?
symbolic_stale <- reactive({
  current <- flux_hash(rv_flux())
  stored  <- rv_flux_hash()
  is.null(stored) || current != stored
})


# =============================================================================
# SECTION 2 -- Gestion de la table symbolique
# =============================================================================

#' Recalculer rv_symbolic en préservant les overrides manuels is_F / is_V
do_recalc_symbolic <- function() {
  # BUG-07 corrigé : capturer fd UNE SEULE FOIS au début
  fd  <- isolate(rv_flux())
  sym <- build_symbolic_table(rv_comp(), fd)
  old <- isolate(rv_symbolic())

  if (!is.null(old) && nrow(old) > 0 && nrow(sym) > 0) {
    for (i in seq_len(nrow(sym))) {
      j <- which(old$flux_id == sym$flux_id[i])
      if (length(j) > 0) {
        j <- j[1]
        sym$is_entry[i]     <- old$is_entry[j]
        sym$is_F[i]         <- old$is_F[j]
        sym$is_V[i]         <- old$is_V[j]
        sym$dfe_zero[i]     <- old$dfe_zero[j]
        sym$dfe_positive[i] <- old$dfe_positive[j]
      }
    }
  }
  rv_symbolic(sym)
  rv_flux_hash(flux_hash(fd))
}

#' Reconstruire rv_symbolic depuis zéro (sans préserver les overrides)
do_reset_symbolic <- function() {
  fd  <- isolate(rv_flux())
  sym <- build_symbolic_table(rv_comp(), fd)
  rv_symbolic(sym)
  rv_flux_hash(flux_hash(fd))
  rv_ngm_result(NULL)
  rv_ngm_computed_hash(NULL)
  rv_sym_df_ngm(NULL)
  rv_infected_locked(FALSE)
}

# Init dès que rv_flux() est non vide et rv_symbolic() est NULL
observe({
  fd <- rv_flux()
  if ((is.null(rv_symbolic()) || nrow(rv_symbolic()) == 0) && nrow(fd) > 0)
    do_reset_symbolic()
})

# Sync rate_name / rate_expression depuis rv_flux (sans toucher is_F/is_V)
observe({
  fd  <- rv_flux()
  sym <- isolate(rv_symbolic())
  if (is.null(sym) || nrow(sym) == 0 || nrow(fd) == 0) return()

  # Rebuild si nombre ou IDs ont changé
  if (nrow(sym) != nrow(fd) ||
      !identical(sort(sym$flux_id), sort(fd$flux_id))) {
    do_reset_symbolic(); return()
  }

  changed       <- FALSE
  needs_rebuild <- FALSE

  for (i in seq_len(nrow(sym))) {
    j <- which(fd$flux_id == sym$flux_id[i])
    if (length(j) == 0) next
    j <- j[1]

    new_from <- if (is.na(fd$from_state_id[j])) "" else fd$from_state_id[j]
    new_to   <- if (is.na(fd$to_state_id[j]))   "" else fd$to_state_id[j]
    new_pt   <- if (is.na(fd$process_type[j]))   "" else fd$process_type[j]
    new_rn   <- if (is.na(fd$rate_name[j]))      "" else fd$rate_name[j]
    new_re   <- if (is.na(fd$rate_expression[j]))"" else fd$rate_expression[j]

    if (!identical(sym$from_state_id[i], new_from) ||
        !identical(sym$to_state_id[i],   new_to)   ||
        !identical(sym$process_type[i],  new_pt)) {
      needs_rebuild <- TRUE; break
    }
    if (!identical(sym$rate_name[i],       new_rn) ||
        !identical(sym$rate_expression[i], new_re)) {
      sym$rate_name[i]       <- new_rn
      sym$rate_expression[i] <- new_re
      changed <- TRUE
    }
  }

  if (needs_rebuild) { do_reset_symbolic(); return() }
  if (changed) {
    rv_symbolic(sym)
    rv_flux_hash(flux_hash(fd))
  }
})

# HTML de la table symbolique
create_symbolic_html <- function(sym_df) {
  if (nrow(sym_df) == 0)
    return(data.frame(
      flux_id = character(0), from_state_id = character(0),
      to_state_id = character(0), process_type = character(0),
      rate_name = character(0), rate_expression = character(0),
      is_entry = character(0), ngm_role = character(0),
      dfe_status = character(0), stringsAsFactors = FALSE))

  mk_ngm_role <- function(is_F, is_V, idx) {
    cur <- if (isTRUE(is_F)) "F" else if (isTRUE(is_V)) "V" else "none"
    sprintf(
      '<select class="compact-select" id="sym_ngm_role_%d"
           onchange="Shiny.setInputValue(\'sym_ngm_role_%d\',this.value)"
           style="min-width:140px;">
           <option value="F"%s>F - Infection</option>
           <option value="V"%s>V - Exit</option>
           <option value="none"%s>Not used</option></select>',
      idx, idx,
      if (cur == "F")    " selected" else "",
      if (cur == "V")    " selected" else "",
      if (cur == "none") " selected" else "")
  }

  mk_dfe_status <- function(dfe_zero, dfe_positive, idx) {
    cur <- if (isTRUE(dfe_zero)) "zero" else "positive"
    sprintf(
      '<select class="compact-select" id="sym_dfe_status_%d"
           onchange="Shiny.setInputValue(\'sym_dfe_status_%d\',this.value)"
           style="min-width:110px;">
           <option value="zero"%s>= 0</option>
           <option value="positive"%s>&gt; 0</option></select>',
      idx, idx,
      if (cur == "zero")     " selected" else "",
      if (cur == "positive") " selected" else "")
  }

  mk_bool <- function(col, val, idx)
    sprintf(
      '<select class="compact-select" id="sym_%s_%d"
           onchange="Shiny.setInputValue(\'sym_%s_%d\',this.value)">
           <option value="FALSE"%s>FALSE</option>
           <option value="TRUE"%s>TRUE</option></select>',
      col, idx, col, idx,
      if (!isTRUE(val)) " selected" else "",
      if ( isTRUE(val)) " selected" else "")

  do.call(rbind, lapply(seq_len(nrow(sym_df)), function(i) {
    s <- sym_df[i, ]
    data.frame(
      flux_id        = s$flux_id,
      from_state_id  = s$from_state_id,
      to_state_id    = s$to_state_id,
      process_type   = s$process_type,
      rate_name      = if (!is.na(s$rate_name)) s$rate_name else "",
      rate_expression= if (!is.na(s$rate_expression)) s$rate_expression else "",
      is_entry       = mk_bool("entry", s$is_entry, i),
      ngm_role       = mk_ngm_role(s$is_F, s$is_V, i),
      dfe_status     = mk_dfe_status(s$dfe_zero, s$dfe_positive, i),
      stringsAsFactors = FALSE)
  }))
}

output$symbolic_table <- renderDT({
  sym_df <- rv_symbolic(); req(sym_df)
  datatable(
    create_symbolic_html(sym_df),
    escape = FALSE, rownames = FALSE, selection = "single",
    editable = list(target = "cell", columns = c(4, 5)),
    options  = list(dom = 't', paging = FALSE,
                    ordering = FALSE, scrollX = TRUE))
}, server = FALSE)

observeEvent(input$symbolic_table_cell_edit, {
  info <- input$symbolic_table_cell_edit
  sd   <- rv_symbolic()
  if (info$col == 4) sd$rate_name[info$row]       <- info$value
  if (info$col == 5) sd$rate_expression[info$row] <- info$value
  rv_symbolic(sd)
  rv_ngm_result(NULL); rv_ngm_computed_hash(NULL)
})

observe({
  sd <- rv_symbolic()
  if (is.null(sd) || nrow(sd) == 0) return()
  for (i in seq_len(nrow(sd))) local({
    ii <- i

    observeEvent(input[[paste0("sym_ngm_role_", ii)]], {
      sd2  <- rv_symbolic(); if (ii > nrow(sd2)) return()
      role <- input[[paste0("sym_ngm_role_", ii)]]
      sd2$is_F[ii] <- role == "F"
      sd2$is_V[ii] <- role == "V"
      rv_symbolic(sd2)
      rv_ngm_result(NULL); rv_ngm_computed_hash(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input[[paste0("sym_dfe_status_", ii)]], {
      sd2    <- rv_symbolic(); if (ii > nrow(sd2)) return()
      status <- input[[paste0("sym_dfe_status_", ii)]]
      sd2$dfe_zero[ii]     <- status == "zero"
      sd2$dfe_positive[ii] <- status == "positive"
      rv_symbolic(sd2)
      rv_ngm_result(NULL); rv_ngm_computed_hash(NULL)
    }, ignoreInit = TRUE)

    observeEvent(input[[paste0("sym_entry_", ii)]], {
      sd2 <- rv_symbolic(); if (ii > nrow(sd2)) return()
      sd2$is_entry[ii] <- as.logical(input[[paste0("sym_entry_", ii)]])
      rv_symbolic(sd2)
      rv_ngm_result(NULL); rv_ngm_computed_hash(NULL)
    }, ignoreInit = TRUE)
  })
})

observeEvent(input$recalc_symbolic,        { do_reset_symbolic()
  showNotification("\u2705 Table symbolique r\u00e9initialis\u00e9e.",
                   type = "message", duration = 4) })
observeEvent(input$recalc_symbolic_inline, { do_reset_symbolic()
  showNotification("\u2705 Table symbolique reconstruite.",
                   type = "message", duration = 3) })

output$symbolic_status <- renderText({
  s <- rv_symbolic()
  if (is.null(s) || nrow(s) == 0) return("Aucun flux.")
  paste0(nrow(s), " flux | ", sum(s$is_entry), " entr\u00e9es | ",
         sum(s$is_F), " termes F | ", sum(s$is_V), " termes V")
})

output$symbolic_stale_banner <- renderUI({
  if (!symbolic_stale()) return(NULL)
  tags$div(
    style = "background:#fff3cd;border:1px solid #ffc107;border-radius:5px;
             padding:10px 14px;margin-bottom:10px;",
    tags$div(style = "display:flex;align-items:center;gap:10px;",
             tags$span(style = "font-size:18px;", "\u26a0\ufe0f"),
             tags$div(
               tags$strong("Table symbolique non synchronis\u00e9e"),
               tags$br(),
               tags$span(style = "font-size:13px;color:#555;",
                         "Les flux ont \u00e9t\u00e9 modifi\u00e9s depuis le dernier recalcul.")),
             tags$div(style = "margin-left:auto;",
                      actionButton("recalc_symbolic_inline",
                                   "\U0001f504 Recalculer",
                                   class = "btn-warning btn-sm"))))
})

output$ngm_stale_warning <- renderUI({
  if (!symbolic_stale()) return(NULL)
  tags$div(
    style = "background:#f8d7da;border:1px solid #f5c6cb;border-radius:5px;
             padding:8px 14px;margin-bottom:10px;font-size:13px;",
    tags$strong("\u26a0\ufe0f Attention : "),
    "Table symbolique non \u00e0 jour. Recalculez avant de continuer.")
})

output$symbolic_validation_warnings <- renderUI({
  fd  <- isolate(rv_flux())
  cd  <- isolate(rv_comp())
  sym <- rv_symbolic()
  if (is.null(sym) || nrow(sym) == 0 || nrow(fd) == 0) return(NULL)

  warns <- list()
  outflows <- fd[!is.na(fd$to_state_id) & fd$to_state_id == "", ]
  for (i in seq_len(nrow(outflows))) {
    fid     <- outflows$flux_id[i]
    from_id <- outflows$from_state_id[i]
    if (is.na(from_id) || from_id == "") next
    from_comp <- cd[cd$state_id == from_id, ]
    if (nrow(from_comp) == 0) next
    from_type <- from_comp$state_type[1]
    sym_row   <- sym[sym$flux_id == fid, ]
    if (nrow(sym_row) == 0) next
    if (from_type %in% INFECTED_STATE_TYPES &&
        !isTRUE(sym_row$is_V[1]) && !isTRUE(sym_row$is_empty[1])) {
      warns[[length(warns) + 1]] <- tags$div(
        style = "background:#fff9e6;border-left:3px solid #ffc107;
                 padding:10px;margin-bottom:8px;border-radius:4px;",
        tags$p(style = "margin:0;font-size:13px;",
               tags$strong(sprintf("Flux %s (%s \u2192 \u2205) :", fid, from_id)),
               " Outflow infectieux devrait avoir ",
               tags$code("is_V = TRUE")))
    }
  }
  if (length(warns) > 0) tagList(warns) else NULL
})


# =============================================================================
# SECTION 3 -- Ordre des infectés
# =============================================================================

observe({
  cd      <- rv_comp()
  new_inf <- cd$state_id[cd$state_type %in% INFECTED_STATE_TYPES]
  old_ord <- rv_infected_order()
  kept    <- old_ord[old_ord %in% new_inf]
  added   <- new_inf[!new_inf %in% old_ord]
  rv_infected_order(c(kept, added))
  rv_infected_locked(FALSE)
  rv_ngm_result(NULL); rv_sym_df_ngm(NULL)
})

output$infected_order_ui <- renderUI({
  ord    <- rv_infected_order()
  locked <- rv_infected_locked()
  cd     <- rv_comp()
  if (length(ord) == 0)
    return(tags$p(style = "color:#999;font-style:italic;",
                  "Aucun compartiment infect\u00e9 d\u00e9tect\u00e9."))

  badges <- lapply(seq_along(ord), function(i) {
    sid   <- ord[i]
    stype <- cd$state_type[cd$state_id == sid]
    stype <- if (length(stype) > 0) stype[1] else "?"
    tags$div(
      class    = if (locked) "infected-badge infected-badge-locked"
                 else "infected-badge",
      style    = paste0("background:", dfe_type_badge_color(stype), ";"),
      `data-id`= sid,
      if (!locked) tags$span(class = "drag-handle", "\u28bf"),
      tags$span(style = "font-weight:700;color:#555;font-size:12px;",
                paste0(i, ".")),
      tags$code(style = "font-size:13px;", sid),
      tags$span(style = "font-size:11px;color:#777;",
                paste0("(", stype, ")")))
  })

  tagList(
    tags$div(id = "infected-sortable-list",
             class = "infected-sortable", badges),
    tags$div(style = "display:flex;gap:10px;align-items:center;margin-top:6px;",
             if (!locked)
               tagList(
                 actionButton("btn_lock_infected",
                              "\U0001f512 Confirmer l'ordre",
                              class = "btn-success"),
                 tags$span(style = "color:#666;font-size:12px;",
                           "Glissez pour r\u00e9ordonner, puis confirmez."))
             else
               tagList(
                 tags$span(
                   style = "background:#d4edda;border:1px solid #c3e6cb;
                            padding:5px 12px;border-radius:4px;
                            font-weight:600;color:#155724;",
                   paste0("\U0001f512 Ordre confirm\u00e9 : [",
                          paste(ord, collapse = ", "), "]")),
                 actionButton("btn_unlock_infected", "\u270f\ufe0f Modifier",
                              class = "btn-sm btn-warning"))))
})

observeEvent(input$infected_order_sorted, {
  new_ord <- input$infected_order_sorted
  valid   <- rv_infected_order()
  new_ord <- new_ord[new_ord %in% valid]
  missing <- valid[!valid %in% new_ord]
  rv_infected_order(c(new_ord, missing))
}, ignoreNULL = TRUE)

observeEvent(input$main_tabs, {
  if (input$main_tabs != "Symbolic / NGM") return()
  if (!rv_infected_locked() && length(rv_infected_order()) > 1)
    session$sendCustomMessage("initSortable", list())
}, ignoreInit = TRUE)

observeEvent(input$btn_unlock_infected, {
  rv_infected_locked(FALSE)
  rv_ngm_result(NULL); rv_sym_df_ngm(NULL)
  if (length(rv_infected_order()) > 1)
    session$sendCustomMessage("initSortable", list())
})
observeEvent(input$btn_lock_infected, {
  rv_infected_locked(TRUE)
  rv_ngm_result(NULL); rv_sym_df_ngm(NULL)
})


# =============================================================================
# SECTION 4 -- Calcul NGM (appel Python)
# =============================================================================

output$btn_compute_ngm_ui <- renderUI({
  r      <- rv_ngm_result()
  stale  <- symbolic_stale()
  locked <- rv_infected_locked()
  n_inf  <- length(rv_infected_order())

  # -- Avertissement preventif selon dimension de K ----------------------
  complexity_note <- if (n_inf == 4)
    tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;border-radius:5px;
               padding:8px 12px;margin-top:8px;font-size:12px;",
      "\u26a0\ufe0f Matrice K 4x4 : calcul symbolique peut prendre 15s. ",
      "Fallback numerique automatique si necessaire.")
  else if (n_inf >= 5)
    tags$div(
      style = "background:#f8d7da;border:1px solid #f5c6cb;border-radius:5px;
               padding:8px 12px;margin-top:8px;font-size:12px;",
      "\U0001f4ca Matrice K ", n_inf, "x", n_inf,
      " : pas de solution symbolique (Abel-Ruffini). ",
      "R\u2080 calcule par rayon spectral numerique.")
  else NULL

  tagList(
    if (!is.null(r) && scal_lgl(r$ok))
      actionButton("btn_compute_ngm", "\u2705 Recalculer R\u2080",
                   class = "btn-success btn-lg", style = "width:100%;")
    else if (stale || !locked)
      tags$div(
        actionButton("btn_compute_ngm", "\u2699\ufe0f Calculer R\u2080 (SymPy)",
                     class = "btn-secondary btn-lg",
                     style = "width:100%;opacity:0.7;"),
        tags$small(style = "color:#666;display:block;margin-top:4px;",
                   if (!locked) "\U0001f512 Confirmez l'ordre des infectes d'abord"
                   else         "\u26a0\ufe0f Synchronisez la table symbolique d'abord"))
    else
      actionButton("btn_compute_ngm", "\u2699\ufe0f Calculer R\u2080 (SymPy)",
                   class = "btn-primary btn-lg", style = "width:100%;"),
    complexity_note
  )
})

output$ngm_compute_status <- renderUI({
  r <- rv_ngm_result()
  if (is.null(r))
    return(tags$span(style = "color:#888;font-size:13px;font-style:italic;",
                     "R\u00e9sultat ici apr\u00e8s calcul."))

  if (scal_lgl(r$ok)) {
    method <- r$steps$`7_eigenvalues`$method %||% "symbolic"
    badge  <- switch(method,
      symbolic = tags$span(
        style = "background:#d4edda;border:1px solid #c3e6cb;
                 padding:6px 12px;border-radius:5px;font-weight:600;
                 color:#155724;font-size:13px;",
        "\u2705 R\u2080 symbolique exact"),
      numeric  = tags$span(
        style = "background:#fff3cd;border:1px solid #ffc107;
                 padding:6px 12px;border-radius:5px;font-weight:600;
                 color:#856404;font-size:13px;",
        "\U0001f4ca R\u2080 num\u00e9rique (rayon spectral)"),
      block    = tags$span(
        style = "background:#d1ecf1;border:1px solid #bee5eb;
                 padding:6px 12px;border-radius:5px;font-weight:600;
                 color:#0c5460;font-size:13px;",
        "\U0001f9e9 R\u2080 par blocs (structure detect\u00e9e)"),
      tags$span(
        style = "background:#d4edda;border:1px solid #c3e6cb;
                 padding:6px 12px;border-radius:5px;font-weight:600;
                 color:#155724;font-size:13px;",
        "\u2705 Calcul r\u00e9ussi")
    )
    badge
  } else {
    label <- switch(r$error_type %||% "other",
                    parse      = "\u274c Erreur de parsing",
                    singular_V = "\u26a0\ufe0f Matrice V singuli\u00e8re",
                    eigenval   = "\u274c Erreur valeurs propres",
                    "\u274c Erreur")
    tags$span(style = "background:#f8d7da;border:1px solid #f5c6cb;
               padding:8px 14px;border-radius:5px;font-weight:600;color:#721c24;",
              label)
  }
})

observeEvent(input$btn_compute_ngm, {
  if (!rv_infected_locked()) {
    showNotification(
      "\U0001f512 Confirmez l'ordre des compartiments infect\u00e9s.",
      type = "warning", duration = 5)
    return()
  }

  ctx <- build_sympy_context(rv_comp(), rv_flux(), rv_dfe(), rv_params())
  ctx$infected_states <- rv_infected_order()

  # Toujours reconstruire sym_df depuis les flux COURANTS
  # Ne pas utiliser rv_symbolic() qui peut contenir les flux d'un ancien modele
  sym_df <- build_symbolic_table(rv_comp(), rv_flux())
  rv_sym_df_ngm(sym_df)

  infected_ord <- rv_infected_order()
  fi <- list(); vi <- list()

  # vapply + isTRUE pour eviter NA & condition = NA en R
  is_F_vec   <- vapply(sym_df$is_F,   isTRUE, logical(1))
  is_V_vec   <- vapply(sym_df$is_V,   isTRUE, logical(1))
  is_ent_vec <- if ("is_entry" %in% names(sym_df))
    vapply(sym_df$is_entry, isTRUE, logical(1))
  else
    rep(FALSE, nrow(sym_df))

  for (sid in infected_ord) {
    f_in <- sym_df[is_F_vec &
                   !is.na(sym_df$to_state_id) &
                   sym_df$to_state_id == sid, ]
    fi[[sid]] <- if (nrow(f_in) > 0)
      paste(f_in$rate_expression, collapse = " + ") else "0"

    v_out    <- sym_df[is_V_vec & !is_F_vec &
                       !is.na(sym_df$from_state_id) &
                       sym_df$from_state_id == sid, ]
    v_in     <- sym_df[is_V_vec & !is_F_vec &
                       !is.na(sym_df$to_state_id) &
                       sym_df$to_state_id   == sid, ]
    entry_in <- sym_df[is_ent_vec & !is_F_vec &
                       !is.na(sym_df$to_state_id) &
                       sym_df$to_state_id   == sid, ]

    parts_pos <- if (nrow(v_out) > 0) v_out$rate_expression else character(0)
    parts_neg <- c(
      if (nrow(v_in)     > 0) v_in$rate_expression     else character(0),
      if (nrow(entry_in) > 0) entry_in$rate_expression else character(0))

    pos_str <- if (length(parts_pos) > 0)
      paste(parts_pos, collapse = " + ") else "0"
    neg_str <- if (length(parts_neg) > 0)
      paste(paste0("(", parts_neg, ")"), collapse = " - ") else ""

    vi[[sid]] <- if (neg_str == "") pos_str
                 else paste0(pos_str, " - ", neg_str)
  }
  ctx$fi_exprs <- fi
  ctx$vi_exprs <- vi

  # === LOG DIAGNOSTIC : ce qui est envoye a compute_ngm ===
  message("[M10 ctx] infected_states: ",
          paste(ctx$infected_states, collapse=", "))
  message("[M10 ctx] all_states: ",
          paste(ctx$all_states, collapse=", "))
  message("[M10 ctx] dfe_subs: ",
          paste(names(ctx$dfe_subs), unlist(ctx$dfe_subs), sep="=", collapse=", "))
  for (sid in ctx$infected_states) {
    message("[M10 ctx] F_", sid, " = ", ctx$fi_exprs[[sid]] %||% "ABSENT")
    message("[M10 ctx] V_", sid, " = ", ctx$vi_exprs[[sid]] %||% "ABSENT")
  }
  message("[M10 ctx] param_vals: ",
          paste(names(ctx$param_vals), unlist(ctx$param_vals), sep="=", collapse=", "))
  # ========================================================

  ctx_json <- jsonlite::toJSON(ctx, auto_unbox = TRUE,
                               null = "null", na = "null", force = TRUE)

  url <- NGM_BASE_URL
  if (is.null(url)) {
    showNotification("Serveur FastAPI non disponible.", type = "error")
    return()
  }
  withProgress(message = "Calcul NGM en cours...", value = 0.5, {
    result <- tryCatch({
      api_compute_ngm(ctx, url)
    }, error = function(e) {
      list(ok = FALSE, error = conditionMessage(e),
           error_type = "other", v_diagnosis = NULL, steps = list())
    })
  })

  rv_ngm_result(result)
  rv_ngm_computed_hash(isolate(current_model_hash()))
})

# Résultats NGM -- étapes ①–⑧
output$ngm_results_ui <- renderUI({
  r      <- rv_ngm_result()
  sym_df <- rv_sym_df_ngm()
  if (is.null(r)) return(NULL)
  if (is.null(sym_df)) sym_df <- rv_symbolic()

  # ── Erreur ────────────────────────────────────────────────────────────────
  if (!scal_lgl(r$ok)) {
    err_block <- tags$div(
      style = "background:#f8d7da;border:1px solid #f5c6cb;padding:14px;
               border-radius:6px;margin-bottom:10px;",
      tags$h5(style = "color:#721c24;", "\u274c Erreur de calcul"),
      tags$p(style = "font-family:monospace;white-space:pre-wrap;
             font-size:12px;", r$error))
    if (!is.null(r$v_diagnosis)) {
      d <- r$v_diagnosis
      return(tagList(err_block, tags$div(
        style = "background:#fff3cd;border:1px solid #ffc107;
                 padding:14px;border-radius:6px;",
        tags$h5("\U0001f50d Diagnostic matrice V"),
        tags$p(paste0("Rang : ", d$rank, "/", d$n)),
        tags$p(paste0("det(V) = ", d$det_symbolic)),
        if (length(d$zero_rows) > 0)
          tags$p(style = "color:#721c24;",
                 paste0("Lignes nulles : ",
                        paste(d$zero_rows, collapse = ", "))),
        tags$p(style = "color:#666;font-size:12px;",
               "\U0001f4a1 Cause probable : compartiment infectieux sans flux is_V."))))
    }
    return(err_block)
  }

  steps  <- r$steps
  method <- steps$`7_eigenvalues`$method %||% "symbolic"
  n_inf  <- length(rv_infected_order())

  # -- Bandeau methode (affiché en tête des résultats) ----------------------
  method_banner <- switch(method,
    symbolic = NULL,   # methode normale : pas de bandeau
    numeric  = tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;border-radius:6px;
               padding:12px 16px;margin-bottom:16px;",
      tags$div(style = "display:flex;align-items:center;gap:10px;",
        tags$span(style = "font-size:22px;", "\U0001f4ca"),
        tags$div(
          tags$strong(style = "color:#856404;font-size:14px;",
            paste0("Matrice K ", n_inf, "x", n_inf,
                   " -- R\u2080 calcul\u00e9 par rayon spectral num\u00e9rique")),
          tags$br(),
          tags$span(style = "font-size:12px;color:#666;",
            "La dimension de K d\u00e9passe 3 -- pas de formule analytique ",
            "g\u00e9n\u00e9rale (th\u00e9or\u00e8me d'Abel-Ruffini). ",
            "R\u2080 = max|\u03bbi| calcul\u00e9 num\u00e9riquement. ",
            "L'\u00e9valuation num\u00e9rique et la sensibilit\u00e9 restent ",
            "pleinement fonctionnelles.")
        )
      )
    ),
    block = tags$div(
      style = "background:#d1ecf1;border:1px solid #bee5eb;border-radius:6px;
               padding:12px 16px;margin-bottom:16px;",
      tags$div(style = "display:flex;align-items:center;gap:10px;",
        tags$span(style = "font-size:22px;", "\U0001f9e9"),
        tags$div(
          tags$strong(style = "color:#0c5460;font-size:14px;",
            paste0("Matrice K ", n_inf, "x", n_inf,
                   " -- Structure en blocs d\u00e9tect\u00e9e")),
          tags$br(),
          tags$span(style = "font-size:12px;color:#555;",
            "K est d\u00e9composable en sous-syst\u00e8mes ind\u00e9pendants. ",
            "R\u2080 = max(\u03c1(K\u2081), \u03c1(K\u2082), ...) -- ",
            "chaque bloc correspond \u00e0 un sous-syst\u00e8me \u00e9pid\u00e9mique s\u00e9par\u00e9."),
          if (!is.null(steps$`7_eigenvalues`$block_info)) {
            bi <- steps$`7_eigenvalues`$block_info
            tags$p(style = "margin:6px 0 0 0;font-size:12px;font-family:monospace;",
                   paste0("Blocs : ", bi))
          }
        )
      )
    ),
    NULL  # fallback
  )

  result_box <- function(title, latex_str, note = NULL,
                         bg = "#f8f9fa", border = "#dee2e6") {
    tags$div(
      style = paste0("background:", bg, ";border:1px solid ", border,
                     ";padding:14px;border-radius:6px;margin-bottom:12px;"),
      tags$h5(style = "margin-top:0;", title),
      tags$div(style = "font-size:16px;text-align:center;
               padding:8px 0;overflow-x:auto;",
               withMathJax(paste0("\\[", latex_str, "\\]"))),
      if (!is.null(note))
        tags$p(style = "color:#666;font-size:12px;margin-bottom:0;", note))
  }

  # ODE
  build_ode_latex <- function(comp_df, sym_df) {
    lines <- character(0)
    for (i in seq_len(nrow(comp_df))) {
      sid   <- comp_df$state_id[i]
      stype <- comp_df$state_type[i]
      if (!is.na(stype) && stype == "dead") next
      f_in  <- sym_df[!is.na(sym_df$to_state_id) &
                      sym_df$to_state_id == sid &
                      !is.na(sym_df$rate_expression) &
                      trimws(sym_df$rate_expression) != "", ]
      f_out <- sym_df[!is.na(sym_df$from_state_id) &
                      sym_df$from_state_id == sid &
                      !is.na(sym_df$rate_expression) &
                      trimws(sym_df$rate_expression) != "", ]
      pos <- if (nrow(f_in) > 0) f_in$rate_expression else character(0)
      neg <- if (nrow(f_out) > 0) f_out$rate_expression else character(0)
      rhs <- if (length(pos) == 0 && length(neg) == 0) "0"
             else if (length(pos) == 0)
               paste0("- ", paste(paste0("(", neg, ")"), collapse = " - "))
             else if (length(neg) == 0)
               paste(pos, collapse = " + ")
             else
               paste0(paste(pos, collapse = " + "), " - ",
                      paste(paste0("(", neg, ")"), collapse = " - "))
      lines <- c(lines,
                 paste0("\\frac{d", sid, "}{dt} &= ", rhs))
    }
    paste0("\\begin{aligned}\n",
           paste(lines, collapse = " \\\\\\\\\n"),
           "\n\\end{aligned}")
  }

  tagList(
    method_banner,
    tags$div(
      style = "background:#f0f7ff;border:1px solid #4a90d9;padding:14px;
               border-radius:6px;margin-bottom:16px;",
      tags$h5(style = "margin-top:0;color:#1a4a7a;",
              "\u24ea \u00c9quations du mod\u00e8le"),
      tags$div(style = "font-size:15px;text-align:left;
               padding:8px 0;overflow-x:auto;",
               withMathJax(paste0("\\[",
                                   build_ode_latex(rv_comp(), sym_df),
                                   "\\]")))),

    result_box("\u2460 Vecteur infect\u00e9 \\(\\mathbf{x}\\)",
               steps[["1_symbols"]]$latex,
               bg = "#e8f4f8", border = "#bee5eb"),

    fluidRow(
      column(6, result_box(
        "\u2461 Vecteur \\(\\mathbf{F}(\\mathbf{x})\\)",
        steps[["2_FV_vectors"]]$latex_F,
        bg = "#fff8e1", border = "#ffc107")),
      column(6, result_box(
        "\u2461 Vecteur \\(\\mathbf{V}(\\mathbf{x})\\)",
        steps[["2_FV_vectors"]]$latex_V,
        bg = "#f3e5f5", border = "#ce93d8"))),

    fluidRow(
      column(6, result_box(
        "\u2462 \\(\\mathcal{F}\\) \u2014 Jacobien",
        steps[["3_jacobians"]]$latex_Fmat,
        bg = "#fff8e1", border = "#ffc107")),
      column(6, result_box(
        "\u2462 \\(\\mathcal{V}\\) \u2014 Jacobien",
        steps[["3_jacobians"]]$latex_Vmat,
        bg = "#f3e5f5", border = "#ce93d8"))),

    tags$div(
      style = "background:#e3f2fd;border:1px solid #90caf9;padding:8px 14px;
               border-radius:5px;margin-bottom:8px;font-size:13px;",
      tags$strong("Substitutions DFE : "),
      withMathJax(paste0("\\(",
                          steps[["4_dfe_substitution"]]$latex_dfe,
                          "\\)"))),

    fluidRow(
      column(6, result_box(
        "\u2463 \\(\\mathcal{F}|_{DFE}\\)",
        steps[["4_dfe_substitution"]]$latex_Fmat_dfe,
        bg = "#fff8e1", border = "#ffc107")),
      column(6, result_box(
        "\u2463 \\(\\mathcal{V}|_{DFE}\\)",
        steps[["4_dfe_substitution"]]$latex_Vmat_dfe,
        bg = "#f3e5f5", border = "#ce93d8"))),

    result_box(
      "\u2464 \\(\\mathcal{V}^{-1}|_{DFE}\\)",
      steps[["5_V_inv"]]$latex_Vmat_inv,
      bg = "#f3e5f5", border = "#ce93d8"),

    result_box(
      "\u2465 \\(K = \\mathcal{F} \\cdot \\mathcal{V}^{-1}\\)",
      steps[["6_K_matrix"]]$latex_K,
      bg = "#e8f5e9", border = "#a5d6a7"),

    result_box(
      "\u2466 Valeurs propres de \\(K\\)",
      steps[["7_eigenvalues"]]$latex_evals,
      note = steps[["7_eigenvalues"]]$R0_note,
      bg = "#fce4ec", border = "#f48fb1"),

    {
      ev7     <- steps[["7_eigenvalues"]]
      method  <- ev7$method %||% "symbolic"
      dim_n   <- ev7$matrix_dimension %||% 0
      blk_str <- ev7$block_structure

      # -- Badge methode de calcul ----------------------------------------
      method_badge <- switch(method,
        symbolic = tags$span(
          style = "background:#28a745;color:white;padding:4px 12px;
                   border-radius:12px;font-size:12px;font-weight:600;",
          "\u2705 Expression symbolique exacte"),
        numeric  = tags$span(
          style = "background:#fd7e14;color:white;padding:4px 12px;
                   border-radius:12px;font-size:12px;font-weight:600;",
          paste0("\U0001f4ca Rayon spectral numerique",
                 if (dim_n > 0) paste0(" (K ", dim_n, "x", dim_n, ")") else "")),
        block    = tags$span(
          style = "background:#6f42c1;color:white;padding:4px 12px;
                   border-radius:12px;font-size:12px;font-weight:600;",
          paste0("\U0001f9e9 Structure en blocs detectee",
                 if (!is.null(blk_str))
                   paste0(" (", length(blk_str), " blocs)")
                 else "")),
        tags$span(style = "background:#6c757d;color:white;padding:4px 12px;
                   border-radius:12px;font-size:12px;", method)
      )

      # -- Boite principale R0 -------------------------------------------
      tagList(
        tags$div(
          style = "background:#1a237e;color:white;padding:20px;
                   border-radius:8px;text-align:center;margin-bottom:16px;",
          tags$h4(style = "color:white;margin-bottom:8px;",
                  "\u2467 Nombre de reproduction de base"),
          tags$div(style = "margin-bottom:10px;", method_badge),
          if (method == "symbolic" || method == "block") {
            tags$div(style = "font-size:22px;",
                     withMathJax(paste0("\\[ R_0 = \\rho(K) = ",
                                         ev7$R0_latex, " \\]")))
          } else {
            # Methode numerique : afficher la valeur directement
            R0_num <- ev7$R0_numeric_direct %||% ev7$R0_expr_str
            tagList(
              tags$div(style = "font-size:28px;font-weight:700;
                        font-family:monospace;margin:10px 0;",
                       paste0("R\u2080 = ", R0_num)),
              tags$div(style = "font-size:13px;color:#ccc;",
                       ev7$R0_note)
            )
          },
          # Avertissements biologiques
          if (!is.null(ev7$biological_warnings) &&
              length(ev7$biological_warnings) > 0) {
            tags$div(style = "margin-top:12px;",
              lapply(ev7$biological_warnings, function(w) {
                bg_col <- switch(w$severity %||% "info",
                  error   = "#f8d7da", warning = "#fff3cd",
                  info    = "#d1ecf1", "#e2e3e5")
                tags$div(
                  style = paste0("background:", bg_col,
                                 ";border-left:4px solid #ccc;",
                                 "padding:8px 12px;margin-bottom:6px;",
                                 "border-radius:4px;color:#333;"),
                  tags$span(style = "font-size:13px;", w$message))
              })
            )
          }
        ),

        # -- Structure en blocs si detectee --------------------------------
        if (!is.null(blk_str) && length(blk_str) > 0) {
          tags$div(
            style = "background:#f3e5f5;border:1px solid #ce93d8;
                     border-radius:8px;padding:16px;margin-bottom:16px;",
            tags$h5(style = "color:#6f42c1;margin-top:0;",
                    "\U0001f9e9 Structure en blocs"),
            tags$p(style = "font-size:13px;color:#555;margin-bottom:10px;",
                   "La matrice K a une structure diagonale par blocs. ",
                   "R\u2080 = max des rayons spectraux de chaque bloc."),
            tags$table(
              style = "width:100%;border-collapse:collapse;font-size:13px;",
              tags$thead(tags$tr(
                style = "background:#6f42c1;color:white;",
                tags$th(style = "padding:6px 10px;", "Bloc"),
                tags$th(style = "padding:6px 10px;", "Compartiments"),
                tags$th(style = "padding:6px 10px;", "Taille"),
                tags$th(style = "padding:6px 10px;", "R\u2080 du bloc"),
                tags$th(style = "padding:6px 10px;", "Methode")
              )),
              tags$tbody(lapply(seq_along(blk_str), function(bi) {
                bl <- blk_str[[bi]]
                is_max <- bl$R0 == max(sapply(blk_str, function(b) b$R0))
                tags$tr(
                  style = paste0(
                    "border-bottom:1px solid #dee2e6;",
                    if (is_max) "background:#f3e5f5;font-weight:600;" else ""),
                  tags$td(style = "padding:5px 10px;", paste0("K", bi)),
                  tags$td(style = "padding:5px 10px;font-family:monospace;",
                          paste0("{", paste(bl$indices, collapse=","), "}")),
                  tags$td(style = "padding:5px 10px;text-align:center;",
                          bl$size),
                  tags$td(style = "padding:5px 10px;text-align:right;",
                          tagList(
                            tags$strong(bl$R0),
                            if (is_max)
                              tags$span(style = "color:#6f42c1;margin-left:6px;",
                                        "\u2190 R\u2080")
                          )),
                  tags$td(style = "padding:5px 10px;color:#666;", bl$method)
                )
              }))
            )
          )
        }
      )
    },

    # =========================================================================
    # Panneau Export LaTeX
    # =========================================================================
    tags$div(
      style = "background:#f8f9fa;border:1px solid #dee2e6;border-radius:8px;
               padding:16px;margin-top:20px;",
      tags$h5(style = "margin-top:0;color:#495057;",
              "📋 Export LaTeX"),
      tags$p(style = "font-size:12px;color:#666;margin-bottom:14px;",
             "Cliquez sur un bouton pour copier le code LaTeX. ",
             "Compatible avec tout editeur LaTeX (Overleaf, TeXstudio...)."),

      fluidRow(
        # -- R0 symbolique --------------------------------------------------
        column(6,
          tags$div(
            style = "background:white;border:1px solid #dee2e6;
                     border-radius:6px;padding:12px;margin-bottom:10px;",
            tags$div(style = "display:flex;justify-content:space-between;
                              align-items:center;margin-bottom:8px;",
              tags$strong(style = "font-size:13px;", "Expression R₀"),
              actionButton("btn_copy_latex_r0", "Copier",
                           class = "btn-xs btn-outline-primary",
                           style = "padding:2px 8px;font-size:11px;",
                           onclick = paste0(
                             "navigator.clipboard.writeText('",
                             "R_0 = ", gsub("'", "\\'", ev7$R0_latex),
                             "').then(function(){",
                             "document.getElementById('latex_r0_ok').style.display='inline';",
                             "setTimeout(function(){",
                             "document.getElementById('latex_r0_ok').style.display='none';},2000)",
                             "});"))
            ),
            tags$pre(
              style = "background:#f5f5f5;padding:8px;border-radius:4px;
                       font-size:11px;margin:0;white-space:pre-wrap;
                       word-break:break-all;max-height:80px;overflow-y:auto;",
              paste0("R_0 = ", ev7$R0_latex)
            ),
            tags$span(id = "latex_r0_ok",
                      style = "display:none;color:#28a745;font-size:12px;
                               margin-top:4px;",
                      "✅ Copie!")
          )
        ),

        # -- ODE -------------------------------------------------------------
        column(6,
          tags$div(
            style = "background:white;border:1px solid #dee2e6;
                     border-radius:6px;padding:12px;margin-bottom:10px;",
            tags$div(style = "display:flex;justify-content:space-between;
                              align-items:center;margin-bottom:8px;",
              tags$strong(style = "font-size:13px;", "Equations ODE"),
              actionButton("btn_copy_latex_ode", "Copier",
                           class = "btn-xs btn-outline-primary",
                           style = "padding:2px 8px;font-size:11px;",
                           onclick = paste0(
                             "navigator.clipboard.writeText('",
                             gsub("'", "\\'",
                               build_ode_latex(rv_comp(), sym_df)),
                             "').then(function(){",
                             "document.getElementById('latex_ode_ok').style.display='inline';",
                             "setTimeout(function(){",
                             "document.getElementById('latex_ode_ok').style.display='none';},2000)",
                             "});"))
            ),
            tags$pre(
              style = "background:#f5f5f5;padding:8px;border-radius:4px;
                       font-size:11px;margin:0;white-space:pre-wrap;
                       word-break:break-all;max-height:80px;overflow-y:auto;",
              build_ode_latex(rv_comp(), sym_df)
            ),
            tags$span(id = "latex_ode_ok",
                      style = "display:none;color:#28a745;font-size:12px;
                               margin-top:4px;",
                      "✅ Copie!")
          )
        )
      ),

      fluidRow(
        # -- Matrice F -------------------------------------------------------
        column(6,
          tags$div(
            style = "background:white;border:1px solid #dee2e6;
                     border-radius:6px;padding:12px;margin-bottom:10px;",
            tags$div(style = "display:flex;justify-content:space-between;
                              align-items:center;margin-bottom:8px;",
              tags$strong(style = "font-size:13px;", "Matrice F"),
              actionButton("btn_copy_latex_F", "Copier",
                           class = "btn-xs btn-outline-primary",
                           style = "padding:2px 8px;font-size:11px;",
                           onclick = paste0(
                             "navigator.clipboard.writeText('",
                             gsub("'", "\\'",
                               steps[["4_dfe_substitution"]]$latex_Fmat_dfe),
                             "').then(function(){",
                             "document.getElementById('latex_F_ok').style.display='inline';",
                             "setTimeout(function(){",
                             "document.getElementById('latex_F_ok').style.display='none';},2000)",
                             "});"))
            ),
            tags$pre(
              style = "background:#f5f5f5;padding:8px;border-radius:4px;
                       font-size:11px;margin:0;white-space:pre-wrap;
                       word-break:break-all;max-height:80px;overflow-y:auto;",
              steps[["4_dfe_substitution"]]$latex_Fmat_dfe
            ),
            tags$span(id = "latex_F_ok",
                      style = "display:none;color:#28a745;font-size:12px;
                               margin-top:4px;",
                      "✅ Copie!")
          )
        ),

        # -- Matrice V -------------------------------------------------------
        column(6,
          tags$div(
            style = "background:white;border:1px solid #dee2e6;
                     border-radius:6px;padding:12px;margin-bottom:10px;",
            tags$div(style = "display:flex;justify-content:space-between;
                              align-items:center;margin-bottom:8px;",
              tags$strong(style = "font-size:13px;", "Matrice V"),
              actionButton("btn_copy_latex_V", "Copier",
                           class = "btn-xs btn-outline-primary",
                           style = "padding:2px 8px;font-size:11px;",
                           onclick = paste0(
                             "navigator.clipboard.writeText('",
                             gsub("'", "\\'",
                               steps[["4_dfe_substitution"]]$latex_Vmat_dfe),
                             "').then(function(){",
                             "document.getElementById('latex_V_ok').style.display='inline';",
                             "setTimeout(function(){",
                             "document.getElementById('latex_V_ok').style.display='none';},2000)",
                             "});"))
            ),
            tags$pre(
              style = "background:#f5f5f5;padding:8px;border-radius:4px;
                       font-size:11px;margin:0;white-space:pre-wrap;
                       word-break:break-all;max-height:80px;overflow-y:auto;",
              steps[["4_dfe_substitution"]]$latex_Vmat_dfe
            ),
            tags$span(id = "latex_V_ok",
                      style = "display:none;color:#28a745;font-size:12px;
                               margin-top:4px;",
                      "✅ Copie!")
          )
        )
      ),

      # -- Matrice K (NGM) -------------------------------------------------
      tags$div(
        style = "background:white;border:1px solid #dee2e6;
                 border-radius:6px;padding:12px;",
        tags$div(style = "display:flex;justify-content:space-between;
                          align-items:center;margin-bottom:8px;",
          tags$strong(style = "font-size:13px;",
                      "Matrice K = F * V^{-1}"),
          actionButton("btn_copy_latex_K", "Copier",
                       class = "btn-xs btn-outline-primary",
                       style = "padding:2px 8px;font-size:11px;",
                       onclick = paste0(
                         "navigator.clipboard.writeText('",
                         gsub("'", "\\'",
                           steps[["6_K_matrix"]]$latex_K),
                         "').then(function(){",
                         "document.getElementById('latex_K_ok').style.display='inline';",
                         "setTimeout(function(){",
                         "document.getElementById('latex_K_ok').style.display='none';},2000)",
                         "});"))
        ),
        tags$pre(
          style = "background:#f5f5f5;padding:8px;border-radius:4px;
                   font-size:11px;margin:0;white-space:pre-wrap;
                   word-break:break-all;max-height:100px;overflow-y:auto;",
          steps[["6_K_matrix"]]$latex_K
        ),
        tags$span(id = "latex_K_ok",
                  style = "display:none;color:#28a745;font-size:12px;
                           margin-top:4px;",
                  "✅ Copie!")
      )
    )
  )
})

# Onglet Symbolic / NGM
output$symbolic_tab_content <- renderUI({
  deps   <- rv_dependencies()
  synced <- rv_dependencies_synced()
  if (length(deps) > 0 && !synced)
    return(.blocked_by_deps_ui("goto_diagram_from_symbolic"))

  v      <- all_verified()
  dfe_ok <- rv_dfe_validated()

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
      tags$p(strong("En attente : "), paste(raisons, collapse = " et "))))
  }
  if (!dfe_ok)
    return(tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;padding:20px;
               border-radius:6px;margin-top:20px;text-align:center;",
      tags$h4("\U0001f512 DFE non valid\u00e9"),
      tags$p("Allez dans l'onglet ", strong("DFE / Param\u00e8tres"),
             " et cliquez ", strong("\u2705 Valider le DFE"), ".")))

  tagList(
    h4("\u2460 Table symbolique -- Classification"),
    uiOutput("symbolic_validation_warnings"),
    uiOutput("symbolic_stale_banner"),
    DTOutput("symbolic_table"), br(),
    fluidRow(
      column(6,
             actionButton("recalc_symbolic",
                          "\U0001f504 R\u00e9initialiser la table",
                          class = "btn-sm btn-warning",
                          style = "margin-bottom:12px;")),
      column(6, textOutput("symbolic_status"))),
    br(), br(),
    h4("\u2461 Ordre des compartiments infect\u00e9s"),
    uiOutput("infected_order_ui"),
    br(), br(),
    h4("\u2462 Calcul NGM / R\u2080"),
    uiOutput("ngm_stale_warning"),
    fluidRow(
      column(4, uiOutput("btn_compute_ngm_ui")),
      column(8, uiOutput("ngm_compute_status"))),
    br(),
    uiOutput("ngm_results_ui"))
})


# =============================================================================
# SECTION 5 -- Évaluation numérique de R₀
# =============================================================================

output$numeric_eval_content <- renderUI({
  deps   <- rv_dependencies()
  synced <- rv_dependencies_synced()
  if (length(deps) > 0 && !synced)
    return(.blocked_by_deps_ui("goto_diagram_from_numeric"))

  r <- rv_ngm_result()
  if (is.null(r) || !scal_lgl(r$ok))
    return(tags$div(
      style = "background:#fff3cd;border:1px solid #ffc107;border-radius:6px;
               padding:30px;text-align:center;margin-top:40px;",
      tags$h4(style = "color:#856404;", "\U0001f4ca \u00c9valuation Num\u00e9rique"),
      tags$p("Calculez d'abord R\u2080 symbolique dans l'onglet ",
             strong("Symbolic / NGM"), ".")))

  R0_expr_str <- r$steps$`7_eigenvalues`$R0_expr_str
  R0_latex    <- r$steps$`7_eigenvalues`$R0_latex
  params_in_R0 <- r$steps$`1_symbols`$param_names

  if (length(params_in_R0) == 0)
    return(tags$div(
      style = "background:#d4edda;border:1px solid #c3e6cb;border-radius:6px;
               padding:30px;text-align:center;margin-top:40px;",
      tags$h4(style = "color:#155724;", "\u2705 R\u2080 est constant"),
      tags$p("Valeur num\u00e9rique : ", tags$strong(R0_expr_str))))

  tagList(
    tags$h3("\U0001f4ca \u00c9valuation Num\u00e9rique de R\u2080"),
    tags$div(
      style = "background:#f0f7ff;border:1px solid #4a90d9;border-radius:6px;
               padding:14px;margin-bottom:16px;",
      tags$h5(style = "margin-top:0;color:#1a5fa8;",
              "Expression Symbolique de R\u2080"),
      tags$div(style = "background:white;padding:16px;border-radius:4px;
               overflow-x:auto;text-align:center;font-size:18px;",
               withMathJax(paste0("\\[ R_0 = ", R0_latex, " \\]"))),
      tags$p(style = "margin:8px 0 0 0;font-size:13px;color:#666;",
             tags$code(style = "background:#e9ecef;padding:2px 6px;
                       border-radius:3px;", R0_expr_str))),
    tags$h5("Valeurs des Param\u00e8tres"),
    tags$div(
      style = "background:white;border:1px solid #dee2e6;border-radius:6px;
               padding:14px;margin-bottom:16px;",
      tags$table(
        style = "width:100%;border-collapse:collapse;",
        tags$thead(tags$tr(
          style = "background:#495057;color:white;",
          tags$th(style = "padding:8px;width:150px;", "Param\u00e8tre"),
          tags$th(style = "padding:8px;",             "Description"),
          tags$th(style = "padding:8px;width:180px;", "Valeur"),
          tags$th(style = "padding:8px;width:120px;", "Unit\u00e9s"))),
        tags$tbody(lapply(params_in_R0, function(pname) {
          pd  <- rv_params()
          idx <- which(pd$param_name == pname)
          desc  <- if (length(idx) > 0) pd$description[idx[1]] else ""
          units <- if (length(idx) > 0) pd$units[idx[1]] else ""
          cur   <- isolate(rv_numeric_params())[[pname]] %||% ""
          tags$tr(
            style = "border-bottom:1px solid #dee2e6;",
            tags$td(style = "padding:8px;font-family:monospace;font-weight:600;",
                    tags$code(style = "color:#d63384;", pname)),
            tags$td(style = "padding:8px;color:#666;font-size:13px;", desc),
            tags$td(style = "padding:6px;",
                    numericInput(paste0("num_param_", pname), NULL,
                                 value = if (cur != "") as.numeric(cur) else NA,
                                 width = "100%")),
            tags$td(style = "padding:8px;color:#999;font-size:12px;", units))
        })))),
    tags$div(
      style = "display:flex;gap:12px;align-items:center;margin-bottom:16px;",
      actionButton("btn_compute_R0_numeric",
                   "\u2699\ufe0f Calculer R\u2080 Num\u00e9rique",
                   class = "btn-primary btn-lg"),
      actionButton("btn_reset_numeric", "\u21ba R\u00e9initialiser",
                   class = "btn-outline-secondary")),
    uiOutput("R0_numeric_result"))
})

observeEvent(input$btn_compute_R0_numeric, {
  r <- rv_ngm_result()
  if (is.null(r) || !scal_lgl(r$ok)) {
    showNotification("Calculez d'abord R\u2080 symbolique.", type = "error")
    return()
  }
  R0_expr_str  <- r$steps$`7_eigenvalues`$R0_expr_str
  params_in_R0 <- r$steps$`1_symbols`$param_names

  param_vals <- list()
  missing    <- character(0)
  for (pname in params_in_R0) {
    val <- input[[paste0("num_param_", pname)]]
    if (is.null(val) || is.na(val)) missing <- c(missing, pname)
    else param_vals[[pname]] <- val
  }
  if (length(missing) > 0) {
    showNotification(paste0("Valeurs manquantes : ",
                             paste(missing, collapse = ", ")),
                     type = "warning", duration = 5)
    return()
  }
  rv_numeric_params(param_vals)
  tryCatch({
  url <- NGM_BASE_URL
  if (is.null(url)) { showNotification("Serveur FastAPI non disponible.", type="error"); return(NULL) }

    # Pour les grands modeles (methode numerique), R0_expr_str = "?"
    # On recalcule via compute_ngm avec les param_vals
    if (is.null(R0_expr_str) || trimws(R0_expr_str) == "?" ||
        trimws(R0_expr_str) == "") {
      ctx_num <- build_sympy_context(rv_comp(), rv_flux(), rv_dfe(), rv_params())
      ctx_num$infected_states <- rv_infected_order()
      ctx_num$param_vals      <- param_vals
      r_ngm <- api_compute_ngm(ctx_num, url)
      if (scal_lgl(r_ngm$ok)) {
        R0_val <- r_ngm$steps$`7_eigenvalues`$R0_numeric_direct %||%
                  r_ngm$steps$`7_eigenvalues`$R0_expr_str
        result <- list(ok = TRUE,
                       R0_numeric = R0_val,
                       warnings   = list())
      } else {
        result <- list(ok = FALSE,
                       error = r_ngm$error %||% "Erreur calcul numerique")
      }
    } else {
      result <- api_evaluate_r0(R0_expr_str, param_vals, url)
    }

    if (scal_lgl(result$ok)) {
      warnings_list <- if (is.null(result$warnings) ||
                           length(result$warnings) == 0) list()
      else if (is.data.frame(result$warnings))
        lapply(seq_len(nrow(result$warnings)), function(i)
          as.list(result$warnings[i, , drop = FALSE]))
      else result$warnings

      rv_R0_numeric(list(value = result$R0_numeric,
                         warnings = warnings_list))
      showNotification("\u2705 R\u2080 num\u00e9rique calcul\u00e9.",
                       type = "message", duration = 3)
    } else {
      rv_R0_numeric(NULL)
      showNotification(paste0("\u274c ", result$error),
                       type = "error", duration = 10)
    }
  }, error = function(e) {
    rv_R0_numeric(NULL)
    showNotification(paste0("\u274c Erreur Python : ", e$message),
                     type = "error", duration = 10)
  })
})

output$R0_numeric_result <- renderUI({
  R0_data <- rv_R0_numeric()
  if (is.null(R0_data)) return(NULL)
  R0_num   <- R0_data$value
  warnings <- R0_data$warnings %||% list()
  tagList(
    tags$div(
      style = "background:#d4edda;border:1px solid #c3e6cb;border-radius:6px;
               padding:20px;",
      tags$h4(style = "color:#155724;margin-top:0;", "\u2705 R\u00e9sultat"),
      tags$div(style = "font-size:28px;font-weight:700;color:#155724;
               font-family:monospace;text-align:center;",
               paste0("R\u2080 = ", round(R0_num, 4))),
      tags$p(style = "margin:12px 0 0 0;text-align:center;color:#155724;",
             if (R0_num < 1)    "\u2b07\ufe0f \u00c9pid\u00e9mie s'\u00e9teint (R\u2080 < 1)"
             else if (R0_num < 2) "\u26a0\ufe0f Croissance lente (1 \u2264 R\u2080 < 2)"
             else               "\u2b06\ufe0f Croissance rapide (R\u2080 \u2265 2)")),
    if (length(warnings) > 0)
      tags$div(style = "margin-top:16px;",
               lapply(warnings, function(w) {
                 bg_col <- switch(w$severity %||% "info",
                   error = "#f8d7da", warning = "#fff3cd",
                   info  = "#d1ecf1", "#e2e3e5")
                 tags$div(
                   style = paste0("background:", bg_col,
                                  ";border-left:4px solid #ccc;",
                                  "padding:10px 14px;margin-bottom:8px;",
                                  "border-radius:4px;"),
                   tags$span(style = "font-size:14px;", w$message))
               })))
})

observeEvent(input$btn_reset_numeric, {
  r <- rv_ngm_result()
  if (is.null(r) || !scal_lgl(r$ok)) return()
  params_in_R0 <- r$steps$`1_symbols`$param_names
  rv_numeric_params(list())
  rv_R0_numeric(NULL)
  for (pname in params_in_R0)
    updateNumericInput(session, paste0("num_param_", pname), value = NA)
})


# =============================================================================
# SECTION 6 -- Auto-save + Restore
# =============================================================================

# Auto-save toutes les 30 secondes vers localStorage
observe({
  comp       <- rv_comp()
  flux       <- rv_flux()
  dfe        <- rv_dfe()
  params     <- rv_params()
  model_name <- input$model_name

  if (rv_autosave_enabled() &&
      difftime(Sys.time(), rv_last_autosave(), units = "secs") > 30) {
    autosave_data <- list(
      meta = list(
        model_name   = model_name %||% "Untitled",
        autosaved_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        app_version  = APP_VERSION),
      compartments = df_to_json_list(comp),
      fluxes       = df_to_json_list(flux),
      dfe          = df_to_json_list(dfe),
      params       = df_to_json_list(params))

    session$sendCustomMessage("autosave", list(
      data = jsonlite::toJSON(autosave_data, auto_unbox = TRUE, pretty = TRUE)))
    rv_last_autosave(Sys.time())
  }
})

# BUG-01 CORRIGÉ : restore_autosave utilise load_model_list()
observeEvent(input$restore_autosave, {
  tryCatch({
    json_data <- jsonlite::fromJSON(input$restore_autosave,
                                    simplifyVector = FALSE)
    result    <- load_model_list(json_data)   # était load_json_to_model()

    if (result$ok) {
      rv_comp(result$comp_df)
      rv_flux(result$flux_df)
      rv_dfe(result$dfe_df)
      rv_params(result$params_df)
      updateTextInput(session, "model_name", value = result$model_name)
      showNotification(paste0("\u2705 Session restaur\u00e9e : ",
                               result$model_name),
                       type = "message", duration = 5)
      session$sendCustomMessage("clearAutosave", list())
    } else {
      showNotification(paste0("\u274c Restauration \u00e9chou\u00e9e : ",
                               paste(result$errors, collapse = "; ")),
                       type = "error", duration = 10)
    }
  }, error = function(e) {
    showNotification(paste0("\u274c Erreur restore : ", e$message),
                     type = "error", duration = 10)
  })
})

# Navigation inter-onglets
observeEvent(input$goto_diagram_from_symbolic,
  updateTabsetPanel(session, "main_tabs", selected = "\U0001f5fa Diagram"))
observeEvent(input$goto_diagram_from_numeric,
  updateTabsetPanel(session, "main_tabs", selected = "\U0001f5fa Diagram"))


# =============================================================================
# SECTION 7 -- Helper privé : UI "bloqué par dépendances"
# =============================================================================

.blocked_by_deps_ui <- function(btn_id) {
  tags$div(
    style = "padding:40px;text-align:center;",
    tags$div(style = "font-size:64px;color:#ffc107;margin-bottom:20px;", "\u26a0\ufe0f"),
    tags$h3(style = "color:#856404;margin-bottom:20px;",
            "Calcul R\u2080 temporairement bloqu\u00e9"),
    tags$div(
      style = "background:#fff3cd;border:2px solid #ffc107;border-radius:8px;
               padding:24px;max-width:600px;margin:0 auto 20px auto;text-align:left;",
      tags$p(style = "font-size:15px;",
             "\U0001f512 Les d\u00e9pendances inter-esp\u00e8ces ne sont pas synchronis\u00e9es."),
      tags$ol(style = "font-size:14px;",
              tags$li("Allez dans l'onglet ",
                      tags$strong(style = "color:#007bff;",
                                  "\U0001f5fa Diagram")),
              tags$li("Ouvrez la section ",
                      tags$strong("\U0001f517 D\u00e9pendances inter-esp\u00e8ces")),
              tags$li("Cliquez ",
                      tags$strong(style = "color:#28a745;",
                                  "\U0001f504 Recalculer les d\u00e9pendances")))),
    tags$div(style = "margin-top:30px;",
             actionButton(btn_id, "\U0001f5fa Aller au Diagramme",
                          class = "btn-warning btn-lg",
                          style = "font-size:18px;padding:12px 30px;")))
}
