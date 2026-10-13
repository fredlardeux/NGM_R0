# =============================================================================
# M07_diagram_builder.R -- Construction des données visNetwork
# =============================================================================
# Dépendances : M01_constants.R (NODE_COLORS, EDGE_COLORS, INFECTED_STATE_TYPES,
#                                F_PROCESS_TYPES, %||%)
#               M05_dfe_logic.R (dfe_type_badge_color)
# Testé seul  : source("M01_constants.R")
#               source("M02_validation_sympy.R")
#               source("M03_rate_templates.R")
#               source("M06_symbolic_ngm.R")
#               source("M05_dfe_logic.R")
#               source("M07_diagram_builder.R")
# =============================================================================


# =============================================================================
# SECTION 1 -- Génération d'ID de dépendance
# =============================================================================

#' Générer le prochain ID de dépendance disponible
#'
#' @param existing_deps list -- liste de dépendances existantes
#' @return character(1) -- ex: "D1", "D2", ...
generate_dependency_id <- function(existing_deps) {
  if (length(existing_deps) == 0) return("D1")

  ids  <- sapply(existing_deps, function(d) d$dependency_id %||% "D0")
  nums <- suppressWarnings(as.integer(gsub("D", "", ids)))
  nums <- nums[!is.na(nums)]

  if (length(nums) == 0) return("D1")
  paste0("D", max(nums) + 1L)
}


# =============================================================================
# SECTION 2 -- Construction des données du diagramme
# =============================================================================

#' Construire nodes + edges pour visNetwork
#'
#' Corrections appliquées vs original :
#'   BUG-04 : suppression du double merge dep_edges (arêtes dupliquées)
#'   BUG-08 : font.bold supprimé (non supporté par visNetwork)
#'   BUG-09 : shapeProperties.borderDashes supprimé (non supporté)
#'   BUG-14 : font.color initialisé à NA dans nodes normaux pour rbind propre
#'
#' @param comp_df      data.frame -- compartiments
#' @param flux_df      data.frame -- flux
#' @param sym_df       data.frame ou NULL -- table symbolique (is_F)
#' @param dependencies list -- dépendances inter-espèces (auto ou manuelles)
#' @return list(nodes, edges, dependencies)
build_diagram_data <- function(comp_df, flux_df,
                               sym_df = NULL,
                               dependencies = NULL) {

  if (nrow(comp_df) == 0)
    return(list(nodes = data.frame(), edges = data.frame(),
                dependencies = list()))

  # ── NŒUDS : compartiments ─────────────────────────────────────────────────
  nodes <- do.call(rbind, lapply(seq_len(nrow(comp_df)), function(i) {
    sid   <- comp_df$state_id[i]
    lbl   <- comp_df$label[i]
    stype <- comp_df$state_type[i]
    sp    <- comp_df$species[i]
    ver   <- comp_df$verification[i]

    col <- NODE_COLORS[[stype]] %||% NODE_COLORS[["default"]]

    tooltip <- paste0(
      "<b>", sid, "</b><br/>",
      "<i>", lbl, "</i><br/>",
      "Type : <b>", stype, "</b><br/>",
      "Esp\u00e8ce : ", sp, "<br/>",
      "Statut : ",
      if (!is.na(ver) && ver == "ok") "\u2705 ok" else "\u23f3 pending"
    )

    shape <- if (!is.na(stype) && stype == "dead") "box" else "ellipse"

    data.frame(
      id                        = sid,
      label                     = sid,
      title                     = tooltip,
      shape                     = shape,
      group                     = if (!is.na(sp)) sp else "human",
      value                     = 20L,
      color.background          = col$background,
      color.border              = col$border,
      color.highlight.background= col$highlight$background,
      color.highlight.border    = col$highlight$border,
      # BUG-08 corrigé : font.bold supprimé (non supporté)
      # BUG-09 corrigé : shapeProperties.borderDashes supprimé
      # Bordure plus épaisse si pending (via borderWidth)
      borderWidth               = if (!is.na(ver) && ver != "ok") 3L else 2L,
      font.size                 = 16L,
      font.face                 = "arial",
      # BUG-14 corrigé : font.color présent dans les nodes normaux
      font.color                = NA_character_,
      stringsAsFactors          = FALSE
    )
  }))

  # ── NŒUDS SYMBOLES : flux entrants/sortants (birth, death, etc.) ──────────
  if (nrow(flux_df) > 0) {

    symbol_nodes_list <- lapply(seq_len(nrow(flux_df)), function(i) {
      to    <- flux_df$to_state_id[i]
      frm   <- flux_df$from_state_id[i]
      ptype <- flux_df$process_type[i]

      is_outflow <- (!is.na(to)  && to  == "") && (!is.na(frm) && frm != "")
      is_inflow  <- (!is.na(frm) && frm == "") && (!is.na(to)  && to  != "")

      if (!is_outflow && !is_inflow) return(NULL)

      symbol_info <- switch(ptype,
        death       = list(symbol = "\u2020",  bg = "rgba(149,165,166,0.3)",
                           fg = "#2c3e50", label = "Death"),
        birth       = list(symbol = "\u2295",  bg = "#3498db",
                           fg = "#ffffff", label = "Birth"),
        immigration = list(symbol = "\u21e8",  bg = "#27ae60",
                           fg = "#ffffff", label = "Immigration"),
        emigration  = list(symbol = "\u21e6",  bg = "#95a5a6",
                           fg = "#ffffff", label = "Emigration"),
        list(symbol = "\u25ef",
             bg = "#34495e", fg = "#ffffff",
             label = if (is_outflow) "Outflow" else "Inflow")
      )

      comp_id <- if (is_outflow) frm else to
      node_id <- paste0(
        if (is_outflow) "sink_" else "source_",
        ptype, "_", comp_id
      )

      data.frame(
        id                         = node_id,
        label                      = symbol_info$symbol,
        title                      = paste0(
          "<b>", symbol_info$label, "</b><br/>",
          if (is_outflow) paste0("from ", comp_id)
          else paste0("to ", comp_id)
        ),
        shape                      = "box",
        group                      = if (is_outflow) "sink" else "source",
        value                      = 15L,
        color.background           = symbol_info$bg,
        color.border               = symbol_info$bg,
        color.highlight.background = symbol_info$bg,
        color.highlight.border     = "#000000",
        borderWidth                = 2L,
        font.size                  = 24L,
        font.face                  = "arial",
        font.color                 = symbol_info$fg,
        stringsAsFactors           = FALSE
      )
    })

    symbol_nodes <- do.call(rbind, Filter(Negate(is.null), symbol_nodes_list))

    if (!is.null(symbol_nodes) && is.data.frame(symbol_nodes) &&
        nrow(symbol_nodes) > 0) {
      # Harmoniser les colonnes avant rbind
      for (col in setdiff(names(nodes), names(symbol_nodes)))
        symbol_nodes[[col]] <- NA
      nodes <- rbind(nodes, symbol_nodes[, names(nodes), drop = FALSE])
    }
  }

  # ── ARÊTES : flux normaux ─────────────────────────────────────────────────
  if (nrow(flux_df) == 0) {
    edges <- .empty_edges_df()
    return(list(nodes = nodes, edges = edges, dependencies = list()))
  }

  # IDs des flux is_F depuis sym_df
  f_flux_ids <- if (!is.null(sym_df) && nrow(sym_df) > 0)
    sym_df$flux_id[isTRUE(sym_df$is_F) | sym_df$is_F == TRUE]
  else character(0)

  edges_list <- lapply(seq_len(nrow(flux_df)), function(i) {
    fid   <- flux_df$flux_id[i]
    frm   <- flux_df$from_state_id[i]
    to    <- flux_df$to_state_id[i]
    ptype <- flux_df$process_type[i]
    rname <- flux_df$rate_name[i]
    rexpr <- flux_df$rate_expression[i]
    ver   <- flux_df$verification[i]

    is_outflow <- (!is.na(to)  && to  == "") && (!is.na(frm) && frm != "")
    is_inflow  <- (!is.na(frm) && frm == "") && (!is.na(to)  && to  != "")

    # Ignorer flux sans source ni cible valide (sauf outflow/inflow)
    if (!is_outflow && !is_inflow &&
        (is.na(frm) || frm == "" || is.na(to) || to == ""))
      return(NULL)

    if (is_outflow) {
      from_node <- frm
      to_node   <- paste0("sink_", ptype, "_", frm)
      ecol  <- "#000000"
      width <- 2L
    } else if (is_inflow) {
      from_node <- paste0("source_", ptype, "_", to)
      to_node   <- to
      ecol  <- "#3498db"
      width <- 2L
    } else {
      from_node <- frm
      to_node   <- to
      is_F_flux <- fid %in% f_flux_ids ||
                   (!is.na(ptype) && ptype %in% c("transmission",
                                                   "superinfection",
                                                   "relapse"))
      if (is_F_flux) {
        ecol  <- "#d9534f"   # rouge
        width <- 3L
      } else if (!is.na(ptype) &&
                 ptype %in% c("recovery", "waning_immunity")) {
        ecol  <- "#28a745"   # vert
        width <- 2L
      } else {
        ecol  <- "#3498db"   # bleu
        width <- 1L
      }
    }

    dashes <- is.na(ver) || ver != "ok" ||
              is.na(rexpr) || trimws(rexpr) == ""

    elabel <- if (!is.na(rname) && nchar(trimws(rname)) > 0)
      trimws(rname) else ""

    tooltip <- paste0(
      "<b>", fid, "</b><br/>",
      if (is_inflow)   paste0(ptype, " \u2192 ", to)
      else if (is_outflow) paste0(frm, " \u2192 ", ptype)
      else paste0(frm, " \u2192 ", to),
      "<br/>Processus : <b>", ptype, "</b><br/>",
      if (!is.na(rname) && nchar(trimws(rname)) > 0)
        paste0("Taux : ", rname, "<br/>") else "",
      if (!is.na(rexpr) && nchar(trimws(rexpr)) > 0)
        paste0("Expression : <code>", rexpr, "</code><br/>")
      else "\u26a0\ufe0f Expression manquante<br/>",
      "Statut : ",
      if (!is.na(ver) && ver == "ok") "\u2705 ok" else "\u23f3 pending"
    )

    data.frame(
      id         = fid,
      from       = from_node,
      to         = to_node,
      label      = elabel,
      title      = tooltip,
      color      = ecol,
      width      = width,
      dashes     = dashes,
      arrows     = "to",
      font.size  = 11L,
      font.align = "middle",
      smooth     = TRUE,
      stringsAsFactors = FALSE
    )
  })

  edges <- do.call(rbind, Filter(Negate(is.null), edges_list))
  if (is.null(edges) || !is.data.frame(edges))
    edges <- .empty_edges_df()

  # ── ARÊTES DE DÉPENDANCES ─────────────────────────────────────────────────
  # BUG-04 CORRIGÉ : une seule fusion, à la fin, pas deux fois.

  detected_dependencies <- list()

  if (!is.null(dependencies) && length(dependencies) > 0) {
    # Utiliser les dépendances fournies (filtrées sur visible = TRUE)
    visible_deps      <- Filter(function(d) isTRUE(d$visible), dependencies)
    detected_dependencies <- dependencies

    dep_edges <- .build_dep_edges_from_list(visible_deps, edges)

  } else {
    # Auto-détecter depuis les expressions de flux
    dep_result <- .detect_deps_from_flux(flux_df, comp_df, edges)
    detected_dependencies <- dep_result$dependencies
    dep_edges             <- dep_result$dep_edges
  }

  # Fusion unique (BUG-04 : plus de double merge)
  if (!is.null(dep_edges) && is.data.frame(dep_edges) &&
      nrow(dep_edges) > 0) {
    common_cols <- intersect(names(edges), names(dep_edges))
    edges <- rbind(
      edges[, common_cols, drop = FALSE],
      dep_edges[, common_cols, drop = FALSE]
    )
  }

  list(nodes = nodes, edges = edges, dependencies = detected_dependencies)
}


# =============================================================================
# SECTION 3 -- Helpers internes (privés, préfixe .)
# =============================================================================

#' Data.frame vide avec le bon schéma pour les arêtes visNetwork
.empty_edges_df <- function() {
  data.frame(
    id         = character(0), from       = character(0),
    to         = character(0), label      = character(0),
    title      = character(0), color      = character(0),
    width      = integer(0),   dashes     = logical(0),
    arrows     = character(0), font.size  = integer(0),
    font.align = character(0), smooth     = logical(0),
    stringsAsFactors = FALSE
  )
}

#' Construire les arêtes de dépendance depuis une liste de dépendances
.build_dep_edges_from_list <- function(deps, edges) {
  if (length(deps) == 0) return(NULL)

  dep_list <- lapply(deps, function(d) {
    is_cross <- isTRUE(d$is_cross_species)
    dep_color <- if (is_cross) "#e67e22" else "#95a5a6"
    dep_width <- if (is_cross) 2L else 1L

    tooltip <- paste0(
      "<b>Inter-compartiment Dependency</b><br/>",
      "<b>", d$from, "</b> (", d$from_species %||% "?", ") influences<br/>",
      if (!is.null(d$via_flux) && nchar(d$via_flux) > 0)
        paste0("flux ", d$via_flux, " : ") else "",
      "<b>", d$to, "</b><br/>",
      if (is_cross)
        paste0("\u26a0\ufe0f <b>Inter-species</b> coupling<br/>",
               d$from_species, " \u2192 ", d$to_species)
      else "Intra-species coupling"
    )

    data.frame(
      id         = paste0("dep_", d$dependency_id %||%
                            paste0(d$from, "_", d$to)),
      from       = d$from,
      to         = d$to,
      label      = "",
      title      = tooltip,
      color      = dep_color,
      width      = dep_width,
      dashes     = TRUE,
      arrows     = "to",
      font.size  = 9L,
      font.align = "middle",
      smooth     = TRUE,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, Filter(Negate(is.null), dep_list))
}

#' Auto-détecter les dépendances depuis les expressions de flux
.detect_deps_from_flux <- function(flux_df, comp_df, edges) {
  detected  <- list()
  dep_counter <- 1L

  if (nrow(flux_df) == 0 || nrow(comp_df) == 0)
    return(list(dependencies = detected, dep_edges = NULL))

  all_comp_ids <- comp_df$state_id
  dep_edges_list <- list()

  for (i in seq_len(nrow(flux_df))) {
    frm   <- flux_df$from_state_id[i]
    to    <- flux_df$to_state_id[i]
    rexpr <- flux_df$rate_expression[i]
    fid   <- flux_df$flux_id[i]

    if (is.na(frm) || frm == "" || is.na(to)  || to  == "" ||
        is.na(rexpr) || trimws(rexpr) == "") next

    # Compartiments mentionnés dans l'expression, hors source et cible
    for (sid in all_comp_ids) {
      if (sid == frm || sid == to) next
      pattern <- paste0(
        "\\b",
        gsub("([.|()\\^{}+$*?[])", "\\\\\\1", sid),
        "\\b"
      )
      if (!grepl(pattern, rexpr)) next

      # Espèces
      dep_sp <- comp_df$species[comp_df$state_id == sid]
      frm_sp <- comp_df$species[comp_df$state_id == frm]
      dep_sp <- if (length(dep_sp) > 0 && !is.na(dep_sp[1])) dep_sp[1] else "unknown"
      frm_sp <- if (length(frm_sp) > 0 && !is.na(frm_sp[1])) frm_sp[1] else "unknown"

      is_cross <- dep_sp != "unknown" && frm_sp != "unknown" &&
                  dep_sp != frm_sp

      dep_color <- if (is_cross) "#e67e22" else "#95a5a6"
      dep_width <- if (is_cross) 2L else 1L

      dep_id <- paste0("D", dep_counter)
      dep_counter <- dep_counter + 1L

      detected[[length(detected) + 1]] <- list(
        dependency_id = dep_id,
        from          = sid,
        to            = frm,
        from_species  = dep_sp,
        to_species    = frm_sp,
        via_flux      = fid,
        mechanism     = "",
        type          = "auto",
        visible       = TRUE,
        is_cross_species = is_cross
      )

      tooltip <- paste0(
        "<b>D\u00e9pendance inter-compartiments</b><br/>",
        "<b>", sid, "</b> (", dep_sp, ") influence<br/>",
        "le flux ", fid, " : <b>", frm, "</b> \u2192 ", to, "<br/>",
        if (is_cross)
          paste0("\u26a0\ufe0f Couplage <b>inter-esp\u00e8ces</b><br/>",
                 dep_sp, " \u2192 ", frm_sp)
        else "Couplage intra-esp\u00e8ce",
        "<br/>Expression : <code>", rexpr, "</code>"
      )

      dep_edges_list[[length(dep_edges_list) + 1]] <- data.frame(
        id         = paste0("dep_", fid, "_", sid),
        from       = sid,
        to         = frm,
        label      = "",
        title      = tooltip,
        color      = dep_color,
        width      = dep_width,
        dashes     = TRUE,
        arrows     = "to",
        font.size  = 9L,
        font.align = "middle",
        smooth     = TRUE,
        stringsAsFactors = FALSE
      )
    }
  }

  dep_edges <- if (length(dep_edges_list) > 0)
    do.call(rbind, dep_edges_list)
  else NULL

  list(dependencies = detected, dep_edges = dep_edges)
}


# =============================================================================
# SECTION 4 -- Vérifications au chargement
# =============================================================================

stopifnot(
  exists("NODE_COLORS"),
  exists("EDGE_COLORS"),
  exists("INFECTED_STATE_TYPES"),
  exists("F_PROCESS_TYPES"),
  exists("%||%"),
  exists("dfe_type_badge_color")   # M05
)

local({

  # ── generate_dependency_id ────────────────────────────────────────────────
  stopifnot(generate_dependency_id(list()) == "D1")

  deps <- list(
    list(dependency_id = "D1"),
    list(dependency_id = "D3")
  )
  stopifnot(generate_dependency_id(deps) == "D4")

  # ── build_diagram_data : compartiments vides ──────────────────────────────
  result_empty <- build_diagram_data(
    data.frame(), data.frame(), NULL, NULL
  )
  stopifnot(nrow(result_empty$nodes) == 0)

  # ── build_diagram_data : SIR minimal ─────────────────────────────────────
  comp <- data.frame(
    state_id     = c("S_h", "I_h", "R_h"),
    label        = c("Susceptible", "Infectious", "Recovered"),
    state_type   = c("susceptible", "infectious", "recovered"),
    species      = c("human", "human", "human"),
    verification = c("ok", "ok", "ok"),
    stringsAsFactors = FALSE
  )
  flux <- data.frame(
    flux_id         = c("F1", "F2"),
    from_state_id   = c("S_h", "I_h"),
    to_state_id     = c("I_h", "R_h"),
    process_type    = c("transmission", "recovery"),
    rate_name       = c("beta_c", "gamma_r"),
    rate_expression = c("beta_c*S_h*I_h/Ntot", "gamma_r*I_h"),
    verification    = c("ok", "ok"),
    stringsAsFactors = FALSE
  )

  result <- build_diagram_data(comp, flux)

  # 3 nœuds compartiments
  stopifnot(nrow(result$nodes) == 3)
  stopifnot(all(c("S_h","I_h","R_h") %in% result$nodes$id))

  # 2 arêtes normales (pas de symboles birth/death ici)
  # + éventuellement arêtes de dépendances (I_h dans expression de F1)
  stopifnot(nrow(result$edges) >= 2)

  # BUG-04 : vérifier qu'il n'y a pas d'arêtes dupliquées
  edge_ids <- result$edges$id[!grepl("^dep_", result$edges$id)]
  stopifnot(!any(duplicated(edge_ids)))

  # BUG-14 : font.color doit exister dans les nodes
  stopifnot("font.color" %in% names(result$nodes))

  # ── build_diagram_data : flux outflow (mort) ──────────────────────────────
  flux_with_death <- rbind(flux, data.frame(
    flux_id         = "F3",
    from_state_id   = "I_h",
    to_state_id     = "",       # outflow
    process_type    = "death",
    rate_name       = "mu_d",
    rate_expression = "mu_d * I_h",
    verification    = "ok",
    stringsAsFactors = FALSE
  ))

  result2 <- build_diagram_data(comp, flux_with_death)

  # Nœud symbole mort doit être créé
  stopifnot(any(grepl("^sink_death_", result2$nodes$id)))

  # BUG-04 : toujours pas de duplication
  edge_ids2 <- result2$edges$id
  stopifnot(!any(duplicated(edge_ids2)))

  # ── Dépendances fournies (visible = TRUE) ─────────────────────────────────
  deps_in <- list(list(
    dependency_id    = "D1",
    from             = "I_h",
    to               = "S_h",
    from_species     = "human",
    to_species       = "human",
    via_flux         = "F1",
    mechanism        = "direct transmission",
    type             = "auto",
    visible          = TRUE,
    is_cross_species = FALSE
  ))

  result3 <- build_diagram_data(comp, flux, dependencies = deps_in)

  # L'arête de dépendance doit apparaître exactement une fois (BUG-04)
  dep_edge_ids <- result3$edges$id[grepl("^dep_", result3$edges$id)]
  stopifnot(length(dep_edge_ids) == 1)
})

# message("M07_diagram_builder.R charg\u00e9 -- OK")
