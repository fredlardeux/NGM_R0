# =============================================================================
# M12_ui.R -- UI uniquement : fluidPage + CSS + JS
# =============================================================================
# Sourcé HORS server() dans app.R pour exposer l'objet `ui`.
# Ne contient AUCUN output$, observeEvent, reactive, etc.
# =============================================================================

ui <- fluidPage(
  tags$head(
    # Sortable.js (drag & drop ordre infectés)
    tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.15.2/Sortable.min.js"),
    tags$script(HTML("
    Shiny.addCustomMessageHandler('initSortable', function(msg) {
      function tryInit(n) {
        if(n <= 0) return;
        if(typeof Sortable === 'undefined' ||
           !document.getElementById('infected-sortable-list')) {
          setTimeout(function(){ tryInit(n-1); }, 200); return;
        }
        var el = document.getElementById('infected-sortable-list');
        if(el._sortableInstance) {
          try{ el._sortableInstance.destroy(); }catch(e){}
        }
        el._sortableInstance = Sortable.create(el, {
          animation: 150, ghostClass: 'sortable-ghost',
          handle: '.drag-handle',
          onEnd: function(evt) {
            var order = Array.from(el.children)
              .map(function(d){ return d.getAttribute('data-id'); })
              .filter(Boolean);
            if(order.length > 0)
              Shiny.setInputValue('infected_order_sorted', order,
                                  {priority:'event'});
          }
        });
      }
      setTimeout(function(){ tryInit(15); }, 500);
    });
    ")),

    # Auto-save / restore via localStorage
    tags$script(HTML("
    Shiny.addCustomMessageHandler('autosave', function(msg) {
      try {
        localStorage.setItem('ngm_autosave', msg.data);
        localStorage.setItem('ngm_autosave_ts', new Date().toISOString());
      } catch(e) {}
    });
    Shiny.addCustomMessageHandler('clearAutosave', function(msg) {
      localStorage.removeItem('ngm_autosave');
      localStorage.removeItem('ngm_autosave_ts');
    });
    $(document).ready(function() {
      setTimeout(function() {
        var saved = localStorage.getItem('ngm_autosave');
        var ts    = localStorage.getItem('ngm_autosave_ts');
        if(saved && ts) {
          var hrs = (new Date() - new Date(ts)) / 3600000;
          if(hrs < 24 && confirm('Session auto-sauvegardee trouvee (' +
              new Date(ts).toLocaleString() + '). Restaurer?')) {
            Shiny.setInputValue('restore_autosave', saved, {priority:'event'});
          }
        }
      }, 1000);
    });
    ")),

    # CSS
    tags$style(HTML("
    .compact-select {
      padding:4px 6px; font-size:13px; width:100%; height:28px;
      border:1px solid #ccc; border-radius:3px;
      background:white; color:#333;
    }
    .editable-input {
      width:100%; padding:4px; border:1px solid #ccc;
      border-radius:3px; font-size:13px;
    }
    table.dataTable td { vertical-align:middle; padding:4px !important; }
    .infected-sortable {
      display:flex; flex-wrap:wrap; gap:8px;
      align-items:center; margin-bottom:10px; min-height:48px;
    }
    .infected-badge {
      display:flex; align-items:center; gap:6px;
      border:1px solid #ccc; border-radius:5px;
      padding:6px 10px; cursor:grab; user-select:none;
    }
    .infected-badge-locked { opacity:0.75; cursor:default; }
    .drag-handle { color:#aaa; font-size:16px; cursor:grab; margin-right:2px; }
    .sortable-ghost { opacity:0.35; box-shadow:0 0 0 2px #4a90d9; }
    body { margin:0; padding:8px; overflow-x:hidden; }
    .container-fluid { max-width:100%; padding:0; }
    "))
  ),

  titlePanel("NGM R\u2080 Calculator \u2014 Next Generation Matrix v2.0"),
  withMathJax(),

  # ── Barre Save / Load persistante ─────────────────────────────────────────
  tags$div(
    id    = "save_load_bar",
    style = paste0(
      "background:#f0f4f8; border:1px solid #cdd5e0; border-radius:6px;",
      "padding:12px 16px; margin-bottom:14px;",
      "display:flex; align-items:center; justify-content:space-between;",
      "gap:16px; flex-wrap:wrap;"
    ),
    tags$div(
      style = "display:flex; align-items:center; gap:12px;",
      tags$span(style = "white-space:nowrap; font-weight:600;
                         font-size:14px; color:#495057;",
                "\U0001f4be Mod\u00e8le :"),
      textInput("model_name", label = NULL,
                value       = "mon_modele_ngm",
                placeholder = "nom du fichier",
                width       = "240px"),
      downloadButton("btn_save_model",
                     "\u2b07 Sauvegarder (.json)",
                     class = "btn-sm btn-primary",
                     style = "white-space:nowrap; height:36px;")
    ),
    tags$div(
      style = "display:flex; align-items:center; gap:12px;",
      tags$span(style = "white-space:nowrap; font-weight:600;
                         font-size:14px; color:#495057;",
                "\U0001f4c2 Charger :"),
      fileInput("btn_load_model", label = NULL,
                accept      = ".json",
                buttonLabel = "Parcourir\u2026",
                placeholder = "Aucun fichier",
                width       = "280px"),
      uiOutput("load_status_badge")
    )
  ),

  # ── Tableau de bord R₀ en temps réel ───────────────────────────────────────
  uiOutput("r0_dashboard"),

  # ── Onglets ────────────────────────────────────────────────────────────────
  tabsetPanel(id = "main_tabs",

    tabPanel("Compartiments", br(),
      fluidRow(
        column(3, actionButton("add_comp",
                               "\u2795 Ajouter compartiment")),
        column(3, actionButton("del_comp",
                               "\U0001f5d1 Supprimer s\u00e9lection")),
        column(3, actionButton("validate_all_comp", "\u2705 Valider tout",
                               style = "background:#28a745; color:white;"))
      ), br(),
      uiOutput("comp_validation_banner"),
      DTOutput("comp_table"), br(),
      verbatimTextOutput("comp_debug")
    ),

    tabPanel("Flux", br(),
      fluidRow(
        column(3, actionButton("add_flux",
                               "\u2795 Ajouter flux")),
        column(3, actionButton("del_flux",
                               "\U0001f5d1 Supprimer s\u00e9lection")),
        column(3, actionButton("validate_all_flux", "\u2705 Valider tout",
                               style = "background:#28a745; color:white;"))
      ), br(),
      uiOutput("flux_validation_banner"),
      DTOutput("flux_table"), br(),
      verbatimTextOutput("flux_debug")
    ),

    tabPanel("\U0001f5fa Diagram", br(),
      uiOutput("diagram_content")
    ),

    tabPanel("DFE / Param\u00e8tres", br(),
      uiOutput("dfe_tab_content")
    ),

    tabPanel("Symbolic / NGM", br(),
      uiOutput("symbolic_tab_content")
    ),

    tabPanel("\U0001f4ca \u00c9valuation Num\u00e9rique", br(),
      uiOutput("numeric_eval_content")
    ),

    tabPanel("\U0001f3af Sensibilit\u00e9", br(),
      uiOutput("sensitivity_content")
    ),

    tabPanel("\U0001f4ca Scénarios", br(),
      uiOutput("scenarios_content")
    ),

    tabPanel(title = uiOutput("diag_tab_title"), br(),
      uiOutput("diag_content")
    ),

    tabPanel("\U0001f4e4 Exports", br(),
      uiOutput("exports_content")
    ),

    tabPanel("\u2753 Aide", br(),
      uiOutput("help_content")
    )
  )
)