# =============================================================================
# ui.R — User Interface Definition
# =============================================================================

ui <- fluidPage(

  useShinyjs(),
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=SF+Pro+Display:wght@300;400;500;600&family=SF+Mono:wght@400;500&display=swap"
    ),
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600&family=JetBrains+Mono:wght@400;500&display=swap"
    ),
    tags$style(HTML("

      /* ── Base ────────────────────────────────────────────────── */
      *, *::before, *::after { box-sizing: border-box; }

      body {
        background  : #f5f5f7;
        color       : #1d1d1f;
        font-family : 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        font-weight : 400;
        font-size   : 14px;
        margin      : 0;
        padding     : 0;
        -webkit-font-smoothing: antialiased;
      }

      /* ── Header bar ──────────────────────────────────────────── */
      .app-header {
        background    : rgba(255,255,255,0.85);
        backdrop-filter: blur(20px);
        -webkit-backdrop-filter: blur(20px);
        border-bottom : 1px solid #e0e0e5;
        padding       : 16px 32px;
        display       : flex;
        align-items   : center;
        gap           : 14px;
        position      : sticky;
        top           : 0;
        z-index       : 100;
      }
      .app-header h1 {
        font-family    : 'Inter', sans-serif;
        font-size      : 16px;
        font-weight    : 600;
        color          : #1d1d1f;
        margin         : 0;
        letter-spacing : -.2px;
      }
      .app-header .subtitle {
        font-size  : 12px;
        color      : #86868b;
        font-weight: 400;
        margin-top : 1px;
      }
      .tag-wolf {
        margin-left   : auto;
        font-family   : 'JetBrains Mono', monospace;
        font-size     : 10px;
        font-weight   : 500;
        color         : #0071e3;
        border        : 1px solid #cce0f5;
        padding       : 4px 10px;
        border-radius : 20px;
        background    : #f0f7ff;
        letter-spacing: .3px;
      }

      /* ── Layout ──────────────────────────────────────────────── */
      .main-wrap {
        display  : flex;
        height   : calc(100vh - 57px);
        overflow : hidden;
      }
      .sidebar-panel {
        width         : 300px;
        min-width     : 300px;
        background    : #ffffff;
        border-right  : 1px solid #e0e0e5;
        overflow-y    : auto;
        padding       : 20px 18px;
        display       : flex;
        flex-direction: column;
        gap           : 14px;
      }
      .content-panel {
        flex       : 1;
        overflow-y : auto;
        padding    : 28px 32px;
        background : #f5f5f7;
      }

      /* ── Sidebar sections ────────────────────────────────────── */
      .section-label {
        font-family    : 'Inter', sans-serif;
        font-size      : 10px;
        font-weight    : 600;
        color          : #86868b;
        letter-spacing : .8px;
        text-transform : uppercase;
        margin-bottom  : 10px;
        padding-bottom : 8px;
        border-bottom  : 1px solid #f0f0f2;
      }
      .sidebar-card {
        background    : #ffffff;
        border        : 1px solid #e8e8ed;
        border-radius : 12px;
        padding       : 16px;
        box-shadow    : 0 1px 3px rgba(0,0,0,.04);
      }

      /* ── Inputs ──────────────────────────────────────────────── */
      .form-group { margin-bottom: 12px !important; }
      .form-group:last-child { margin-bottom: 0 !important; }

      label {
        font-family  : 'Inter', sans-serif !important;
        font-size    : 11.5px !important;
        font-weight  : 500 !important;
        color        : #6e6e73 !important;
        margin-bottom: 5px !important;
        letter-spacing: -.1px !important;
      }
      .form-control, .selectize-input {
        background    : #f5f5f7 !important;
        border        : 1px solid #e0e0e5 !important;
        color         : #1d1d1f !important;
        border-radius : 8px !important;
        font-family   : 'Inter', sans-serif !important;
        font-size     : 13px !important;
        font-weight   : 400 !important;
        padding       : 7px 11px !important;
        transition    : border-color .15s, box-shadow .15s;
      }
      .form-control:focus, .selectize-input.focus {
        border-color : #0071e3 !important;
        box-shadow   : 0 0 0 3px rgba(0,113,227,.12) !important;
        outline      : none !important;
        background   : #ffffff !important;
      }
      .selectize-dropdown {
        background    : #ffffff !important;
        border        : 1px solid #e0e0e5 !important;
        border-radius : 10px !important;
        box-shadow    : 0 8px 24px rgba(0,0,0,.10) !important;
        margin-top    : 4px !important;
      }
      .selectize-dropdown-content .option {
        font-family : 'Inter', sans-serif !important;
        font-size   : 13px !important;
        color       : #1d1d1f !important;
        padding     : 8px 12px !important;
      }
      .selectize-dropdown-content .option.selected,
      .selectize-dropdown-content .option:hover {
        background : #f0f7ff !important;
        color      : #0071e3 !important;
      }

      /* Sliders — Apple blue */
      .irs--shiny .irs-bar {
        background  : #0071e3 !important;
        border-top  : 1px solid #0071e3 !important;
        border-bottom: 1px solid #0071e3 !important;
        border-radius: 3px !important;
      }
      .irs--shiny .irs-handle {
        background   : #ffffff !important;
        border       : 2px solid #0071e3 !important;
        border-radius: 50% !important;
        box-shadow   : 0 2px 6px rgba(0,0,0,.15) !important;
        width        : 20px !important;
        height       : 20px !important;
        top          : 24px !important;
      }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
        background    : #0071e3 !important;
        border-radius : 6px !important;
        font-family   : 'Inter', sans-serif !important;
        font-size     : 10px !important;
        font-weight   : 500 !important;
      }
      .irs--shiny .irs-line {
        background    : #e0e0e5 !important;
        border-radius : 3px !important;
      }
      .irs--shiny .irs-grid-text {
        color       : #aeaeb2 !important;
        font-family : 'Inter', sans-serif !important;
        font-size   : 9px !important;
      }
      .irs--shiny .irs-min, .irs--shiny .irs-max {
        color       : #aeaeb2 !important;
        font-family : 'Inter', sans-serif !important;
        font-size   : 9px !important;
        background  : transparent !important;
      }

      /* ── Run button ──────────────────────────────────────────── */
      #run_sim {
        width         : 100%;
        background    : #0071e3;
        color         : #ffffff;
        border        : none;
        border-radius : 10px;
        font-family   : 'Inter', sans-serif;
        font-size     : 13px;
        font-weight   : 500;
        letter-spacing: -.1px;
        padding       : 11px 0;
        cursor        : pointer;
        transition    : background .15s, transform .1s, box-shadow .15s;
        box-shadow    : 0 2px 8px rgba(0,113,227,.30);
      }
      #run_sim:hover  {
        background : #0077ed;
        box-shadow : 0 4px 14px rgba(0,113,227,.40);
      }
      #run_sim:active { transform: scale(.98); }
      #run_sim:disabled {
        background : #e0e0e5;
        color      : #aeaeb2;
        cursor     : not-allowed;
        box-shadow : none;
      }

      /* ── Dynamic items-per-factor inputs ─────────────────────── */
      #items_inputs .form-group { margin-bottom: 8px !important; }
      #items_inputs label { color: #aeaeb2 !important; font-size: 11px !important; }

      /* ── Residual display ────────────────────────────────────── */
      #residual_display {
        font-family   : 'JetBrains Mono', monospace !important;
        font-size     : 13px !important;
        color         : #0071e3 !important;
        padding       : 7px 11px !important;
        background    : #f0f7ff !important;
        border        : 1px solid #cce0f5 !important;
        border-radius : 8px !important;
      }

      /* ── Model preview box ───────────────────────────────────── */
      .model-preview {
        background    : #f5f5f7;
        border        : 1px solid #e8e8ed;
        border-radius : 8px;
        padding       : 12px;
        font-family   : 'JetBrains Mono', monospace;
        font-size     : 10px;
        color         : #0071e3;
        white-space   : pre;
        overflow-x    : auto;
        max-height    : 200px;
        overflow-y    : auto;
        line-height   : 1.7;
      }

      /* ── KPI cards ───────────────────────────────────────────── */
      .kpi-row {
        display      : flex;
        gap          : 12px;
        margin-bottom: 20px;
      }
      .kpi-card {
        flex          : 1;
        background    : #ffffff;
        border        : 1px solid #e8e8ed;
        border-radius : 14px;
        padding       : 18px 16px;
        text-align    : center;
        box-shadow    : 0 1px 4px rgba(0,0,0,.05);
        transition    : box-shadow .2s;
      }
      .kpi-card:hover { box-shadow: 0 4px 12px rgba(0,0,0,.08); }
      .kpi-card.highlight {
        border-color : #cce0f5;
        background   : #f0f7ff;
      }
      .kpi-value {
        font-family  : 'Inter', sans-serif;
        font-size    : 28px;
        font-weight  : 600;
        color        : #1d1d1f;
        line-height  : 1;
        margin-bottom: 6px;
        letter-spacing: -1px;
      }
      .kpi-card.highlight .kpi-value { color: #0071e3; }
      .kpi-label {
        font-family    : 'Inter', sans-serif;
        font-size      : 10px;
        font-weight    : 500;
        color          : #86868b;
        letter-spacing : .3px;
        text-transform : uppercase;
      }

      /* ── Results / plot / log wraps ──────────────────────────── */
      .results-wrap, .plot-wrap {
        background    : #ffffff;
        border        : 1px solid #e8e8ed;
        border-radius : 14px;
        padding       : 20px 22px;
        margin-bottom : 18px;
        box-shadow    : 0 1px 4px rgba(0,0,0,.04);
      }
      .results-wrap .section-label,
      .plot-wrap .section-label { margin-bottom: 16px; }

      /* ── DataTables ──────────────────────────────────────────── */
      table.dataTable { border-collapse: collapse !important; width: 100% !important; }
      table.dataTable thead th {
        background    : #f5f5f7 !important;
        color         : #86868b !important;
        font-family   : 'Inter', sans-serif !important;
        font-size     : 11px !important;
        font-weight   : 600 !important;
        border-bottom : 1px solid #e8e8ed !important;
        padding       : 10px 14px !important;
        letter-spacing: .3px;
        text-transform: uppercase;
      }
      table.dataTable tbody td {
        background   : transparent !important;
        color        : #1d1d1f !important;
        font-family  : 'Inter', sans-serif !important;
        font-size    : 13px !important;
        border-bottom: 1px solid #f5f5f7 !important;
        padding      : 10px 14px !important;
      }
      table.dataTable tbody tr:hover td {
        background : #f9f9fb !important;
      }
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_length label,
      .dataTables_wrapper .dataTables_filter label {
        font-family : 'Inter', sans-serif !important;
        font-size   : 12px !important;
        color       : #86868b !important;
      }
      .dataTables_wrapper .dataTables_filter input {
        background    : #f5f5f7 !important;
        border        : 1px solid #e0e0e5 !important;
        color         : #1d1d1f !important;
        border-radius : 8px !important;
        font-family   : 'Inter', sans-serif !important;
        padding       : 5px 10px !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button {
        font-family : 'Inter', sans-serif !important;
        font-size   : 12px !important;
        color       : #86868b !important;
        background  : transparent !important;
        border      : 1px solid transparent !important;
        border-radius: 6px !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current {
        background    : #f0f7ff !important;
        color         : #0071e3 !important;
        border-color  : #cce0f5 !important;
      }

      /* ── Log console ─────────────────────────────────────────── */
      .log-wrap {
        background    : #f5f5f7;
        border        : 1px solid #e8e8ed;
        border-radius : 14px;
        padding       : 14px 16px;
        font-family   : 'JetBrains Mono', monospace;
        font-size     : 11px;
        color         : #86868b;
        white-space   : pre-wrap;
        min-height    : 56px;
        max-height    : 130px;
        overflow-y    : auto;
        line-height   : 1.8;
        margin-bottom : 18px;
      }
      .log-wrap .log-ok    { color: #30a64a; }
      .log-wrap .log-info  { color: #0071e3; }
      .log-wrap .log-warn  { color: #d97800; }

      /* ── Idle / waiting state ────────────────────────────────── */
      .idle-msg {
        display        : flex;
        flex-direction : column;
        align-items    : center;
        justify-content: center;
        height         : 320px;
        gap            : 14px;
        color          : #c7c7cc;
        font-family    : 'Inter', sans-serif;
        font-size      : 13px;
        font-weight    : 400;
      }
      .idle-msg svg { opacity: .5; }

      /* ── Reference footer ────────────────────────────────────── */
      .ref-box {
        font-family : 'Inter', sans-serif;
        font-size   : 11px;
        color       : #aeaeb2;
        border-top  : 1px solid #f0f0f2;
        padding-top : 14px;
        margin-top  : 6px;
        line-height : 1.7;
      }

      /* ── Scrollbars ──────────────────────────────────────────── */
      ::-webkit-scrollbar { width: 5px; height: 5px; }
      ::-webkit-scrollbar-track { background: transparent; }
      ::-webkit-scrollbar-thumb { background: #d1d1d6; border-radius: 10px; }
      ::-webkit-scrollbar-thumb:hover { background: #aeaeb2; }

    "))
  ),

  # ── Header ────────────────────────────────────────────────────────────────
  div(class = "app-header",
    div(
      h1("CFA Sample Size Simulator"),
      div(class = "subtitle", "Monte Carlo Simulation via simsem · lavaan")
    ),
    span(class = "tag-wolf", "HC Encabo")
  ),

  # ── Body ──────────────────────────────────────────────────────────────────
  div(class = "main-wrap",

    # ── Sidebar ──────────────────────────────────────────────────────────
    div(class = "sidebar-panel",

      # — Model structure —
      div(class = "sidebar-card",
        div(class = "section-label", "Model Structure"),
        numericInput("n_factors", "Number of Factors",
                     value = 5, min = 1, max = 10, step = 1),
        div(id = "items_inputs")    # Dynamically rendered
      ),

      # — Parameters —
      div(class = "sidebar-card",
        div(class = "section-label", "Population Parameters"),
        sliderInput("loading_value", "Factor Loading (uniform)",
                    min = 0.30, max = 0.90, value = 0.50, step = 0.05),
        sliderInput("factor_cor", "Factor Correlation (all pairs)",
                    min = 0.00, max = 0.70, value = 0.30, step = 0.05),
        div(class = "form-group",
          tags$label("Residual Variance"),
          div(id = "residual_display",
              style = "font-family:'IBM Plex Mono',monospace; font-size:13px;
                       color:#6ab0ff; padding:6px 10px; background:#0d0f14;
                       border:1px solid #253045; border-radius:3px;",
              "0.750")
        )
      ),

      # — Simulation settings —
      div(class = "sidebar-card",
        div(class = "section-label", "Simulation Settings"),
        selectInput("n_range_from", "N range — From",
                    choices = seq(50, 300, 50), selected = 100),
        selectInput("n_range_to",   "N range — To",
                    choices = seq(100, 700, 50), selected = 400),
        selectInput("n_step",  "Step",
                    choices = c(25, 50, 100), selected = 50),
        selectInput("nRep", "Replications per N",
                    choices = c("200 (first pass)" = 200, "500 (recommended)" = 500,
                                "1000 (publication)" = 1000),
                    selected = 200),
        numericInput("seed", "Random Seed",
                     value = 2024, min = 1, max = 99999, step = 1)
      ),

      # — Run —
      actionButton("run_sim", "▶  RUN SIMULATION", class = "btn"),

      # — Model preview —
      div(class = "sidebar-card",
        div(class = "section-label", "Population Model Preview"),
        div(id = "model_preview", class = "model-preview", "—")
      )

    ), # end sidebar

    # ── Main content ──────────────────────────────────────────────────────
    div(class = "content-panel",

      uiOutput("results_ui")

    )
  ) # end main-wrap
)
