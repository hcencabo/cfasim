
# server.R — Reactive server logic


server <- function(input, output, session) {

  # Derived: items_per_factor 
  items_per_factor <- reactive({
    n <- req(input$n_factors)
    sapply(seq_len(n), function(f) {
      val <- input[[paste0("items_f", f)]]
      if (is.null(val)) 4L else as.integer(val)
    })
  })

  # Derived: residual variance 
  residual_var <- reactive({
    lv <- req(input$loading_value)
    round(1 - lv^2, 6)
  })

  # Update residual display 
  observe({
    rv <- residual_var()
    shinyjs::html("residual_display",
                  sprintf("%.4f  (= 1 − %.2f²)", rv, input$loading_value))
  })

  # Derived: active bias flags 
  active_flags <- reactive({
    get_bias_flags(
      loading_value    = req(input$loading_value),
      factor_cor       = req(input$factor_cor),
      n_factors        = req(input$n_factors),
      items_per_factor = items_per_factor()
    )
  })

  # Update bias warning banner 
  observe({
    flags <- active_flags()
    msgs  <- character(0)

    if (flags["cor_boundary"])
      msgs <- c(msgs,
        if (input$factor_cor >= 0.50)
          "⚠ Strong factor correlation (≥ .50) — correlation bias will be checked"
        else
          "⚠ Near-orthogonal factors (≤ .10) — correlation bias will be checked")

    if (flags["weak_loading"])
      msgs <- c(msgs,
        "⚠ Weak loadings (≤ .40) — loading bias will be checked (below Wolf et al. range)")

    if (flags["id_boundary"])
      msgs <- c(msgs,
        "⚠ 3-indicator factor detected — convergence threshold raised to ≥ 98%")

    if (length(msgs) > 0) {
      shinyjs::show("bias_banner")
      shinyjs::html("bias_banner_text",
                    paste(msgs, collapse = "<br>"))
    } else {
      shinyjs::hide("bias_banner")
    }
  })

  # Render dynamic items-per-factor inputs 
  observe({
    n <- req(input$n_factors)
    output_html <- lapply(seq_len(n), function(f) {
      numericInput(paste0("items_f", f),
                   label = paste0("Items — Factor ", f),
                   value = 4, min = 2, max = 15, step = 1)
    })
    removeUI("#items_inputs > *", multiple = TRUE, immediate = TRUE,
             session = session)
    insertUI("#items_inputs", "beforeEnd", ui = tagList(output_html),
             immediate = TRUE, session = session)
  })

  # Live model preview 
  observe({
    n   <- req(input$n_factors)
    ipf <- items_per_factor()
    lv  <- req(input$loading_value)
    rv  <- residual_var()
    fc  <- req(input$factor_cor)
    tryCatch(
      shinyjs::html("model_preview",
                    build_population_model(n, ipf, lv, rv, fc)),
      error = function(e)
        shinyjs::html("model_preview", paste("Error:", e$message))
    )
  })

  # Validate N range 
  n_range_valid <- reactive({
    as.integer(input$n_range_from) < as.integer(input$n_range_to) &&
      as.integer(input$n_step) > 0
  })
  observe({ shinyjs::toggleState("run_sim", condition = n_range_valid()) })

  # Simulation state 
  sim_state <- reactiveValues(
    running     = FALSE,
    results     = NULL,
    log_msgs    = character(0),
    rec_N       = NA,
    t_start     = NULL,
    bias_flags  = NULL,
    nRep_used   = NULL
  )

  # Run simulation 
  observeEvent(input$run_sim, {
    req(n_range_valid())

    # Snapshot all inputs before async hand-off
    n_factors  <- isolate(input$n_factors)
    ipf        <- isolate(items_per_factor())
    lv         <- isolate(input$loading_value)
    rv         <- isolate(residual_var())
    fc         <- isolate(input$factor_cor)
    nRep       <- as.integer(isolate(input$nRep))
    seed_val   <- isolate(input$seed)
    cand_N     <- seq(as.integer(isolate(input$n_range_from)),
                      as.integer(isolate(input$n_range_to)),
                      by = as.integer(isolate(input$n_step)))
    flags      <- isolate(active_flags())

    pop_model  <- build_population_model(n_factors, ipf, lv, rv, fc)
    ana_model  <- build_analysis_model(n_factors, ipf)

    # Build log prefix including active bias checks
    bias_check_desc <- {
      active <- names(flags)[flags]
      labels <- c(
        cor_boundary = "factor correlation bias",
        weak_loading = "loading bias",
        id_boundary  = "convergence ≥ 98%"
      )
      if (length(active) > 0)
        paste0("Bias checks : ", paste(labels[active], collapse = " | "))
      else
        "Bias checks : none triggered"
    }

    # Warn if nRep < 1000 and bias checks are active
    bias_rep_warn <- if (any(flags) && nRep < 1000)
      sprintf('<span class="log-warn">⚠ Bias checks active but nRep = %d — increase to 1000 for reliable bias estimates</span>', nRep)
    else NULL

    # Reset state
    sim_state$running    <- TRUE
    sim_state$results    <- NULL
    sim_state$rec_N      <- NA
    sim_state$t_start    <- proc.time()[["elapsed"]]
    sim_state$bias_flags <- flags
    sim_state$nRep_used  <- nRep
    sim_state$log_msgs   <- c(
      sprintf('<span class="log-info">Workers   : %d parallel processes</span>',
              n_workers),
      sprintf('<span class="log-info">Config    : %d factors | %d items | loading=%.2f | corr=%.2f</span>',
              n_factors, sum(ipf), lv, fc),
      sprintf('<span class="log-info">Candidate N : %s  |  Reps: %d</span>',
              paste(cand_N, collapse = ", "), nRep),
      sprintf('<span class="log-info">%s</span>', bias_check_desc),
      bias_rep_warn,
      '<span class="log-warn">Dispatching all N values to workers simultaneously…</span>'
    )
    sim_state$log_msgs <- sim_state$log_msgs[!sapply(sim_state$log_msgs, is.null)]

    shinyjs::disable("run_sim")

    # Async parallel block 
    future_promise({

      furrr::future_map_dfr(
        cand_N,
        run_one_n,
        pop_model     = pop_model,
        ana_model     = ana_model,
        nRep          = nRep,
        seed_val      = seed_val,
        bias_flags    = flags,
        loading_value = lv,
        factor_cor    = fc,
        n_factors     = n_factors,
        .options      = furrr::furrr_options(seed = TRUE)
      )

    }, seed = TRUE) %...>% (function(tbl) {

      tbl <- tbl[order(tbl$N), ]

      adequate_rows <- tbl$Adequate == "Yes"
      rec_N <- if (any(adequate_rows, na.rm = TRUE)) {
        min(tbl$N[adequate_rows], na.rm = TRUE)
      } else NA

      elapsed <- round(proc.time()[["elapsed"]] - sim_state$t_start, 1)

      sim_state$log_msgs <- c(sim_state$log_msgs,
        sprintf('<span class="log-ok">✓ Completed in %.1f s  |  %d/%d N values adequate</span>',
                elapsed,
                sum(adequate_rows, na.rm = TRUE),
                nrow(tbl)),
        if (!is.na(rec_N))
          sprintf('<span class="log-ok">✓ Recommended N = %d</span>', rec_N)
        else
          '<span class="log-warn">✗ No N met all criteria — extend the N range</span>'
      )

      sim_state$results <- tbl
      sim_state$rec_N   <- rec_N
      sim_state$running <- FALSE
      shinyjs::enable("run_sim")

    }) %...!% (function(err) {

      sim_state$log_msgs <- c(sim_state$log_msgs,
        sprintf('<span class="log-warn">✗ Simulation error: %s</span>',
                conditionMessage(err))
      )
      sim_state$running <- FALSE
      shinyjs::enable("run_sim")

    })

    NULL
  })

  #  Results UI 
  output$results_ui <- renderUI({

    if (sim_state$running) {
      return(tagList(
        div(class = "idle-msg",
          tags$svg(width="48", height="48", viewBox="0 0 48 48",
            fill="none", xmlns="http://www.w3.org/2000/svg",
            tags$circle(cx="24", cy="24", r="20", stroke="#0071e3",
                        `stroke-width`="2.5", `stroke-dasharray`="94.2",
                        `stroke-dashoffset`="70",
                        style="animation:spin .9s linear infinite;
                               transform-origin:center;")
          ),
          span("Workers running in parallel…"),
          div(class = "log-wrap",
              style = "width:520px; margin-top:12px;",
              HTML(paste(sim_state$log_msgs, collapse = "\n")))
        ),
        tags$style("@keyframes spin{to{transform:rotate(360deg)}}")
      ))
    }

    if (is.null(sim_state$results)) {
      return(
        div(class = "idle-msg",
          tags$svg(width="56", height="56", viewBox="0 0 56 56",
            fill="none", xmlns="http://www.w3.org/2000/svg",
            tags$rect(x="8",  y="8",  width="16", height="16",
                      rx="3", fill="#e8e8ed"),
            tags$rect(x="32", y="8",  width="16", height="16",
                      rx="3", fill="#e8e8ed"),
            tags$rect(x="8",  y="32", width="16", height="16",
                      rx="3", fill="#e8e8ed"),
            tags$rect(x="32", y="32", width="16", height="16",
                      rx="3", fill="#d1d1d6")
          ),
          span("Configure your model in the sidebar, then click Run Simulation")
        )
      )
    }

    tbl    <- sim_state$results
    rec_N  <- sim_state$rec_N
    flags  <- sim_state$bias_flags
    nRep_u <- sim_state$nRep_used

    n_adequate <- sum(tbl$Adequate == "Yes", na.rm = TRUE)
    best_power <- if (any(!is.na(tbl$Min_Power))) max(tbl$Min_Power, na.rm = TRUE) else NA
    best_cfi   <- if (any(!is.na(tbl$Mean_CFI)))  max(tbl$Mean_CFI,  na.rm = TRUE) else NA

    # Build bias check summary badge shown above the table
    bias_summary_ui <- if (!is.null(flags) && any(flags)) {
      active <- names(flags)[flags]
      badge_labels <- c(
        cor_boundary = "Factor Correlation Bias",
        weak_loading = "Loading Bias (< .50)",
        id_boundary  = "Convergence ≥ 98%"
      )
      rep_note <- if (!is.null(nRep_u) && nRep_u < 1000)
        tags$span(style = "color:#d97800; font-size:11px; margin-left:8px;",
                  sprintf("⚠ nRep = %d — bias estimates are approximate", nRep_u))
      else NULL

      div(class = "bias-summary-bar",
        span(class = "bias-bar-title", "Active bias checks:"),
        lapply(active, function(f)
          span(class = "bias-badge", badge_labels[f])
        ),
        rep_note
      )
    } else {
      div(class = "bias-summary-bar bias-bar-none",
        span(class = "bias-bar-title",
             "No bias checks triggered for current parameters")
      )
    }

    tagList(

      # KPI row
      div(class = "kpi-row",
        div(class = "kpi-card highlight",
          div(class = "kpi-value",
              if (!is.na(rec_N)) as.character(rec_N) else "—"),
          div(class = "kpi-label", "Recommended N")
        ),
        div(class = "kpi-card",
          div(class = "kpi-value",
              if (!is.na(best_power)) sprintf("%.3f", best_power) else "—"),
          div(class = "kpi-label", "Max Min. Power")
        ),
        div(class = "kpi-card",
          div(class = "kpi-value",
              if (!is.na(best_cfi)) sprintf("%.3f", best_cfi) else "—"),
          div(class = "kpi-label", "Max Mean CFI")
        ),
        div(class = "kpi-card",
          div(class = "kpi-value", as.character(n_adequate)),
          div(class = "kpi-label", "N values adequate")
        )
      ),

      # Results table
      div(class = "results-wrap",
        div(class = "section-label", "SIMULATION RESULTS"),
        bias_summary_ui,
        DT::dataTableOutput("result_table")
      ),

      # Plot
      div(class = "plot-wrap",
        div(class = "section-label", "POWER & FIT TRAJECTORIES"),
        plotOutput("result_plot", height = "340px")
      ),

      # Log
      div(class = "results-wrap",
        div(class = "section-label", "SIMULATION LOG"),
        div(class = "log-wrap",
          HTML(paste(sim_state$log_msgs, collapse = "\n"))
        )
      ),

      # Reference
      div(class = "ref-box",
        "Wolf, E. J., Harrington, K. M., Clark, S. L., & Miller, M. W. (2013).",
        "Sample size requirements for structural equation models: An evaluation",
        "of power, bias, and solution propriety.",
        tags$em("Educational and Psychological Measurement, 73"), "(6), 913–934."
      )
    )
  })

  # Table 
  output$result_table <- DT::renderDataTable({
    req(sim_state$results)
    tbl   <- sim_state$results
    flags <- sim_state$bias_flags

    # Show bias columns only when at least one flag is active
    show_bias <- !is.null(flags) && any(flags)

    # Decide columns to display
    display_cols <- if (show_bias) {
      c("N", "Min_Power", "Mean_RMSEA", "Mean_CFI",
        "Conv_Rate", "Loading_Bias", "Cor_Bias", "Adequate")
    } else {
      c("N", "Min_Power", "Mean_RMSEA", "Mean_CFI", "Conv_Rate", "Adequate")
    }

    col_labels <- if (show_bias) {
      c("N", "Min Power", "Mean RMSEA", "Mean CFI",
        "Conv. Rate", "Load. Bias", "Cor. Bias", "Adequate?")
    } else {
      c("N", "Min Power", "Mean RMSEA", "Mean CFI", "Conv. Rate", "Adequate?")
    }

    tbl_show <- tbl[, display_cols, drop = FALSE]
    n_num    <- length(display_cols) - 1L   # all except last (Adequate)

    DT::datatable(
      tbl_show,
      colnames  = col_labels,
      rownames  = FALSE,
      options   = list(
        pageLength = 15, dom = "tip",
        columnDefs = list(
          list(className = "dt-right",  targets = 0:(n_num - 1)),
          list(className = "dt-center", targets = n_num)
        )
      )
    ) |>
      DT::formatStyle(
        "Adequate",
        color = DT::styleEqual(
          c("Yes", "No", "Error"),
          c("#30a64a", "#bf4040", "#d97800")
        ),
        fontWeight = "600"
      ) |>
      DT::formatStyle(
        "Min_Power",
        background = DT::styleInterval(c(.60, .80),
                                       c("#fff0f0", "#fffbf0", "#f0fff4")),
        color      = DT::styleInterval(c(.60, .80),
                                       c("#bf4040", "#d97800", "#30a64a"))
      )
  }, server = FALSE)

  # Plot 
  output$result_plot <- renderPlot({
    req(sim_state$results)
    tbl <- sim_state$results[!is.na(sim_state$results$Min_Power), ]
    req(nrow(tbl) > 0)

    p <- ggplot(tbl, aes(x = N)) +
      geom_hline(yintercept = 0.80, color = "#0071e3", linetype = "dashed",
                 linewidth = .5, alpha = .5) +
      geom_hline(yintercept = 0.95, color = "#d97800", linetype = "dashed",
                 linewidth = .5, alpha = .5) +
      geom_col(aes(y = Min_Power), fill = "#e8f2ff", width = 12) +
      geom_line(aes(y = Min_Power, color = "Min Power"), linewidth = 1.4) +
      geom_point(aes(y = Min_Power, color = "Min Power"), size = 3.5,
                 shape = 21, fill = "white", stroke = 1.5) +
      geom_line(aes(y = Mean_CFI,  color = "Mean CFI"),  linewidth = 1.4) +
      geom_point(aes(y = Mean_CFI, color = "Mean CFI"),  size = 3.5,
                 shape = 21, fill = "white", stroke = 1.5) +
      geom_line(aes(y = Mean_RMSEA * 10, color = "RMSEA ×10"),
                linewidth = 1, linetype = "dotted") +
      scale_color_manual(values = c("Min Power" = "#0071e3",
                                    "Mean CFI"  = "#d97800",
                                    "RMSEA ×10" = "#bf4040")) +
      scale_y_continuous(limits = c(0, 1.05),
                         breaks = c(0, .20, .40, .60, .80, 1.0),
                         minor_breaks = NULL) +
      scale_x_continuous(breaks = tbl$N) +
      labs(x = "Sample Size (N)", y = "Value", color = NULL,
           caption = paste("Dashed blue = power threshold (.80)",
                           " |  Dashed amber = CFI threshold (.95)",
                           " |  RMSEA ×10 for visibility")) +
      theme_minimal(base_family = "sans") +
      theme(
        plot.background   = element_rect(fill = "#ffffff", color = NA),
        panel.background  = element_rect(fill = "#ffffff", color = NA),
        panel.grid.major  = element_line(color = "#f0f0f2", linewidth = .5),
        panel.grid.minor  = element_blank(),
        axis.text         = element_text(color = "#86868b", size = 10),
        axis.title        = element_text(color = "#86868b", size = 11),
        legend.background = element_rect(fill = "#f9f9fb", color = "#e8e8ed"),
        legend.text       = element_text(color = "#1d1d1f", size = 11),
        legend.key        = element_rect(fill = "transparent"),
        legend.position   = "bottom",
        plot.caption      = element_text(color = "#aeaeb2", size = 9,
                                         margin = margin(t = 8)),
        axis.text.x       = element_text(angle = 45, hjust = 1)
      )

    adequate_N <- tbl$N[tbl$Adequate == "Yes"]
    if (length(adequate_N) > 0)
      p <- p + annotate("rect",
                        xmin = min(adequate_N) - 15,
                        xmax = max(tbl$N) + 15,
                        ymin = 0, ymax = 1.05,
                        fill = "#0071e3", alpha = .04)
    p

  }, bg = "#ffffff")

}
