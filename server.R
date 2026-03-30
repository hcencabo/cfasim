# =============================================================================
# server.R — Reactive server logic
# =============================================================================
# Performance strategy
# ─────────────────────────────────────────────────────────────────────────────
# 1. PARALLEL:  All candidate-N simulations are dispatched simultaneously via
#    furrr::future_map_dfr() inside a single future({}) block. Each worker
#    runs run_one_n() independently — no shared state, no locks.
#
# 2. NON-BLOCKING:  The future block returns a Promise. The UI remains
#    responsive while workers run. Results arrive via then() callback.
#
# 3. ZERO LOOP OVERHEAD: No per-iteration incProgress / log_add / reactiveValues
#    writes inside the computation. All UI updates happen once at the end.
#
# 4. DETERMINISTIC:  Each worker seeds as (user_seed + N), giving different
#    but reproducible streams across N values regardless of worker order.
# =============================================================================

server <- function(input, output, session) {

  # ── Derived: items_per_factor (vector of integers) ──────────────────────
  items_per_factor <- reactive({
    n <- req(input$n_factors)
    sapply(seq_len(n), function(f) {
      val <- input[[paste0("items_f", f)]]
      if (is.null(val)) 4L else as.integer(val)
    })
  })

  # ── Derived: residual variance (auto from loading) ───────────────────────
  residual_var <- reactive({
    lv <- req(input$loading_value)
    round(1 - lv^2, 6)
  })

  # ── Update residual display ───────────────────────────────────────────────
  observe({
    rv <- residual_var()
    shinyjs::html("residual_display",
                  sprintf("%.4f  (= 1 − %.2f²)", rv, input$loading_value))
  })

  # ── Render dynamic items-per-factor inputs ────────────────────────────────
  observe({
    n          <- req(input$n_factors)
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

  # ── Live model preview ────────────────────────────────────────────────────
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

  # ── Validate N range ──────────────────────────────────────────────────────
  n_range_valid <- reactive({
    as.integer(input$n_range_from) < as.integer(input$n_range_to) &&
      as.integer(input$n_step) > 0
  })
  observe({ shinyjs::toggleState("run_sim", condition = n_range_valid()) })

  # ── Simulation state ──────────────────────────────────────────────────────
  sim_state <- reactiveValues(
    running  = FALSE,
    results  = NULL,
    log_msgs = character(0),
    rec_N    = NA,
    t_start  = NULL
  )

  # ── Run simulation ────────────────────────────────────────────────────────
  observeEvent(input$run_sim, {
    req(n_range_valid())

    # Snapshot all inputs immediately (before async)
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

    pop_model  <- build_population_model(n_factors, ipf, lv, rv, fc)
    ana_model  <- build_analysis_model(n_factors, ipf)

    # Reset state and lock UI
    sim_state$running  <- TRUE
    sim_state$results  <- NULL
    sim_state$rec_N    <- NA
    sim_state$t_start  <- proc.time()[["elapsed"]]
    sim_state$log_msgs <- c(
      sprintf('<span class="log-info">Workers  : %d parallel processes</span>',
              n_workers),
      sprintf('<span class="log-info">Config   : %d factors | %d items | loading=%.2f | corr=%.2f</span>',
              n_factors, sum(ipf), lv, fc),
      sprintf('<span class="log-info">Candidate N : %s  |  Reps each: %d</span>',
              paste(cand_N, collapse = ", "), nRep),
      '<span class="log-warn">Dispatching all N values to workers simultaneously…</span>'
    )
    shinyjs::disable("run_sim")

    # ── Async parallel block ────────────────────────────────────────────────
    # future() runs in a background process — UI stays fully responsive.
    # furrr::future_map_dfr distributes cand_N across workers in parallel.
    future_promise({

      furrr::future_map_dfr(
        cand_N,
        run_one_n,
        pop_model = pop_model,
        ana_model = ana_model,
        nRep      = nRep,
        seed_val  = seed_val,
        .options  = furrr::furrr_options(seed = TRUE)
      )

    }, seed = TRUE) %...>% (function(tbl) {

      # ── Back on the main thread — update reactive state once ──────────────
      tbl <- tbl[order(tbl$N), ]

      adequate_rows <- tbl$Adequate == "Yes"
      rec_N <- if (any(adequate_rows, na.rm = TRUE)) {
        min(tbl$N[adequate_rows], na.rm = TRUE)
      } else {
        NA
      }

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

      # ── Error handler ─────────────────────────────────────────────────────
      sim_state$log_msgs <- c(sim_state$log_msgs,
        sprintf('<span class="log-warn">✗ Simulation error: %s</span>',
                conditionMessage(err))
      )
      sim_state$running <- FALSE
      shinyjs::enable("run_sim")

    })

    # Return NULL immediately — server loop continues, UI stays live
    NULL
  })

  # ── Results UI ────────────────────────────────────────────────────────────
  output$results_ui <- renderUI({

    if (sim_state$running) {
      return(tagList(
        div(class = "idle-msg",
          tags$svg(
            width="48", height="48", viewBox="0 0 48 48",
            fill="none", xmlns="http://www.w3.org/2000/svg",
            tags$circle(cx="24", cy="24", r="20", stroke="#0071e3",
                        `stroke-width`="2.5", `stroke-dasharray`="94.2",
                        `stroke-dashoffset`="70",
                        style="animation:spin .9s linear infinite;
                               transform-origin:center;")
          ),
          span("Workers running in parallel…"),
          # Show live log while computing
          div(class = "log-wrap",
              style = "width:480px; margin-top:10px;",
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

    tbl   <- sim_state$results
    rec_N <- sim_state$rec_N

    n_adequate <- sum(tbl$Adequate == "Yes", na.rm = TRUE)
    best_power <- if (any(!is.na(tbl$Min_Power))) max(tbl$Min_Power, na.rm = TRUE) else NA
    best_cfi   <- if (any(!is.na(tbl$Mean_CFI)))  max(tbl$Mean_CFI,  na.rm = TRUE) else NA

    tagList(

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
          div(class = "kpi-label", "N values meeting criteria")
        )
      ),

      div(class = "results-wrap",
        div(class = "section-label", "SIMULATION RESULTS"),
        DT::dataTableOutput("result_table")
      ),

      div(class = "plot-wrap",
        div(class = "section-label", "POWER & FIT TRAJECTORIES"),
        plotOutput("result_plot", height = "340px")
      ),

      div(class = "results-wrap",
        div(class = "section-label", "SIMULATION LOG"),
        div(class = "log-wrap",
          HTML(paste(sim_state$log_msgs, collapse = "\n"))
        )
      ),

      div(class = "ref-box",
        "Wolf, E. J., Harrington, K. M., Clark, S. L., & Miller, M. W. (2013).",
        "Sample size requirements for structural equation models: An evaluation",
        "of power, bias, and solution propriety.",
        tags$em("Educational and Psychological Measurement, 73"), "(6), 913–934."
      )
    )
  })

  # ── Table ─────────────────────────────────────────────────────────────────
  output$result_table <- DT::renderDataTable({
    req(sim_state$results)

    DT::datatable(
      sim_state$results,
      colnames  = c("N", "Min Loading Power", "Mean RMSEA",
                    "Mean CFI", "Conv. Rate", "Adequate?"),
      rownames  = FALSE,
      options   = list(
        pageLength = 15, dom = "tip",
        columnDefs = list(
          list(className = "dt-right",  targets = 0:4),
          list(className = "dt-center", targets = 5)
        )
      )
    ) |>
      DT::formatStyle(
        "Adequate",
        color      = DT::styleEqual(c("Yes", "No", "Error"),
                                    c("#30a64a", "#bf4040", "#d97800")),
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

  # ── Plot ──────────────────────────────────────────────────────────────────
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
      geom_line(aes(y = Mean_CFI,   color = "Mean CFI"),  linewidth = 1.4) +
      geom_point(aes(y = Mean_CFI,  color = "Mean CFI"),  size = 3.5,
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
