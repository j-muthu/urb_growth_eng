#===============================================================================
# SHINY APP - SERVER
#===============================================================================

function(input, output, session) {

  #_____________________________________________________________________________
  # HELPER: title wrapping ####
  #_____________________________________________________________________________

  apply_title_wrap <- function(p_plotly, gg_plot) {
    title_text <- gg_plot$labels$title %||% ""
    wrapped <- stringr::str_wrap(title_text, width = 80)
    n_lines <- length(strsplit(wrapped, "\n")[[1]])
    top_margin <- 60 + (n_lines - 1) * 20
    p_plotly %>% plotly::layout(
      title = list(text = gsub("\n", "<br>", wrapped)),
      margin = list(t = top_margin)
    )
  }

  #_____________________________________________________________________________
  # HELPER: client-side responsive layout via ResizeObserver ####
  #_____________________________________________________________________________

  make_responsive <- function(p_plotly) {
    htmlwidgets::onRender(p_plotly, "
      function(el, x) {
        function adjust() {
          var w = el.getBoundingClientRect().width;
          var update = {};
          if (w < 600) {
            update.legend = {orientation: 'h', x: 0, y: -0.2};
            update['xaxis.tickangle'] = -45;
          } else {
            update.legend = {orientation: 'v', x: 1.02, y: 1};
            update['xaxis.tickangle'] = 0;
          }
          Plotly.relayout(el, update);
        }
        new ResizeObserver(adjust).observe(el);
      }
    ")
  }

  #_____________________________________________________________________________
  # HELPER: add reference rate annotations to plotly object ####
  #_____________________________________________________________________________

  add_ref_annotations <- function(p_plotly, ref_rates) {
    annotations <- lapply(seq_len(nrow(ref_rates)), function(i) {
      list(
        x = ref_rates$rate[i],
        y = 1,
        yref = "paper",
        text = ref_rates$label[i],
        showarrow = FALSE,
        textangle = -90,
        xanchor = "left",
        yanchor = "top",
        font = list(size = 10, color = "gray40")
      )
    })
    p_plotly %>% plotly::layout(annotations = annotations)
  }

  #_____________________________________________________________________________
  # RESET BUTTON ####
  #_____________________________________________________________________________

  observeEvent(input$reset_params, {
    updateSliderInput(session, "gamma",  value = PARAMS_CENTRAL$gamma)
    updateSliderInput(session, "theta",  value = PARAMS_CENTRAL$theta)
    updateSliderInput(session, "sigma",  value = PARAMS_CENTRAL$sigma)
    updateSliderInput(session, "beta",   value = PARAMS_CENTRAL$beta)
    updateSliderInput(session, "lambda", value = PARAMS_CENTRAL$lambda)
  })

  #_____________________________________________________________________________
  # RUN SWEEP ####
  #_____________________________________________________________________________

  sweep_results <- reactiveVal(default_sweep_results)

  observeEvent(input$run_sweep, {
    params <- list(
      gamma  = input$gamma,
      theta  = input$theta,
      sigma  = input$sigma,
      beta   = input$beta,
      lambda = input$lambda
    )

    # Check if params match central estimates (pre-computed)
    is_central <- all(mapply(
      function(a, b) isTRUE(all.equal(a, b)),
      params, PARAMS_CENTRAL
    ))

    if (is_central) {
      if (input$city_set == "all") {
        sweep_results(default_sweep_results)
      } else {
        cs <- input$city_set
        sweep_results(list(
          agg = default_sweep_results$agg %>%
            filter(city_set == cs),
          city_income = default_sweep_results$city_income %>%
            filter(city_set == cs),
          city_cons = default_sweep_results$city_cons %>%
            filter(city_set == cs),
          params = params,
          city_set = cs
        ))
      }
      return()
    }

    sets_to_run <- if (input$city_set == "all") {
      names(cities_sets)
    } else {
      input$city_set
    }
    n_sets <- length(sets_to_run)
    n_total <- n_sets * length(rate_sequence)
    completed <- 0

    withProgress(
      message = "Running counterfactual sweep...",
      value = 0, {
      all_results <- map(sets_to_run, function(cs) {
        result <- run_sweep(
          city_set_name = cs,
          cities_in_cf = cities_sets[[cs]],
          rate_sequence = rate_sequence,
          pop_totals = pop_totals,
          params = params,
          city_data = city_data,
          progress_callback = function(r, total) {
            completed <<- completed + 1
            incProgress(
              1 / n_total,
              detail = sprintf("%s (%d/%d)", cs, r, total)
            )
          }
        )
        result
      })
      names(all_results) <- sets_to_run
    })

    sweep_results(list(
      agg = bind_rows(map(all_results, "agg")),
      city_income = bind_rows(map(all_results, "city_income")),
      city_cons = bind_rows(map(all_results, "city_cons")),
      params = params,
      city_set = input$city_set
    ))
  })

  #_____________________________________________________________________________
  # AGGREGATE CHARTS ####
  #_____________________________________________________________________________

  render_agg_chart <- function(y_var, ylabel, title_metric) {
    res <- sweep_results()
    cs <- res$city_set

    if (cs == "all") {
      p <- create_agg_multi_set_chart(
        res$agg, y_var, ylabel, title_metric,
        reference_rates, res$params
      )
    } else {
      p <- create_line_chart(
        res$agg, cs, y_var, ylabel, title_metric,
        reference_rates, res$params
      )
    }

    ggplotly(p, tooltip = "text") %>%
      plotly::layout(hovermode = "x unified") %>%
      apply_title_wrap(p) %>%
      add_ref_annotations(reference_rates) %>%
      make_responsive()
  }

  output$plot_newcomer_cons <- renderPlotly({
    render_agg_chart("pct_chg_newcomer_cons",
                     "% change in consumption",
                     "Change in newcomer consumption")
  })

  output$plot_national_income <- renderPlotly({
    render_agg_chart("pct_chg_national_income_pc",
                     "% change in income per capita",
                     "Change in national income per capita")
  })

  output$plot_incumbent_cons <- renderPlotly({
    render_agg_chart("pct_chg_cons_incumb",
                     "% change in consumption",
                     "Change in D&P incumbent consumption per capita")
  })

  output$plot_social_cons <- renderPlotly({
    render_agg_chart("pct_chg_cons_social",
                     "% change in consumption",
                     "Change in social planner consumption per capita")
  })

  #_____________________________________________________________________________
  # CITY-LEVEL CHARTS ####
  #_____________________________________________________________________________

  make_city_plotly <- function(city_data_col, city_set_name,
                               y_var, ylabel,
                               title_metric, params) {
    res <- sweep_results()
    p <- create_city_line_chart(
      res[[city_data_col]], city_set_name,
      y_var, ylabel, title_metric,
      reference_rates, params
    )
    ggplotly(p, tooltip = "text") %>%
      plotly::layout(hovermode = "x unified") %>%
      apply_title_wrap(p) %>%
      add_ref_annotations(reference_rates) %>%
      make_responsive()
  }

  # --- Dynamic UI: sub-tabs when "all", single chart otherwise ---

  output$city_income_ui <- renderUI({
    res <- sweep_results()
    if (res$city_set == "all") {
      do.call(tabsetPanel, lapply(
        names(cities_sets), function(cs) {
          tabPanel(
            city_set_labels[[cs]],
            br(),
            plotlyOutput(
              paste0("city_income_", cs),
              height = "550px"
            )
          )
        }
      ))
    } else {
      plotlyOutput(
        "plot_city_income_single", height = "550px"
      )
    }
  })

  output$city_cons_ui <- renderUI({
    res <- sweep_results()
    if (res$city_set == "all") {
      do.call(tabsetPanel, lapply(
        names(cities_sets), function(cs) {
          tabPanel(
            city_set_labels[[cs]],
            br(),
            plotlyOutput(
              paste0("city_cons_", cs),
              height = "550px"
            )
          )
        }
      ))
    } else {
      plotlyOutput(
        "plot_city_cons_single", height = "550px"
      )
    }
  })

  # --- Eagerly register renderPlotly for each city set ---

  lapply(names(cities_sets), function(cs) {
    output[[paste0("city_income_", cs)]] <- renderPlotly({
      res <- sweep_results()
      req(res$city_set == "all")
      make_city_plotly(
        "city_income", cs, "pct_chg_y",
        "% change in income per capita",
        "Change in city income per capita",
        res$params
      )
    })

    output[[paste0("city_cons_", cs)]] <- renderPlotly({
      res <- sweep_results()
      req(res$city_set == "all")
      make_city_plotly(
        "city_cons", cs, "pct_chg_c",
        "% change in consumption",
        "Change in incumbent consumption by city",
        res$params
      )
    })
  })

  # --- Single city set outputs ---

  output$plot_city_income_single <- renderPlotly({
    res <- sweep_results()
    req(res$city_set != "all")
    make_city_plotly(
      "city_income", res$city_set, "pct_chg_y",
      "% change in income per capita",
      "Change in city income per capita",
      res$params
    )
  })

  output$plot_city_cons_single <- renderPlotly({
    res <- sweep_results()
    req(res$city_set != "all")
    make_city_plotly(
      "city_cons", res$city_set, "pct_chg_c",
      "% change in consumption",
      "Change in incumbent consumption by city",
      res$params
    )
  })
}
