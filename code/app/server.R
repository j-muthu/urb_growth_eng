#===============================================================================
# SHINY APP - SERVER
#===============================================================================

function(input, output, session) {

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

  sweep_results <- eventReactive(input$run_sweep, {
    params <- list(
      gamma  = input$gamma,
      theta  = input$theta,
      sigma  = input$sigma,
      beta   = input$beta,
      lambda = input$lambda
    )

    sets_to_run <- if (input$city_set == "all") names(cities_sets) else input$city_set
    n_sets <- length(sets_to_run)
    n_total <- n_sets * length(rate_sequence)
    completed <- 0

    withProgress(message = "Running counterfactual sweep...", value = 0, {
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
            incProgress(1 / n_total,
                        detail = sprintf("%s (%d/%d)", cs, r, total))
          }
        )
        result
      })
      names(all_results) <- sets_to_run
    })

    list(
      agg = bind_rows(map(all_results, "agg")),
      city_income = bind_rows(map(all_results, "city_income")),
      city_cons = bind_rows(map(all_results, "city_cons")),
      params = params,
      city_set = input$city_set
    )
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
      add_ref_annotations(reference_rates)
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
    render_agg_chart("pct_chg_incumbent_cons",
                     "% change in consumption",
                     "Change in incumbent consumption (aggregate)")
  })

  #_____________________________________________________________________________
  # CITY-LEVEL CHARTS ####
  #_____________________________________________________________________________

  render_city_chart <- function(city_data_col, y_var, ylabel, title_metric) {
    res <- sweep_results()
    cs <- res$city_set
    city_set_to_use <- if (cs == "all") "top_6" else cs

    p <- create_city_line_chart(
      res[[city_data_col]], city_set_to_use, y_var, ylabel,
      title_metric, reference_rates, res$params
    )

    ggplotly(p, tooltip = "text") %>%
      plotly::layout(hovermode = "x unified") %>%
      add_ref_annotations(reference_rates)
  }

  output$plot_city_income <- renderPlotly({
    render_city_chart("city_income", "pct_chg_y",
                      "% change in income per capita",
                      "Change in city income per capita")
  })

  output$plot_city_cons <- renderPlotly({
    render_city_chart("city_cons", "pct_chg_c",
                      "% change in consumption",
                      "Change in incumbent consumption by city")
  })
}
