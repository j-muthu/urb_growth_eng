#===============================================================================
# SHINY APP - UI
#===============================================================================

fluidPage(
  titlePanel("What if England's Planning Regulation was Relaxed?"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      selectInput("city_set", "City set",
                  choices = c("All city sets" = "all",
                              setNames(names(city_set_labels), city_set_labels)),
                  selected = "all"),

      hr(),

      h4("Model Parameters"),

      sliderInput("gamma", "\u03B3 (commuting cost elasticity)",
                  min = PARAM_GAMMA$min, max = PARAM_GAMMA$max,
                  value = PARAM_GAMMA$central, step = 0.001),

      sliderInput("theta", "\u03B8 (congestion elasticity)",
                  min = PARAM_THETA$min, max = PARAM_THETA$max,
                  value = PARAM_THETA$central, step = 0.001),

      sliderInput("sigma", "\u03C3 (short-run agglomeration)",
                  min = PARAM_SIGMA$min, max = PARAM_SIGMA$max,
                  value = PARAM_SIGMA$central, step = 0.001),

      sliderInput("beta", "\u03B2 (learning agglomeration)",
                  min = PARAM_BETA$min, max = PARAM_BETA$max,
                  value = PARAM_BETA$central, step = 0.001),

      sliderInput("lambda", "\u03BB (land share in rural prod.)",
                  min = PARAM_LAMBDA$min, max = PARAM_LAMBDA$max,
                  value = PARAM_LAMBDA$central, step = 0.001),

      hr(),

      actionButton("reset_params", "Reset to Central Estimates",
                    class = "btn-default", width = "100%"),
      br(), br(),
      actionButton("run_sweep", "Run Counterfactual",
                    class = "btn-primary", width = "100%")
    ),

    mainPanel(
      width = 9,

      tabsetPanel(
        id = "chart_tabs",

        tabPanel("National Income",
                 br(),
                 plotlyOutput("plot_national_income", height = "550px")),

        tabPanel("D&P Incumbent Consumption",
                 br(),
                 plotlyOutput("plot_incumbent_cons", height = "550px")),

        tabPanel("Social Planner Consumption",
                 br(),
                 plotlyOutput("plot_social_cons", height = "550px")),

        tabPanel("Newcomer Consumption",
                 br(),
                 plotlyOutput("plot_newcomer_cons", height = "550px")),

        tabPanel("City Income",
                 br(),
                 uiOutput("city_income_ui")),

        tabPanel("City Consumption",
                 br(),
                 uiOutput("city_cons_ui"))
      )
    )
  )
)
