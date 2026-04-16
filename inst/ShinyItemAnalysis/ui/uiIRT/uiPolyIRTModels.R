# Polytomous IRT models - unified tab with model dropdown
# (mirrors the dichotomous models tab structure)

uiPolyIRTModels <- tabPanel(
  "Polytomous models",
  value = "irt_polytomous",
  tabsetPanel(

    # common header -----------------------------------------------------------
    header = tagList(
      h3("Polytomous model"),
      p(
        "Polytomous IRT models extend IRT to items with ordered response categories.",
        "Select a model from the dropdown below. Models differ in their assumptions",
        "about item discrimination and threshold structure."
      ),
      fluidRow(
        column(
          2,
          selectInput(
            inputId = "IRT_poly_model",
            label = "Model",
            choices = c(
              "NRM" = "NRM",
              "GRM" = "GRM",
              "RSM" = "RSM",
              "PCM" = "PCM",
              "GPCM" = "GPCM"
            )
          )
        ),
        column(
          2,
          conditionalPanel(
            condition = "input.IRT_poly_model == 'NRM'",
            selectInput(
              inputId = "IRT_poly_nrm_parametrization",
              label = "Parameterization",
              choices = c(
                "BLIS" = "blis",
                "BLIRT" = "blirt",
                "Bock" = "bock",
                "Thissen et al." = "thissen"
              )
            )
          )
        ),
        column(
          4,
          sliderInput(
            inputId = "IRT_poly_theta_range",
            label = "Latent trait (logit) range for plots",
            min = -10, max = 10, value = c(-6, 6), step = 0.5
          )
        )
      ),
      uiOutput("IRT_poly_model_description_ui"),
      uiOutput("IRT_poly_equation_ui"),
      uiOutput("IRT_poly_model_converged")
    ),

    ## Summary tab -----------------------------------------------------------
    tabPanel("Summary",
      value = "irt_poly_summary",
      fluidRow(
        column(
          12,
          div(
            style = "margin-bottom: 25px;",
            h4("Expected item score curves"),
            plotlyOutput("IRT_poly_summary_expected"),
            downloadButton(
              outputId = "IRT_poly_summary_expected_download",
              label = "Download figure"
            )
          ),
          div(
            style = "margin-bottom: 25px;",
            h4("Category probability curves"),
            p(
              "For category probability curves of individual items, please see the",
              strong("Items"), "subtab.",
              "(Plotting all items at once would result in visual clutter.)"
            )
          ),
          div(
            style = "margin-bottom: 25px;",
            h4("Item information curves"),
            plotlyOutput("IRT_poly_summary_iic"),
            downloadButton(
              outputId = "IRT_poly_summary_iic_download",
              label = "Download figure"
            )
          ),
          div(
            style = "margin-bottom: 25px;",
            h4("Test information curve and SE"),
            plotlyOutput("IRT_poly_summary_tic"),
            downloadButton(
              outputId = "IRT_poly_summary_tic_download",
              label = "Download figure"
            )
          ),
          div(
            style = "margin-bottom: 25px;",
            h4("Table of estimated parameters"),
            uiOutput("IRT_poly_summary_coef_note"),
            tableOutput("IRT_poly_summary_coef"),
            downloadButton(
              outputId = "IRT_poly_summary_coef_download",
              label = "Download table"
            )
          ),
          div(
            style = "margin-bottom: 25px;",
            h4("Ability estimates"),
            p(
              "This table shows the response and factor scores for only six respondents.",
              "If you want to see the scores for all respondents, click on",
              strong("Download abilities"), "button."
            ),
            checkboxInput("IRT_poly_use_wle",
                          "Use WLE scores (default: EAP)", value = FALSE),
            tableOutput("IRT_poly_summary_ability"),
            textOutput("IRT_poly_summary_ability_reliability_text"),
            downloadButton(
              outputId = "IRT_poly_summary_ability_download",
              label = "Download abilities",
              style = "margin-bottom: 25px; margin-top: 10px;"
            ),
            textOutput("IRT_poly_summary_ability_correlation_text"),
            plotlyOutput("IRT_poly_summary_ability_plot"),
            downloadButton(
              outputId = "IRT_poly_summary_ability_plot_download",
              label = "Download figure"
            )
          ),
          conditionalPanel(
            condition = "input.IRT_poly_model == 'RSM' || input.IRT_poly_model == 'PCM' || input.IRT_poly_model == 'GPCM'",
            div(
              style = "margin-bottom: 25px;",
              h4("Wright map"),
              p(
                "The Wright map displays person ability estimates and item step",
                "parameters on one scale."
              ),
              plotlyOutput("IRT_poly_summary_wrightmap"),
              downloadButton(
                outputId = "IRT_poly_summary_wrightmap_download",
                label = "Download figure"
              )
            )
          )
        )
      )
    ),

    ## Items tab -------------------------------------------------------------
    tabPanel("Items",
      value = "irt_poly_items",
      fluidRow(
        column(
          12,
          fluidRow(
            column(
              2,
              sliderInput(
                inputId = "IRT_poly_items",
                label = "Item",
                min = 1, value = 1, max = 20,
                step = 1, animate = TRUE, ticks = FALSE
              )
            ),
            column(
              3,
              checkboxInput(
                inputId = "IRT_poly_show_observed",
                label = "Show observed proportions",
                value = FALSE
              )
            ),
            column(
              2,
              conditionalPanel(
                condition = "input.IRT_poly_show_observed == true",
                numericInput(
                  inputId = "IRT_poly_observed_groups",
                  label = "Number of groups",
                  value = 3, min = 2, max = 20, step = 1
                )
              )
            )
          ),
          div(
            style = "margin-bottom: 25px;",
            h4("Expected item score curve"),
            plotlyOutput("IRT_poly_items_expected"),
            downloadButton(
              outputId = "IRT_poly_items_expected_download",
              label = "Download figure"
            )
          ),
          div(
            style = "margin-bottom: 25px;",
            h4("Category probability curves"),
            plotlyOutput("IRT_poly_items_icc"),
            downloadButton(
              outputId = "IRT_poly_items_icc_download",
              label = "Download figure"
            )
          ),
          div(
            style = "margin-bottom: 25px;",
            h4("Item information curve"),
            plotlyOutput("IRT_poly_items_iic"),
            downloadButton(
              outputId = "IRT_poly_items_iic_download",
              label = "Download figure"
            )
          ),
          div(
            style = "margin-bottom: 25px;",
            h4("Table of parameters"),
            fluidRow(column(12, align = "center", tableOutput("IRT_poly_items_coef")))
          )
        )
      )
    ),

    # common footer -----------------------------------------------------------
    footer = tagList(
      h4("Selected R code"),
      conditionalPanel(
        "input.IRT_poly_model == 'NRM'",
        code(includeText("sc/irt/bock.R"))
      ),
      conditionalPanel(
        "input.IRT_poly_model == 'GRM'",
        code(includeText("sc/irt/grm.R"))
      ),
      conditionalPanel(
        "input.IRT_poly_model == 'RSM'",
        code(includeText("sc/irt/rsm.R"))
      ),
      conditionalPanel(
        "input.IRT_poly_model == 'PCM'",
        code(includeText("sc/irt/pcm.R"))
      ),
      conditionalPanel(
        "input.IRT_poly_model == 'GPCM'",
        code(includeText("sc/irt/gpcm.R"))
      )
    )
  )
)


# Polytomous model comparison UI -------------------------------------------
uiPolyIRTComparison <- tabPanel(
  "Model comparison",
  value = "irt_poly_comp",
  h3("Polytomous IRT model selection"),
  p(
    "Polytomous IRT models can be compared by several information criteria.",
    "The NRM (fitted to ordinal data), GRM, RSM, PCM, and GPCM are all included.",
    "Likelihood ratio tests (LRT) are provided for nested model pairs:",
    "RSM vs PCM (PCM relaxes the equal-step constraint) and",
    "PCM vs GPCM (GPCM adds item-specific discrimination).",
    "The GRM uses a different link function and the NRM is not nested with PCM/GPCM,",
    "so only information criteria are reported for those."
  ),
  p("Models can be compared by: "),
  tags$ul(
    tags$li(strong("AIC"), "is the Akaike information criterion (Akaike, 1974), "),
    tags$li(strong("BIC"), "is the Bayesian information criterion (Schwarz, 1978),"),
    tags$li(strong("logLik"), "is the logarithm of likelihood."),
    tags$li(strong("LRT"), "is the likelihood ratio test (for nested models only).")
  ),
  h4("Table of comparison statistics"),
  p("Row ", strong("BEST"), "indicates which model has the lowest value of given information criterion."),
  tableOutput("IRT_poly_comparison"),
  tags$style(type = "text/css", "#IRT_poly_comparison tr:last-child {font-weight:bold;}"),
  uiOutput("IRT_poly_comparison_model_converged"),
  br(),
  h4("Likelihood ratio tests (nested models)"),
  h5("RSM vs PCM"),
  tableOutput("IRT_poly_lrt_rsm_pcm"),
  h5("PCM vs GPCM"),
  tableOutput("IRT_poly_lrt_pcm_gpcm"),
  br(),
  h4("Selected R code"),
  code(includeText("sc/irt/poly_comp.R"))
)
