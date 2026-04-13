# Helper function to create a polytomous model tab panel
.make_poly_tab <- function(model_id, model_label, model_description, equation_ui,
                           show_wright_map = FALSE) {
  model_lower <- tolower(model_id)
  tab_value <- paste0("irt_poly_", model_lower)

  tabPanel(
    model_label,
    value = tab_value,
    tabsetPanel(

      # common header -----------------------------------------------------------
      header = tagList(
        h3(model_label),
        p(model_description),
        equation_ui,
        uiOutput(paste0("IRT_poly_", model_lower, "_model_converged"))
      ),

      ## Summary tab -----------------------------------------------------------
      tabPanel("Summary",
        value = paste0("irt_poly_", model_lower, "_summary"),
        fluidRow(
          column(
            12,
            div(
              style = "margin-bottom: 25px;",
              h4("Expected item score curves"),
              plotlyOutput(paste0("IRT_poly_", model_lower, "_summary_expected")),
              downloadButton(
                outputId = paste0("IRT_poly_", model_lower, "_summary_expected_download"),
                label = "Download figure"
              )
            ),
            div(
              style = "margin-bottom: 25px;",
              h4("Category probability curves"),
              p("For category probability curves of individual items, please see the",
                strong("Items"), "subtab.",
                "(Plotting all items at once would result in a visual clutter.)")
            ),
            div(
              style = "margin-bottom: 25px;",
              h4("Item information curves"),
              plotlyOutput(paste0("IRT_poly_", model_lower, "_summary_iic")),
              downloadButton(
                outputId = paste0("IRT_poly_", model_lower, "_summary_iic_download"),
                label = "Download figure"
              )
            ),
            div(
              style = "margin-bottom: 25px;",
              h4("Test information curve and SE"),
              plotlyOutput(paste0("IRT_poly_", model_lower, "_summary_tic")),
              downloadButton(
                outputId = paste0("IRT_poly_", model_lower, "_summary_tic_download"),
                label = "Download figure"
              )
            ),
            div(
              style = "margin-bottom: 25px;",
              h4("Table of estimated parameters"),
              p(
                "Parameter estimates are completed by SX2 item fit statistics",
                "(Orlando & Thissen, 2000).",
                if (model_id == "PCM") {
                  "For PCM, infit and outfit mean-square statistics (Wright & Masters, 1982) are also provided."
                }
              ),
              tableOutput(paste0("IRT_poly_", model_lower, "_summary_coef")),
              downloadButton(
                outputId = paste0("IRT_poly_", model_lower, "_summary_coef_download"),
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
              tableOutput(paste0("IRT_poly_", model_lower, "_summary_ability")),
              downloadButton(
                outputId = paste0("IRT_poly_", model_lower, "_summary_ability_download"),
                label = "Download abilities",
                style = "margin-bottom: 25px;"
              ),
              textOutput(paste0("IRT_poly_", model_lower, "_summary_ability_correlation_text")),
              plotlyOutput(paste0("IRT_poly_", model_lower, "_summary_ability_plot")),
              downloadButton(
                outputId = paste0("IRT_poly_", model_lower, "_summary_ability_plot_download"),
                label = "Download figure"
              )
            ),
            if (show_wright_map) {
              div(
                style = "margin-bottom: 25px;",
                h4("Wright map"),
                p("The Wright map displays person ability estimates and item step",
                  "parameters on one scale."),
                plotlyOutput(paste0("IRT_poly_", model_lower, "_summary_wrightmap")),
                downloadButton(
                  outputId = paste0("IRT_poly_", model_lower, "_summary_wrightmap_download"),
                  label = "Download figure"
                )
              )
            }
          )
        )
      ),

      ## Items tab -------------------------------------------------------------
      tabPanel("Items",
        value = paste0("irt_poly_", model_lower, "_items"),
        fluidRow(
          column(
            12,
            sliderInput(
              inputId = paste0("IRT_poly_", model_lower, "_items"),
              label = "Item",
              min = 1, value = 1, max = 20,
              step = 1, animate = TRUE
            ),
            checkboxInput(
              inputId = paste0("IRT_poly_", model_lower, "_show_observed"),
              label = "Show observed proportions",
              value = FALSE
            ),
            div(
              style = "margin-bottom: 25px;",
              h4("Expected item score curve"),
              plotlyOutput(paste0("IRT_poly_", model_lower, "_items_expected")),
              downloadButton(
                outputId = paste0("IRT_poly_", model_lower, "_items_expected_download"),
                label = "Download figure"
              )
            ),
            div(
              style = "margin-bottom: 25px;",
              h4("Category probability curves"),
              plotlyOutput(paste0("IRT_poly_", model_lower, "_items_icc")),
              downloadButton(
                outputId = paste0("IRT_poly_", model_lower, "_items_icc_download"),
                label = "Download figure"
              )
            ),
            div(
              style = "margin-bottom: 25px;",
              h4("Item information curve"),
              plotlyOutput(paste0("IRT_poly_", model_lower, "_items_iic")),
              downloadButton(
                outputId = paste0("IRT_poly_", model_lower, "_items_iic_download"),
                label = "Download figure"
              )
            ),
            div(
              style = "margin-bottom: 25px;",
              h4("Table of parameters"),
              fluidRow(column(12, align = "center", tableOutput(paste0("IRT_poly_", model_lower, "_items_coef"))))
            )
          )
        )
      ),

      # common footer -----------------------------------------------------------
      footer = tagList(
        h4("Selected R code"),
        code(includeText(paste0("sc/irt/", model_lower, ".R")))
      )
    )
  )
}


# GRM tab ------------------------------------------------------------------
uiPolyIRTGRM <- .make_poly_tab(
  model_id = "GRM",
  model_label = "Graded response model",
  model_description = paste0(
    "The Graded Response Model (GRM; Samejima, 1969) is suitable for items ",
    "with ordered polytomous response categories. It models the cumulative probability ",
    "of responding in category k or higher. Each item has its own discrimination ",
    "parameter and a set of threshold parameters."
  ),
  equation_ui = tagList(
    h4("Equation"),
    withMathJax(
      p("Cumulative probability: $$P^*(Y_{pi} \\geq k | \\theta_p) = \\frac{e^{a_i(\\theta_p - b_{ik})}}{1 + e^{a_i(\\theta_p - b_{ik})}}$$"),
      p("Category probability: $$P(Y_{pi} = k | \\theta_p) = P^*(Y_{pi} \\geq k | \\theta_p) - P^*(Y_{pi} \\geq k+1 | \\theta_p)$$")
    )
  ),
  show_wright_map = FALSE
)


# RSM tab ------------------------------------------------------------------
uiPolyIRTRSM <- .make_poly_tab(
  model_id = "RSM",
  model_label = "Rating scale model",
  model_description = paste0(
    "The Rating Scale Model (RSM; Andrich, 1978) is a Rasch-family model for ",
    "ordered polytomous items. It assumes equal step structure across all items -- the ",
    "threshold distances are the same for every item, with only item location parameters ",
    "varying. This makes it suitable for rating scales where all items share the same ",
    "response format."
  ),
  equation_ui = tagList(
    h4("Equation"),
    withMathJax(
      p("$$P(Y_{pi} = k | \\theta_p) = \\frac{e^{\\sum_{j=0}^{k}(\\theta_p - b_i - \\tau_j)}}{\\sum_{l=0}^{K} e^{\\sum_{j=0}^{l}(\\theta_p - b_i - \\tau_j)}}$$"),
      p("where \\(b_i\\) is the item location and \\(\\tau_j\\) are common step parameters.")
    )
  ),
  show_wright_map = TRUE
)


# PCM tab ------------------------------------------------------------------
uiPolyIRTPCM <- .make_poly_tab(
  model_id = "PCM",
  model_label = "Partial credit model",
  model_description = paste0(
    "The Partial Credit Model (PCM; Masters, 1982) is a Rasch-family model for ",
    "ordered polytomous items. Unlike the RSM, each item has its own set of step ",
    "parameters (thresholds), allowing different step structures across items. ",
    "All items share equal discrimination."
  ),
  equation_ui = tagList(
    h4("Equation"),
    withMathJax(
      p("$$P(Y_{pi} = k | \\theta_p) = \\frac{e^{\\sum_{j=0}^{k}(\\theta_p - \\delta_{ij})}}{\\sum_{l=0}^{K} e^{\\sum_{j=0}^{l}(\\theta_p - \\delta_{ij})}}$$"),
      p("where \\(\\delta_{ij}\\) are item-specific step parameters.")
    )
  ),
  show_wright_map = TRUE
)


# GPCM tab -----------------------------------------------------------------
uiPolyIRTGPCM <- .make_poly_tab(
  model_id = "GPCM",
  model_label = "Generalized partial credit model",
  model_description = paste0(
    "The Generalized Partial Credit Model (GPCM; Muraki, 1992) extends the PCM ",
    "by allowing each item to have its own discrimination parameter in addition ",
    "to item-specific step parameters. It is the most flexible of the adjacent-category ",
    "logit models."
  ),
  equation_ui = tagList(
    h4("Equation"),
    withMathJax(
      p("$$P(Y_{pi} = k | \\theta_p) = \\frac{e^{\\sum_{j=0}^{k} a_i(\\theta_p - \\delta_{ij})}}{\\sum_{l=0}^{K} e^{\\sum_{j=0}^{l} a_i(\\theta_p - \\delta_{ij})}}$$"),
      p("where \\(a_i\\) is the item discrimination and \\(\\delta_{ij}\\) are item-specific step parameters.")
    )
  ),
  show_wright_map = FALSE
)


# Polytomous model comparison UI -------------------------------------------
uiPolyIRTComparison <- tabPanel(
  "Polytomous model comparison",
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
