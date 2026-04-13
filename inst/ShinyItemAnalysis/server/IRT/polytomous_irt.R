# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# POLYTOMOUS IRT MODELS (GRM, RSM, PCM, GPCM) ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL FITTING ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** GRM model ####
IRT_poly_model_grm <- reactive({
  data <- ordinal()
  fit <- mirt(
    data,
    model = 1, itemtype = "graded",
    SE = TRUE, verbose = FALSE,
    technical = list(NCYCLES = input$ncycles)
  )
  fit
})

# ** RSM model ####
IRT_poly_model_rsm <- reactive({
  data <- ordinal()
  fit <- mirt(
    data,
    model = 1, itemtype = "rsm",
    SE = TRUE, verbose = FALSE,
    technical = list(NCYCLES = input$ncycles)
  )
  fit
})

# ** PCM model ####
IRT_poly_model_pcm <- reactive({
  data <- ordinal()
  fit <- mirt(
    data,
    model = 1, itemtype = "Rasch",
    SE = TRUE, verbose = FALSE,
    technical = list(NCYCLES = input$ncycles)
  )
  fit
})

# ** GPCM model ####
IRT_poly_model_gpcm <- reactive({
  data <- ordinal()
  fit <- mirt(
    data,
    model = 1, itemtype = "gpcm",
    SE = TRUE, verbose = FALSE,
    technical = list(NCYCLES = input$ncycles)
  )
  fit
})

# ** Selected model ####
IRT_poly_model <- reactive({
  fit <- switch(input$IRT_poly_model,
    "GRM" = IRT_poly_model_grm(),
    "RSM" = IRT_poly_model_rsm(),
    "PCM" = IRT_poly_model_pcm(),
    "GPCM" = IRT_poly_model_gpcm()
  )
  fit
})


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL DESCRIPTION ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

IRT_poly_model_description <- reactive({
  txt <- switch(input$IRT_poly_model,
    "GRM" =
      paste0(
        "The <b>Graded Response Model</b> (GRM; Samejima, 1969) is suitable for items ",
        "with ordered polytomous response categories. It models the cumulative probability ",
        "of responding in category \\(k\\) or higher. Each item has its own discrimination ",
        "parameter \\(a_i\\) and a set of threshold parameters \\(b_{ik}\\). "
      ),
    "RSM" =
      paste0(
        "The <b>Rating Scale Model</b> (RSM; Andrich, 1978) is a Rasch-family model for ",
        "ordered polytomous items. It assumes equal step structure across all items — the ",
        "threshold distances are the same for every item, with only item location parameters ",
        "varying. This makes it suitable for rating scales where all items share the same ",
        "response format. "
      ),
    "PCM" =
      paste0(
        "The <b>Partial Credit Model</b> (PCM; Masters, 1982) is a Rasch-family model for ",
        "ordered polytomous items. Unlike the RSM, each item has its own set of step ",
        "parameters (thresholds), allowing different step structures across items. ",
        "All items share equal discrimination. "
      ),
    "GPCM" =
      paste0(
        "The <b>Generalized Partial Credit Model</b> (GPCM; Muraki, 1992) extends the PCM ",
        "by allowing each item to have its own discrimination parameter \\(a_i\\) in addition ",
        "to item-specific step parameters. It is the most flexible of the adjacent-category ",
        "logit models. "
      )
  )
  txt
})

output$IRT_poly_model_description <- renderText({
  IRT_poly_model_description()
})


# ** Equation ####
IRT_poly_equation <- reactive({
  txt <- switch(input$IRT_poly_model,
    "GRM" = withMathJax(
      p("Cumulative probability: $$P^*(Y_{pi} \\geq k | \\theta_p) = \\frac{e^{a_i(\\theta_p - b_{ik})}}{1 + e^{a_i(\\theta_p - b_{ik})}}$$"),
      p("Category probability: $$P(Y_{pi} = k | \\theta_p) = P^*(Y_{pi} \\geq k | \\theta_p) - P^*(Y_{pi} \\geq k+1 | \\theta_p)$$")
    ),
    "RSM" = withMathJax(
      p("$$P(Y_{pi} = k | \\theta_p) = \\frac{e^{\\sum_{j=0}^{k}(\\theta_p - b_i - \\tau_j)}}{\\sum_{l=0}^{K} e^{\\sum_{j=0}^{l}(\\theta_p - b_i - \\tau_j)}}$$"),
      p("where \\(b_i\\) is the item location and \\(\\tau_j\\) are common step parameters.")
    ),
    "PCM" = withMathJax(
      p("$$P(Y_{pi} = k | \\theta_p) = \\frac{e^{\\sum_{j=0}^{k}(\\theta_p - \\delta_{ij})}}{\\sum_{l=0}^{K} e^{\\sum_{j=0}^{l}(\\theta_p - \\delta_{ij})}}$$"),
      p("where \\(\\delta_{ij}\\) are item-specific step parameters.")
    ),
    "GPCM" = withMathJax(
      p("$$P(Y_{pi} = k | \\theta_p) = \\frac{e^{\\sum_{j=0}^{k} a_i(\\theta_p - \\delta_{ij})}}{\\sum_{l=0}^{K} e^{\\sum_{j=0}^{l} a_i(\\theta_p - \\delta_{ij})}}$$"),
      p("where \\(a_i\\) is the item discrimination and \\(\\delta_{ij}\\) are item-specific step parameters.")
    )
  )
  txt
})

output$IRT_poly_equation <- renderUI({
  IRT_poly_equation()
})


# ** Check whether model converged ####
output$IRT_poly_model_converged <- renderUI({
  fit <- IRT_poly_model()
  txt <- ifelse(extract.mirt(fit, "converged"),
    "",
    paste0(
      "<font color = 'orange'> Estimation process terminated without convergence after ",
      extract.mirt(fit, "iterations"), " iterations. Estimates are not reliable. ",
      "Try to increase a number of iterations of the EM algorithm in Settings. </font>"
    )
  )
  HTML(txt)
})


# ** Update item slider ####
observe({
  item_count <- ncol(ordinal())
  updateSliderInput(
    session = session,
    inputId = "IRT_poly_items",
    max = item_count
  )
})


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * SUMMARY ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Table of parameters ####
IRT_poly_summary_coef <- reactive({
  fit <- IRT_poly_model()

  # GRM and GPCM support IRTpars, PCM and RSM use default parametrization
  use_irt <- input$IRT_poly_model %in% c("GRM", "GPCM")

  par_tab <- coef(fit, IRTpars = use_irt, simplify = TRUE)$items

  # SEs
  if (dim(fit@vcov)[1] > 1) {
    se_list <- coef(fit, IRTpars = use_irt, printSE = TRUE)
    se_list[["GroupPars"]] <- NULL
    se_tab <- do.call(rbind, lapply(seq_along(se_list), function(i) {
      se_row <- se_list[[i]]["SE", ]
      length(se_row) <- ncol(par_tab)
      se_row
    }))
  } else {
    se_tab <- matrix(NA, nrow = nrow(par_tab), ncol = ncol(par_tab))
  }

  # Interleave par and SE columns
  tab <- cbind(par_tab, se_tab)[, order(c(seq(ncol(par_tab)), seq(ncol(se_tab))))]

  # Rename columns
  par_names <- colnames(par_tab)
  col_names <- as.vector(rbind(par_names, paste0("SE(", par_names, ")")))
  colnames(tab) <- col_names

  # SX2 fit statistics
  tab_fit <- tryCatch(
    itemfit(fit, na.rm = TRUE)[, c("S_X2", "df.S_X2", "p.S_X2")],
    error = function(e) NULL
  )

  if (!is.null(tab_fit)) {
    tab <- data.frame(tab, tab_fit)
    fit_cols <- (ncol(tab) - 2):ncol(tab)
    colnames(tab)[fit_cols] <- c("SX2-value", "df", "p-value")
  }

  # Infit/outfit for PCM (Rasch-family)
  if (input$IRT_poly_model == "PCM") {
    infit_tab <- tryCatch(
      itemfit(fit, fit_stats = "infit")[, c("outfit", "infit")],
      error = function(e) NULL
    )
    if (!is.null(infit_tab)) {
      tab <- data.frame(tab, infit_tab)
      colnames(tab)[(ncol(tab) - 1):ncol(tab)] <- c("Outfit MNSQ", "Infit MNSQ")
    }
  }

  rownames(tab) <- item_names()
  tab
})

output$IRT_poly_summary_coef <- renderTable(
  IRT_poly_summary_coef(),
  rownames = TRUE, striped = TRUE, na = ""
)

# ** Download table ####
output$IRT_poly_summary_coef_download <- downloadHandler(
  filename = function() {
    paste0("tab_IRT_poly_", input$IRT_poly_model, "_parameters.csv")
  },
  content = function(file) {
    write.csv(IRT_poly_summary_coef(), file)
  }
)
