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

# ** Theta grid for plots ####
IRT_thetas_for_plots <- reactive({
  seq(-4, 4, length.out = 501)
})


# ** Expected item score curves (Summary) ####
IRT_poly_summary_expected <- reactive({
  fit <- IRT_poly_model()
  thetas <- IRT_thetas_for_plots()
  mod_item_names <- fit@Data$data |> colnames()

  d <- map2_dfr(
    mod_item_names,
    item_names(),
    ~ {
      probs <- probtrace(extract.item(fit, .x), thetas)
      n_cats <- ncol(probs)
      exp_score <- probs %*% (0:(n_cats - 1))
      tibble(
        Ability = thetas,
        Expected = as.numeric(exp_score),
        Item = .y
      )
    }
  )
  d$Item <- factor(d$Item, levels = item_names())

  g <- d |> ggplot(aes(x = Ability, y = Expected, color = Item)) +
    geom_line() +
    ylab("Expected item score") +
    theme_app()

  # Overlay observed mean scores if toggled on
  if (isTRUE(input$IRT_poly_show_observed)) {
    data <- ordinal()
    fs <- as.vector(fscores(fit))
    n_bins <- 10
    bins <- cut(fs, breaks = quantile(fs, probs = seq(0, 1, length.out = n_bins + 1)),
                include.lowest = TRUE)
    bin_mids <- tapply(fs, bins, mean)

    obs_df <- map2_dfr(
      seq_len(ncol(data)),
      item_names(),
      ~ {
        obs_mean <- tapply(data[[.x]], bins, mean, na.rm = TRUE)
        tibble(
          Ability = as.numeric(bin_mids),
          Expected = as.numeric(obs_mean),
          Item = .y
        )
      }
    )
    obs_df$Item <- factor(obs_df$Item, levels = item_names())
    g <- g + geom_point(data = obs_df, aes(x = Ability, y = Expected, color = Item),
                        size = 2, alpha = 0.7)
  }

  g
})

output$IRT_poly_summary_expected <- renderPlotly({
  g <- IRT_poly_summary_expected()
  p <- ggplotly(g)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_poly_summary_expected_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_poly_", input$IRT_poly_model, "_expected.png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_poly_summary_expected() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# ** Item information curves (Summary) ####
IRT_poly_summary_iic <- reactive({
  fit <- IRT_poly_model()
  thetas <- IRT_thetas_for_plots()
  mod_item_names <- fit@Data$data |> colnames()

  d <- map2_dfr(
    mod_item_names,
    item_names(),
    ~ tibble(
      Ability = thetas,
      Information = iteminfo(extract.item(fit, .x), thetas),
      Item = .y
    )
  )
  d$Item <- factor(d$Item, levels = item_names())

  g <- d |> ggplot(aes(x = Ability, y = Information, color = Item)) +
    geom_line() +
    theme_app()
  g
})

output$IRT_poly_summary_iic <- renderPlotly({
  g <- IRT_poly_summary_iic()
  p <- ggplotly(g)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_poly_summary_iic_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_poly_", input$IRT_poly_model, "_IIC.png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_poly_summary_iic() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# ** Test information curve and SE (Summary) ####
IRT_poly_summary_tic <- reactive({
  fit <- IRT_poly_model()
  thetas <- IRT_thetas_for_plots()

  test_info_se <- tibble(
    Ability = thetas,
    Information = testinfo(fit, thetas),
    SE = 1 / sqrt(Information)
  )

  g <- ggplot(test_info_se, aes(x = Ability)) +
    geom_line(aes(y = Information, col = "info")) +
    geom_line(aes(y = SE, col = "se")) +
    scale_color_manual("", values = c("blue", "pink"), labels = c("Information", "SE")) +
    scale_y_continuous("Information", sec.axis = sec_axis(~., name = "SE")) +
    theme(axis.title.y = element_text(color = "pink")) +
    theme_app()
  g
})

output$IRT_poly_summary_tic <- renderPlotly({
  g <- IRT_poly_summary_tic()
  p <- ggplotly(g)

  p$x$data[[1]]$text <- gsub("<br />colour: info", "", p$x$data[[1]]$text)
  p$x$data[[2]]$text <- gsub("<br />colour: se", "", p$x$data[[2]]$text)

  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_poly_summary_tic_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_poly_", input$IRT_poly_model, "_TIC.png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_poly_summary_tic() +
        theme(
          text = element_text(size = setting_figures$text_size),
          legend.position = "right", legend.key.size = unit(0.8, "lines")
        ),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ABILITY ESTIMATES ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

IRT_poly_summary_fscores_zscores <- reactive({
  fit <- IRT_poly_model()
  fscore_with_ses <- fscores(fit, full.scores.SE = TRUE)
  colnames(fscore_with_ses) <- c("F-score", "SE(F-score)")

  tab <- data.frame(
    `Total score` = total_score(),
    `Z-score` = z_score(),
    `T-score` = t_score(),
    fscore_with_ses,
    check.names = FALSE
  )

  rownames(tab) <- paste("Respondent", 1L:nrow(tab))
  tab
})

output$IRT_poly_summary_ability <- renderTable(
  {
    factors <- IRT_poly_summary_fscores_zscores()
    head(factors, n = 6)
  },
  rownames = TRUE
)

output$IRT_poly_summary_ability_download <- downloadHandler(
  filename = function() {
    paste0("IRT_poly_", input$IRT_poly_model, "_abilities.csv")
  },
  content = function(file) {
    write.csv(IRT_poly_summary_fscores_zscores(), file)
  }
)

# ** Correlation text ####
IRT_poly_summary_ability_correlation <- reactive({
  tab <- IRT_poly_summary_fscores_zscores()
  cor(tab[["F-score"]], tab[["Z-score"]], use = "pairwise.complete.obs")
})

output$IRT_poly_summary_ability_correlation_text <- renderText({
  paste0(
    "This scatterplot shows the relationship between the standardized total ",
    "score (Z-score) and the factor score estimated by the IRT model. The ",
    "Pearson correlation coefficient between these two scores is ",
    sprintf("%.3f", IRT_poly_summary_ability_correlation()), ". "
  )
})

# ** Scatterplot ####
IRT_poly_summary_ability_plot <- reactive({
  df <- IRT_poly_summary_fscores_zscores()

  ggplot(df, aes(`Z-score`, `F-score`)) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_app()
})

output$IRT_poly_summary_ability_plot <- renderPlotly({
  g <- IRT_poly_summary_ability_plot()
  p <- ggplotly(g)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_poly_summary_ability_plot_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_poly_", input$IRT_poly_model, "_abilities.png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_poly_summary_ability_plot() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# ** Wright map (PCM and RSM only) ####
IRT_poly_summary_wrightmap_args <- reactive({
  fit <- IRT_poly_model()
  fscore <- as.vector(fscores(fit))

  pars <- coef(fit, simplify = TRUE)$items
  d_cols <- grep("^d", colnames(pars))
  b <- pars[, d_cols, drop = FALSE]

  item.names <- item_names()

  list(theta = fscore, b = b, item.names = item.names)
})

output$IRT_poly_summary_wrightmap <- renderPlotly({
  args <- IRT_poly_summary_wrightmap_args()

  b_matrix <- args$b
  step_labels <- paste0(
    rep(args$item.names, each = ncol(b_matrix)),
    " Step ", rep(1:ncol(b_matrix), times = nrow(b_matrix))
  )
  b_vec <- as.vector(t(b_matrix))

  valid <- !is.na(b_vec)
  b_vec <- b_vec[valid]
  step_labels <- step_labels[valid]

  plts <- ShinyItemAnalysis:::gg_wright_internal(
    theta = args$theta,
    b = b_vec,
    item.names = step_labels
  )

  plt_left <- plts[[1]] |> ggplotly()
  plt_right <- plts[[2]] |> ggplotly() |>
    layout(yaxis = list(side = "right"))

  subplot(plt_left, plt_right, titleY = TRUE, margin = 0) |>
    plotly::config(displayModeBar = FALSE)
})

output$IRT_poly_summary_wrightmap_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_poly_", input$IRT_poly_model, "_WrightMap.png")
  },
  content = function(file) {
    args <- IRT_poly_summary_wrightmap_args()
    b_matrix <- args$b
    b_vec <- as.vector(t(b_matrix))
    step_labels <- paste0(
      rep(args$item.names, each = ncol(b_matrix)),
      " Step ", rep(1:ncol(b_matrix), times = nrow(b_matrix))
    )
    valid <- !is.na(b_vec)
    ggsave(file,
      plot = ggWrightMap(args$theta, b_vec[valid], item.names = step_labels[valid]),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)
