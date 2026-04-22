# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# REPORTS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# * Name of dataset in report ####
dataName <- reactive({
  dataset$name
})

observe({
  updateTextInput(
    session = session,
    inputId = "reportDataName",
    value = paste(dataName(), "dataset")
  )
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * VALIDITY ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# * Criterion present ####
criterionPresent <- reactive({
  (any(dataset$criterion != "missing") | is.null(dataset$criterion))
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * IRT MODELS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Model ####
report_IRT_binary_model <- reactive({
  if (input$report_IRT_binary_model == "none") {
    fit <- NULL
  } else {
    fit <- switch(input$report_IRT_binary_model,
      "Rasch" = IRT_binary_model_rasch(),
      "1PL" = IRT_binary_model_1pl(),
      "2PL" = IRT_binary_model_2pl(),
      "3PL" = IRT_binary_model_3pl(),
      "4PL" = IRT_binary_model_4pl()
    )
  }
  fit
})

# ** ICC equation ####
# so far it takes parametrization setting from the IRT section
report_IRT_binary_equation <- reactive({
  if (input$IRT_binary_summary_parametrization == "irt") {
    txt1 <- switch(input$report_IRT_binary_model,
      "none" = "",
      "Rasch" = "{(\\theta_p - b_i)}",
      "1PL" = "{a(\\theta_p - b_i)}",
      "2PL" = "{a_i(\\theta_p - b_i)}",
      "3PL" = "{a_i(\\theta_p - b_i)}",
      "4PL" = "{a_i(\\theta_p - b_i)}"
    )
  } else {
    txt1 <- switch(input$report_IRT_binary_model,
      "none" = "",
      "Rasch" = "{\\beta_{i0} + \\theta_p}",
      "1PL" = "{\\beta_{i0} + \\beta_{1} \\theta_p}",
      "2PL" = "{\\beta_{i0} + \\beta_{i1} \\theta_p}",
      "3PL" = "{\\beta_{i0} + \\beta_{i1} \\theta_p}",
      "4PL" = "{\\beta_{i0} + \\beta_{i1} \\theta_p}"
    )
  }

  txt2 <- switch(input$report_IRT_binary_model,
    "none" = "",
    "Rasch" = "",
    "1PL" = "",
    "2PL" = "",
    "3PL" = "c_i + (1 - c_i)",
    "4PL" = "c_i + (d_i - c_i)"
  )

  if (input$report_IRT_binary_model == "none") {
    txt <- ""
  } else {
    txt <- paste0(
      "$$\\mathrm{P}(Y_{pi} = 1|\\theta_p) = \\pi_{pi} = ", txt2,
      "\\frac{e^", txt1, "}{1 + e^", txt1, "}$$"
    )
  }
  txt
})

# ** Wright map ####
# in reports there is Wright map based on 1PL model, does not matter which
# IRT model is selected
report_IRT_binary_wrightmap <- reactive({
  if (input$report_IRT_binary_model == "none") {
    g <- ""
  } else {
    fit <- IRT_binary_model_1pl()
    fscore <- as.vector(fscores(fit, verbose = FALSE))
    b <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items[, "b"]

    g <- ggWrightMap(fscore, b, item.names = item_names())
  }
  g
})

# ** Plot of ICC ####
report_IRT_binary_icc <- reactive({
  fit <- report_IRT_binary_model()

  if (is.null(fit)) {
    g <- ""
  } else {
    mod_item_names <- fit@Data$data |> colnames()

    d <- map2_dfr(
      mod_item_names,
      item_names(), # names from user
      ~ tibble(
        Ability = IRT_thetas_for_plots(), # vector only
        Probability = probtrace(extract.item(fit, .x), IRT_thetas_for_plots())[, 2], # ascending probs
        Item = .y,
      )
    )
    d$Item <- factor(d$Item, levels = item_names())

    g <- d |> ggplot(aes(x = Ability, y = Probability, color = Item)) +
      geom_line() +
      ylab("Probability of correct answer") +
      theme_app()
  }
  g
})

# ** Plot of IIC ####
report_IRT_binary_iic <- reactive({
  fit <- report_IRT_binary_model()

  if (is.null(fit)) {
    g <- ""
  } else {
    mod_item_names <- fit@Data$data |> colnames()

    d <- map2_dfr(
      mod_item_names,
      item_names(), # names from user
      ~ tibble(
        Ability = IRT_thetas_for_plots(), # vector only
        Information = iteminfo(extract.item(fit, .x), IRT_thetas_for_plots()),
        Item = .y,
      )
    )
    d$Item <- factor(d$Item, levels = item_names())

    g <- d |> ggplot(aes(x = Ability, y = Information, color = Item)) +
      geom_line() +
      theme_app()
  }
  g
})

# ** Plot of TIC ####
report_IRT_binary_tic <- reactive({
  fit <- report_IRT_binary_model()

  if (is.null(fit)) {
    g <- ""
  } else {
    plt <- plot(fit, type = "infoSE")

    vals <- plt$panel.args
    x <- vals[[1]]$x
    y <- vals[[1]]$y
    df <- data.frame(cbind(Ability = x, Information = y))

    df$SE <- 1 / sqrt(df$Information)

    g <- ggplot(data = df, aes(x = Ability)) +
      geom_line(aes(y = Information, col = "info")) +
      geom_line(aes(y = SE, col = "se")) +
      scale_color_manual("", values = c("blue", "pink"), labels = c("Information", "SE")) +
      scale_y_continuous("Information",
        sec.axis = sec_axis(~., name = "SE")
      ) +
      theme(axis.title.y = element_text(color = "pink")) +
      theme_app()
  }
  g
})

# ** Table of parameters ####
report_IRT_binary_coef <- reactive({
  fit <- report_IRT_binary_model()

  if (is.null(fit)) {
    tab <- ""
  } else {
    n <- length(item_names())

    # parametrization should be added as option into reports
    IRTpars <- input$IRT_binary_summary_parametrization == "irt"

    par_tab <- coef(fit, IRTpars = IRTpars, simplify = TRUE)$items
    if (dim(fit@vcov)[1] > 1) {
      se_list <- coef(fit, IRTpars = IRTpars, printSE = TRUE)
      se_tab <- do.call(rbind, lapply(1:nrow(par_tab), function(i) se_list[[i]]["SE", ]))
    } else {
      se_tab <- cbind(rep(NA, nrow(par_tab)), NA, NA, NA)
    }

    tab <- cbind(par_tab, se_tab)[, order(c(seq(ncol(par_tab)), seq(ncol(se_tab))))]


    item_fit_cols <- c("S_X2", "df.S_X2", "p.S_X2")

    tab_fit <- itemfit(fit, na.rm = TRUE)[, item_fit_cols]

    if (!is.null(tryCatch(round(tab_fit, 3), error = function(e) {
      cat("ERROR : ", conditionMessage(e), "\n")
    }))) {
      tab <- data.frame(tab, tab_fit)
      colnames(tab)[9:11] <- c("SX2-value", "df", "p-value")
    } else {
      tab <- data.frame(tab, cbind("-", "-", "-"))
      colnames(tab)[9:11] <- c("SX2-value", "df", "p-value")
    }

    if (IRTpars) {
      colnames(tab)[1:8] <- paste0(
        c("", "SE("),
        paste0("\\(\\mathit{", rep(c("a", "b", "c", "d"), each = 2), "}\\)"),
        c("", ")")
      )
    } else {
      colnames(tab)[1:8] <- paste0(
        c("", "SE("),
        paste0("\\(\\mathit{", rep(c("\\beta_{1}", "\\beta_{0}", "c", "d"), each = 2), "}\\)"),
        c("", ")")
      )
      tab <- tab[, c(3:4, 1:2, 5:8, 9:11)]
    }

    rownames(tab) <- item_names()
    tab
  }
  tab
})

# ** Ability estimates plot ####
report_IRT_binary_ability_plot <- reactive({
  fit <- report_IRT_binary_model()

  if (is.null(fit)) {
    g <- ""
  } else {
    fscore <- as.vector(fscores(fit, verbose = FALSE))
    zscore <- z_score()

    df <- data.frame(fscore, zscore)

    g <- ggplot(df, aes(.data$zscore, .data$fscore)) +
      geom_point(size = 3) +
      labs(x = "Standardized total score", y = "Factor score") +
      theme_app() +
      theme(
        legend.box.just = "left",
        legend.justification = c(1, 0),
        legend.position = "inside",
        legend.position.inside = c(1, 0),
        legend.box = "vertical",
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(hjust = 0),
        legend.title = element_text(hjust = 0)
      )
  }
  g
})

# ** Ability estimates table ####
report_IRT_binary_ability_table <- reactive({
  fit <- report_IRT_binary_model()

  if (is.null(fit)) {
    tab <- ""
  } else {
    score <- as.vector(total_score())
    zscore <- as.vector(z_score())
    fscore <- fscores(fit, verbose = FALSE)

    tab <- data.frame(score, zscore, fscore)
    tab <- data.frame(
      Min = sapply(tab, min, na.rm = TRUE),
      Max = sapply(tab, max, na.rm = TRUE),
      Mean = sapply(tab, mean, na.rm = TRUE),
      Median = sapply(tab, median, na.rm = TRUE),
      SD = sapply(tab, sd, na.rm = TRUE),
      Skewness = sapply(tab, ShinyItemAnalysisPoly:::skewness),
      Kurtosis = sapply(tab, ShinyItemAnalysisPoly:::kurtosis)
    )
    rownames(tab) <- c("Total Scores", "Z-Scores", "F-scores")
  }
  tab
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * POLYTOMOUS IRT (reports) ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Report-side poly reactives thin-wrap existing poly IRT reactives so the
# download handler can pass a uniform set of params irrespective of data type.

report_IRT_poly_model_name <- reactive({
  input$IRT_poly_model %||% "GRM"
})

report_IRT_poly_equation <- reactive({
  switch(report_IRT_poly_model_name(),
    "NRM"  = "$$P(Y_{pi} = k | \\theta_p) = \\frac{e^{a_{ik}\\theta_p + c_{ik}}}{\\sum_{l=0}^{K_i} e^{a_{il}\\theta_p + c_{il}}}$$",
    "GRM"  = "$$P^*(Y_{pi} \\geq k | \\theta_p) = \\frac{e^{a_i(\\theta_p - b_{ik})}}{1 + e^{a_i(\\theta_p - b_{ik})}}$$",
    "RSM"  = "$$P(Y_{pi} = k | \\theta_p) = \\frac{e^{\\sum_{j=0}^{k}(\\theta_p - b_i - \\tau_j)}}{\\sum_{l=0}^{K} e^{\\sum_{j=0}^{l}(\\theta_p - b_i - \\tau_j)}}$$",
    "PCM"  = "$$P(Y_{pi} = k | \\theta_p) = \\frac{e^{\\sum_{j=0}^{k}(\\theta_p - \\delta_{ij})}}{\\sum_{l=0}^{K} e^{\\sum_{j=0}^{l}(\\theta_p - \\delta_{ij})}}$$",
    "GPCM" = "$$P(Y_{pi} = k | \\theta_p) = \\frac{e^{\\sum_{j=0}^{k} a_i(\\theta_p - \\delta_{ij})}}{\\sum_{l=0}^{K} e^{\\sum_{j=0}^{l} a_i(\\theta_p - \\delta_{ij})}}$$"
  )
})

# Wright map only defined for RSM/PCM/GPCM in the poly tab. For GRM/NRM we
# fall back to an empty placeholder so the Rmd can detect and skip.
report_IRT_poly_wrightmap <- reactive({
  if (!(report_IRT_poly_model_name() %in% c("RSM", "PCM", "GPCM"))) {
    return(NULL)
  }
  tryCatch(IRT_poly_wrightmap_plots(), error = function(e) NULL)
})

report_IRT_poly_icc <- reactive({
  tryCatch(IRT_poly_summary_expected_reactive(), error = function(e) "")
})

report_IRT_poly_iic <- reactive({
  tryCatch(IRT_poly_summary_iic_reactive(), error = function(e) "")
})

report_IRT_poly_tic <- reactive({
  tryCatch(IRT_poly_summary_tic_reactive(), error = function(e) "")
})

report_IRT_poly_coef <- reactive({
  tryCatch(IRT_poly_summary_coef_reactive(), error = function(e) NULL)
})

report_IRT_poly_ability_plot <- reactive({
  tryCatch(IRT_poly_ability_plot_reactive(), error = function(e) "")
})

report_IRT_poly_ability_table <- reactive({
  tryCatch({
    fscore <- as.vector(IRT_poly_fscores_raw()[, 1])
    score  <- as.vector(total_score())
    zscore <- as.vector(z_score())
    tab <- data.frame(score, zscore, fscore)
    data.frame(
      Min      = sapply(tab, min,    na.rm = TRUE),
      Max      = sapply(tab, max,    na.rm = TRUE),
      Mean     = sapply(tab, mean,   na.rm = TRUE),
      Median   = sapply(tab, median, na.rm = TRUE),
      SD       = sapply(tab, sd,     na.rm = TRUE),
      Skewness = sapply(tab, ShinyItemAnalysisPoly:::skewness),
      Kurtosis = sapply(tab, ShinyItemAnalysisPoly:::kurtosis),
      row.names = c("Total Scores", "Z-Scores", "F-scores")
    )
  }, error = function(e) NULL)
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * POLYTOMOUS DIF (reports) ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Cumulative difORD — reuses the live DIF_cumulative_method() output driven by
# current DIF tab inputs. For MVP parity in reports, we inherit those settings.
report_DIF_ord_fit <- reactive({
  tryCatch(DIF_cumulative_method(), error = function(e) NULL)
})

report_DIF_ord_print <- reactive({
  fit <- report_DIF_ord_fit()
  if (is.null(fit) || inherits(fit, "error")) return("DIF (cumulative logit) could not be fit.")
  out <- utils::capture.output(print(fit))
  paste(out, collapse = "\n")
})

report_DIF_ord_plot <- reactive({
  fit <- report_DIF_ord_fit()
  if (is.null(fit)) return("")
  flagged <- fit$DIFitems
  if (is.character(flagged) && flagged[1] == "No DIF item detected") return("")
  p <- tryCatch(
    plot(fit, item = flagged[1], plot.type = "cumulative"),
    error = function(e) NULL
  )
  if (is.list(p) && !inherits(p, "ggplot")) p <- p[[1]]
  if (inherits(p, "ggplot")) p else ""
})

# Adjacent-category difORD — uses live DIF_adjacent_model() from Adjacent tab.
report_DIF_adj_fit <- reactive({
  tryCatch(DIF_adjacent_model(), error = function(e) NULL)
})

report_DIF_adj_print <- reactive({
  fit <- report_DIF_adj_fit()
  if (is.null(fit) || inherits(fit, "error")) return("DIF (adjacent category logit) could not be fit.")
  out <- utils::capture.output(print(fit))
  paste(out, collapse = "\n")
})

report_DIF_adj_plot <- reactive({
  fit <- report_DIF_adj_fit()
  if (is.null(fit)) return("")
  flagged <- fit$DIFitems
  if (is.character(flagged) && flagged[1] == "No DIF item detected") return("")
  p <- tryCatch(
    plot(fit, item = flagged[1]),
    error = function(e) NULL
  )
  if (is.list(p) && !inherits(p, "ggplot")) p <- p[[1]]
  if (inherits(p, "ggplot")) p else ""
})

# Multinomial difNLR (poly nominal) — uses live DIF_multinomial_method() from Multinomial tab.
report_DIF_multinom_fit <- reactive({
  tryCatch(DIF_multinomial_method(), error = function(e) NULL)
})

report_DIF_multinom_print <- reactive({
  fit <- report_DIF_multinom_fit()
  if (is.null(fit) || inherits(fit, "error")) return("DIF (multinomial) could not be fit.")
  out <- utils::capture.output(print(fit))
  paste(out, collapse = "\n")
})

report_DIF_multinom_plot <- reactive({
  fit <- report_DIF_multinom_fit()
  if (is.null(fit)) return("")
  flagged <- fit$DDFitems
  if (is.character(flagged) && flagged[1] == "No DDF item detected") return("")
  tryCatch(
    plotDIFMultinomial_report_wrapper(fit, flagged[1]),
    error = function(e) ""
  )
})

# Helper: extract a single-item plot from difNLR multinomial output.
plotDIFMultinomial_report_wrapper <- function(fit, item) {
  p <- plot(fit, item = item)
  if (inherits(p, "list") && length(p) > 0) p[[1]] else p
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DIF ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# * Group present ####
groupPresent <- reactive({
  (any(dataset$group != "missing") | is.null(dataset$group))
})

# * Observed score / DIF matching present ####
DIFmatchingPresent <- reactive({
  (any(dataset$DIFmatching != "missing") | is.null(dataset$DIFmatching))
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * PROGRESS BAR ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# * Progress bar ####
observeEvent(input$generate, {
  withProgress(message = "Creating content", value = 0, style = "notification", {
    list( # header
      author = input$reportAuthor,
      dataset = input$reportDataName,
      # datasets
      a = nominal(),
      k = key(),
      itemNames = item_names(),
      # total scores
      incProgress(0.05),
      results = t(totalscores_table_Input()),
      histogram_totalscores = totalscores_histogram_Input(),
      cutScore = input$slider_totalscores_histogram,
      # standard scores
      standardscores_table = standardscores_table_Input(),
      incProgress(0.05),
      # validity section
      corr_plot = {
        if (input$corr_report) {
          if (input$customizeCheck) {
            corr_plot_Input_report()
          } else {
            corr_plot_Input()
          }
        } else {
          ""
        }
      },
      corr_plot_numclust = ifelse(input$customizeCheck, input$corr_plot_clust_report, input$corr_plot_clust),
      corr_plot_clustmethod = ifelse(input$customizeCheck, input$corr_plot_clustmethod_report, input$corr_plot_clustmethod),
      corr_type = ifelse(input$customizeCheck, input$corr_plot_type_of_corr_report, input$type_of_corr),
      # scree_plot = {
      #   if (input$corr_report) {
      #     scree_plot_Input()
      #   } else {
      #     ""
      #   }
      # },
      isCriterionPresent = criterionPresent(),
      validity_check = input$predict_report,
      validity_plot = {
        if (input$predict_report) {
          if (criterionPresent()) {
            validity_plot_Input()
          } else {
            ""
          }
        }
      },
      validity_table = {
        if (input$predict_report) {
          if (criterionPresent()) {
            validity_table_Input()
          } else {
            ""
          }
        }
      },
      incProgress(0.05),
      # item analysis
      DDplot = report_itemanalysis_DDplot(),
      DDplotRange1 = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_range_slider[[1]], input$itemanalysis_DDplot_range_slider[[1]]),
      DDplotRange2 = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_range_slider[[2]], input$itemanalysis_DDplot_range_slider[[2]]),
      DDplotNumGroups = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_groups_slider, input$itemanalysis_DDplot_groups_slider),
      DDplotDiscType = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_discrimination, input$itemanalysis_DDplot_discrimination),
      itemexam = report_itemanalysis_table(),
      cronbachs_alpha_table = reliability_cronbachalpha_table_Input(),
      incProgress(0.05),
      # distractors
      distractor_plot = report_distractor_plot(),
      type_distractor_plot = input$report_distractor_type,
      distractor_plot_legend_length = report_distractor_plot_legend_length(),
      incProgress(0.25),
      # regression
      multiplot = report_regression_multinomial_plot(),
      incProgress(0.05),
      # irt (poly-aware)
      irt_wrightmap = if (isTRUE(data_type() %in% c("ordinal", "nominal"))) report_IRT_poly_wrightmap() else report_IRT_binary_wrightmap(),
      irt_equation = if (isTRUE(data_type() %in% c("ordinal", "nominal"))) report_IRT_poly_equation() else report_IRT_binary_equation(),
      irt_model = if (isTRUE(data_type() %in% c("ordinal", "nominal"))) report_IRT_poly_model_name() else input$report_IRT_binary_model,
      irt_parametrization = input$IRT_binary_summary_parametrization,
      irt_icc = if (isTRUE(data_type() %in% c("ordinal", "nominal"))) report_IRT_poly_icc() else report_IRT_binary_icc(),
      irt_iic = if (isTRUE(data_type() %in% c("ordinal", "nominal"))) report_IRT_poly_iic() else report_IRT_binary_iic(),
      irt_tic = if (isTRUE(data_type() %in% c("ordinal", "nominal"))) report_IRT_poly_tic() else report_IRT_binary_tic(),
      irt_coef = if (isTRUE(data_type() %in% c("ordinal", "nominal"))) report_IRT_poly_coef() else report_IRT_binary_coef(),
      irt_ability_plot = if (isTRUE(data_type() %in% c("ordinal", "nominal"))) report_IRT_poly_ability_plot() else report_IRT_binary_ability_plot(),
      irt_ability_table = if (isTRUE(data_type() %in% c("ordinal", "nominal"))) report_IRT_poly_ability_table() else report_IRT_binary_ability_table(),
      incProgress(0.25),
      # DIF
      ### presence of group vector
      isGroupPresent = groupPresent(),
      ### histograms by group
      histCheck = input$histCheck,
      DIF_total_table = {
        if (groupPresent()) {
          if (input$histCheck) {
            DIF_total_table()
          }
        }
      },
      DIF_total_hist = {
        if (groupPresent()) {
          if (input$histCheck) {
            DIF_total_histogram()
          }
        }
      },
      DIF_total_ttest = {
        if (groupPresent()) {
          if (input$histCheck) {
            DIF_total_ttest()
          }
        }
      },
      ### delta plot
      deltaplotCheck = input$deltaplotCheck,
      deltaplot = {
        if (groupPresent()) {
          if (input$deltaplotCheck) {
            report_DIF_DP_plot()
          }
        }
      },
      DP_text_normal = {
        if (groupPresent()) {
          if (input$deltaplotCheck) {
            report_DIF_DP()
          }
        }
      },
      ### Mantel-Haenszel
      MHCheck = input$MHCheck,
      DIF_MH_print = {
        if (groupPresent()) {
          if (input$MHCheck) {
            report_DIF_MH_model()
          }
        }
      },
      ### logistic regression
      logregCheck = input$logregCheck,
      DIF_logistic_plot = {
        if (groupPresent()) {
          if (input$logregCheck) {
            report_DIF_logistic_plot()
          }
        }
      },
      DIF_logistic_print = {
        if (groupPresent()) {
          if (input$logregCheck) {
            report_DIF_logistic_model()
          }
        }
      },
      ### DDF multinomial
      multiCheck = input$multiCheck,
      DIF_multinomial_print = {
        if (groupPresent()) {
          if (input$multiCheck) {
            report_DIF_multinomial_method()
          }
        }
      },
      DIF_multinomial_plot = {
        if (groupPresent()) {
          if (input$multiCheck) {
            report_DIF_multinomial_plot()
          }
        }
      },
      incProgress(0.25),
      ### sessionInfo
      sessionInfo = input$include_session
    )
  })

  output$download_report_button <- renderUI({
    if (is.null(input$generate)) {
      return(NULL)
    }
    downloadButton(
      outputId = "report",
      label = "Download report",
      class = "btn btn-primary"
    )
  })
})

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * DOWNLOAD REPORT ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# * Download report ####
output$report <- downloadHandler(
  filename = reactive({
    paste0("report.", input$report_format)
  }),
  content = function(file) {
    is_poly <- isTRUE(data_type() %in% c("ordinal", "nominal"))
    reportPath <- file.path(
      getwd(),
      paste0("report", input$report_format,
             if (is_poly) "_poly" else "", ".Rmd")
    )
    parameters <- list( # header
      author = input$reportAuthor,
      dataset = input$reportDataName,
      data_type = data_type(),
      # datasets
      a = nominal(),
      k = key(),
      itemNames = item_names(),
      # total scores
      totalscores_table = t(totalscores_table_Input()),
      histogram_totalscores = totalscores_histogram_Input(),
      cutScore = input$slider_totalscores_histogram,
      # standard scores
      standardscores_table = standardscores_table_Input(),
      # validity section
      corr_plot = {
        if (input$corr_report) {
          if (input$customizeCheck) {
            corr_plot_Input_report()
          } else {
            corr_plot_Input()
          }
        } else {
          ""
        }
      },
      corr_plot_numclust = ifelse(input$customizeCheck, input$corr_plot_clust_report, input$corr_plot_clust),
      corr_plot_clustmethod = ifelse(input$customizeCheck, input$corr_plot_clustmethod_report, input$corr_plot_clustmethod),
      corr_type = ifelse(input$customizeCheck, input$corr_plot_type_of_corr_report, input$type_of_corr),
      # scree_plot = {
      #   if (input$corr_report) {
      #     scree_plot_Input()
      #   } else {
      #     ""
      #   }
      # },
      isCriterionPresent = criterionPresent(),
      validity_check = input$predict_report,
      validity_plot = {
        if (input$predict_report) {
          if (criterionPresent()) {
            validity_plot_Input()
          } else {
            ""
          }
        }
      },
      validity_table = {
        if (input$predict_report) {
          if (criterionPresent()) {
            validity_table_Input()
          } else {
            ""
          }
        }
      },
      # item analysis
      DDplot = report_itemanalysis_DDplot(),
      DDplotRange1 = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_range_slider[[1]], input$itemanalysis_DDplot_range_slider[[1]]),
      DDplotRange2 = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_range_slider[[2]], input$itemanalysis_DDplot_range_slider[[2]]),
      DDplotNumGroups = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_groups_slider, input$itemanalysis_DDplot_groups_slider),
      DDplotDiscType = ifelse(input$customizeCheck, input$report_itemanalysis_DDplot_discrimination, input$itemanalysis_DDplot_discrimination),
      itemexam = report_itemanalysis_table(),
      cronbachs_alpha_table = reliability_cronbachalpha_table_Input(),
      # distractors
      distractor_plot = report_distractor_plot(),
      type_distractor_plot = input$report_distractor_type,
      distractor_plot_legend_length = report_distractor_plot_legend_length(),
      # regression
      multiplot = report_regression_multinomial_plot(),
      # irt
      irt_wrightmap = if (is_poly) report_IRT_poly_wrightmap() else report_IRT_binary_wrightmap(),
      irt_equation = if (is_poly) report_IRT_poly_equation() else report_IRT_binary_equation(),
      irt_model = if (is_poly) report_IRT_poly_model_name() else input$report_IRT_binary_model,
      irt_parametrization = input$IRT_binary_summary_parametrization,
      irt_icc = if (is_poly) report_IRT_poly_icc() else report_IRT_binary_icc(),
      irt_iic = if (is_poly) report_IRT_poly_iic() else report_IRT_binary_iic(),
      irt_tic = if (is_poly) report_IRT_poly_tic() else report_IRT_binary_tic(),
      irt_coef = if (is_poly) report_IRT_poly_coef() else report_IRT_binary_coef(),
      irt_ability_plot = if (is_poly) report_IRT_poly_ability_plot() else report_IRT_binary_ability_plot(),
      irt_ability_table = if (is_poly) report_IRT_poly_ability_table() else report_IRT_binary_ability_table(),
      # polytomous DIF (only consumed by poly Rmd)
      DIF_ord_print = if (is_poly && groupPresent()) report_DIF_ord_print() else "",
      DIF_ord_plot  = if (is_poly && groupPresent()) report_DIF_ord_plot()  else "",
      DIF_adj_print = if (is_poly && groupPresent() && isTRUE(data_type() == "ordinal")) report_DIF_adj_print() else "",
      DIF_adj_plot  = if (is_poly && groupPresent() && isTRUE(data_type() == "ordinal")) report_DIF_adj_plot()  else "",
      DIF_multinom_print = if (is_poly && groupPresent() && isTRUE(data_type() == "nominal")) report_DIF_multinom_print() else "",
      DIF_multinom_plot  = if (is_poly && groupPresent() && isTRUE(data_type() == "nominal")) report_DIF_multinom_plot()  else "",
      # DIF
      ### presence of group vector
      isGroupPresent = groupPresent(),
      ### histograms by groups
      histCheck = input$histCheck,
      DIF_total_table = {
        if (groupPresent()) {
          if (input$histCheck) {
            DIF_total_table()
          }
        }
      },
      DIF_total_hist = {
        if (groupPresent()) {
          if (input$histCheck) {
            DIF_total_histogram()
          }
        }
      },
      DIF_total_ttest = {
        if (groupPresent()) {
          if (input$histCheck) {
            DIF_total_ttest()
          }
        }
      },
      ### delta plot
      deltaplotCheck = input$deltaplotCheck,
      DIF_deltaplot = {
        if (groupPresent()) {
          if (input$deltaplotCheck) {
            report_DIF_DP_plot()
          }
        }
      },
      DIF_deltaplot_text = {
        if (groupPresent()) {
          if (input$deltaplotCheck) {
            report_DIF_DP()
          }
        }
      },
      ### Mantel-Haenszel
      MHCheck = input$MHCheck,
      DIF_MH_print = {
        if (groupPresent()) {
          if (input$MHCheck) {
            report_DIF_MH_model()
          }
        }
      },
      ### logistic regression
      logregCheck = input$logregCheck,
      DIF_logistic_plot = {
        if (groupPresent()) {
          if (input$logregCheck) {
            report_DIF_logistic_plot()
          }
        }
      },
      DIF_logistic_print = {
        if (groupPresent()) {
          if (input$logregCheck) {
            report_DIF_logistic_model()
          }
        }
      },
      ### multinomial regression
      multiCheck = input$multiCheck,
      DIF_multinomial_print = {
        if (groupPresent()) {
          if (input$multiCheck) {
            report_DIF_multinomial_method()
          }
        }
      },
      DIF_multinomial_plot = {
        if (groupPresent()) {
          if (input$multiCheck) {
            report_DIF_multinomial_plot()
          }
        }
      },
      ### sessionInfo
      sessionInfo = input$include_session
    )
    rmarkdown::render(reportPath,
      output_file = file,
      params = parameters, envir = new.env(parent = globalenv())
    )
  }
)

# source('tests/helper_functions/markdown_render.R', local = TRUE)
#
# exportTestValues(report = report_react())
