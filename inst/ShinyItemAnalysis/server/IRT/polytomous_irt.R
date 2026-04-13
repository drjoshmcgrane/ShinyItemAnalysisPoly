# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# POLYTOMOUS IRT MODELS (NRM, GRM, RSM, PCM, GPCM) ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL FITTING ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** NRM model (fitted on ordinal data) ####
IRT_poly_model_nrm <- reactive({
  data <- ordinal()
  # Recode to 0-based integers so mirt doesn't waste a category-0 slot
  # when data starts at 1
  data_int <- data |> purrr::modify(function(x) as.integer(as.factor(x)) - 1L)
  fit <- mirt(
    data_int,
    model = 1, itemtype = "nominal",
    SE = TRUE, verbose = FALSE,
    technical = list(NCYCLES = input$ncycles), TOL = input$tol
  )
  fit
})

# ** GRM model ####
IRT_poly_model_grm <- reactive({
  data <- ordinal()
  fit <- mirt(
    data,
    model = 1, itemtype = "graded",
    SE = TRUE, verbose = FALSE,
    technical = list(NCYCLES = input$ncycles), TOL = input$tol
  )
  fit
})

# ** RSM model ####
IRT_poly_model_rsm <- reactive({
  data <- ordinal()
  # RSM requires all items to have the same number of response categories
  n_cats <- sapply(data, function(x) length(unique(x)))
  validate(
    need(
      length(unique(n_cats)) == 1,
      "The Rating Scale Model requires all items to have the same number of response categories. Use PCM or GPCM instead."
    )
  )
  fit <- mirt(
    data,
    model = 1, itemtype = "rsm",
    SE = TRUE, verbose = FALSE,
    technical = list(NCYCLES = input$ncycles), TOL = input$tol
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
    technical = list(NCYCLES = input$ncycles), TOL = input$tol
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
    technical = list(NCYCLES = input$ncycles), TOL = input$tol
  )
  fit
})


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL SELECTION (dropdown-based) ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Selected model fit based on dropdown
IRT_poly_model <- reactive({
  model <- input$IRT_poly_model
  req(model)
  switch(model,
    "NRM" = IRT_poly_model_nrm(),
    "GRM" = IRT_poly_model_grm(),
    "RSM" = IRT_poly_model_rsm(),
    "PCM" = IRT_poly_model_pcm(),
    "GPCM" = IRT_poly_model_gpcm()
  )
})


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL DESCRIPTION AND EQUATIONS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output$IRT_poly_model_description_ui <- renderUI({
  model <- input$IRT_poly_model
  req(model)
  txt <- switch(model,
    "NRM" = paste0(
      "The <b>Nominal Response Model</b> (NRM; Bock, 1972) was originally designed for ",
      "items with nominal (unordered) categories. When applied to ordinal data, it serves ",
      "as a flexible baseline model for comparison. "
    ),
    "GRM" = paste0(
      "The <b>Graded Response Model</b> (GRM; Samejima, 1969) is suitable for items ",
      "with ordered polytomous response categories. It models the cumulative probability ",
      "of responding in category \\(k\\) or higher. Each item has its own discrimination ",
      "parameter \\(a_i\\) and a set of threshold parameters \\(b_{ik}\\). "
    ),
    "RSM" = paste0(
      "The <b>Rating Scale Model</b> (RSM; Andrich, 1978) is a Rasch-family model for ",
      "ordered polytomous items. It assumes equal step structure across all items \u2014 the ",
      "threshold distances are the same for every item, with only item location parameters ",
      "varying. This makes it suitable for rating scales where all items share the same ",
      "response format. "
    ),
    "PCM" = paste0(
      "The <b>Partial Credit Model</b> (PCM; Masters, 1982) is a Rasch-family model for ",
      "ordered polytomous items. Unlike the RSM, each item has its own set of step ",
      "parameters (thresholds), allowing different step structures across items. ",
      "All items share equal discrimination. "
    ),
    "GPCM" = paste0(
      "The <b>Generalized Partial Credit Model</b> (GPCM; Muraki, 1992) extends the PCM ",
      "by allowing each item to have its own discrimination parameter \\(a_i\\) in addition ",
      "to item-specific step parameters. It is the most flexible of the adjacent-category ",
      "logit models. "
    )
  )
  HTML(txt)
})

output$IRT_poly_equation_ui <- renderUI({
  model <- input$IRT_poly_model
  req(model)

  eq <- switch(model,
    "NRM" = {
      nrm_param <- input$IRT_poly_nrm_parametrization
      nrm_eq <- switch(nrm_param,
        "blis" = tagList(
          p("$$\\pi_{pik} = \\mathrm{P}(Y_{pi} = k|\\theta_p) = \\frac{e^{ {\\beta_0}_{ik} +  {\\beta_1}_{ik}\\theta_p  }}{\\sum_{l=0}^{K_i} e^{ {\\beta_0}_{il} +  {\\beta_1}_{il}\\theta_p }}$$"),
          p("with constrains", "\\({\\beta_1}_{i0} = 0\\)", "and", "\\({\\beta_0}_{i0} = 0\\).")
        ),
        "blirt" = tagList(
          p("$$ \\pi_{pik} = \\mathrm{P}(Y_{pi} = k|\\theta_p) = \\frac{e^{ a_{ik}( \\theta_p - b_{ik} ) }} {\\sum_{l=0}^{K_i} e^{ a_{il}( \\theta_p - b_{il} ) }}$$"),
          p("with constrains", "\\(a_{i0} = 0\\)", "and", "\\(b_{i0} = 0\\).")
        ),
        "bock" = tagList(
          p("$$\\pi_{pik} = \\mathrm{P}(Y_{pi} = k|\\theta_p) = \\frac{e^{ \\alpha_{ik}\\theta_p +c_{ik} }}{\\sum_{l=0}^{K_i} e^{\\alpha_{il}\\theta_p +c_{il}}}$$"),
          p("with constrains", "\\(\\sum_{k=0}^{K_i}a_k = 0\\)", "and", "\\(\\sum_{k=0}^{K_i}c_k = 0\\).")
        ),
        "thissen" = tagList(
          p("$$ \\pi_{pik} = \\mathrm{P}(Y_{pi} = k|\\theta_p) = \\frac{e^{ a^*_i a_{ik}^s \\theta_p + c_{ik} }}{\\sum_{l=0}^{K_i} e^{ a^*_i a_{il}^s \\theta_p + c_{il}  }}$$"),
          p("with constrains", "\\(a_{i0}^s = 0\\),", "\\(a_{iK}^s = K\\)", "and", "\\(d_{i0} = 0\\).")
        )
      )
      tagList(h4("Equation"), withMathJax(nrm_eq))
    },
    "GRM" = tagList(
      h4("Equation"),
      withMathJax(
        p("Cumulative probability: $$P^*(Y_{pi} \\geq k | \\theta_p) = \\frac{e^{a_i(\\theta_p - b_{ik})}}{1 + e^{a_i(\\theta_p - b_{ik})}}$$"),
        p("Category probability: $$P(Y_{pi} = k | \\theta_p) = P^*(Y_{pi} \\geq k | \\theta_p) - P^*(Y_{pi} \\geq k+1 | \\theta_p)$$")
      )
    ),
    "RSM" = tagList(
      h4("Equation"),
      withMathJax(
        p("$$P(Y_{pi} = k | \\theta_p) = \\frac{e^{\\sum_{j=0}^{k}(\\theta_p - b_i - \\tau_j)}}{\\sum_{l=0}^{K} e^{\\sum_{j=0}^{l}(\\theta_p - b_i - \\tau_j)}}$$"),
        p("where \\(b_i\\) is the item location and \\(\\tau_j\\) are common step parameters.")
      )
    ),
    "PCM" = tagList(
      h4("Equation"),
      withMathJax(
        p("$$P(Y_{pi} = k | \\theta_p) = \\frac{e^{\\sum_{j=0}^{k}(\\theta_p - \\delta_{ij})}}{\\sum_{l=0}^{K} e^{\\sum_{j=0}^{l}(\\theta_p - \\delta_{ij})}}$$"),
        p("where \\(\\delta_{ij}\\) are item-specific step parameters.")
      )
    ),
    "GPCM" = tagList(
      h4("Equation"),
      withMathJax(
        p("$$P(Y_{pi} = k | \\theta_p) = \\frac{e^{\\sum_{j=0}^{k} a_i(\\theta_p - \\delta_{ij})}}{\\sum_{l=0}^{K} e^{\\sum_{j=0}^{l} a_i(\\theta_p - \\delta_{ij})}}$$"),
        p("where \\(a_i\\) is the item discrimination and \\(\\delta_{ij}\\) are item-specific step parameters.")
      )
    )
  )
  eq
})


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * CONVERGENCE CHECK ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * UPDATE ITEM SLIDER ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

observe({
  item_count <- ncol(ordinal())
  updateSliderInput(
    session = session,
    inputId = "IRT_poly_items",
    max = item_count
  )
})


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * SUMMARY TAB OUTPUTS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Coefficient note ####
output$IRT_poly_summary_coef_note <- renderUI({
  model <- input$IRT_poly_model
  req(model)
  note <- paste0(
    "Parameter estimates are completed by SX2 item fit statistics ",
    "(Orlando & Thissen, 2000). SX2 statistics are computed only when ",
    "no missing data are present."
  )
  if (model == "PCM") {
    note <- paste0(
      note,
      " For PCM, infit and outfit mean-square statistics ",
      "(Wright & Masters, 1982) are also provided."
    )
  }

  tags <- tagList(p(note))

  # For RSM, show shared threshold parameters in a separate line
  if (model == "RSM") {
    fit <- IRT_poly_model()
    pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
    b_cols <- grep("^b\\d", colnames(pars))
    shared_b <- round(pars[1, b_cols], 3)
    b_text <- paste(paste0(names(shared_b), " = ", shared_b), collapse = ", ")
    tags <- tagList(
      tags,
      p(strong("Shared threshold parameters (equal across all items): "), b_text)
    )
  }

  tags
})

# ** Table of parameters (Summary) ####
IRT_poly_summary_coef_reactive <- reactive({
  fit <- IRT_poly_model()
  model <- input$IRT_poly_model
  req(model)

  if (model == "NRM") {
    use_irt <- isTRUE(input$IRT_poly_nrm_parametrization %in% c("blirt", "bock"))
  } else {
    use_irt <- TRUE
  }
  par_tab <- coef(fit, IRTpars = use_irt, simplify = TRUE)$items

  # For mixed-category data (e.g., PCM/GPCM/GRM): binary items get a,b,g,u columns
  # while polytomous items get a,b1,b2,... Move binary 'b' into 'b1' and drop b,g,u
  # (keep 'a' for GPCM/GRM where it is the discrimination parameter)
  if (model %in% c("PCM", "GPCM", "GRM") && "b" %in% colnames(par_tab) && "b1" %in% colnames(par_tab)) {
    b_col <- par_tab[, "b"]
    b1_col <- par_tab[, "b1"]
    # For binary items: b has a value, b1 is NA; copy b into b1
    needs_copy <- !is.na(b_col) & is.na(b1_col)
    par_tab[needs_copy, "b1"] <- b_col[needs_copy]
    # Drop columns that are fixed/uninformative: b, g, u always; a only for PCM (always 1)
    drop_cols <- intersect(c("b", "g", "u"), colnames(par_tab))
    if (model == "PCM") drop_cols <- c(drop_cols, intersect("a", colnames(par_tab)))
    par_tab <- par_tab[, !colnames(par_tab) %in% drop_cols, drop = FALSE]
  }

  # For RSM: thresholds (b1, b2, ...) are shared across items.
  # Keep only the item-specific location parameter (c) and rename it.
  if (model == "RSM") {
    shared_cols <- grep("^(a1|b\\d)", colnames(par_tab))
    par_tab <- par_tab[, -shared_cols, drop = FALSE]
    colnames(par_tab) <- gsub("^c$", "Location", colnames(par_tab))
  }

  if (dim(fit@vcov)[1] > 1) {
    se_list <- coef(fit, IRTpars = use_irt, printSE = TRUE)
    se_list[["GroupPars"]] <- NULL

    if (model == "RSM") {
      # Extract SE for location parameter only (last column before GroupPars)
      se_tab <- do.call(rbind, lapply(seq_along(se_list), function(i) {
        se_row <- se_list[[i]]["SE", ]
        # Keep only the 'c' parameter SE (last one)
        c_idx <- which(names(se_row) == "c")
        if (length(c_idx) == 0) c_idx <- length(se_row)
        se_row[c_idx]
      }))
      colnames(se_tab) <- "SE(Location)"
      tab <- data.frame(par_tab, se_tab)
    } else {
      # Column names in par_tab after any mixed-category cleanup
      keep_cols <- colnames(par_tab)
      se_tab <- do.call(rbind, lapply(seq_along(se_list), function(i) {
        se_row <- se_list[[i]]["SE", ]
        se_names <- names(se_row)
        # For mixed-category data: apply same column logic as par_tab
        # Binary items have a,b,g,u; polytomous have a,b1,b2,...
        # If we dropped a,b,g,u from par_tab, match by name instead of truncating
        if ("b" %in% se_names && !("b" %in% keep_cols) && "b1" %in% keep_cols) {
          # This is a binary item with b but no b1 in its SE row; copy b -> b1
          if (!"b1" %in% se_names) {
            se_row["b1"] <- se_row["b"]
          } else if (is.na(se_row["b1"])) {
            se_row["b1"] <- se_row["b"]
          }
        }
        # Select only the columns that exist in par_tab
        out <- rep(NA_real_, length(keep_cols))
        names(out) <- keep_cols
        matched <- intersect(keep_cols, names(se_row))
        out[matched] <- se_row[matched]
        out
      }))
      colnames(se_tab) <- keep_cols
      tab <- cbind(par_tab, se_tab)[, order(c(seq(ncol(par_tab)), seq(ncol(se_tab))))]
      par_names <- colnames(par_tab)
      col_names <- as.vector(rbind(par_names, paste0("SE(", par_names, ")")))
      colnames(tab) <- col_names
    }
  } else {
    if (model == "RSM") {
      tab <- data.frame(par_tab, `SE(Location)` = NA, check.names = FALSE)
    } else {
      se_tab <- matrix(NA_real_, nrow = nrow(par_tab), ncol = ncol(par_tab))
      colnames(se_tab) <- colnames(par_tab)
      tab <- cbind(par_tab, se_tab)[, order(c(seq(ncol(par_tab)), seq(ncol(se_tab))))]
      par_names <- colnames(par_tab)
      col_names <- as.vector(rbind(par_names, paste0("SE(", par_names, ")")))
      colnames(tab) <- col_names
    }
  }

  tab_fit <- tryCatch(
    itemfit(fit, na.rm = TRUE)[, c("S_X2", "df.S_X2", "p.S_X2")],
    error = function(e) NULL
  )
  if (!is.null(tab_fit)) {
    tab <- data.frame(tab, tab_fit)
    fit_cols <- (ncol(tab) - 2):ncol(tab)
    colnames(tab)[fit_cols] <- c("SX2-value", "df", "p-value")
  }

  if (model == "PCM") {
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
  IRT_poly_summary_coef_reactive(),
  rownames = TRUE, striped = TRUE, na = ""
)

output$IRT_poly_summary_coef_download <- downloadHandler(
  filename = function() {
    paste0("tab_IRT_poly_", input$IRT_poly_model, "_parameters.csv")
  },
  content = function(file) {
    write.csv(IRT_poly_summary_coef_reactive(), file)
  }
)

# ** Expected item score curves (Summary) ####
IRT_poly_summary_expected_reactive <- reactive({
  fit <- IRT_poly_model()
  data <- ordinal()
  thetas <- IRT_thetas_for_plots()
  mod_item_names <- fit@Data$data |> colnames()

  d <- map2_dfr(
    mod_item_names,
    item_names(),
    ~ {
      probs <- probtrace(extract.item(fit, .x), thetas)
      n_cats <- ncol(probs)
      # Use actual category values from data to match observed scale
      unique_vals <- sort(unique(data[[.x]]))
      if (length(unique_vals) != n_cats) {
        unique_vals <- 0:(n_cats - 1)
      }
      exp_score <- probs %*% unique_vals
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
  g
})

output$IRT_poly_summary_expected <- renderPlotly({
  g <- IRT_poly_summary_expected_reactive()
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
      plot = IRT_poly_summary_expected_reactive() +
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
IRT_poly_summary_iic_reactive <- reactive({
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
  g <- IRT_poly_summary_iic_reactive()
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
      plot = IRT_poly_summary_iic_reactive() +
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
IRT_poly_summary_tic_reactive <- reactive({
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
    theme_app() +
    theme(axis.title.y = element_text(color = "pink"))
  g
})

output$IRT_poly_summary_tic <- renderPlotly({
  g <- IRT_poly_summary_tic_reactive()
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
      plot = IRT_poly_summary_tic_reactive() +
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

# ** Ability estimates ####
IRT_poly_fscores_reactive <- reactive({
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
    factors <- IRT_poly_fscores_reactive()
    head(factors, n = 6)
  },
  rownames = TRUE
)

output$IRT_poly_summary_ability_download <- downloadHandler(
  filename = function() {
    paste0("IRT_poly_", input$IRT_poly_model, "_abilities.csv")
  },
  content = function(file) {
    write.csv(IRT_poly_fscores_reactive(), file)
  }
)

# Correlation text
IRT_poly_corr_reactive <- reactive({
  tab <- IRT_poly_fscores_reactive()
  cor(tab[["F-score"]], tab[["Z-score"]], use = "pairwise.complete.obs")
})

output$IRT_poly_summary_ability_correlation_text <- renderText({
  paste0(
    "This scatterplot shows the relationship between the standardized total ",
    "score (Z-score) and the factor score estimated by the IRT model. The ",
    "Pearson correlation coefficient between these two scores is ",
    sprintf("%.3f", IRT_poly_corr_reactive()), ". "
  )
})

# Scatterplot
IRT_poly_ability_plot_reactive <- reactive({
  df <- IRT_poly_fscores_reactive()
  ggplot(df, aes(`Z-score`, `F-score`)) +
    geom_point(size = 3) +
    labs(x = "Standardized total score", y = "Factor score") +
    theme_app()
})

output$IRT_poly_summary_ability_plot <- renderPlotly({
  g <- IRT_poly_ability_plot_reactive()
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
      plot = IRT_poly_ability_plot_reactive() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# ** Wright map (RSM, PCM, GPCM) ####
IRT_poly_wrightmap_args_reactive <- reactive({
  model <- input$IRT_poly_model
  req(model %in% c("RSM", "PCM", "GPCM"))
  fit <- IRT_poly_model()
  fscore <- as.vector(fscores(fit))
  pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
  b_cols <- grep("^b\\d", colnames(pars))

  if (model == "RSM") {
    # RSM: shared thresholds b_j + item-specific location c.
    # Item-specific step difficulties = b_j - c_i
    shared_b <- pars[1, b_cols]
    c_vals <- pars[, "c"]
    b <- t(outer(as.numeric(shared_b), as.numeric(c_vals), "-"))
    colnames(b) <- names(shared_b)
    rownames(b) <- rownames(pars)
  } else {
    # PCM / GPCM: b columns are already item-specific thresholds
    b <- pars[, b_cols, drop = FALSE]
    # Mixed-category data: binary items have difficulty in 'b', not 'b1'
    # Copy b -> b1 so binary items appear on the Wright map
    if ("b" %in% colnames(pars) && "b1" %in% colnames(b)) {
      b_col <- pars[, "b"]
      needs_copy <- !is.na(b_col) & is.na(b[, "b1"])
      b[needs_copy, "b1"] <- b_col[needs_copy]
    }
  }

  item.names <- item_names()
  list(theta = fscore, b = b, item.names = item.names)
})

# Custom polytomous Wright map: items on x-axis, thresholds as points
IRT_poly_wrightmap_plots <- reactive({
  args <- IRT_poly_wrightmap_args_reactive()
  b_matrix <- args$b
  theta <- args$theta
  inames <- args$item.names
  n_steps <- ncol(b_matrix)

  # Build data frame of thresholds: one row per item-step
  df_b <- do.call(rbind, lapply(seq_along(inames), function(i) {
    steps <- b_matrix[i, ]
    valid <- !is.na(steps)
    if (!any(valid)) return(NULL)
    data.frame(
      Item = inames[i],
      Step = paste0("b", which(valid)),
      Difficulty = as.numeric(steps[valid]),
      stringsAsFactors = FALSE
    )
  }))
  df_b$Item <- factor(df_b$Item, levels = inames)

  binwidth <- 0.5
  lim_min <- min(c(theta, df_b$Difficulty), na.rm = TRUE) - binwidth
  lim_max <- max(c(theta, df_b$Difficulty), na.rm = TRUE) + binwidth

  # Left panel: person histogram (coord_flip so theta is vertical)
  g1 <- ggplot(data.frame(theta = theta), aes(x = .data$theta)) +
    geom_histogram(binwidth = binwidth, fill = "blue", col = "black", na.rm = TRUE) +
    xlim(lim_min, lim_max) +
    coord_flip() +
    scale_y_reverse() +
    xlab("Respondent latent trait") +
    theme_app() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  # Right panel: items on x-axis, threshold points on y-axis
  g2 <- ggplot(df_b, aes(x = Item, y = Difficulty, color = Step)) +
    geom_point(size = 3) +
    scale_y_continuous(position = "right", limits = c(lim_min, lim_max)) +
    ylab("Item threshold difficulty") +
    xlab("") +
    theme_app() +
    theme(axis.text.x = element_text(angle = 70, hjust = 1))

  list(g1, g2)
})

output$IRT_poly_summary_wrightmap <- renderPlotly({
  plts <- IRT_poly_wrightmap_plots()
  plt_left <- plts[[1]] |> ggplotly()
  plt_right <- plts[[2]] |> ggplotly() |>
    layout(yaxis = list(side = "right"))
  subplot(plt_left, plt_right, titleY = TRUE, margin = 0.05) |>
    plotly::config(displayModeBar = FALSE)
})

output$IRT_poly_summary_wrightmap_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_poly_", input$IRT_poly_model, "_WrightMap.png")
  },
  content = function(file) {
    plts <- IRT_poly_wrightmap_plots()
    grobs <- lapply(plts, ggplot2::ggplotGrob)
    # Align panel heights so y-axes match between histogram and item plot
    max_heights <- grid::unit.pmax(grobs[[1]]$heights, grobs[[2]]$heights)
    grobs[[1]]$heights <- max_heights
    grobs[[2]]$heights <- max_heights
    g_combined <- gridExtra::arrangeGrob(grobs = grobs, ncol = 2)
    ggsave(file,
      plot = g_combined,
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ITEMS TAB OUTPUTS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Expected item score curve (Items) ####
IRT_poly_items_expected_reactive <- reactive({
  item <- input$IRT_poly_items
  req(item)
  fit <- IRT_poly_model()
  data <- ordinal()
  thetas <- IRT_thetas_for_plots()

  probs <- probtrace(extract.item(fit, item), thetas)
  n_cats <- ncol(probs)
  # Use actual category values from data so expected curve matches observed scale
  unique_vals <- sort(unique(data[[item]]))
  if (length(unique_vals) != n_cats) {
    unique_vals <- 0:(n_cats - 1)
  }
  exp_score <- probs %*% unique_vals

  d <- tibble(Ability = thetas, Expected = as.numeric(exp_score))

  g <- d |> ggplot(aes(x = Ability, y = Expected)) +
    geom_line() +
    ylab("Expected item score") +
    ggtitle(item_names()[item]) +
    theme_app()

  if (isTRUE(input$IRT_poly_show_observed)) {
    fs <- as.vector(fscores(fit))
    n_bins <- input$IRT_poly_observed_groups %||% 3
    bins <- cut(fs, breaks = quantile(fs, probs = seq(0, 1, length.out = n_bins + 1)),
                include.lowest = TRUE)
    bin_mids <- tapply(fs, bins, mean)
    obs_mean <- tapply(data[[item]], bins, mean, na.rm = TRUE)

    obs_df <- tibble(
      Ability = as.numeric(bin_mids),
      Expected = as.numeric(obs_mean)
    )
    g <- g + geom_point(data = obs_df, aes(x = Ability, y = Expected),
                        size = 3, alpha = 0.7)
  }
  g
})

output$IRT_poly_items_expected <- renderPlotly({
  g <- IRT_poly_items_expected_reactive()
  p <- ggplotly(g)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_poly_items_expected_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_poly_", input$IRT_poly_model, "_expected_",
           item_names()[input$IRT_poly_items], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_poly_items_expected_reactive() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** ICC / Category probability curves (Items) ####
IRT_poly_items_icc_reactive <- reactive({
  item <- input$IRT_poly_items
  req(item)
  fit <- IRT_poly_model()
  data <- ordinal()
  thetas <- IRT_thetas_for_plots()

  probs <- as_tibble(probtrace(extract.item(fit, item), thetas))
  # Use actual response values from ordinal data for category names
  unique_cats <- sort(unique(data[[item]]))
  cat_names <- paste0("Cat ", unique_cats)
  # If mirt returns more/fewer columns, fall back to 0-based
  if (ncol(probs) != length(cat_names)) {
    cat_names <- paste0("Cat ", 0:(ncol(probs) - 1))
    unique_cats <- 0:(ncol(probs) - 1)
  }
  names(probs) <- cat_names

  probs <- probs |>
    bind_cols(Ability = thetas) |>
    pivot_longer(-Ability, names_to = "Category", values_to = "Probability")
  probs$Category <- factor(probs$Category, levels = cat_names)

  g <- probs |>
    ggplot(aes(x = Ability, y = Probability, color = Category)) +
    geom_line() +
    ggtitle(item_names()[item]) +
    coord_cartesian(ylim = c(0, 1)) +
    theme_app()

  if (isTRUE(input$IRT_poly_show_observed)) {
    fs <- as.vector(fscores(fit))
    n_bins <- input$IRT_poly_observed_groups %||% 3
    bins <- cut(fs, breaks = quantile(fs, probs = seq(0, 1, length.out = n_bins + 1)),
                include.lowest = TRUE)
    bin_mids <- tapply(fs, bins, mean)

    item_responses <- data[[item]]

    obs_df <- map_dfr(seq_along(unique_cats), function(idx) {
      k <- unique_cats[idx]
      obs_prop <- tapply(as.numeric(item_responses == k), bins, mean, na.rm = TRUE)
      tibble(
        Ability = as.numeric(bin_mids),
        Probability = as.numeric(obs_prop),
        Category = cat_names[idx]
      )
    })
    obs_df$Category <- factor(obs_df$Category, levels = cat_names)
    g <- g + geom_point(data = obs_df, aes(x = Ability, y = Probability, color = Category),
                        size = 2, alpha = 0.7)
  }
  g
})

output$IRT_poly_items_icc <- renderPlotly({
  g <- IRT_poly_items_icc_reactive()
  p <- ggplotly(g)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_poly_items_icc_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_poly_", input$IRT_poly_model, "_ICC_",
           item_names()[input$IRT_poly_items], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_poly_items_icc_reactive() +
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

# ** IIC for selected item (Items) ####
IRT_poly_items_iic_reactive <- reactive({
  item <- input$IRT_poly_items
  req(item)
  fit <- IRT_poly_model()
  thetas <- IRT_thetas_for_plots()

  d <- tibble(
    Ability = thetas,
    Information = iteminfo(extract.item(fit, item), thetas)
  )

  g <- d |> ggplot(aes(x = Ability, y = Information)) +
    geom_line() +
    ggtitle(item_names()[item]) +
    theme_app()
  g
})

output$IRT_poly_items_iic <- renderPlotly({
  g <- IRT_poly_items_iic_reactive()
  p <- ggplotly(g)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_poly_items_iic_download <- downloadHandler(
  filename = function() {
    paste0("fig_IRT_poly_", input$IRT_poly_model, "_IIC_",
           item_names()[input$IRT_poly_items], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_poly_items_iic_reactive() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)

# ** Parameter table for selected item ####
output$IRT_poly_items_coef <- renderTable(
  {
    item <- input$IRT_poly_items
    req(item)
    IRT_poly_summary_coef_reactive()[item, , drop = FALSE]
  },
  rownames = TRUE, striped = TRUE, na = ""
)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL COMPARISON ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Helper: check whether RSM is viable (all items same number of categories)
IRT_poly_rsm_viable <- reactive({
  data <- ordinal()
  n_cats <- sapply(data, function(x) length(unique(x)))
  length(unique(n_cats)) == 1
})

# ** Convergence check ####
output$IRT_poly_comparison_model_converged <- renderUI({
  fitNRM <- IRT_poly_model_nrm()
  fitGRM <- IRT_poly_model_grm()
  fitPCM <- IRT_poly_model_pcm()
  fitGPCM <- IRT_poly_model_gpcm()
  rsm_ok <- IRT_poly_rsm_viable()

  txt_nrm <- ifelse(extract.mirt(fitNRM, "converged"), "",
    "Estimation process in the <b>NRM</b> terminated without convergence. <br>")
  txt_grm <- ifelse(extract.mirt(fitGRM, "converged"), "",
    "Estimation process in the <b>GRM</b> terminated without convergence. <br>")
  txt_rsm <- ""
  if (rsm_ok) {
    fitRSM <- IRT_poly_model_rsm()
    txt_rsm <- ifelse(extract.mirt(fitRSM, "converged"), "",
      "Estimation process in the <b>RSM</b> terminated without convergence. <br>")
  }
  txt_pcm <- ifelse(extract.mirt(fitPCM, "converged"), "",
    "Estimation process in the <b>PCM</b> terminated without convergence. <br>")
  txt_gpcm <- ifelse(extract.mirt(fitGPCM, "converged"), "",
    "Estimation process in the <b>GPCM</b> terminated without convergence. <br>")

  txt <- paste0(txt_nrm, txt_grm, txt_rsm, txt_pcm, txt_gpcm)
  if (!rsm_ok) {
    txt <- paste0(txt,
      "<b>RSM</b> excluded: items do not all have the same number of response categories. <br>")
  }
  if (txt != "") {
    txt <- paste0("<font color = 'orange'>", txt, "</font>")
  }
  HTML(txt)
})

# ** Information criteria table ####
IRT_poly_comparison <- reactive({
  fitNRM <- IRT_poly_model_nrm()
  fitGRM <- IRT_poly_model_grm()
  fitPCM <- IRT_poly_model_pcm()
  fitGPCM <- IRT_poly_model_gpcm()
  rsm_ok <- IRT_poly_rsm_viable()

  get_ic <- function(fit) {
    aic_val <- tryCatch(fit@Fit$AIC, error = function(e) NA_real_)
    bic_val <- tryCatch(fit@Fit$BIC, error = function(e) NA_real_)
    ll_val <- tryCatch(fit@Fit$logLik, error = function(e) NA_real_)
    data.frame(
      AIC = round(aic_val, 3),
      BIC = round(bic_val, 3),
      logLik = round(ll_val, 3)
    )
  }

  na_row <- data.frame(AIC = NA_real_, BIC = NA_real_, logLik = NA_real_)

  df <- rbind(
    get_ic(fitNRM),
    get_ic(fitGRM),
    if (rsm_ok) get_ic(IRT_poly_model_rsm()) else na_row,
    get_ic(fitPCM),
    get_ic(fitGPCM)
  )

  nam <- c("NRM", "GRM", "RSM", "PCM", "GPCM")
  rownames(df) <- nam

  best_row <- data.frame(
    AIC = nam[which.min(df[, "AIC"])],
    BIC = nam[which.min(df[, "BIC"])],
    logLik = ""
  )
  df[] <- lapply(df, as.character)
  # Mark RSM as N/A if not viable
  if (!rsm_ok) df["RSM", ] <- "N/A"
  df <- rbind(df, best_row)
  rownames(df)[nrow(df)] <- "BEST"

  df
})

output$IRT_poly_comparison <- renderTable(
  IRT_poly_comparison(),
  rownames = TRUE, striped = TRUE
)

# ** LRT: RSM vs PCM ####
IRT_poly_lrt_rsm_pcm <- reactive({
  req(IRT_poly_rsm_viable())
  fitRSM <- IRT_poly_model_rsm()
  fitPCM <- IRT_poly_model_pcm()

  lrt <- anova(fitRSM, fitPCM)
  lrt <- round(lrt, 3)
  rownames(lrt) <- c("RSM", "PCM")
  lrt
})

output$IRT_poly_lrt_rsm_pcm <- renderTable(
  {
    if (!IRT_poly_rsm_viable()) {
      return(data.frame(Note = "RSM not available: items have unequal numbers of response categories."))
    }
    IRT_poly_lrt_rsm_pcm()
  },
  rownames = TRUE, striped = TRUE
)

# ** LRT: PCM vs GPCM ####
IRT_poly_lrt_pcm_gpcm <- reactive({
  fitPCM <- IRT_poly_model_pcm()
  fitGPCM <- IRT_poly_model_gpcm()

  lrt <- anova(fitPCM, fitGPCM)
  lrt <- round(lrt, 3)
  rownames(lrt) <- c("PCM", "GPCM")
  lrt
})

output$IRT_poly_lrt_pcm_gpcm <- renderTable(
  IRT_poly_lrt_pcm_gpcm(),
  rownames = TRUE, striped = TRUE
)
