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

# ** NRM model (fitted on ordinal data for comparison) ####
IRT_poly_model_nrm <- reactive({
  data <- ordinal()
  data_nom <- data |> purrr::modify(as.factor)
  fit <- mirt(
    data_nom,
    model = 1, itemtype = "nominal",
    SE = TRUE, verbose = FALSE,
    technical = list(NCYCLES = input$ncycles)
  )
  fit
})


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * TAB-BASED MODEL SELECTION ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Determine current model from which tab is active
IRT_poly_current_model <- reactive({
  tab <- input[["IRT models"]]
  switch(tab,
    "irt_poly_grm" = "GRM",
    "irt_poly_rsm" = "RSM",
    "irt_poly_pcm" = "PCM",
    "irt_poly_gpcm" = "GPCM",
    "GRM"
  )
})

# Selected model fit
IRT_poly_model <- reactive({
  fit <- switch(IRT_poly_current_model(),
    "GRM" = IRT_poly_model_grm(),
    "RSM" = IRT_poly_model_rsm(),
    "PCM" = IRT_poly_model_pcm(),
    "GPCM" = IRT_poly_model_gpcm()
  )
  fit
})

# Resolve current item slider
IRT_poly_current_item <- reactive({
  model <- IRT_poly_current_model()
  input[[paste0("IRT_poly_", tolower(model), "_items")]]
})

# Resolve current show_observed checkbox
IRT_poly_current_show_observed <- reactive({
  model <- IRT_poly_current_model()
  isTRUE(input[[paste0("IRT_poly_", tolower(model), "_show_observed")]])
})


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL DESCRIPTION ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

IRT_poly_model_description <- reactive({
  txt <- switch(IRT_poly_current_model(),
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
        "ordered polytomous items. It assumes equal step structure across all items \u2014 the ",
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


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * UPDATE ITEM SLIDERS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

observe({
  item_count <- ncol(ordinal())
  for (m in c("grm", "rsm", "pcm", "gpcm")) {
    updateSliderInput(
      session = session,
      inputId = paste0("IRT_poly_", m, "_items"),
      max = item_count
    )
  }
})


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * PER-MODEL OUTPUT GENERATION ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# This helper creates all the server outputs for one model, by defining reactives
# that call the shared computation functions.

local({
  models <- list(
    list(id = "GRM", lower = "grm", fit_fn = "IRT_poly_model_grm", show_wright = FALSE),
    list(id = "RSM", lower = "rsm", fit_fn = "IRT_poly_model_rsm", show_wright = TRUE),
    list(id = "PCM", lower = "pcm", fit_fn = "IRT_poly_model_pcm", show_wright = TRUE),
    list(id = "GPCM", lower = "gpcm", fit_fn = "IRT_poly_model_gpcm", show_wright = FALSE)
  )

  for (m in models) {
    local({
      model_id <- m$id
      model_lower <- m$lower
      get_fit <- m$fit_fn
      show_wright <- m$show_wright

      # Convergence check
      output[[paste0("IRT_poly_", model_lower, "_model_converged")]] <- renderUI({
        fit <- get(get_fit)()
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

      # ==== SUMMARY ====

      # ** Table of parameters (Summary) ####
      coef_reactive_name <- paste0("IRT_poly_", model_lower, "_summary_coef_reactive")
      assign(coef_reactive_name, reactive({
        fit <- get(get_fit)()
        use_irt <- model_id %in% c("GRM", "GPCM")
        par_tab <- coef(fit, IRTpars = use_irt, simplify = TRUE)$items

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

        tab <- cbind(par_tab, se_tab)[, order(c(seq(ncol(par_tab)), seq(ncol(se_tab))))]
        par_names <- colnames(par_tab)
        col_names <- as.vector(rbind(par_names, paste0("SE(", par_names, ")")))
        colnames(tab) <- col_names

        tab_fit <- tryCatch(
          itemfit(fit, na.rm = TRUE)[, c("S_X2", "df.S_X2", "p.S_X2")],
          error = function(e) NULL
        )
        if (!is.null(tab_fit)) {
          tab <- data.frame(tab, tab_fit)
          fit_cols <- (ncol(tab) - 2):ncol(tab)
          colnames(tab)[fit_cols] <- c("SX2-value", "df", "p-value")
        }

        if (model_id == "PCM") {
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
      }), envir = parent.env(environment()))

      output[[paste0("IRT_poly_", model_lower, "_summary_coef")]] <- renderTable(
        get(coef_reactive_name)(),
        rownames = TRUE, striped = TRUE, na = ""
      )

      output[[paste0("IRT_poly_", model_lower, "_summary_coef_download")]] <- downloadHandler(
        filename = function() {
          paste0("tab_IRT_poly_", model_id, "_parameters.csv")
        },
        content = function(file) {
          write.csv(get(coef_reactive_name)(), file)
        }
      )

      # ** Expected item score curves (Summary) ####
      expected_summary_name <- paste0("IRT_poly_", model_lower, "_summary_expected_reactive")
      assign(expected_summary_name, reactive({
        fit <- get(get_fit)()
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
        g
      }), envir = parent.env(environment()))

      output[[paste0("IRT_poly_", model_lower, "_summary_expected")]] <- renderPlotly({
        g <- get(expected_summary_name)()
        p <- ggplotly(g)
        p$elementId <- NULL
        p |> plotly::config(displayModeBar = FALSE)
      })

      output[[paste0("IRT_poly_", model_lower, "_summary_expected_download")]] <- downloadHandler(
        filename = function() {
          paste0("fig_IRT_poly_", model_id, "_expected.png")
        },
        content = function(file) {
          ggsave(file,
            plot = get(expected_summary_name)() +
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
      iic_summary_name <- paste0("IRT_poly_", model_lower, "_summary_iic_reactive")
      assign(iic_summary_name, reactive({
        fit <- get(get_fit)()
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
      }), envir = parent.env(environment()))

      output[[paste0("IRT_poly_", model_lower, "_summary_iic")]] <- renderPlotly({
        g <- get(iic_summary_name)()
        p <- ggplotly(g)
        p$elementId <- NULL
        p |> plotly::config(displayModeBar = FALSE)
      })

      output[[paste0("IRT_poly_", model_lower, "_summary_iic_download")]] <- downloadHandler(
        filename = function() {
          paste0("fig_IRT_poly_", model_id, "_IIC.png")
        },
        content = function(file) {
          ggsave(file,
            plot = get(iic_summary_name)() +
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
      tic_summary_name <- paste0("IRT_poly_", model_lower, "_summary_tic_reactive")
      assign(tic_summary_name, reactive({
        fit <- get(get_fit)()
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
      }), envir = parent.env(environment()))

      output[[paste0("IRT_poly_", model_lower, "_summary_tic")]] <- renderPlotly({
        g <- get(tic_summary_name)()
        p <- ggplotly(g)
        p$x$data[[1]]$text <- gsub("<br />colour: info", "", p$x$data[[1]]$text)
        p$x$data[[2]]$text <- gsub("<br />colour: se", "", p$x$data[[2]]$text)
        p$elementId <- NULL
        p |> plotly::config(displayModeBar = FALSE)
      })

      output[[paste0("IRT_poly_", model_lower, "_summary_tic_download")]] <- downloadHandler(
        filename = function() {
          paste0("fig_IRT_poly_", model_id, "_TIC.png")
        },
        content = function(file) {
          ggsave(file,
            plot = get(tic_summary_name)() +
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
      fscores_name <- paste0("IRT_poly_", model_lower, "_fscores_reactive")
      assign(fscores_name, reactive({
        fit <- get(get_fit)()
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
      }), envir = parent.env(environment()))

      output[[paste0("IRT_poly_", model_lower, "_summary_ability")]] <- renderTable(
        {
          factors <- get(fscores_name)()
          head(factors, n = 6)
        },
        rownames = TRUE
      )

      output[[paste0("IRT_poly_", model_lower, "_summary_ability_download")]] <- downloadHandler(
        filename = function() {
          paste0("IRT_poly_", model_id, "_abilities.csv")
        },
        content = function(file) {
          write.csv(get(fscores_name)(), file)
        }
      )

      # Correlation text
      corr_name <- paste0("IRT_poly_", model_lower, "_corr_reactive")
      assign(corr_name, reactive({
        tab <- get(fscores_name)()
        cor(tab[["F-score"]], tab[["Z-score"]], use = "pairwise.complete.obs")
      }), envir = parent.env(environment()))

      output[[paste0("IRT_poly_", model_lower, "_summary_ability_correlation_text")]] <- renderText({
        paste0(
          "This scatterplot shows the relationship between the standardized total ",
          "score (Z-score) and the factor score estimated by the IRT model. The ",
          "Pearson correlation coefficient between these two scores is ",
          sprintf("%.3f", get(corr_name)()), ". "
        )
      })

      # Scatterplot
      ability_plot_name <- paste0("IRT_poly_", model_lower, "_ability_plot_reactive")
      assign(ability_plot_name, reactive({
        df <- get(fscores_name)()
        ggplot(df, aes(`Z-score`, `F-score`)) +
          geom_point(size = 3) +
          labs(x = "Standardized total score", y = "Factor score") +
          theme_app()
      }), envir = parent.env(environment()))

      output[[paste0("IRT_poly_", model_lower, "_summary_ability_plot")]] <- renderPlotly({
        g <- get(ability_plot_name)()
        p <- ggplotly(g)
        p$elementId <- NULL
        p |> plotly::config(displayModeBar = FALSE)
      })

      output[[paste0("IRT_poly_", model_lower, "_summary_ability_plot_download")]] <- downloadHandler(
        filename = function() {
          paste0("fig_IRT_poly_", model_id, "_abilities.png")
        },
        content = function(file) {
          ggsave(file,
            plot = get(ability_plot_name)() +
              theme(text = element_text(size = setting_figures$text_size)),
            device = "png",
            height = setting_figures$height, width = setting_figures$width,
            dpi = setting_figures$dpi
          )
        }
      )

      # ** Wright map (PCM and RSM only) ####
      if (show_wright) {
        wright_args_name <- paste0("IRT_poly_", model_lower, "_wrightmap_args_reactive")
        assign(wright_args_name, reactive({
          fit <- get(get_fit)()
          fscore <- as.vector(fscores(fit))
          pars <- coef(fit, simplify = TRUE)$items
          d_cols <- grep("^d", colnames(pars))
          b <- pars[, d_cols, drop = FALSE]
          item.names <- item_names()
          list(theta = fscore, b = b, item.names = item.names)
        }), envir = parent.env(environment()))

        output[[paste0("IRT_poly_", model_lower, "_summary_wrightmap")]] <- renderPlotly({
          args <- get(wright_args_name)()
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

        output[[paste0("IRT_poly_", model_lower, "_summary_wrightmap_download")]] <- downloadHandler(
          filename = function() {
            paste0("fig_IRT_poly_", model_id, "_WrightMap.png")
          },
          content = function(file) {
            args <- get(wright_args_name)()
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
      }


      # ==== ITEMS ====

      # ** Expected item score curve (Items) ####
      expected_items_name <- paste0("IRT_poly_", model_lower, "_items_expected_reactive")
      assign(expected_items_name, reactive({
        item <- input[[paste0("IRT_poly_", model_lower, "_items")]]
        req(item)
        fit <- get(get_fit)()
        thetas <- IRT_thetas_for_plots()

        probs <- probtrace(extract.item(fit, item), thetas)
        n_cats <- ncol(probs)
        exp_score <- probs %*% (0:(n_cats - 1))

        d <- tibble(Ability = thetas, Expected = as.numeric(exp_score))

        g <- d |> ggplot(aes(x = Ability, y = Expected)) +
          geom_line() +
          ylab("Expected item score") +
          ggtitle(item_names()[item]) +
          theme_app()

        if (isTRUE(input[[paste0("IRT_poly_", model_lower, "_show_observed")]])) {
          data <- ordinal()
          fs <- as.vector(fscores(fit))
          n_bins <- 10
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
      }), envir = parent.env(environment()))

      output[[paste0("IRT_poly_", model_lower, "_items_expected")]] <- renderPlotly({
        g <- get(expected_items_name)()
        p <- ggplotly(g)
        p$elementId <- NULL
        p |> plotly::config(displayModeBar = FALSE)
      })

      output[[paste0("IRT_poly_", model_lower, "_items_expected_download")]] <- downloadHandler(
        filename = function() {
          item <- input[[paste0("IRT_poly_", model_lower, "_items")]]
          paste0("fig_IRT_poly_", model_id, "_expected_", item_names()[item], ".png")
        },
        content = function(file) {
          ggsave(file,
            plot = get(expected_items_name)() +
              theme(text = element_text(size = setting_figures$text_size)),
            device = "png",
            height = setting_figures$height, width = setting_figures$width,
            dpi = setting_figures$dpi
          )
        }
      )

      # ** ICC for selected item (Items) ####
      icc_items_name <- paste0("IRT_poly_", model_lower, "_items_icc_reactive")
      assign(icc_items_name, reactive({
        item <- input[[paste0("IRT_poly_", model_lower, "_items")]]
        req(item)
        fit <- get(get_fit)()
        thetas <- IRT_thetas_for_plots()

        probs <- as_tibble(probtrace(extract.item(fit, item), thetas))
        # Use actual response values from ordinal data for category names
        data <- ordinal()
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

        if (isTRUE(input[[paste0("IRT_poly_", model_lower, "_show_observed")]])) {
          fs <- as.vector(fscores(fit))
          n_bins <- 10
          bins <- cut(fs, breaks = quantile(fs, probs = seq(0, 1, length.out = n_bins + 1)),
                      include.lowest = TRUE)
          bin_mids <- tapply(fs, bins, mean)

          item_responses <- data[[item]]
          n_cats <- length(cat_names)

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
      }), envir = parent.env(environment()))

      output[[paste0("IRT_poly_", model_lower, "_items_icc")]] <- renderPlotly({
        g <- get(icc_items_name)()
        p <- ggplotly(g)
        p$elementId <- NULL
        p |> plotly::config(displayModeBar = FALSE)
      })

      output[[paste0("IRT_poly_", model_lower, "_items_icc_download")]] <- downloadHandler(
        filename = function() {
          item <- input[[paste0("IRT_poly_", model_lower, "_items")]]
          paste0("fig_IRT_poly_", model_id, "_ICC_", item_names()[item], ".png")
        },
        content = function(file) {
          ggsave(file,
            plot = get(icc_items_name)() +
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
      iic_items_name <- paste0("IRT_poly_", model_lower, "_items_iic_reactive")
      assign(iic_items_name, reactive({
        item <- input[[paste0("IRT_poly_", model_lower, "_items")]]
        req(item)
        fit <- get(get_fit)()
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
      }), envir = parent.env(environment()))

      output[[paste0("IRT_poly_", model_lower, "_items_iic")]] <- renderPlotly({
        g <- get(iic_items_name)()
        p <- ggplotly(g)
        p$elementId <- NULL
        p |> plotly::config(displayModeBar = FALSE)
      })

      output[[paste0("IRT_poly_", model_lower, "_items_iic_download")]] <- downloadHandler(
        filename = function() {
          item <- input[[paste0("IRT_poly_", model_lower, "_items")]]
          paste0("fig_IRT_poly_", model_id, "_IIC_", item_names()[item], ".png")
        },
        content = function(file) {
          ggsave(file,
            plot = get(iic_items_name)() +
              theme(text = element_text(size = setting_figures$text_size)),
            device = "png",
            height = setting_figures$height, width = setting_figures$width,
            dpi = setting_figures$dpi
          )
        }
      )

      # ** Parameter table for selected item ####
      output[[paste0("IRT_poly_", model_lower, "_items_coef")]] <- renderTable(
        {
          item <- input[[paste0("IRT_poly_", model_lower, "_items")]]
          req(item)
          get(coef_reactive_name)()[item, , drop = FALSE]
        },
        rownames = TRUE, striped = TRUE, na = ""
      )
    })
  }
})


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL COMPARISON ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Convergence check ####
output$IRT_poly_comparison_model_converged <- renderUI({
  fitGRM <- IRT_poly_model_grm()
  fitRSM <- IRT_poly_model_rsm()
  fitPCM <- IRT_poly_model_pcm()
  fitGPCM <- IRT_poly_model_gpcm()
  fitNRM <- IRT_poly_model_nrm()

  txt_grm <- ifelse(extract.mirt(fitGRM, "converged"), "",
    "Estimation process in the <b>GRM</b> terminated without convergence. <br>")
  txt_rsm <- ifelse(extract.mirt(fitRSM, "converged"), "",
    "Estimation process in the <b>RSM</b> terminated without convergence. <br>")
  txt_pcm <- ifelse(extract.mirt(fitPCM, "converged"), "",
    "Estimation process in the <b>PCM</b> terminated without convergence. <br>")
  txt_gpcm <- ifelse(extract.mirt(fitGPCM, "converged"), "",
    "Estimation process in the <b>GPCM</b> terminated without convergence. <br>")
  txt_nrm <- ifelse(extract.mirt(fitNRM, "converged"), "",
    "Estimation process in the <b>NRM</b> terminated without convergence. <br>")

  txt <- paste0(txt_grm, txt_rsm, txt_pcm, txt_gpcm, txt_nrm)
  if (txt != "") {
    txt <- paste0(
      "<font color = 'orange'>", txt,
      "Estimates are not reliable. Try to increase a number of iterations ",
      "of the EM algorithm in Settings. </font>"
    )
  }
  HTML(txt)
})

# ** Information criteria table ####
IRT_poly_comparison <- reactive({
  fitRSM <- IRT_poly_model_rsm()
  fitPCM <- IRT_poly_model_pcm()
  fitGPCM <- IRT_poly_model_gpcm()
  fitGRM <- IRT_poly_model_grm()
  fitNRM <- IRT_poly_model_nrm()

  # Get AIC/BIC/logLik for each model individually
  get_ic <- function(fit) {
    data.frame(
      AIC = round(fit@Fit$AIC, 3),
      BIC = round(fit@Fit$BIC, 3),
      logLik = round(fit@Fit$logLik, 3)
    )
  }

  df <- rbind(
    get_ic(fitNRM),
    get_ic(fitRSM),
    get_ic(fitPCM),
    get_ic(fitGPCM),
    get_ic(fitGRM)
  )

  nam <- c("NRM", "RSM", "PCM", "GPCM", "GRM")
  rownames(df) <- nam

  best_row <- c(
    nam[which.min(df[, "AIC"])],
    nam[which.min(df[, "BIC"])],
    ""
  )
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
  fitRSM <- IRT_poly_model_rsm()
  fitPCM <- IRT_poly_model_pcm()

  lrt <- anova(fitRSM, fitPCM)
  lrt <- round(lrt, 3)
  rownames(lrt) <- c("RSM", "PCM")
  lrt
})

output$IRT_poly_lrt_rsm_pcm <- renderTable(
  IRT_poly_lrt_rsm_pcm(),
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
