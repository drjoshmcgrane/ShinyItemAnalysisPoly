# Polytomous IRT Models, Fit Statistics, and Model Comparison — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add infit/outfit for Rasch-family models, polytomous IRT model fitting (GRM, RSM, PCM, GPCM) with full Shiny tabs, observed proportions overlay on ICCs, expected value curves, and polytomous model comparison with LRT.

**Architecture:** All polytomous model server logic lives in a single new file `server/IRT/polytomous_irt.R`, sourced from `server/IRT.R`. A new UI file `ui/uiIRT/uiPolyIRTModels.R` defines the tab. Dichotomous changes (infit/outfit, observed proportions) are modifications to existing `server/IRT.R` and `ui/uiIRT.R`. All models use `mirt` which is already a dependency.

**Tech Stack:** R, Shiny, mirt, ggplot2, plotly, dplyr/tibble/purrr/tidyr (all already imported)

---

## File Structure

### New Files
| File | Responsibility |
|------|---------------|
| `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R` | Server logic: model fitting, parameter tables, plots, abilities, comparison for GRM/RSM/PCM/GPCM |
| `inst/ShinyItemAnalysis/ui/uiIRT/uiPolyIRTModels.R` | UI: polytomous models tab (Summary, Items subtabs) and comparison tab |
| `inst/ShinyItemAnalysis/sc/irt/grm.R` | Sample R code for GRM |
| `inst/ShinyItemAnalysis/sc/irt/rsm.R` | Sample R code for RSM |
| `inst/ShinyItemAnalysis/sc/irt/pcm.R` | Sample R code for PCM |
| `inst/ShinyItemAnalysis/sc/irt/gpcm.R` | Sample R code for GPCM |
| `inst/ShinyItemAnalysis/sc/irt/poly_comp.R` | Sample R code for polytomous model comparison |

### Modified Files
| File | Change |
|------|--------|
| `inst/ShinyItemAnalysis/server/IRT.R` | Source new polytomous_irt.R; add infit/outfit to Rasch/1PL tables; add observed proportions to dichotomous ICCs |
| `inst/ShinyItemAnalysis/ui/uiIRT.R` | Source new UI; add polytomous models + comparison tabs to navbar; add observed proportions checkbox |
| `inst/ShinyItemAnalysis/sc/irt/rasch.R` | Add infit/outfit example |
| `inst/ShinyItemAnalysis/sc/irt/1pl.R` | Add infit/outfit example |

---

### Task 1: Add Infit/Outfit to Rasch/1PL Parameter Tables

**Files:**
- Modify: `inst/ShinyItemAnalysis/server/IRT.R:583-631` (the `IRT_binary_summary_coef` reactive)
- Modify: `inst/ShinyItemAnalysis/sc/irt/rasch.R`
- Modify: `inst/ShinyItemAnalysis/sc/irt/1pl.R`

- [ ] **Step 1: Modify `IRT_binary_summary_coef` to add infit/outfit for Rasch/1PL**

In `inst/ShinyItemAnalysis/server/IRT.R`, replace the existing `IRT_binary_summary_coef` reactive (lines 583-631) with:

```r
# ** Table of parameters ####
IRT_binary_summary_coef <- reactive({
  fit <- IRT_binary_model()
  n <- length(item_names())

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

  # Add infit/outfit for Rasch and 1PL models
  if (input$IRT_binary_summary_model %in% c("Rasch", "1PL")) {
    infit_tab <- tryCatch(
      itemfit(fit, fit_stats = "infit")[, c("outfit", "infit")],
      error = function(e) NULL
    )
    if (!is.null(infit_tab)) {
      tab <- data.frame(tab, infit_tab)
      colnames(tab)[(ncol(tab) - 1):ncol(tab)] <- c("Outfit MNSQ", "Infit MNSQ")
    }
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
    tab <- tab[, c(3:4, 1:2, 5:8, 9:ncol(tab))]
  }

  rownames(tab) <- item_names()
  tab
})
```

- [ ] **Step 2: Update the download handler for parameter table**

In `inst/ShinyItemAnalysis/server/IRT.R`, find the `IRT_binary_summary_coef_download` handler (lines 642-665). The existing CSV download code references `colnames(tab)[1:8]` — this still works because the infit/outfit columns are appended at the end with plain-text names that don't need LaTeX cleanup. No change needed here.

- [ ] **Step 3: Update the UI description text for the parameter table**

In `inst/ShinyItemAnalysis/ui/uiIRT.R`, find the paragraph at lines 102-107 that says:

```r
        p(
          "Estimates of item parameters can be displayed using the IRT or intercept/slope ",
          strong("parametrization,"), "which can be selected at the top of this tab. Parameter estimates
                 are completed by SX2 item fit statistics (Orlando & Thissen, 2000). SX2 statistics are computed
                 only when no missing data are present."
        ),
```

Replace with:

```r
        p(
          "Estimates of item parameters can be displayed using the IRT or intercept/slope ",
          strong("parametrization,"), "which can be selected at the top of this tab. Parameter estimates
                 are completed by SX2 item fit statistics (Orlando & Thissen, 2000). SX2 statistics are computed
                 only when no missing data are present. For Rasch and 1PL models, infit and outfit mean-square
                 statistics (Wright & Masters, 1982) are also provided."
        ),
```

Do the same for the Items subtab parameter table description at lines 249-254.

- [ ] **Step 4: Update sample R code for Rasch**

In `inst/ShinyItemAnalysis/sc/irt/rasch.R`, after line 30 (`itemfit(fit)`), add:

```r
# infit and outfit statistics
itemfit(fit, fit_stats = "infit")
```

- [ ] **Step 5: Update sample R code for 1PL**

In `inst/ShinyItemAnalysis/sc/irt/1pl.R`, after line 58 (`itemfit(fit)`), add:

```r
# infit and outfit statistics
itemfit(fit, fit_stats = "infit")
```

- [ ] **Step 6: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT.R inst/ShinyItemAnalysis/ui/uiIRT.R inst/ShinyItemAnalysis/sc/irt/rasch.R inst/ShinyItemAnalysis/sc/irt/1pl.R
git commit -m "feat: add infit/outfit MNSQ statistics for Rasch and 1PL models"
```

---

### Task 2: Add Observed Proportions Toggle to Dichotomous ICC Plots

**Files:**
- Modify: `inst/ShinyItemAnalysis/server/IRT.R:408-431` (Summary ICC reactive) and `:859-880` (Items ICC reactive)
- Modify: `inst/ShinyItemAnalysis/ui/uiIRT.R:42-68` (Summary controls) and `:189-223` (Items controls)

- [ ] **Step 1: Add checkbox to Summary subtab UI**

In `inst/ShinyItemAnalysis/ui/uiIRT.R`, after the closing `)` of the `fluidRow` containing model/parametrization selectors (after line 68), add:

```r
        fluidRow(
          column(
            3,
            checkboxInput(
              inputId = "IRT_binary_summary_show_observed",
              label = "Show observed proportions",
              value = FALSE
            )
          )
        ),
```

- [ ] **Step 2: Add checkbox to Items subtab UI**

In `inst/ShinyItemAnalysis/ui/uiIRT.R`, within the Items tab fluidRow (around line 218, after the item slider column), add a new column:

```r
          column(
            3,
            checkboxInput(
              inputId = "IRT_binary_items_show_observed",
              label = "Show observed proportions",
              value = FALSE
            )
          )
```

- [ ] **Step 3: Modify Summary ICC reactive to overlay observed proportions**

In `inst/ShinyItemAnalysis/server/IRT.R`, replace the `IRT_binary_summary_icc` reactive (lines 408-431) with:

```r
# ** Plot of ICC ####
IRT_binary_summary_icc <- reactive({
  fit <- IRT_binary_model()

  # names from the model
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

  # Overlay observed proportions if toggled on
  if (isTRUE(input$IRT_binary_summary_show_observed)) {
    data <- binary()
    fs <- as.vector(fscores(fit))
    n_bins <- 10
    bins <- cut(fs, breaks = quantile(fs, probs = seq(0, 1, length.out = n_bins + 1)),
                include.lowest = TRUE)
    bin_mids <- tapply(fs, bins, mean)

    obs_df <- map2_dfr(
      seq_len(ncol(data)),
      item_names(),
      ~ {
        obs_prop <- tapply(data[[.x]], bins, mean, na.rm = TRUE)
        tibble(
          Ability = as.numeric(bin_mids),
          Probability = as.numeric(obs_prop),
          Item = .y
        )
      }
    )
    obs_df$Item <- factor(obs_df$Item, levels = item_names())
    g <- g + geom_point(data = obs_df, aes(x = Ability, y = Probability, color = Item),
                        size = 2, alpha = 0.7)
  }

  g
})
```

- [ ] **Step 4: Modify Items ICC reactive to overlay observed proportions**

In `inst/ShinyItemAnalysis/server/IRT.R`, replace the `IRT_binary_items_icc` reactive (lines 859-880) with:

```r
# ** Plot of ICC for selected item ####
IRT_binary_items_icc <- reactive({
  item <- input$IRT_binary_items
  fit <- IRT_binary_model()
  n_items <- extract.mirt(fit, "nitems")
  curve_col <- gg_color_hue(n_items)[item]

  d <- tibble(
    Ability = IRT_thetas_for_plots(), # vector only
    Probability = probtrace(
      extract.item(fit, item),
      IRT_thetas_for_plots()
    )[, 2] # ascending probs
  )

  g <- d |> ggplot(aes(x = Ability, y = Probability)) +
    geom_line(color = curve_col) +
    ylab("Probability of correct answer") +
    ggtitle(item_names()[item]) +
    ylim(0, 1) +
    theme_app()

  # Overlay observed proportions if toggled on
  if (isTRUE(input$IRT_binary_items_show_observed)) {
    data <- binary()
    fs <- as.vector(fscores(fit))
    n_bins <- 10
    bins <- cut(fs, breaks = quantile(fs, probs = seq(0, 1, length.out = n_bins + 1)),
                include.lowest = TRUE)
    bin_mids <- tapply(fs, bins, mean)
    obs_prop <- tapply(data[[item]], bins, mean, na.rm = TRUE)

    obs_df <- tibble(
      Ability = as.numeric(bin_mids),
      Probability = as.numeric(obs_prop)
    )
    g <- g + geom_point(data = obs_df, aes(x = Ability, y = Probability),
                        color = curve_col, size = 3, alpha = 0.7)
  }

  g
})
```

- [ ] **Step 5: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT.R inst/ShinyItemAnalysis/ui/uiIRT.R
git commit -m "feat: add observed proportions overlay toggle on dichotomous ICC plots"
```

---

### Task 3: Create Sample R Code Files for Polytomous Models

**Files:**
- Create: `inst/ShinyItemAnalysis/sc/irt/grm.R`
- Create: `inst/ShinyItemAnalysis/sc/irt/rsm.R`
- Create: `inst/ShinyItemAnalysis/sc/irt/pcm.R`
- Create: `inst/ShinyItemAnalysis/sc/irt/gpcm.R`
- Create: `inst/ShinyItemAnalysis/sc/irt/poly_comp.R`

- [ ] **Step 1: Create `grm.R`**

Create `inst/ShinyItemAnalysis/sc/irt/grm.R`:

```r
library(mirt)
library(ShinyItemAnalysis)

# loading data
data(CZmaturaS, package = "ShinyItemAnalysis")
Data <- CZmaturaS[, grep("^b", names(CZmaturaS))]

# fitting Graded Response Model (GRM)
fit <- mirt(Data, model = 1, itemtype = "graded", SE = TRUE)

# item characteristic curves (category probability curves)
plot(fit, type = "trace", facet_items = FALSE)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters - IRT parametrization
coef(fit, IRTpars = TRUE, simplify = TRUE)
# estimated parameters with SE
coef(fit, IRTpars = TRUE, printSE = TRUE)

# item fit statistics
itemfit(fit)

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(Data)))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)
```

- [ ] **Step 2: Create `pcm.R`**

Create `inst/ShinyItemAnalysis/sc/irt/pcm.R`:

```r
library(mirt)
library(ShinyItemAnalysis)

# loading data
data(CZmaturaS, package = "ShinyItemAnalysis")
Data <- CZmaturaS[, grep("^b", names(CZmaturaS))]

# fitting Partial Credit Model (PCM)
# mirt fits PCM when itemtype = "Rasch" is applied to polytomous data
fit <- mirt(Data, model = 1, itemtype = "Rasch", SE = TRUE)

# item characteristic curves (category probability curves)
plot(fit, type = "trace", facet_items = FALSE)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters
coef(fit, simplify = TRUE)
coef(fit, printSE = TRUE)

# item fit statistics
itemfit(fit)

# infit and outfit statistics
itemfit(fit, fit_stats = "infit")

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(Data)))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# Wright map
b <- coef(fit, simplify = TRUE)$items
ggWrightMap(fs, b[, grep("^d", colnames(b))])
```

- [ ] **Step 3: Create `rsm.R`**

Create `inst/ShinyItemAnalysis/sc/irt/rsm.R`:

```r
library(mirt)
library(ShinyItemAnalysis)

# loading data
data(CZmaturaS, package = "ShinyItemAnalysis")
Data <- CZmaturaS[, grep("^b", names(CZmaturaS))]

# fitting Rating Scale Model (RSM)
fit <- mirt(Data, model = 1, itemtype = "rsm", SE = TRUE)

# item characteristic curves (category probability curves)
plot(fit, type = "trace", facet_items = FALSE)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters
coef(fit, simplify = TRUE)
coef(fit, printSE = TRUE)

# item fit statistics
itemfit(fit)

# infit and outfit statistics
itemfit(fit, fit_stats = "infit")

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(Data)))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# Wright map
b <- coef(fit, simplify = TRUE)$items
ggWrightMap(fs, b[, grep("^d", colnames(b))])
```

- [ ] **Step 4: Create `gpcm.R`**

Create `inst/ShinyItemAnalysis/sc/irt/gpcm.R`:

```r
library(mirt)
library(ShinyItemAnalysis)

# loading data
data(CZmaturaS, package = "ShinyItemAnalysis")
Data <- CZmaturaS[, grep("^b", names(CZmaturaS))]

# fitting Generalized Partial Credit Model (GPCM)
fit <- mirt(Data, model = 1, itemtype = "gpcm", SE = TRUE)

# item characteristic curves (category probability curves)
plot(fit, type = "trace", facet_items = FALSE)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters - IRT parametrization
coef(fit, IRTpars = TRUE, simplify = TRUE)
coef(fit, IRTpars = TRUE, printSE = TRUE)

# item fit statistics
itemfit(fit)

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(Data)))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)
```

- [ ] **Step 5: Create `poly_comp.R`**

Create `inst/ShinyItemAnalysis/sc/irt/poly_comp.R`:

```r
library(mirt)
library(ShinyItemAnalysis)

# loading data
data(CZmaturaS, package = "ShinyItemAnalysis")
Data <- CZmaturaS[, grep("^b", names(CZmaturaS))]

# fitting polytomous IRT models
fitRSM <- mirt(Data, model = 1, itemtype = "rsm")
fitPCM <- mirt(Data, model = 1, itemtype = "Rasch")
fitGPCM <- mirt(Data, model = 1, itemtype = "gpcm")
fitGRM <- mirt(Data, model = 1, itemtype = "graded")

# information criteria for all models
anova(fitRSM, fitPCM, fitGPCM, fitGRM)

# likelihood ratio tests for nested models
anova(fitRSM, fitPCM) # RSM vs PCM
anova(fitPCM, fitGPCM) # PCM vs GPCM
```

- [ ] **Step 6: Commit**

```bash
git add inst/ShinyItemAnalysis/sc/irt/grm.R inst/ShinyItemAnalysis/sc/irt/rsm.R inst/ShinyItemAnalysis/sc/irt/pcm.R inst/ShinyItemAnalysis/sc/irt/gpcm.R inst/ShinyItemAnalysis/sc/irt/poly_comp.R
git commit -m "feat: add sample R code for polytomous IRT models and comparison"
```

---

### Task 4: Create Polytomous Models UI

**Files:**
- Create: `inst/ShinyItemAnalysis/ui/uiIRT/uiPolyIRTModels.R`
- Modify: `inst/ShinyItemAnalysis/ui/uiIRT.R`

- [ ] **Step 1: Create the polytomous models UI file**

Create `inst/ShinyItemAnalysis/ui/uiIRT/uiPolyIRTModels.R`:

```r
uiPolyIRTModels <- tabPanel(
  "Polytomous models",
  value = "irt_polytomous",
  tabsetPanel(

    # common header -----------------------------------------------------------
    header = tagList(
      h3("Polytomous IRT models"),
      p(
        "Polytomous IRT models extend item response theory to items with more than two",
        "ordered response categories. These models estimate item and person parameters",
        "using marginal maximum likelihood via the", strong("mirt"), "package."
      ),
      fluidRow(
        column(
          2,
          selectInput(
            inputId = "IRT_poly_model",
            label = "Model",
            choices = c(
              "GRM" = "GRM",
              "RSM" = "RSM",
              "PCM" = "PCM",
              "GPCM" = "GPCM"
            )
          )
        ),
        column(
          3,
          checkboxInput(
            inputId = "IRT_poly_show_observed",
            label = "Show observed proportions",
            value = FALSE
          )
        )
      ),
      uiOutput("IRT_poly_model_description"),
      uiOutput("IRT_poly_equation", inline = TRUE),
      uiOutput("IRT_poly_model_converged")
    ),


    ## Summary tab -------------------------------------------------------------
    tabPanel("Summary",
      value = "irt_poly_summary",
      fluidRow(
        column(
          12,
          # Expected item score curves
          div(
            style = "margin-bottom: 25px;",
            h4("Expected item score curves"),
            plotlyOutput("IRT_poly_summary_expected"),
            downloadButton(
              outputId = "IRT_poly_summary_expected_download",
              label = "Download figure"
            )
          ),

          # ICC plot
          div(
            style = "margin-bottom: 25px;",
            h4("Category probability curves"),
            p("For category probability curves of individual items, please see the",
              strong("Items"), "subtab."),
          ),

          # IIC plot
          div(
            style = "margin-bottom: 25px;",
            h4("Item information curves"),
            plotlyOutput("IRT_poly_summary_iic"),
            downloadButton(
              outputId = "IRT_poly_summary_iic_download",
              label = "Download figure"
            )
          ),

          # Test information and SE
          div(
            style = "margin-bottom: 25px;",
            h4("Test information curve and SE"),
            plotlyOutput("IRT_poly_summary_tic"),
            downloadButton(
              outputId = "IRT_poly_summary_tic_download",
              label = "Download figure"
            )
          ),

          # Table of parameters
          div(
            style = "margin-bottom: 25px;",
            h4("Table of estimated parameters"),
            p(
              "Parameter estimates are completed by SX2 item fit statistics",
              "(Orlando & Thissen, 2000). For PCM, infit and outfit mean-square",
              "statistics (Wright & Masters, 1982) are also provided."
            ),
            tableOutput("IRT_poly_summary_coef"),
            downloadButton(
              outputId = "IRT_poly_summary_coef_download",
              label = "Download table"
            )
          ),

          # Ability estimates
          div(
            style = "margin-bottom: 25px;",
            h4("Ability estimates"),
            p(
              "This table shows the response and factor scores for only six respondents.",
              "If you want to see the scores for all respondents, click on",
              strong("Download abilities"), "button."
            ),
            tableOutput("IRT_poly_summary_ability"),
            downloadButton(
              outputId = "IRT_poly_summary_ability_download",
              label = "Download abilities",
              style = "margin-bottom: 25px;"
            ),
            textOutput("IRT_poly_summary_ability_correlation_text"),
            plotlyOutput("IRT_poly_summary_ability_plot"),
            downloadButton(
              outputId = "IRT_poly_summary_ability_plot_download",
              label = "Download figure"
            )
          ),

          # Wright map (PCM and RSM only)
          conditionalPanel(
            condition = "input.IRT_poly_model == 'PCM' || input.IRT_poly_model == 'RSM'",
            div(
              style = "margin-bottom: 25px;",
              h4("Wright map"),
              p("The Wright map displays person ability estimates and item step",
                "parameters on one scale."),
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


    ## Items tab ---------------------------------------------------------------
    tabPanel("Items",
      value = "irt_poly_items",
      fluidRow(
        column(
          12,
          sliderInput(
            inputId = "IRT_poly_items",
            label = "Item",
            min = 1, value = 1, max = 20,
            step = 1, animate = TRUE
          ),

          # Expected item score curve
          div(
            style = "margin-bottom: 25px;",
            h4("Expected item score curve"),
            plotlyOutput("IRT_poly_items_expected"),
            downloadButton(
              outputId = "IRT_poly_items_expected_download",
              label = "Download figure"
            )
          ),

          # ICC plot
          div(
            style = "margin-bottom: 25px;",
            h4("Category probability curves"),
            plotlyOutput("IRT_poly_items_icc"),
            downloadButton(
              outputId = "IRT_poly_items_icc_download",
              label = "Download figure"
            )
          ),

          # IIC plot
          div(
            style = "margin-bottom: 25px;",
            h4("Item information curve"),
            plotlyOutput("IRT_poly_items_iic"),
            downloadButton(
              outputId = "IRT_poly_items_iic_download",
              label = "Download figure"
            )
          ),

          # Parameter table
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


# Polytomous model comparison UI
uiPolyIRTComparison <- tabPanel(
  "Polytomous model comparison",
  value = "irt_poly_comp",
  h3("Polytomous IRT model selection"),
  p(
    "Polytomous IRT models can be compared by several information criteria.",
    "Likelihood ratio tests (LRT) are provided for nested model pairs:",
    "RSM vs PCM (PCM relaxes the equal-step constraint) and",
    "PCM vs GPCM (GPCM adds item-specific discrimination).",
    "The GRM uses a different link function and is not nested with PCM/GPCM,",
    "so only information criteria are reported."
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
```

- [ ] **Step 2: Source the new UI and add tabs to the navbar menu**

In `inst/ShinyItemAnalysis/ui/uiIRT.R`, add a source line after line 7 (`source("ui/uiIRT/uiPolyIRT.R", ...)`):

```r
# source polytomous IRT models UI (GRM, RSM, PCM, GPCM)
source("ui/uiIRT/uiPolyIRTModels.R", local = T, encoding = "UTF-8")
```

Then in the `navbarMenu` definition, after line 309 (`uiPolyIRT,`), add:

```r
  uiPolyIRTModels,
  uiPolyIRTComparison,
```

So the polytomous section of the navbar becomes:

```r
  "----",
  "Polytomous models",
  uiPolyIRT, # UI sourced in the beginning of this .R file
  uiPolyIRTModels,
  uiPolyIRTComparison,
  "----",
```

- [ ] **Step 3: Commit**

```bash
git add inst/ShinyItemAnalysis/ui/uiIRT/uiPolyIRTModels.R inst/ShinyItemAnalysis/ui/uiIRT.R
git commit -m "feat: add UI for polytomous IRT models tab and comparison tab"
```

---

### Task 5: Create Polytomous Models Server — Model Fitting and Parameters

**Files:**
- Create: `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R`
- Modify: `inst/ShinyItemAnalysis/server/IRT.R` (add source line)

- [ ] **Step 1: Add source line in `server/IRT.R`**

In `inst/ShinyItemAnalysis/server/IRT.R`, after line 2 (`source("server/IRT/polytomous.R", ...)`), add:

```r
source("server/IRT/polytomous_irt.R", local = T, encoding = "UTF-8")
```

- [ ] **Step 2: Create `polytomous_irt.R` with model fitting reactives**

Create `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R` with the following content:

```r
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
```

- [ ] **Step 3: Add parameter table reactive**

Append to `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R`:

```r
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * SUMMARY ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Table of parameters ####
IRT_poly_summary_coef <- reactive({
  fit <- IRT_poly_model()

  # Extract parameters
  # GRM and GPCM support IRTpars, PCM and RSM use default parametrization
  use_irt <- input$IRT_poly_model %in% c("GRM", "GPCM")

  par_tab <- coef(fit, IRTpars = use_irt, simplify = TRUE)$items

  # SEs
  if (dim(fit@vcov)[1] > 1) {
    se_list <- coef(fit, IRTpars = use_irt, printSE = TRUE)
    se_list[["GroupPars"]] <- NULL
    se_tab <- do.call(rbind, lapply(seq_along(se_list), function(i) {
      se_row <- se_list[[i]]["SE", ]
      # Pad with NA if fewer params than max
      length(se_row) <- ncol(par_tab)
      se_row
    }))
  } else {
    se_tab <- matrix(NA, nrow = nrow(par_tab), ncol = ncol(par_tab))
  }

  # Interleave par and SE columns
  tab <- cbind(par_tab, se_tab)[, order(c(seq(ncol(par_tab)), seq(ncol(se_tab))))]

  # Rename columns: par, SE(par), par, SE(par), ...
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
```

- [ ] **Step 4: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R inst/ShinyItemAnalysis/server/IRT.R
git commit -m "feat: add polytomous IRT model fitting, descriptions, and parameter tables"
```

---

### Task 6: Add Polytomous ICC, IIC, TIC, and Expected Value Curve Plots

**Files:**
- Modify: `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R`

- [ ] **Step 1: Add expected item score curves (Summary)**

Append to `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R`:

```r
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
```

- [ ] **Step 2: Add IIC plot (Summary)**

Append to `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R`:

```r
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
```

- [ ] **Step 3: Add TIC + SE plot (Summary)**

Append to `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R`:

```r
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
```

- [ ] **Step 4: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R
git commit -m "feat: add expected score curves, IIC, and TIC plots for polytomous models"
```

---

### Task 7: Add Polytomous Ability Estimates and Wright Map

**Files:**
- Modify: `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R`

- [ ] **Step 1: Add ability estimates**

Append to `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R`:

```r
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
```

- [ ] **Step 2: Add Wright map for PCM/RSM**

Append to `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R`:

```r
# ** Wright map (PCM and RSM only) ####
IRT_poly_summary_wrightmap_args <- reactive({
  fit <- IRT_poly_model()
  fscore <- as.vector(fscores(fit))

  # Extract step/threshold parameters
  pars <- coef(fit, simplify = TRUE)$items
  # Get the d (step) columns
  d_cols <- grep("^d", colnames(pars))
  b <- pars[, d_cols, drop = FALSE]

  item.names <- item_names()

  list(theta = fscore, b = b, item.names = item.names)
})

output$IRT_poly_summary_wrightmap <- renderPlotly({
  args <- IRT_poly_summary_wrightmap_args()

  # For polytomous Wright map, we flatten step params
  # Each item-step becomes a separate entry
  b_matrix <- args$b
  item_names_rep <- rep(args$item.names, each = ncol(b_matrix))
  step_labels <- paste0(
    rep(args$item.names, each = ncol(b_matrix)),
    " Step ", rep(1:ncol(b_matrix), times = nrow(b_matrix))
  )
  b_vec <- as.vector(t(b_matrix))

  # Remove NA entries (items with fewer categories)
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
```

- [ ] **Step 3: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R
git commit -m "feat: add ability estimates and Wright map for polytomous models"
```

---

### Task 8: Add Polytomous Items Subtab (ICC, IIC, Expected, Parameters)

**Files:**
- Modify: `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R`

- [ ] **Step 1: Add Items subtab reactives**

Append to `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R`:

```r
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * ITEMS ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Expected item score curve (Items) ####
IRT_poly_items_expected <- reactive({
  item <- input$IRT_poly_items
  fit <- IRT_poly_model()
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

  # Overlay observed mean scores if toggled on
  if (isTRUE(input$IRT_poly_show_observed)) {
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
})

output$IRT_poly_items_expected <- renderPlotly({
  g <- IRT_poly_items_expected()
  p <- ggplotly(g)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_poly_items_expected_download <- downloadHandler(
  filename = function() {
    item <- input$IRT_poly_items
    paste0("fig_IRT_poly_", input$IRT_poly_model, "_expected_", item_names()[item], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_poly_items_expected() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# ** ICC for selected item (Items) ####
IRT_poly_items_icc <- reactive({
  item <- input$IRT_poly_items
  fit <- IRT_poly_model()
  thetas <- IRT_thetas_for_plots()

  probs <- as_tibble(probtrace(extract.item(fit, item), thetas))
  cat_names <- paste0("Cat ", 0:(ncol(probs) - 1))
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

  # Overlay observed category proportions if toggled on
  if (isTRUE(input$IRT_poly_show_observed)) {
    data <- ordinal()
    fs <- as.vector(fscores(fit))
    n_bins <- 10
    bins <- cut(fs, breaks = quantile(fs, probs = seq(0, 1, length.out = n_bins + 1)),
                include.lowest = TRUE)
    bin_mids <- tapply(fs, bins, mean)

    item_responses <- data[[item]]
    n_cats <- length(cat_names)

    obs_df <- map_dfr(0:(n_cats - 1), function(k) {
      obs_prop <- tapply(as.numeric(item_responses == k), bins, mean, na.rm = TRUE)
      tibble(
        Ability = as.numeric(bin_mids),
        Probability = as.numeric(obs_prop),
        Category = cat_names[k + 1]
      )
    })
    obs_df$Category <- factor(obs_df$Category, levels = cat_names)
    g <- g + geom_point(data = obs_df, aes(x = Ability, y = Probability, color = Category),
                        size = 2, alpha = 0.7)
  }

  g
})

output$IRT_poly_items_icc <- renderPlotly({
  g <- IRT_poly_items_icc()
  p <- ggplotly(g)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_poly_items_icc_download <- downloadHandler(
  filename = function() {
    item <- input$IRT_poly_items
    paste0("fig_IRT_poly_", input$IRT_poly_model, "_ICC_", item_names()[item], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_poly_items_icc() +
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
IRT_poly_items_iic <- reactive({
  item <- input$IRT_poly_items
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
  g <- IRT_poly_items_iic()
  p <- ggplotly(g)
  p$elementId <- NULL
  p |> plotly::config(displayModeBar = FALSE)
})

output$IRT_poly_items_iic_download <- downloadHandler(
  filename = function() {
    item <- input$IRT_poly_items
    paste0("fig_IRT_poly_", input$IRT_poly_model, "_IIC_", item_names()[item], ".png")
  },
  content = function(file) {
    ggsave(file,
      plot = IRT_poly_items_iic() +
        theme(text = element_text(size = setting_figures$text_size)),
      device = "png",
      height = setting_figures$height, width = setting_figures$width,
      dpi = setting_figures$dpi
    )
  }
)


# ** Parameter table for selected item ####
IRT_poly_items_coef <- reactive({
  item <- input$IRT_poly_items
  IRT_poly_summary_coef()[item, , drop = FALSE]
})

output$IRT_poly_items_coef <- renderTable(
  IRT_poly_items_coef(),
  rownames = TRUE, striped = TRUE, na = ""
)
```

- [ ] **Step 2: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R
git commit -m "feat: add Items subtab with ICC, IIC, expected curves for polytomous models"
```

---

### Task 9: Add Polytomous Model Comparison

**Files:**
- Modify: `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R`

- [ ] **Step 1: Add model comparison reactives**

Append to `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R`:

```r
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# * MODEL COMPARISON ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ** Convergence check ####
output$IRT_poly_comparison_model_converged <- renderUI({
  fitGRM <- IRT_poly_model_grm()
  fitRSM <- IRT_poly_model_rsm()
  fitPCM <- IRT_poly_model_pcm()
  fitGPCM <- IRT_poly_model_gpcm()

  txt_grm <- ifelse(extract.mirt(fitGRM, "converged"), "",
    "Estimation process in the <b>GRM</b> terminated without convergence. <br>")
  txt_rsm <- ifelse(extract.mirt(fitRSM, "converged"), "",
    "Estimation process in the <b>RSM</b> terminated without convergence. <br>")
  txt_pcm <- ifelse(extract.mirt(fitPCM, "converged"), "",
    "Estimation process in the <b>PCM</b> terminated without convergence. <br>")
  txt_gpcm <- ifelse(extract.mirt(fitGPCM, "converged"), "",
    "Estimation process in the <b>GPCM</b> terminated without convergence. <br>")

  txt <- paste0(txt_grm, txt_rsm, txt_pcm, txt_gpcm)
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

  df <- anova(fitRSM, fitPCM, fitGPCM, fitGRM)
  df <- round(df, 3)
  df <- df[, c("AIC", "BIC", "logLik")]

  nam <- c("RSM", "PCM", "GPCM", "GRM")
  rownames(df) <- nam

  # BEST row
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
```

- [ ] **Step 2: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R
git commit -m "feat: add polytomous model comparison with LRT for nested models"
```

---

### Task 10: Final Integration and Smoke Test

**Files:**
- All previously created/modified files

- [ ] **Step 1: Verify all source calls are wired up**

Check that `inst/ShinyItemAnalysis/server/IRT.R` has these source lines at the top:

```r
source("server/IRT/polytomous.R", local = T, encoding = "UTF-8")
source("server/IRT/polytomous_irt.R", local = T, encoding = "UTF-8")
source("server/IRT/training.R", local = T, encoding = "UTF-8")
```

Check that `inst/ShinyItemAnalysis/ui/uiIRT.R` has these source lines at the top:

```r
source("ui/uiIRT/uiDIRT.R", local = T, encoding = "UTF-8")
source("ui/uiIRT/uiPolyIRT.R", local = T, encoding = "UTF-8")
source("ui/uiIRT/uiPolyIRTModels.R", local = T, encoding = "UTF-8")
source("ui/uiIRT/uiPolyTraining.R", local = T, encoding = "UTF-8")
```

And the navbar menu includes `uiPolyIRTModels` and `uiPolyIRTComparison` after `uiPolyIRT`.

- [ ] **Step 2: Launch the app and test**

```r
library(ShinyItemAnalysis)
run_app()
```

Verify:
1. Navigate to IRT models > Dichotomous models > Summary. Select Rasch model. Check that Infit MNSQ and Outfit MNSQ columns appear in the parameter table.
2. Toggle "Show observed proportions" checkbox. Check that points appear on the ICC plot.
3. Navigate to IRT models > Polytomous models. Select each model (GRM, RSM, PCM, GPCM). Check that:
   - Model description and equation display correctly
   - Expected item score curves render
   - IIC and TIC plots render
   - Parameter table populates (with infit/outfit for PCM)
   - Ability estimates table shows
   - Wright map appears for PCM and RSM
4. Switch to Items subtab. Slide through items, check ICC/IIC/expected plots render.
5. Toggle "Show observed proportions" on polytomous ICC and expected score plots.
6. Navigate to Polytomous model comparison. Check IC table and LRT tables display.

- [ ] **Step 3: Final commit**

```bash
git add -A
git commit -m "feat: complete polytomous IRT models integration with fit statistics and model comparison"
```
