# ShinyItemAnalysisPoly <img src="inst/ShinyItemAnalysis/www/sia_logo.svg" align="right" width=150/>

Extended polytomous IRT and DIF analysis for [ShinyItemAnalysis](https://github.com/patriciamar/ShinyItemAnalysis)

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

---

## About

ShinyItemAnalysisPoly is a fork of [ShinyItemAnalysis](https://github.com/patriciamar/ShinyItemAnalysis) (v1.5.5) by Martinkova, Hladka, and Netik. It extends the interactive Shiny application with polytomous IRT models, IRT-based DIF matching, and improved missing data handling. Maintained by [Josh McGrane](https://github.com/drjoshmcgrane) for teaching and research.

All original ShinyItemAnalysis functionality is preserved.

## What's different in this fork

### Polytomous IRT models

- **Full polytomous IRT support**: GRM, RSM, PCM, and GPCM alongside the existing NRM, with a unified dropdown model selector.
- **Redesigned parameter tables**: interleaved SE columns, SX2 item fit statistics, and infit/outfit MNSQ for Rasch-family models (RSM, PCM).
- **Delta-method SEs** for derived RSM threshold parameters.
- **Model comparison tab**: AIC, BIC, log-likelihood, and likelihood ratio tests for nested model pairs (RSM vs PCM, PCM vs GPCM).
- **Wright maps** for RSM, PCM, and GPCM with aligned item/person panels and step-parameter legend.

### DIF/Fairness enhancements

- **IRT theta matching criterion** added to all regression-based DIF methods: logistic, NLR, cumulative logit, adjacent category logit, and multinomial (DDF).
- **No-listwise-deletion wrappers** for `difNLR`, `difORD`, and `ddfMLR`: theta-matched DIF is routed through per-item wrappers that preserve respondents with partial item missingness, sidestepping the upstream listwise-deletion behavior.
- **Custom theta-purification loops** that refit the IRT model on non-flagged items each iteration, for both dichotomous and polytomous DIF.
- **Total score and uploaded matching options** added to NLR (previously only standardized total score).
- **Dynamic DIF equations** that update to reflect the selected matching criterion (theta vs Z-score).
- **Consistent "Matching criterion" terminology** across all DIF pages, descriptions, notes, and plot captions.

### Missing data handling

- **Checkbox default changed to unchecked**: "Replace missing values by 0" is no longer enabled by default. This allows IRT models to handle missing data natively via FIML, rather than silently scoring missing responses as incorrect.
- **Per-item missing data in DIF**: when an external matching variable (IRT theta or uploaded) is used, respondents with partially missing item data are no longer dropped by listwise deletion. The edited logistic regression and Mantel-Haenszel DIF functions now filter only on the group and matching variables, letting the per-item model fitting handle item-level missingness.
- **DIF validation checks** no longer require complete item data across all items to count group membership; only the group variable is checked.
- **Split-half reliability** uses `complete.obs` for the correlation when custom splits are selected, preventing NA output when respondents have item-level missing data.

> **Note for achievement tests**: when missing responses should be scored as incorrect (e.g., timed tests, omitted items = wrong), check the "Replace missing values by 0" box in the Data tab. The tooltip explains when this is appropriate.

### Global enhancements

- **EAP/WLE toggle** for ability estimates, with IRT reliability output (empirical and marginal) under person-estimate tables.
- **Global logit-range sliders** on both dichotomous and polytomous IRT tabs.
- **Convergence tolerance setting** for all IRT models.
- **Sample R code** aligned with the Anxiety dataset across polytomous models.
- **Medical 100 Combined** test dataset with binary and polytomous items.

## Installation

Install from GitHub:

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("drjoshmcgrane/ShinyItemAnalysisPoly", dependencies = TRUE)
```

## Usage

```r
# run in the console (recommended)
ShinyItemAnalysisPoly::run_app(background = FALSE)

# or run as a background job in RStudio
ShinyItemAnalysisPoly::run_app()
```

## Known limitations

- The total score (`rowSums`) is intentionally `NA` for respondents with any missing item data. Analyses that use total score as a matching variable will exclude these respondents.

## Upstream

Based on [ShinyItemAnalysis v1.5.5](https://github.com/patriciamar/ShinyItemAnalysis). The official CRAN version can be installed with:

```r
install.packages("ShinyItemAnalysis")
```

Visit [shinyitemanalysis.org](https://shinyitemanalysis.org/) for documentation on the original package.

## References

When using this software, please cite the original package:

> Martinkova P., & Hladka A. (2023). Computational Aspects of Psychometric Methods: With R. Chapman and Hall/CRC. doi: [10.1201/9781003054313](https://doi.org/10.1201/9781003054313).

> Martinkova P., & Drabinova A. (2018). ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests. The R Journal, 10(2), 503--515. doi: [10.32614/RJ-2018-074](https://doi.org/10.32614/RJ-2018-074).

## Getting help

For bugs or questions about the polytomous/DIF/missing-data extensions, [open an issue](https://github.com/drjoshmcgrane/ShinyItemAnalysisPoly/issues). For issues with core ShinyItemAnalysis functionality, please report upstream at [patriciamar/ShinyItemAnalysis](https://github.com/patriciamar/ShinyItemAnalysis/issues).

## License

GNU GPL v3, the same license as the original ShinyItemAnalysis.
