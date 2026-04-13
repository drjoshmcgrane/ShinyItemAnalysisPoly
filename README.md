# ShinyItemAnalysisPoly <img src="inst/ShinyItemAnalysis/www/sia_logo.svg" align="right" width=150/> 
Extended polytomous IRT models for ShinyItemAnalysis

## About this fork

This is a fork of [ShinyItemAnalysis](https://github.com/patriciamar/ShinyItemAnalysis) by Martinkova et al., extended with full polytomous IRT model support. It is maintained by [Josh McGrane](https://github.com/drjoshmcgrane) for teaching and research purposes.

**What's new in this fork:**

 * Polytomous IRT models: GRM, RSM, PCM, and GPCM (in addition to the existing NRM)
 * Unified polytomous model tab with dropdown model selector
 * Polytomous model comparison with AIC, BIC, log-likelihood, and LRT for nested pairs (RSM vs PCM, PCM vs GPCM)
 * Wright maps for RSM, PCM, and GPCM
 * Observed proportion overlays with configurable number of groups
 * Mixed-category data support (binary and polytomous items in the same dataset)
 * Medical 100 Combined test dataset (binary + polytomous)
 * Infit/outfit MNSQ statistics for Rasch family models

All original ShinyItemAnalysis functionality is preserved.

## Installation

Install this fork from GitHub:

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("drjoshmcgrane/ShinyItemAnalysisPoly", dependencies = TRUE)
```

## Usage

```r
ShinyItemAnalysisPoly::run_app()
```

Or in RStudio, use the `Run ShinyItemAnalysis` entry in the [Addins](https://docs.posit.co/ide/user/ide/guide/productivity/add-ins.html) menu.

## Upstream

This fork is based on [ShinyItemAnalysis v1.5.5](https://github.com/patriciamar/ShinyItemAnalysis) by Martinkova, Hladka, and Netik. The official CRAN version of ShinyItemAnalysis can be installed with:

```r
install.packages("ShinyItemAnalysis")
```

Visit [shinyitemanalysis.org](https://shinyitemanalysis.org/) for documentation on the original package.

## References

When using this software, please cite both the original package and this fork:

> Martinková P., & Hladká A. (2023) Computational Aspects of Psychometric Methods: With R. (1st ed.). Chapman and Hall/CRC. doi: 10.1201/9781003054313.

> Martinková P., & Drabinová A. (2018) ShinyItemAnalysis for teaching psychometrics and to enforce routine analysis of educational tests. The R Journal, 10(2), 503-515. [doi: 10.32614/RJ-2018-074](https://doi.org/10.32614/RJ-2018-074).

## Getting help

If you find a bug or need help with the polytomous extensions, [open an issue](https://github.com/drjoshmcgrane/ShinyItemAnalysisPoly/issues). For issues with the core ShinyItemAnalysis functionality, please report upstream at [patriciamar/ShinyItemAnalysis](https://github.com/patriciamar/ShinyItemAnalysis/issues).

## License

This program is free software under the terms of the [GNU GPL 3](https://www.gnu.org/licenses/gpl-3.0.en.html), the same license as the original ShinyItemAnalysis.
