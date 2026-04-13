# Polytomous IRT Models, Fit Statistics, and Model Comparison

## Overview

Extend ShinyItemAnalysis to support polytomous IRT model fitting (GRM, RSM, PCM, GPCM), add infit/outfit statistics for Rasch-family models, overlay observed proportions on ICCs, add expected value curves for polytomous items, and provide model comparison with LRT for nested models.

## 1. Infit/Outfit for Rasch/1PL (Dichotomous)

### Current State

`IRT_binary_summary_coef` in `server/IRT.R:583-631` builds a parameter table with SX2 fit statistics via `itemfit(fit, na.rm = TRUE)` extracting `S_X2`, `df.S_X2`, `p.S_X2`.

### Change

When the selected model is Rasch or 1PL, additionally call `itemfit(fit, fit_stats = "infit")` which returns `outfit` (outfit MNSQ) and `infit` (infit MNSQ) columns from `mirt`. Append these two columns to the parameter table after the SX2 columns:

| Parameters... | SEs... | SX2 | df | p | Outfit MNSQ | Infit MNSQ |

For 2PL/3PL/4PL these columns are omitted (infit/outfit are Rasch-family diagnostics).

The Items subtab single-item parameter display and downloadable CSV reflect the same columns.

Sample R code files (`sc/irt/rasch.R`, `sc/irt/1pl.R`) updated to show `itemfit(fit, fit_stats = "infit")`.

## 2. Polytomous Models Tab (GRM, RSM, PCM, GPCM)

### UI Structure

A new tab "Polytomous models" in the IRT navbar menu alongside the existing "Nominal response model". Model selector dropdown at the top: GRM, RSM, PCM, GPCM.

### Summary Subtab

- **Model description** with equation, conditional on selected model
- **Expected item score curves** for all items (weighted sum of category scores times probabilities)
- **ICC plots** — Category probability curves for all items via `probtrace()`. For GRM: cumulative probability curves. For RSM/PCM/GPCM: category response curves
- **IIC plots** — Item information curves for all items
- **TIC + SE plot** — Test information curve with standard error
- **Parameter table** with SEs. For PCM: additionally include infit/outfit MNSQ. For GRM: discrimination `a` + threshold `b` params. For RSM: common discrimination + step params. For PCM: step/threshold params. For GPCM: item-specific discrimination + step params
- **Ability estimates** — Factor scores (first 6 shown), download, Z-score vs F-score correlation and scatterplot
- **Wright map** — For PCM and RSM only (Rasch-family), using `ggWrightMap()` with item step parameters

### Items Subtab

- Item slider selector
- Expected item score curve for selected item
- ICC for selected item (category probability curves)
- IIC for selected item
- Parameter table for selected item

### Model Fitting via mirt

- GRM: `mirt(data, 1, itemtype = "graded", SE = TRUE)`
- RSM: `mirt(data, 1, itemtype = "rsm", SE = TRUE)`
- PCM: `mirt(data, 1, itemtype = "Rasch", SE = TRUE)` (mirt treats Rasch on polytomous data as PCM)
- GPCM: `mirt(data, 1, itemtype = "gpcm", SE = TRUE)`

All use the `ordinal()` reactive as their data source.

## 3. Observed Proportions on ICCs

### Toggle

Checkbox "Show observed proportions" near the ICC plots. Available on both dichotomous and polytomous tabs (Summary and Items subtabs). Defaults to off.

### Dichotomous Models

Group respondents into ability bins based on factor scores (`fscores()`). Use quantile-based binning (10 groups). For each bin, compute observed proportion correct. Overlay as `geom_point()` on the ICC plot at bin midpoints.

### Polytomous Models — Category Probability Curves

Same binning. For each item with K categories, compute observed proportion in each category per bin. Overlay points color-matched to category curves.

### Polytomous Models — Expected Value Curves

When toggle is on, overlay observed mean item score per ability bin on the expected value curves.

### Computation

1. `fscores(fit)` for ability estimates
2. `cut()` into quantile-based bins (10 bins)
3. For each bin: mean of indicator (dichotomous) or category proportions / mean score (polytomous)
4. Plot bin midpoint on x-axis, observed value on y-axis

## 4. Expected Value Curves (Polytomous Only)

### Summary Subtab

Plot section "Expected item score curves" showing all items overlaid. Expected score: `E(X_i | theta) = sum(k * P(X_i = k | theta))` for k = 0, 1, ..., K.

### Items Subtab

Expected score curve for the selected item.

### Computation

Use `probtrace()` weighted by category scores, or `mirt::expected.item()`.

## 5. Model Comparison

### Dichotomous

Existing tab (`server/IRT.R:1025-1045`) already handles 1PL/2PL/3PL/4PL comparison with AIC/BIC/logLik. No changes needed.

### Polytomous

New "Model comparison" tab under "Polytomous models" in the navbar menu.

Table structure:

| Model | AIC | BIC | logLik | LRT | df | p-value |
|-------|-----|-----|--------|-----|----|---------| 
| RSM   | ... | ... | ...    | --  | -- | --      |
| PCM   | ... | ... | ...    | vs RSM | ... | ... |
| GPCM  | ... | ... | ...    | vs PCM | ... | ... |
| GRM   | ... | ... | ...    | --  | -- | --      |
| BEST  | ... | ... |        |     |    |         |

Nested LRT pairs:
- RSM vs PCM (PCM relaxes equal-step constraint)
- PCM vs GPCM (GPCM adds item-specific discrimination)

GRM gets AIC/BIC/logLik only (different link function, not nested).

BEST row indicates lowest AIC and BIC.

Implementation: Fit all four, `mirt::anova()` for nested pairs, extract AIC/BIC/logLik from each. Convergence warnings in orange, matching existing pattern.

## 6. File Changes

### New Files

| File | Purpose |
|------|---------|
| `inst/ShinyItemAnalysis/ui/uiIRT/uiPolyIRTModels.R` | UI for polytomous models tab |
| `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R` | Server logic for GRM/RSM/PCM/GPCM |
| `inst/ShinyItemAnalysis/sc/irt/grm.R` | Sample R code for GRM |
| `inst/ShinyItemAnalysis/sc/irt/rsm.R` | Sample R code for RSM |
| `inst/ShinyItemAnalysis/sc/irt/pcm.R` | Sample R code for PCM |
| `inst/ShinyItemAnalysis/sc/irt/gpcm.R` | Sample R code for GPCM |
| `inst/ShinyItemAnalysis/sc/irt/poly_comp.R` | Sample R code for polytomous comparison |

### Modified Files

| File | Change |
|------|--------|
| `inst/ShinyItemAnalysis/server/IRT.R` | Add infit/outfit to Rasch/1PL tables; add observed proportions on dichotomous ICCs |
| `inst/ShinyItemAnalysis/ui/uiIRT.R` | Source new UI; add polytomous models + comparison tabs to navbar; add observed proportions checkbox to dichotomous tab |
| `inst/ShinyItemAnalysis/sc/irt/rasch.R` | Add `itemfit(fit, fit_stats = "infit")` example |
| `inst/ShinyItemAnalysis/sc/irt/1pl.R` | Same |

### Unchanged Files

- `DESCRIPTION` / `NAMESPACE` — `mirt` already imported
- `inst/ShinyItemAnalysis/server/IRT/polytomous.R` — NRM untouched
- `inst/ShinyItemAnalysis/ui/uiIRT/uiPolyIRT.R` — NRM UI untouched
- Report generation, training modules — out of scope
