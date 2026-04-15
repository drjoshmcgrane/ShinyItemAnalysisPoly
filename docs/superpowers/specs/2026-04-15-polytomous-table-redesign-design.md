# Polytomous Parameter Table Redesign

**Date:** 2026-04-15
**Scope:** `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R` — the Summary tab
parameter table and Wright map for the polytomous IRT models (NRM, GRM, RSM,
PCM, GPCM). Includes a fix to the long-standing KaTeX italicisation bug in
polytomous column headers.

## Goals

1. Per-model table layouts that clearly show which parameters are estimated,
   which are fixed by model definition, and which do not exist for a given
   item type.
2. Full support for mixed poly + dichotomous data in all adjacent-category
   models (RSM, PCM, GPCM) and in GRM.
3. Column headers rendered with the same italicised KaTeX styling as the
   dichotomous table (`a`, `b`, `SE(a)`, `SE(b)` all italicised via
   `\(\mathit{...}\)`).
4. Wright map continues to render correctly for RSM, PCM, GPCM (including
   mixed data).
5. Correct fit-statistic columns per model family (SX2 for all; infit/outfit
   only for RSM and PCM).

## Root Causes Fixed

### KaTeX header bug

The dichotomous table renders italicised headers via KaTeX because its single
`data.frame()` merge is followed by a wholesale `colnames(tab) <- ...`
replacement. The polytomous code path does *multiple* `data.frame()` merges
(params + SEs, then fit stats, then infit/outfit). Each merge calls
`make.names()` on the existing columns, mangling already-formatted names like
`SE(a)` into `SE.a.`. When the italicisation block then tries to match
`^SE\(`, the mangled names don't match and get wrapped incorrectly, producing
headers like `\(\mathit{SE.a.}\)` that render as garbled text.

**Fix:** pass `check.names = FALSE` to every `data.frame()` and every
`cbind()` that builds up `tab`. Apply italicisation *only* as the final step,
immediately before returning from the reactive.

### RSM tables missing thresholds

The previous RSM layout dropped `a` and all `b_j` columns, keeping only a
single `Location` column, and surfaced the shared thresholds as a text note.
This hid item-specific behaviour and was inconsistent with PCM/GPCM. The new
layout computes item-specific thresholds `b_ik = b_k − c_i` in the table,
with delta-method SEs.

## Per-Model Table Layouts

Rule of thumb: a cell shows `NA` (rendered blank) whenever there is no
estimate — either because the parameter is fixed by the model (e.g. `a` in
RSM/PCM) or because the item does not have that parameter (e.g. `b2` for a
binary item in mixed data).

### NRM

Unchanged from current behaviour. The NRM path already handles its four
parametrizations correctly and is out of scope for this change.

### GRM

Columns: `a, SE(a), b1, SE(b1), b2, SE(b2), ..., bK, SE(bK), SX2-value, df,
p-value`.

Mixed data: binary items have mirt coef columns `a, b, g, u`. The `b` value
is folded into `b1`; `g` and `u` are dropped; SEs folded analogously. Higher
threshold columns (`b2`, ...) are `NA` for binary items.

### RSM

Columns: `a, SE(a), b1, SE(b1), ..., bK, SE(bK), SX2-value, df, p-value,
Outfit MNSQ, Infit MNSQ`.

- `a = 1` for every item; `SE(a) = NA` (discrimination fixed by model).
- `b_ik = b_k(shared) − c_i` computed per item from mirt's shared step
  parameters and the item's location `c_i`.
- `SE(b_ik)` via the delta method from `vcov(fit)`:
  `sqrt(var(b_k) + var(c_i) − 2·cov(b_k, c_i))`. If vcov is unavailable or
  not positive-definite, fall back to `NA`.
- The "Shared threshold parameters" note above the table is retained for
  interpretability.

Mixed data: RSM already requires all items to have the same number of
response categories (existing `validate()` at line 42–47), so mixed-category
data does not reach the RSM path. No change needed.

### PCM

Columns: `a, SE(a), b1, SE(b1), ..., bK, SE(bK), SX2-value, df, p-value,
Outfit MNSQ, Infit MNSQ`.

- `a = 1`, `SE(a) = NA`.
- `b_ik` and `SE(b_ik)` taken directly from mirt's item-specific step
  parameters and their SEs.
- Mixed data: binary items folded as in GRM (`b → b1`, drop `g`/`u`).

### GPCM

Columns: `a, SE(a), b1, SE(b1), ..., bK, SE(bK), SX2-value, df, p-value`.

- `a` and `SE(a)` from mirt directly (item-specific).
- `b_ik`, `SE(b_ik)` from mirt directly.
- Mixed data: as for PCM.
- No infit/outfit (these are Rasch-family diagnostics; GPCM is not Rasch
  because `a` varies).

## Architecture

Replace the single monolithic `IRT_poly_summary_coef_reactive` with a
dispatcher that delegates to model-family builders and then two shared
helpers.

```
IRT_poly_summary_coef_reactive
  ├─ build_nrm_table(fit)                           # unchanged
  ├─ build_grm_table(fit)                           # a + thresholds (+ SEs)
  └─ build_masters_table(fit, model)                # RSM / PCM / GPCM
  ↓
  append_fit_stats(tab, fit, include_infit)         # SX2 always; infit/outfit conditional
  ↓
  italicise_colnames(tab)                           # final step
```

Each builder returns a data frame with `check.names = FALSE` preserved end to
end. Column names at builder output are plain (`a`, `SE(a)`, `b1`, ...); the
italicise helper wraps them into KaTeX at the very end.

### `build_masters_table(fit, model)`

Unified for RSM/PCM/GPCM because the column shape is identical
(`a, SE(a), b1, SE(b1), ...`); only the sources differ.

```
pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items

switch(model,
  "RSM" = {
    a_vec      <- rep(1, n_items)
    se_a_vec   <- rep(NA, n_items)
    thresholds <- outer(shared_b, c_i, "-")        # n_items x K
    se_thresh  <- delta_method_ses(vcov(fit), ...)  # NA fallback
  },
  "PCM" = {
    a_vec      <- rep(1, n_items)
    se_a_vec   <- rep(NA, n_items)
    thresholds <- pars[, b_cols, drop = FALSE]
    se_thresh  <- se_list_to_matrix(se_list, b_cols)
    # mixed: fold binary 'b' into 'b1'
  },
  "GPCM" = {
    a_vec      <- pars[, "a"]
    se_a_vec   <- se_list_to_vector(se_list, "a")
    thresholds <- pars[, b_cols, drop = FALSE]
    se_thresh  <- se_list_to_matrix(se_list, b_cols)
    # mixed: fold binary 'b' into 'b1'
  }
)

tab <- interleave(a_vec, se_a_vec, thresholds, se_thresh)
```

### `append_fit_stats(tab, fit, include_infit)`

`include_infit <- model %in% c("RSM", "PCM")`.

SX2 via `itemfit(fit, na.rm = TRUE)`; tolerate failure with `NA`. Infit/outfit
via `itemfit(fit, fit_stats = "infit")`; same tolerance.

Always call with `check.names = FALSE` on the `data.frame()` merge.

### `italicise_colnames(tab)`

Applied at the very end. Skip list: `SX2-value`, `df`, `p-value`,
`Outfit MNSQ`, `Infit MNSQ`. For any other column:

- Names matching `^SE\((.+)\)$` → `SE(\(\mathit{<inner>}\))`
- All others → `\(\mathit{<name>}\)`

Assignment `colnames(tab) <- nms` with no subsequent `data.frame()` call means
the italicised names reach `renderTable` intact.

## Wright Map

No structural change. The existing `IRT_poly_wrightmap_args_reactive` already
derives RSM item-specific thresholds as `b_j − c_i` (lines 673–680) and
handles mixed-data binary-item folding for PCM/GPCM (lines 682–691). This
logic is retained as-is.

The refactor touches only `IRT_poly_summary_coef_reactive` and its helpers;
`IRT_poly_wrightmap_args_reactive` and `IRT_poly_wrightmap_plots` remain
untouched. A post-refactor manual test confirms Wright map still renders for
RSM / PCM / GPCM, including mixed data.

## Testing Checklist

Manual test each model on an appropriate dataset (app running locally):

| Model | Dataset | Check |
|---|---|---|
| NRM | Anxiety (5-cat) | table unchanged |
| GRM | Anxiety (5-cat) | `a, SE(a), b1..b4, SE(b1..b4), SX2/df/p` with italicised headers |
| GRM | mixed (Anxiety + binary) | binary items show `b1` filled, `b2..b4` blank |
| RSM | Anxiety (5-cat) | `a=1`, `SE(a)=NA`; `b1..b4` item-specific (differ per item), threshold SEs via delta method; SX2 + infit/outfit |
| PCM | Anxiety | same shape as RSM, thresholds item-specific from mirt |
| PCM | mixed | binary `b` in `b1`, higher thresholds blank |
| GPCM | Anxiety | `a` varies per item, `SE(a)` present, no infit/outfit |
| GPCM | mixed | as above with binary folding |

Wright map post-refactor:
- RSM on Anxiety: renders without error.
- PCM on Anxiety: renders without error.
- GPCM on Anxiety: renders without error.
- PCM on mixed: binary items appear as single-point items on the map.

Download handler (CSV): plain names (`a`, `SE(a)`, ...), not KaTeX markup.
Current code writes `IRT_poly_summary_coef_reactive()` directly to CSV, which
would include KaTeX-wrapped names. Add a plain-name export path (strip
italicisation) before `write.csv`, matching the dichotomous download which
rewrites columns before writing.

## Out of Scope

- NRM table internals (all four parametrizations stay as-is).
- Ability estimates table (`IRT_poly_fscores_reactive`), plots, item-tab
  outputs — only the Summary parameter table is being redesigned.
- Server code outside `polytomous_irt.R`.
