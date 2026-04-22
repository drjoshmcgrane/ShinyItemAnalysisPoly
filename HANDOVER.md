# Handover — difNLR/difORD listwise deletion fix (integrated, pending smoke-test)

## Status

- **Committed**: three per-item wrappers (`.difORD_no_drop`, `.difNLR_no_drop`, `.ddfMLR_no_drop`) inserted into `inst/ShinyItemAnalysis/server/DIF.R` (before `difNLR_theta_puri`). All three `_theta_puri` helpers and all four non-puri theta branches route through the wrappers when `matching_val == "theta"`. Non-theta paths unchanged.
- **CLI-verified**: `Data <- as.data.frame(Data)` up front, joint-rebuild of `ordPAR`/`nlrPAR`/`mlrPAR` from parM0/parM1 post-`p.adjust`, and ddfMLR per-item pre-filter. End-to-end `coef()`, `plot()`, and `rownames(item_names())` all pass on CZmaturaS (cumulative/adjacent, m=30), GMAT (3PL, m=20), and dataMedicaltest (multinomial, m=20) — including `data.table` inputs that reproduced the previous [30] vs [15] regression.
- **Chromote smoke-test (2026-04-22)**: 5/5 DIF theta-matching paths clean (0 shiny-output errors):
  - CZmaturaS → Cumulative logit + theta ✓
  - CZmaturaS → Adjacent category logit + theta ✓
  - CZmaturaS → Cumulative logit + theta + purification ON ✓
  - dataMedical → Multinomial + theta ✓
  - GMAT → Generalized logistic (NLR) + theta ✓
  Drivers: `/tmp/uat_full.R`, `/tmp/uat_rest.R`.
- **Report render (headless Rmd, 2026-04-22)**: DIF sections of `reporthtml_poly.Rmd` render cleanly (628 KB HTML) when fed real `.difORD_no_drop` outputs for CZmaturaS (cumulative + adjacent under theta matching). Driver: `/tmp/mini_render.R` + `/tmp/mini_dif.Rmd`. Chromote-driven full-app render was unstable in this session (Shiny worker went unresponsive after the `generate` click; `/tmp/uat_report*.R`), so manual browser download of the PDF/HTML remains worth doing once.
- **Pre-existing observation (not a regression)**: `plot.difORD` returns a `list` of ggplots (one per response category). The Rmd plot chunks (`DIF-ord-plot`, `DIF-adj-plot`) gate on `inherits(params$DIF_ord_plot, "ggplot")`, which is `FALSE` for a list, so the plots are skipped in the poly report today. Fix is one-line in `server/Reports.R::report_DIF_ord_plot()` (e.g., return `fit$plot[[1]]` or wrap in `patchwork::wrap_plots`) — file as separate follow-up.

## Why this is safe to retry now (vs the previous revert)

The prior integration reverted because the live app threw `'names' attribute [30] must be the same length as the vector [15]` and related `coef.difORD` failures. Two root causes were fixed in the current wrappers:

1. **`data.table` input** (the [30]/[15] root). App's `ordinal()` returns a `data.table`. `Data[, j, drop = FALSE]` on a data.table has different semantics from data.frame — the per-item difORD calls silently got garbage and stitched lists ended up with half-length content. Fixed by `Data <- as.data.frame(Data)` at each wrapper's entry.
2. **Per-item vs joint significance mismatch** (the `.deltamethod.ORD.log2irt` unary-minus crash). Per-item difORD picks ordPAR from its own m=1 significance, but the wrapper applies `p.adjust` jointly — shapes disagreed. Fixed by rebuilding `ordPAR`/`ordSE` (and equivalents for NLR/MLR) from `parM0`/`parM1` + `seM0`/`seM1` after computing `significant`.

Additional defensive fixes:
- `match` stored as `setNames(as.data.frame(replicate(m, match)), paste0("MATCH", seq_len(m)))` for all three wrappers — required by upstream `plot.difORD`'s `match[, i]` indexing.
- `.ddfMLR_no_drop` pre-filters rows per item to dodge an upstream `ddfMLR` bug where inner `MLR()` receives the unresolved `match` after `.resolve_missing` shrinks Data.

## Verification recipe for next session

1. Launch the app (`/tmp/launch.R` or `devtools::load_all() + run_app()`).
2. CZmaturaS → DIF → cumulative tab → matching = `IRT theta`, purification off. Expect zero shiny errors on:
   - `_summary_dif_items`, `_summary_coef`, `_summary_table_note`
   - `_items_plot_cumulative` (toggle items, especially a poly item like b18, b29)
3. Repeat on the adjacent tab.
4. Toggle purification on; check `_summary_purification_info` and `_summary_purification_table`.
5. Medical → multinomial DIF with theta matching.
6. GMAT → NLR DIF with theta matching (binary).
7. Render PDF + HTML poly reports — confirm DIF sections populate without errors.

Chromote driver template: `/tmp/uat_nadrop.R`. Use `Shiny.setInputValue(id, n, {priority:'event'})` (not `.click()`; see `memory/feedback_chromote_actionbutton.md`).

## Hook points in DIF.R (for reference)

| Site | What it does |
|---|---|
| `.difORD_no_drop`, `.difNLR_no_drop`, `.ddfMLR_no_drop` | Wrappers inserted just above `difNLR_theta_puri` |
| `difNLR_theta_puri` | Calls `.difNLR_no_drop` inside the theta-purification loop |
| `.ddfMLR_theta_puri` | Calls `.ddfMLR_no_drop` |
| `.difORD_theta_puri` | Calls `.difORD_no_drop` |
| `DIF_NLR_method` | Routes to `.difNLR_no_drop` when `matching_val == "theta"` |
| `DIF_cumulative_method` | Routes to `.difORD_no_drop` (model=`cumulative`) when theta |
| `DIF_adjacent_method` | Routes to `.difORD_no_drop` (model=`adjacent`) when theta |
| `DIF_multinomial_method` | Routes to `.ddfMLR_no_drop` when theta |

## Reference

- Upstream difNLR source: `Rscript -e 'cat(deparse(difNLR::difORD), sep="\n")'`
- `.deltamethod.ORD.log2irt`: `getFromNamespace(".deltamethod.ORD.log2irt", "difNLR")`
- Extracted wrapper reference (with commentary on each fix): `/tmp/wrappers_reference.R`
- CLI reproducer for the [30]/[15] regression: `/tmp/repro_nodrop.R`
