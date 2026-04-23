# Handover — no-drop DIF wrappers + docs/report polish (shipped)

## Status (2026-04-23)

All work from this branch is committed and pushed to `poly/master`. Tip: `bd80a1f`.

- **No-drop DIF wrappers** (`d7a493e` and earlier): `.difORD_no_drop`, `.difNLR_no_drop`, `.ddfMLR_no_drop` in `inst/ShinyItemAnalysis/server/DIF.R`. All theta-matched DIF (logistic via existing path; cumulative/adjacent/NLR/multinomial via these wrappers, incl. their `_theta_puri` variants) skip `difNLR`/`difORD`/`ddfMLR` listwise deletion.
- **CLI-verified**: `as.data.frame(Data)` at wrapper entry (fixes data.table [30]/[15] regression); joint-rebuild of `ordPAR`/`nlrPAR`/`mlrPAR` from `parM0`/`parM1` post `p.adjust`; ddfMLR per-item pre-filter. `coef()`, `plot()`, `rownames(item_names())` pass on CZmaturaS, GMAT, dataMedicaltest.
- **Chromote smoke-test (2026-04-22)**: 5/5 clean (CZ cumulative/adjacent/+purification, Medical multinomial, GMAT NLR). Drivers: `/tmp/uat_full.R`, `/tmp/uat_rest.R`.
- **Poly report plot fix** (`405250f`): `report_DIF_ord_plot()` and `report_DIF_adj_plot()` now unwrap the list `plot.difORD` returns to its first ggplot. Headless render of `reporthtml_poly.Rmd` grows 628 KB → 1.57 MB with plots embedded. Driver: `/tmp/mini_render.R` + `/tmp/mini_dif.Rmd`.
- **Docs refresh** (`c8b026a`, `11c3f4e`, `fb25976`, `bd80a1f`): NEWS `1.5.5.9000` entry added; README DIF section updated and stale "Known limitations" folded into the missing-data note; About tab no longer claims CRAN availability and reads version dynamically via `utils::packageVersion()`.

## Open follow-ups

- Manual browser download of the poly PDF/HTML report is still worth doing once end-to-end. Chromote-driven full-app render was unstable in this session (Shiny worker went unresponsive after `generate`; `/tmp/uat_report*.R`), which is why the verification was done via headless `rmarkdown::render`.
- `uiReferences.R` CRAN links are all for dependencies (fine). `uiModules.R:27` points at upstream SIA module repo — correct context, left as-is.

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
