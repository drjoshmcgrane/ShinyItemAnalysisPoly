# Polytomous Parameter Table Redesign Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Redesign the polytomous IRT Summary-tab parameter table per the
spec at `docs/superpowers/specs/2026-04-15-polytomous-table-redesign-design.md`:
per-model column layouts, correct fit-statistic selection, mixed poly+dich
data support, fixed KaTeX header italicisation, and aligned sample R code.

**Architecture:** Extract pure helper functions into a new file
`inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R`. The Shiny reactive
in `polytomous_irt.R` becomes a thin dispatcher. Helpers are unit-tested
with a standalone `testthat` script runnable via `Rscript`. Shiny
integration verified manually through the app.

**Tech Stack:** R, Shiny, `mirt`, `xtable` (via `renderTable`), `testthat`.

---

## File Structure

**Create:**
- `inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R` — pure helpers:
  `build_nrm_table`, `build_grm_table`, `build_masters_table`,
  `append_fit_stats`, `italicise_colnames`, `strip_katex`,
  `rsm_threshold_ses_delta`.
- `tests/manual/test-poly-tables.R` — testthat script: fits small mirt
  models on toy data and asserts table shape / column names / mixed-data
  behaviour.

**Modify:**
- `inst/ShinyItemAnalysis/server/IRT.R:1-4` — add `source()` line for new
  helper file before `polytomous_irt.R`.
- `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R:239-425` — replace
  `IRT_poly_summary_coef_note`, `IRT_poly_summary_coef_reactive`,
  `output$IRT_poly_summary_coef`, and the CSV download handler.
- `inst/ShinyItemAnalysis/sc/irt/rsm.R` — align with new app output.
- `inst/ShinyItemAnalysis/sc/irt/pcm.R` — align with new app output.
- `inst/ShinyItemAnalysis/sc/irt/gpcm.R` — add Wright map section.

**Untouched:**
- `IRT_poly_wrightmap_args_reactive` / `IRT_poly_wrightmap_plots` at
  `polytomous_irt.R:664-777`.
- `inst/ShinyItemAnalysis/sc/irt/grm.R` — already correct.
- `inst/ShinyItemAnalysis/sc/irt/bock.R` — NRM out of scope.
- Items tab, ability tab, plots.

---

## Task 1: Add helper file skeleton and wire it into the server

**Files:**
- Create: `inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R`
- Modify: `inst/ShinyItemAnalysis/server/IRT.R:1-4`

- [ ] **Step 1: Create the helper file with stubs**

Create `inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R`:

```r
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# POLYTOMOUS PARAMETER TABLE HELPERS ####
# Pure functions used by IRT_poly_summary_coef_reactive.
# Design contract: docs/superpowers/specs/2026-04-15-polytomous-table-redesign-design.md
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Forward declarations - filled in by subsequent tasks.
build_nrm_table       <- function(fit, use_irt) stop("not yet implemented")
build_grm_table       <- function(fit) stop("not yet implemented")
build_masters_table   <- function(fit, model) stop("not yet implemented")
append_fit_stats      <- function(tab, fit, include_infit) stop("not yet implemented")
italicise_colnames    <- function(tab) stop("not yet implemented")
strip_katex           <- function(tab) stop("not yet implemented")
rsm_threshold_ses_delta <- function(fit) stop("not yet implemented")
```

- [ ] **Step 2: Source the helper file from `server/IRT.R`**

Edit `inst/ShinyItemAnalysis/server/IRT.R` lines 1-4. Change:

```r
# source server logic for polytomous IRT models
source("server/IRT/polytomous.R", local = T, encoding = "UTF-8")
source("server/IRT/polytomous_irt.R", local = T, encoding = "UTF-8")
source("server/IRT/training.R", local = T, encoding = "UTF-8")
```

to:

```r
# source server logic for polytomous IRT models
source("server/IRT/polytomous.R", local = T, encoding = "UTF-8")
source("server/IRT/poly_table_helpers.R", local = T, encoding = "UTF-8")
source("server/IRT/polytomous_irt.R", local = T, encoding = "UTF-8")
source("server/IRT/training.R", local = T, encoding = "UTF-8")
```

- [ ] **Step 3: Verify the app still starts**

Run from the repo root:

```bash
Rscript -e 'shiny::runApp("inst/ShinyItemAnalysis", launch.browser = FALSE, port = 4567)' &
sleep 4 && curl -s http://127.0.0.1:4567 >/dev/null && echo OK
pkill -f 'port = 4567'
```

Expected: `OK`. Helper stubs never execute at startup, so `stop()` doesn't
fire.

- [ ] **Step 4: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT.R \
        inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R
git commit -m "refactor(poly-table): scaffold helper file and source from IRT.R"
```

---

## Task 2: Create the testthat manual test script (initially failing)

**Files:**
- Create: `tests/manual/test-poly-tables.R`

Unit tests for the helpers. They'll fail until subsequent tasks implement
each helper. Run with: `Rscript tests/manual/test-poly-tables.R`.

- [ ] **Step 1: Write the test script with fixtures and all assertions**

Create `tests/manual/test-poly-tables.R`:

```r
# Manual unit tests for polytomous table helpers.
# Run: Rscript tests/manual/test-poly-tables.R
suppressPackageStartupMessages({
  library(testthat)
  library(mirt)
})

# Source helpers in a test environment so the Shiny `local = TRUE` trick
# isn't needed.
source("inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R")

# ---- fixtures ---------------------------------------------------------------

set.seed(42)
n <- 300

# 5-item, 4-category ordinal toy data
sim_poly <- function(n_items = 5, n_cats = 4, n = 300) {
  as.data.frame(matrix(sample(0:(n_cats - 1), n * n_items, replace = TRUE),
                       nrow = n, ncol = n_items))
}

# Mixed dataset: 3 polytomous items (4 categories) + 2 binary items
sim_mixed <- function(n = 300) {
  poly <- matrix(sample(0:3, n * 3, replace = TRUE), n, 3)
  bin  <- matrix(sample(0:1, n * 2, replace = TRUE), n, 2)
  as.data.frame(cbind(poly, bin))
}

fit_grm  <- mirt(sim_poly(), 1, itemtype = "graded", SE = TRUE, verbose = FALSE)
fit_pcm  <- mirt(sim_poly(), 1, itemtype = "Rasch",  SE = TRUE, verbose = FALSE)
fit_gpcm <- mirt(sim_poly(), 1, itemtype = "gpcm",   SE = TRUE, verbose = FALSE)
fit_rsm  <- mirt(sim_poly(), 1, itemtype = "rsm",    SE = TRUE, verbose = FALSE)

fit_pcm_mixed  <- mirt(sim_mixed(), 1, itemtype = "Rasch", SE = TRUE, verbose = FALSE)
fit_gpcm_mixed <- mirt(sim_mixed(), 1, itemtype = "gpcm",  SE = TRUE, verbose = FALSE)
fit_grm_mixed  <- mirt(sim_mixed(), 1, itemtype = "graded", SE = TRUE, verbose = FALSE)

# ---- italicise_colnames / strip_katex ---------------------------------------

test_that("italicise_colnames wraps params and preserves skip list", {
  tab <- data.frame(a = 1, `SE(a)` = 2, b1 = 3, `SE(b1)` = 4,
                    `SX2-value` = 5, df = 6, `p-value` = 7,
                    `Outfit MNSQ` = 8, `Infit MNSQ` = 9,
                    check.names = FALSE)
  out <- italicise_colnames(tab)
  nms <- colnames(out)
  expect_equal(nms[1], "\\(\\mathit{a}\\)")
  expect_equal(nms[2], "SE(\\(\\mathit{a}\\))")
  expect_equal(nms[3], "\\(\\mathit{b1}\\)")
  expect_equal(nms[4], "SE(\\(\\mathit{b1}\\))")
  expect_equal(nms[5], "SX2-value")
  expect_equal(nms[6], "df")
  expect_equal(nms[7], "p-value")
  expect_equal(nms[8], "Outfit MNSQ")
  expect_equal(nms[9], "Infit MNSQ")
})

test_that("strip_katex is the inverse of italicise_colnames on headers", {
  tab <- data.frame(a = 1, `SE(a)` = 2, check.names = FALSE)
  round_trip <- strip_katex(italicise_colnames(tab))
  expect_equal(colnames(round_trip), c("a", "SE(a)"))
})

# ---- build_grm_table --------------------------------------------------------

test_that("build_grm_table produces interleaved a/SE(a)/b/SE(b) columns", {
  tab <- build_grm_table(fit_grm)
  expect_equal(colnames(tab)[1:2], c("a", "SE(a)"))
  expect_true(all(c("b1", "SE(b1)", "b2", "SE(b2)", "b3", "SE(b3)")
                  %in% colnames(tab)))
  expect_equal(nrow(tab), 5)
  expect_true(all(!is.na(tab$a)))
  expect_true(all(!is.na(tab$`SE(a)`)))
})

test_that("build_grm_table handles mixed data (binary b -> b1, higher NA)", {
  tab <- build_grm_table(fit_grm_mixed)
  # last two rows are the binary items
  expect_true(all(!is.na(tab$b1)))
  expect_true(all(is.na(tab$b2[4:5])))
  expect_true(all(is.na(tab$b3[4:5])))
  # no residual b / g / u columns
  expect_false(any(c("b", "g", "u") %in% colnames(tab)))
})

# ---- build_masters_table: GPCM ---------------------------------------------

test_that("build_masters_table GPCM has estimated a and SE(a)", {
  tab <- build_masters_table(fit_gpcm, "GPCM")
  expect_equal(colnames(tab)[1:2], c("a", "SE(a)"))
  expect_true(all(!is.na(tab$a)))
  expect_true(all(!is.na(tab$`SE(a)`)))
  expect_true(all(c("b1", "SE(b1)", "b2", "SE(b2)", "b3", "SE(b3)")
                  %in% colnames(tab)))
})

test_that("build_masters_table GPCM mixed: binary b -> b1", {
  tab <- build_masters_table(fit_gpcm_mixed, "GPCM")
  expect_true(all(!is.na(tab$b1)))
  expect_true(all(is.na(tab$b2[4:5])))
  expect_false(any(c("b", "g", "u") %in% colnames(tab)))
})

# ---- build_masters_table: PCM ----------------------------------------------

test_that("build_masters_table PCM: a=1, SE(a)=NA", {
  tab <- build_masters_table(fit_pcm, "PCM")
  expect_true(all(tab$a == 1))
  expect_true(all(is.na(tab$`SE(a)`)))
  expect_true(all(c("b1", "SE(b1)", "b2", "SE(b2)", "b3", "SE(b3)")
                  %in% colnames(tab)))
})

test_that("build_masters_table PCM mixed: binary b -> b1", {
  tab <- build_masters_table(fit_pcm_mixed, "PCM")
  expect_true(all(!is.na(tab$b1)))
  expect_true(all(is.na(tab$b2[4:5])))
})

# ---- build_masters_table: RSM ----------------------------------------------

test_that("build_masters_table RSM: a=1, SE(a)=NA, thresholds differ per item", {
  tab <- build_masters_table(fit_rsm, "RSM")
  expect_true(all(tab$a == 1))
  expect_true(all(is.na(tab$`SE(a)`)))
  # b1 must vary across items (derived = b_k - c_i)
  expect_gt(sd(tab$b1), 0)
  expect_gt(sd(tab$b2), 0)
})

test_that("build_masters_table RSM: threshold SEs non-NA when vcov available", {
  tab <- build_masters_table(fit_rsm, "RSM")
  # at least one SE should be computable
  expect_true(any(!is.na(tab$`SE(b1)`)))
})

# ---- append_fit_stats ------------------------------------------------------

test_that("append_fit_stats adds SX2/df/p without infit when requested", {
  tab <- build_grm_table(fit_grm)
  out <- append_fit_stats(tab, fit_grm, include_infit = FALSE)
  expect_true(all(c("SX2-value", "df", "p-value") %in% colnames(out)))
  expect_false(any(c("Outfit MNSQ", "Infit MNSQ") %in% colnames(out)))
})

test_that("append_fit_stats adds infit/outfit when requested", {
  tab <- build_masters_table(fit_pcm, "PCM")
  out <- append_fit_stats(tab, fit_pcm, include_infit = TRUE)
  expect_true(all(c("SX2-value", "df", "p-value",
                    "Outfit MNSQ", "Infit MNSQ") %in% colnames(out)))
  # KaTeX column names must not have been introduced yet
  expect_false(any(grepl("mathit", colnames(out))))
})

# ---- run --------------------------------------------------------------------
cat("\n--- poly table helpers: all tests passed ---\n")
```

- [ ] **Step 2: Run the test script to confirm it currently fails**

```bash
Rscript tests/manual/test-poly-tables.R 2>&1 | tail -20
```

Expected: a failure because helpers all `stop("not yet implemented")`.

- [ ] **Step 3: Commit**

```bash
git add tests/manual/test-poly-tables.R
git commit -m "test(poly-table): add helper unit tests (initially failing)"
```

---

## Task 3: Implement `italicise_colnames` and `strip_katex`

**Files:**
- Modify: `inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R`

- [ ] **Step 1: Replace the two stubs with working implementations**

In `poly_table_helpers.R`, replace the `italicise_colnames` and
`strip_katex` stubs with:

```r
italicise_colnames <- function(tab) {
  skip <- c("SX2-value", "df", "p-value", "Outfit MNSQ", "Infit MNSQ")
  nms <- colnames(tab)
  for (i in seq_along(nms)) {
    if (nms[i] %in% skip) next
    if (grepl("^SE\\((.+)\\)$", nms[i])) {
      inner <- sub("^SE\\((.+)\\)$", "\\1", nms[i])
      nms[i] <- paste0("SE(\\(\\mathit{", inner, "}\\))")
    } else {
      nms[i] <- paste0("\\(\\mathit{", nms[i], "}\\)")
    }
  }
  colnames(tab) <- nms
  tab
}

strip_katex <- function(tab) {
  nms <- colnames(tab)
  nms <- gsub("^SE\\(\\\\\\(\\\\mathit\\{(.+)\\}\\\\\\)\\)$",
              "SE(\\1)", nms)
  nms <- gsub("^\\\\\\(\\\\mathit\\{(.+)\\}\\\\\\)$", "\\1", nms)
  colnames(tab) <- nms
  tab
}
```

- [ ] **Step 2: Run the test script**

```bash
Rscript tests/manual/test-poly-tables.R 2>&1 | grep -E "italicise|strip|FAIL|Error" | head -10
```

Expected: the two italicise/strip tests pass; remaining failures are about
not-yet-implemented helpers.

- [ ] **Step 3: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R
git commit -m "feat(poly-table): implement italicise_colnames and strip_katex"
```

---

## Task 4: Implement `append_fit_stats`

**Files:**
- Modify: `inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R`

- [ ] **Step 1: Replace the stub**

Replace `append_fit_stats` with:

```r
append_fit_stats <- function(tab, fit, include_infit) {
  sx2 <- tryCatch(
    itemfit(fit, na.rm = TRUE)[, c("S_X2", "df.S_X2", "p.S_X2")],
    error = function(e) NULL
  )
  if (!is.null(sx2)) {
    tab <- data.frame(tab, sx2, check.names = FALSE)
    colnames(tab)[(ncol(tab) - 2):ncol(tab)] <-
      c("SX2-value", "df", "p-value")
  }

  if (include_infit) {
    infit <- tryCatch(
      itemfit(fit, fit_stats = "infit")[, c("outfit", "infit")],
      error = function(e) NULL
    )
    if (!is.null(infit)) {
      tab <- data.frame(tab, infit, check.names = FALSE)
      colnames(tab)[(ncol(tab) - 1):ncol(tab)] <-
        c("Outfit MNSQ", "Infit MNSQ")
    }
  }
  tab
}
```

Note: `check.names = FALSE` on every `data.frame()` call is the heart of
the KaTeX-header fix.

- [ ] **Step 2: Smoke-test append_fit_stats in isolation**

```bash
Rscript -e '
source("inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R")
suppressPackageStartupMessages(library(mirt))
set.seed(1)
dat <- as.data.frame(matrix(sample(0:3, 300*5, replace=TRUE), 300, 5))
fit <- mirt(dat, 1, itemtype = "Rasch", SE = TRUE, verbose = FALSE)
tab <- data.frame(a = rep(1, 5), check.names = FALSE)
out <- append_fit_stats(tab, fit, include_infit = TRUE)
print(colnames(out))
stopifnot(all(c("SX2-value","df","p-value","Outfit MNSQ","Infit MNSQ") %in% colnames(out)))
cat("OK\n")
'
```

Expected: `OK` with all five appended columns listed.

- [ ] **Step 3: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R
git commit -m "feat(poly-table): implement append_fit_stats with check.names=FALSE"
```

---

## Task 5: Implement `build_grm_table`

**Files:**
- Modify: `inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R`

- [ ] **Step 1: Add a shared internal helper**

Add near the top of `poly_table_helpers.R`, above the builders:

```r
# Interleave a parameter matrix with its SE matrix so columns come out as
# p1, SE(p1), p2, SE(p2), ... Both matrices must have identical column names.
.interleave_with_ses <- function(pars, ses) {
  stopifnot(identical(colnames(pars), colnames(ses)))
  out <- cbind(pars, ses)[, order(c(seq_len(ncol(pars)),
                                    seq_len(ncol(ses)))),
                          drop = FALSE]
  p_names  <- colnames(pars)
  colnames(out) <- as.vector(rbind(p_names, paste0("SE(", p_names, ")")))
  as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

# Build a matrix of SEs aligned to a target set of column names, pulling
# from mirt's printSE list. Missing columns become NA. For mixed-category
# fits, fold a binary item's 'b' SE into its 'b1' slot.
.ses_to_matrix <- function(fit, target_cols, fold_b_into_b1 = FALSE) {
  se_list <- coef(fit, IRTpars = TRUE, printSE = TRUE)
  se_list[["GroupPars"]] <- NULL
  mat <- do.call(rbind, lapply(seq_along(se_list), function(i) {
    se_row <- se_list[[i]]["SE", ]
    if (fold_b_into_b1 && "b" %in% names(se_row) && "b1" %in% target_cols) {
      if (!"b1" %in% names(se_row) || is.na(se_row["b1"])) {
        se_row["b1"] <- se_row["b"]
      }
    }
    out <- setNames(rep(NA_real_, length(target_cols)), target_cols)
    keep <- intersect(target_cols, names(se_row))
    out[keep] <- se_row[keep]
    out
  }))
  colnames(mat) <- target_cols
  mat
}

# For mixed poly+dich data, fold each binary item's difficulty from
# column 'b' into 'b1' and drop columns b, g, u from par_tab.
.fold_binary_pars <- function(par_tab) {
  if ("b" %in% colnames(par_tab) && "b1" %in% colnames(par_tab)) {
    b_col  <- par_tab[, "b"]
    b1_col <- par_tab[, "b1"]
    needs  <- !is.na(b_col) & is.na(b1_col)
    par_tab[needs, "b1"] <- b_col[needs]
  }
  drop_cols <- intersect(c("b", "g", "u"), colnames(par_tab))
  par_tab[, !colnames(par_tab) %in% drop_cols, drop = FALSE]
}
```

- [ ] **Step 2: Replace the `build_grm_table` stub**

```r
build_grm_table <- function(fit) {
  pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
  pars <- .fold_binary_pars(pars)

  target_cols <- colnames(pars)
  ses <- .ses_to_matrix(fit, target_cols, fold_b_into_b1 = TRUE)

  .interleave_with_ses(pars, ses)
}
```

- [ ] **Step 3: Run the GRM tests**

```bash
Rscript tests/manual/test-poly-tables.R 2>&1 | grep -E "build_grm|PASS|FAIL|Error" | head -20
```

Expected: both GRM tests (basic and mixed) pass.

- [ ] **Step 4: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R
git commit -m "feat(poly-table): implement build_grm_table with mixed data support"
```

---

## Task 6: Implement `build_masters_table` for GPCM and PCM

**Files:**
- Modify: `inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R`

- [ ] **Step 1: Replace the stub with PCM + GPCM branches (RSM still stubbed)**

```r
build_masters_table <- function(fit, model) {
  stopifnot(model %in% c("RSM", "PCM", "GPCM"))

  pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
  n_items <- nrow(pars)

  if (model == "RSM") {
    return(.build_rsm_table(fit, pars))   # implemented in Task 7
  }

  # PCM / GPCM share the same shape.
  pars <- .fold_binary_pars(pars)

  b_cols <- grep("^b\\d", colnames(pars), value = TRUE)
  b_mat  <- pars[, b_cols, drop = FALSE]
  b_ses  <- .ses_to_matrix(fit, b_cols, fold_b_into_b1 = TRUE)

  if (model == "PCM") {
    a_vec   <- rep(1, n_items)
    a_ses   <- rep(NA_real_, n_items)
  } else { # GPCM
    a_vec <- if ("a" %in% colnames(pars)) pars[, "a"] else rep(NA_real_, n_items)
    a_ses <- .ses_to_matrix(fit, "a", fold_b_into_b1 = FALSE)[, "a"]
  }

  a_df <- data.frame(a = a_vec, `SE(a)` = a_ses,
                     check.names = FALSE, stringsAsFactors = FALSE)
  b_df <- .interleave_with_ses(b_mat, b_ses)
  out  <- data.frame(a_df, b_df, check.names = FALSE,
                     stringsAsFactors = FALSE)
  rownames(out) <- rownames(pars)
  out
}

# RSM-specific stub, filled in by Task 7.
.build_rsm_table <- function(fit, pars) stop("not yet implemented")
```

- [ ] **Step 2: Run PCM + GPCM tests**

```bash
Rscript tests/manual/test-poly-tables.R 2>&1 | grep -E "PCM|GPCM|FAIL|Error" | head -30
```

Expected: all four PCM/GPCM tests pass; RSM tests still failing.

- [ ] **Step 3: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R
git commit -m "feat(poly-table): implement PCM and GPCM branches of build_masters_table"
```

---

## Task 7: Implement RSM branch with delta-method SEs

**Files:**
- Modify: `inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R`

In mirt's RSM, each item contributes one location parameter `c_i` and the
model estimates shared step parameters `b_k` once. Item-specific thresholds
are `b_ik = b_k − c_i`. SE via delta method using `vcov(fit)`.

- [ ] **Step 1: Implement `rsm_threshold_ses_delta`**

Replace the `rsm_threshold_ses_delta` stub with:

```r
# Returns a named list with:
#   shared_b : named numeric vector of shared thresholds (from IRTpars = TRUE)
#   c_vals   : named numeric vector of per-item location parameters
#   thresholds_mat : n_items x n_steps matrix of item-specific b_ik = b_k - c_i
#   ses_mat : n_items x n_steps matrix of delta-method SEs, or all-NA on failure
rsm_threshold_ses_delta <- function(fit) {
  pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
  b_cols <- grep("^b\\d", colnames(pars), value = TRUE)
  shared_b <- pars[1, b_cols]
  c_vals   <- pars[, "c"]
  thresholds_mat <- t(outer(as.numeric(shared_b), as.numeric(c_vals), "-"))
  colnames(thresholds_mat) <- b_cols
  rownames(thresholds_mat) <- rownames(pars)

  ses_mat <- matrix(NA_real_, nrow = length(c_vals), ncol = length(b_cols),
                    dimnames = list(rownames(pars), b_cols))

  vc <- tryCatch(vcov(fit), error = function(e) NULL)
  if (is.null(vc) || !is.matrix(vc) || nrow(vc) < 2) {
    return(list(shared_b = shared_b, c_vals = c_vals,
                thresholds_mat = thresholds_mat, ses_mat = ses_mat))
  }

  par_names <- rownames(vc)

  # mirt vcov uses names like "d1_1", "d2_1", ... and "a1_i" etc. Switch
  # to the slot-based parameter table for robust lookup.
  par_df <- mod2values(fit)
  # Filter to estimated params present in vcov (est == TRUE).
  est_df <- par_df[par_df$est, , drop = FALSE]
  stopifnot(nrow(est_df) == nrow(vc))

  # For each item i and step k, SE(b_k - c_i) = sqrt(var(b_k) + var(c_i) - 2 cov).
  # In mirt's RSM, the shared 'b_k' are stored in rows where item is the
  # special shared row; item-specific 'c' has name 'c' per item.
  # Below uses the convention name == "c" for location and name matching
  # the mirt step indexing for shared thresholds.
  for (i in seq_along(c_vals)) {
    itm_c_idx <- which(est_df$item == rownames(pars)[i] & est_df$name == "c")
    if (length(itm_c_idx) != 1) next
    for (k in seq_along(b_cols)) {
      step_name <- paste0("t", k)     # mirt's RSM internal step name
      step_idx  <- which(est_df$name == step_name)
      if (length(step_idx) != 1) next
      var_bk <- vc[step_idx, step_idx]
      var_ci <- vc[itm_c_idx, itm_c_idx]
      cov_bc <- vc[step_idx, itm_c_idx]
      v <- var_bk + var_ci - 2 * cov_bc
      if (is.finite(v) && v > 0) ses_mat[i, k] <- sqrt(v)
    }
  }

  list(shared_b = shared_b, c_vals = c_vals,
       thresholds_mat = thresholds_mat, ses_mat = ses_mat)
}
```

Note: mirt's RSM internally names the shared step parameters `t1, t2, ...`
(see `mod2values(fit)`). If that naming ever differs in a future mirt
release, the `step_name` line is the single point of change — verify with
`mod2values(fit)` during debugging.

- [ ] **Step 2: Implement `.build_rsm_table`**

Replace the `.build_rsm_table` stub:

```r
.build_rsm_table <- function(fit, pars) {
  n_items <- nrow(pars)
  delta <- rsm_threshold_ses_delta(fit)

  a_vec <- rep(1, n_items)
  a_ses <- rep(NA_real_, n_items)

  b_mat <- delta$thresholds_mat
  b_ses <- delta$ses_mat

  a_df <- data.frame(a = a_vec, `SE(a)` = a_ses,
                     check.names = FALSE, stringsAsFactors = FALSE)
  b_df <- .interleave_with_ses(b_mat, b_ses)
  out  <- data.frame(a_df, b_df, check.names = FALSE,
                     stringsAsFactors = FALSE)
  rownames(out) <- rownames(pars)
  out
}
```

- [ ] **Step 3: Run RSM tests**

```bash
Rscript tests/manual/test-poly-tables.R 2>&1 | tail -15
```

Expected: the script's closing `--- poly table helpers: all tests passed ---`
line. If the `SE` test fails because mirt uses a different internal step
name, inspect `mod2values(fit_rsm)` and adjust the `step_name` line.

- [ ] **Step 4: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R
git commit -m "feat(poly-table): implement RSM branch with delta-method SEs"
```

---

## Task 8: Port `build_nrm_table` from the current reactive

**Files:**
- Modify: `inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R`

The NRM table format is to remain unchanged; lift the existing logic into
the helper file verbatim so the dispatcher can call it.

- [ ] **Step 1: Replace the stub**

Copy lines 279-365 from the current `IRT_poly_summary_coef_reactive` (the
NRM-specific path, which is the `else` path of the `model == "RSM"` check
when `model == "NRM"`) into a function. The NRM-specific behaviour is:
`use_irt <- isTRUE(input$IRT_poly_nrm_parametrization %in% c("blirt", "bock"))`,
then the generic cbind-interleave path.

```r
build_nrm_table <- function(fit, use_irt) {
  pars <- coef(fit, IRTpars = use_irt, simplify = TRUE)$items

  target_cols <- colnames(pars)
  ses <- .ses_to_matrix_nrm(fit, target_cols, use_irt)

  .interleave_with_ses(pars, ses)
}

# NRM uses its own parametrization flag, so it can't share .ses_to_matrix
# which hard-codes IRTpars = TRUE.
.ses_to_matrix_nrm <- function(fit, target_cols, use_irt) {
  se_list <- coef(fit, IRTpars = use_irt, printSE = TRUE)
  se_list[["GroupPars"]] <- NULL
  mat <- do.call(rbind, lapply(seq_along(se_list), function(i) {
    se_row <- se_list[[i]]["SE", ]
    out <- setNames(rep(NA_real_, length(target_cols)), target_cols)
    keep <- intersect(target_cols, names(se_row))
    out[keep] <- se_row[keep]
    out
  }))
  colnames(mat) <- target_cols
  mat
}
```

- [ ] **Step 2: Smoke-test NRM builder**

```bash
Rscript -e '
source("inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R")
suppressPackageStartupMessages(library(mirt))
set.seed(2)
dat <- as.data.frame(matrix(sample(0:3, 300*4, replace=TRUE), 300, 4))
dat[] <- lapply(dat, function(x) as.integer(as.factor(x)) - 1L)
fit <- mirt(dat, 1, itemtype = "nominal", SE = TRUE, verbose = FALSE)
tab <- build_nrm_table(fit, use_irt = TRUE)
stopifnot(nrow(tab) == 4)
stopifnot(ncol(tab) %% 2 == 0)
cat("OK\n")
'
```

Expected: `OK`.

- [ ] **Step 3: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R
git commit -m "feat(poly-table): port NRM builder into helper file"
```

---

## Task 9: Rewrite `IRT_poly_summary_coef_reactive` as a thin dispatcher

**Files:**
- Modify: `inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R:273-416`

- [ ] **Step 1: Replace the reactive and its renderTable**

In `polytomous_irt.R`, replace the block from the comment
`# ** Table of parameters (Summary) ####` (line 273) through the closing
`)` of `output$IRT_poly_summary_coef` (line 416) with:

```r
# ** Table of parameters (Summary) ####
IRT_poly_summary_coef_reactive <- reactive({
  fit   <- IRT_poly_model()
  model <- input$IRT_poly_model
  req(model)

  tab <- switch(model,
    "NRM"  = build_nrm_table(
      fit,
      use_irt = isTRUE(input$IRT_poly_nrm_parametrization %in%
                         c("blirt", "bock"))
    ),
    "GRM"  = build_grm_table(fit),
    "RSM"  = build_masters_table(fit, "RSM"),
    "PCM"  = build_masters_table(fit, "PCM"),
    "GPCM" = build_masters_table(fit, "GPCM")
  )

  tab <- append_fit_stats(tab, fit,
                          include_infit = model %in% c("RSM", "PCM"))
  tab <- italicise_colnames(tab)
  rownames(tab) <- item_names()
  tab
})

output$IRT_poly_summary_coef <- renderTable(
  {
    IRT_poly_summary_coef_reactive()
  },
  include.rownames = TRUE,
  include.colnames = TRUE,
  striped = TRUE,
  na = ""
)
```

- [ ] **Step 2: Update the CSV download handler**

Immediately below the `renderTable` call (currently lines 418-425), replace
the download handler with:

```r
output$IRT_poly_summary_coef_download <- downloadHandler(
  filename = function() {
    paste0("tab_IRT_poly_", input$IRT_poly_model, "_parameters.csv")
  },
  content = function(file) {
    write.csv(strip_katex(IRT_poly_summary_coef_reactive()), file)
  }
)
```

- [ ] **Step 3: Update the coef note (remove RSM-specific shared-threshold text)**

RSM thresholds are now shown in-table (derived per item). The "Shared
threshold parameters" note stays per the spec — it's still useful context.
Leave the `output$IRT_poly_summary_coef_note` block (lines 239-271)
unchanged.

- [ ] **Step 4: Start the app and verify every model renders without error**

Launch the app locally and click through each model in the dropdown, on
the Anxiety dataset:

```bash
Rscript -e 'shiny::runApp("inst/ShinyItemAnalysis", launch.browser = TRUE)'
```

Manually verify for each model:

| Model | Expected column headers (italicised in browser) |
|---|---|
| NRM | whatever the current NRM layout produces (unchanged) |
| GRM | `a SE(a) b1 SE(b1) b2 SE(b2) b3 SE(b3) b4 SE(b4) SX2-value df p-value` |
| RSM | `a SE(a) b1 SE(b1) ... SX2-value df p-value Outfit MNSQ Infit MNSQ` |
| PCM | as RSM |
| GPCM | as RSM without Outfit/Infit |

Verify `a`, `b1`, etc. render in italics in the browser (KaTeX). Verify
blank (not `NA` text) in cells where parameters don't exist.

- [ ] **Step 5: Commit**

```bash
git add inst/ShinyItemAnalysis/server/IRT/polytomous_irt.R
git commit -m "refactor(poly-table): replace monolithic reactive with dispatcher"
```

---

## Task 10: Verify Wright map still renders for RSM, PCM, GPCM

**Files:** none modified

- [ ] **Step 1: With the app running, verify each Wright map**

For each of RSM, PCM, GPCM on the Anxiety dataset:

1. Select the model in the dropdown.
2. Scroll to the Wright map panel.
3. Confirm it renders without the "data must be a data.frame" error.
4. Confirm threshold points appear at reasonable latent-trait locations.

Then load a mixed dataset (e.g. a CSV that has both binary and ordinal
columns, or use the app's upload feature with one of the test datasets) and
re-verify PCM/GPCM Wright maps render with binary items showing a single
point.

- [ ] **Step 2: Verify the Wright map download**

Click the download button below the Wright map. Open the downloaded PNG
and confirm both panels (histogram + item thresholds) are present and
aligned.

- [ ] **Step 3: No commit needed** (verification only).

---

## Task 11: Update `sc/irt/rsm.R` sample

**Files:**
- Modify: `inst/ShinyItemAnalysis/sc/irt/rsm.R`

- [ ] **Step 1: Replace the file contents**

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

# estimated parameters (IRT parametrization)
pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
b_cols <- grep("^b\\d", colnames(pars))

# item-specific thresholds b_ik = b_k(shared) - c_i (as shown in the app)
shared_b <- pars[1, b_cols]
c_vals   <- pars[, "c"]
thresholds <- t(outer(as.numeric(shared_b), as.numeric(c_vals), "-"))
colnames(thresholds) <- names(shared_b)
rownames(thresholds) <- rownames(pars)
thresholds

# SEs (printSE = TRUE gives SEs for the shared steps + item c values)
coef(fit, IRTpars = TRUE, printSE = TRUE)

# item fit statistics
itemfit(fit)

# infit and outfit statistics (RSM is Rasch-family)
itemfit(fit, fit_stats = "infit")

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(Data)))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# Wright map (uses derived item-specific thresholds)
ggWrightMap(fs, thresholds)
```

- [ ] **Step 2: Run the sample script end-to-end**

```bash
Rscript inst/ShinyItemAnalysis/sc/irt/rsm.R 2>&1 | tail -5
```

Expected: completes without error; correlation value printed at end.

- [ ] **Step 3: Commit**

```bash
git add inst/ShinyItemAnalysis/sc/irt/rsm.R
git commit -m "docs(sc): align rsm.R sample with redesigned app output"
```

---

## Task 12: Update `sc/irt/pcm.R` sample

**Files:**
- Modify: `inst/ShinyItemAnalysis/sc/irt/pcm.R`

- [ ] **Step 1: Replace the file contents**

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

# estimated parameters (IRT parametrization)
coef(fit, IRTpars = TRUE, simplify = TRUE)
coef(fit, IRTpars = TRUE, printSE = TRUE)

# item fit statistics
itemfit(fit)

# infit and outfit statistics (PCM is Rasch-family)
itemfit(fit, fit_stats = "infit")

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(Data)))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# Wright map
pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
b <- pars[, grep("^b\\d", colnames(pars)), drop = FALSE]
ggWrightMap(fs, b)
```

- [ ] **Step 2: Run the sample script end-to-end**

```bash
Rscript inst/ShinyItemAnalysis/sc/irt/pcm.R 2>&1 | tail -5
```

Expected: completes without error.

- [ ] **Step 3: Commit**

```bash
git add inst/ShinyItemAnalysis/sc/irt/pcm.R
git commit -m "docs(sc): align pcm.R sample with redesigned app output"
```

---

## Task 13: Update `sc/irt/gpcm.R` sample

**Files:**
- Modify: `inst/ShinyItemAnalysis/sc/irt/gpcm.R`

- [ ] **Step 1: Append Wright map section to the file**

The GPCM sample is otherwise correct (already uses `IRTpars = TRUE`). Add
only the Wright map section at the end. After the existing `cor(fs, sts)`
line add:

```r

# Wright map
pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
b <- pars[, grep("^b\\d", colnames(pars)), drop = FALSE]
ggWrightMap(fs, b)
```

Also replace the `# factor scores` block just above so `fs` is assigned
(currently the file already assigns it — confirm; no change needed if
already assigned).

- [ ] **Step 2: Run the sample script end-to-end**

```bash
Rscript inst/ShinyItemAnalysis/sc/irt/gpcm.R 2>&1 | tail -5
```

Expected: completes without error.

- [ ] **Step 3: Commit**

```bash
git add inst/ShinyItemAnalysis/sc/irt/gpcm.R
git commit -m "docs(sc): add Wright map section to gpcm.R sample"
```

---

## Task 14: Final verification pass

**Files:** none modified

- [ ] **Step 1: Re-run all helper unit tests**

```bash
Rscript tests/manual/test-poly-tables.R 2>&1 | tail -3
```

Expected: `--- poly table helpers: all tests passed ---`.

- [ ] **Step 2: Full manual UI walkthrough**

```bash
Rscript -e 'shiny::runApp("inst/ShinyItemAnalysis", launch.browser = TRUE)'
```

Walk each model (NRM, GRM, RSM, PCM, GPCM) on the Anxiety dataset and on a
mixed dataset (e.g. binary HCI columns combined with polytomous Anxiety):

- Parameter table columns match Task 9's expected layout.
- Italic headers render correctly (KaTeX) — no garbled `X..mathit..` text.
- Blank cells where parameters don't exist.
- Wright map renders for RSM, PCM, GPCM (no error).
- CSV download contains plain column names (no `\(\mathit` markup).
- Sample R code at the bottom matches what the tab is showing.

- [ ] **Step 3: Push to `poly` remote** (user instruction from CLAUDE memory)

```bash
git push poly master
```

---

## Self-Review Checklist (for the plan author, not the executor)

- [x] Spec sections covered: KaTeX fix (Tasks 3, 9), per-model layouts
  (Tasks 5-8), mixed data (Tasks 5, 6), RSM derived thresholds + delta
  (Task 7), fit stats selection (Task 4 + dispatcher in 9), Wright map
  verification (Task 10), CSV download plain-names (Task 9), sample R code
  (Tasks 11-13).
- [x] No placeholders — every step shows full code or exact command.
- [x] Type/name consistency: helper names match across Tasks 1 (stubs), 3,
  4, 5, 6, 7, 8, 9. `.interleave_with_ses`, `.ses_to_matrix`,
  `.fold_binary_pars` are defined in Task 5, used in Tasks 6, 7, 8.
- [x] Files and line numbers are specific.
