# Manual unit tests for the polytomous Wright map threshold matrix.
# Run: Rscript tests/manual/test-poly-wrightmap.R
suppressPackageStartupMessages({
  library(testthat)
})

source("inst/ShinyItemAnalysis/server/IRT/poly_table_helpers.R")

# ---- fixtures ---------------------------------------------------------------
# Column names below are what mirt::coef(..., IRTpars = TRUE)$items actually
# returns; confirmed empirically against GMAT (all binary) and dataMedicalgraded.

# All items binary, itemtype = "Rasch": mirt drops to the DICHOTOMOUS
# parameterisation - difficulty lands in 'b' and there is no 'b1' at all.
pars_binary_pcm <- matrix(
  c(1, -0.107, 0, 1,
    1,  0.412, 0, 1,
    1, -0.883, 0, 1),
  nrow = 3, byrow = TRUE,
  dimnames = list(c("Item1", "Item2", "Item3"), c("a", "b", "g", "u"))
)

# All items binary, itemtype = "gpcm": mirt keeps 'b1'.
pars_binary_gpcm <- matrix(
  c(0.791, -0.144,
    1.102,  0.377,
    0.655, -0.901),
  nrow = 3, byrow = TRUE,
  dimnames = list(c("Item1", "Item2", "Item3"), c("a", "b1"))
)

# Genuinely polytomous: three thresholds per item.
pars_poly <- matrix(
  c(1, -1.2, 0.1, 1.4,
    1, -0.8, 0.3, 1.1,
    1, -1.5, 0.0, 1.9),
  nrow = 3, byrow = TRUE,
  dimnames = list(c("Item1", "Item2", "Item3"), c("a", "b1", "b2", "b3"))
)

# Mixed: two polytomous items plus one binary item whose difficulty is in 'b'.
pars_mixed <- matrix(
  c(1, -1.2, 0.1, NA,
    1, -0.8, 0.3, NA,
    1,   NA,  NA, 0.45),
  nrow = 3, byrow = TRUE,
  dimnames = list(c("Item1", "Item2", "Item3"), c("a", "b1", "b2", "b"))
)

# ---- .poly_threshold_matrix() -----------------------------------------------

test_that("all-binary PCM data yields one threshold column", {
  # Regression test: a dichotomous item is a single-threshold item, so it
  # belongs on the Wright map. Before this was handled, grep("^b\\d") matched
  # zero columns for all-binary Rasch/PCM fits, the threshold matrix came back
  # with no columns, and the Wright map silently failed to render.
  b <- .poly_threshold_matrix(pars_binary_pcm, "PCM")
  expect_equal(ncol(b), 1L)
  expect_identical(colnames(b), "b1")
  expect_equal(unname(b[, 1]), c(-0.107, 0.412, -0.883))
  expect_identical(rownames(b), c("Item1", "Item2", "Item3"))
})

test_that("all-binary GPCM data already has b1 and is passed through", {
  b <- .poly_threshold_matrix(pars_binary_gpcm, "GPCM")
  expect_equal(ncol(b), 1L)
  expect_identical(colnames(b), "b1")
  expect_equal(unname(b[, 1]), c(-0.144, 0.377, -0.901))
})

test_that("genuinely polytomous data keeps all thresholds", {
  b <- .poly_threshold_matrix(pars_poly, "PCM")
  expect_equal(ncol(b), 3L)
  expect_identical(colnames(b), c("b1", "b2", "b3"))
  expect_equal(unname(b["Item3", ]), c(-1.5, 0.0, 1.9))
})

test_that("mixed binary/polytomous data folds the binary 'b' into b1", {
  b <- .poly_threshold_matrix(pars_mixed, "PCM")
  expect_equal(ncol(b), 2L)
  expect_identical(colnames(b), c("b1", "b2"))
  # the binary item's difficulty is promoted into b1 ...
  expect_equal(unname(b["Item3", "b1"]), 0.45)
  # ... without disturbing the polytomous items
  expect_equal(unname(b["Item1", ]), c(-1.2, 0.1))
  expect_true(is.na(b["Item3", "b2"]))
})

test_that("the coefficient table builds for all-binary PCM data", {
  # Regression test for the second failure seen in the app log:
  #   .interleave_with_ses -> colnames<- : length of 'dimnames' [2] not equal
  #   to array extent, reached via build_masters_table.
  # Same cause as the Wright map's: .fold_binary_pars() dropped 'b' without
  # promoting it, leaving zero threshold columns.
  skip_if_not_installed("mirt")
  suppressPackageStartupMessages(library(mirt))

  set.seed(1)
  n <- 400; J <- 5
  th <- rnorm(n)
  bs <- seq(-1, 1, length.out = J)
  dat <- sapply(seq_len(J), function(j) rbinom(n, 1, plogis(th - bs[j])))
  colnames(dat) <- paste0("Item", seq_len(J))

  fit <- mirt(dat, 1, itemtype = "Rasch", SE = TRUE, verbose = FALSE)

  pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
  expect_true("b" %in% colnames(pars))
  expect_false("b1" %in% colnames(pars))   # the shape that used to break

  tab <- build_masters_table(fit, "PCM")
  expect_s3_class(tab, "data.frame")
  expect_equal(nrow(tab), J)
  expect_true(all(c("a", "SE(a)", "b1", "SE(b1)") %in% colnames(tab)))
  expect_false(any(is.na(tab$b1)))

  # ... and the Wright map threshold matrix from that same fit
  b <- .poly_threshold_matrix(pars, "PCM")
  expect_equal(ncol(b), 1L)
  expect_equal(nrow(b), J)
  expect_false(any(is.na(b[, 1])))
})

test_that("every threshold matrix has at least one column for fitted data", {
  # The Wright map cannot render from a zero-column matrix; assert none of the
  # shapes mirt can return produce one.
  for (p in list(pars_binary_pcm, pars_binary_gpcm, pars_poly, pars_mixed)) {
    expect_gt(ncol(.poly_threshold_matrix(p, "PCM")), 0L)
  }
})
