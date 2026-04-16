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

poly_data  <- sim_poly()
mixed_data <- sim_mixed()

fit_grm  <- mirt(poly_data, 1, itemtype = "graded", SE = TRUE, SE.type = "Fisher", verbose = FALSE)
fit_pcm  <- mirt(poly_data, 1, itemtype = "Rasch",  SE = TRUE, SE.type = "Fisher", verbose = FALSE)
fit_gpcm <- mirt(poly_data, 1, itemtype = "gpcm",   SE = TRUE, SE.type = "Fisher", verbose = FALSE)
fit_rsm  <- mirt(poly_data, 1, itemtype = "rsm",    SE = TRUE, SE.type = "Fisher", verbose = FALSE)

fit_pcm_mixed  <- mirt(mixed_data, 1, itemtype = "Rasch",  SE = TRUE, SE.type = "Fisher", verbose = FALSE)
fit_gpcm_mixed <- mirt(mixed_data, 1, itemtype = "gpcm",   SE = TRUE, SE.type = "Fisher", verbose = FALSE)
fit_grm_mixed  <- mirt(mixed_data, 1, itemtype = "graded", SE = TRUE, SE.type = "Fisher", verbose = FALSE)

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
