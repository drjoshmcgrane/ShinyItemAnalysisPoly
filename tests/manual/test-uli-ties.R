# Run from the package root: Rscript tests/manual/test-uli-ties.R
library(testthat)
source("R/gDiscrim.R")

test_that("floor ties return unavailable ULI rather than an indexing error", {
  d <- data.frame(a = c(rep(1, 40), rep(2, 30), rep(3, 30)),
                  b = c(rep(1, 40), rep(2, 30), rep(3, 30)))
  expect_warning(result <- gDiscrim(d), "score groups empty")
  expect_identical(result, c(a = NA_real_, b = NA_real_))
  expect_warning(shifted <- gDiscrim(d - 1), "score groups empty")
  expect_identical(result, shifted)
})

test_that("duplicate cutpoints and missing rows are handled explicitly", {
  d <- data.frame(a = rep(1, 12), b = rep(1, 12))
  expect_warning(result <- gDiscrim(d), "too few unique cutpoints")
  expect_true(all(is.na(result)))
  d[,] <- NA_real_
  expect_warning(result <- gDiscrim(d), "no complete response rows")
  expect_true(all(is.na(result)))
})

test_that("valid groups retain their original numerical results", {
  d <- data.frame(a = 1:12, b = 1:12)
  expect_equal(gDiscrim(d), c(a = 8/11, b = 8/11))
  expect_equal(gDiscrim(d, k = 4, l = 2, u = 4), c(a = 6/11, b = 6/11))
  expect_error(gDiscrim(d, k = 3, u = 4), "lower or equal")
})
