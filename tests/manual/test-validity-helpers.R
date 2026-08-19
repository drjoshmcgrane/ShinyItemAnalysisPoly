# Manual unit tests for the Validity tab helpers.
# Run: Rscript tests/manual/test-validity-helpers.R
suppressPackageStartupMessages({
  library(testthat)
})

source("inst/ShinyItemAnalysis/server/validity_helpers.R")

# ---- .validity_theta_model() ------------------------------------------------

test_that("binary data is served by the dichotomous IRT model", {
  expect_identical(.validity_theta_model("binary"), "dichotomous")
})

test_that("nominal data is served by the dichotomous IRT model", {
  # Regression test: nominal responses (GMAT, HCI, MSAT-B, Medical 100, and
  # the default for uploaded CSVs) are scored to binary with key2binary, so
  # theta comes from the dichotomous model. Dispatching on the raw data type
  # without this case made criterion validity report "IRT theta is not
  # available" for those datasets even with the model fitted.
  expect_identical(.validity_theta_model("nominal"), "dichotomous")
})

test_that("ordinal data is served by the polytomous IRT model", {
  expect_identical(.validity_theta_model("ordinal"), "polytomous")
})

test_that("theta does not apply to continuous or unknown data types", {
  expect_true(is.na(.validity_theta_model("continuous")))
  expect_true(is.na(.validity_theta_model(NA_character_)))
  expect_true(is.na(.validity_theta_model(NULL)))
  expect_true(is.na(.validity_theta_model("something else")))
})

test_that("every data type the Data tab can set is handled explicitly", {
  # The values dataset$data_type can take: the toy-data observer in
  # server/Data.R sets binary/ordinal/continuous/nominal, and the upload radio
  # in ui/uiData.R offers binary/nominal/ordinal.
  known <- c("binary", "nominal", "ordinal", "continuous")
  got <- vapply(known, .validity_theta_model, character(1))
  expect_identical(
    unname(got),
    c("dichotomous", "dichotomous", "polytomous", NA_character_)
  )
})
