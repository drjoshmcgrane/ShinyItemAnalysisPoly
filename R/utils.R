#' Remove columns that are empty
#'
#' @param .data data.frame
#'
#' @keywords internal
#'
#' @return cleaned df
#'
remove_empty_cols <- function(.data) {
  mask_keep <- colSums(!is.na(.data)) > 0
  .data[, mask_keep, drop = FALSE]
}


na_to_null <- function(x) {
  if (is.na(x)) NULL else x
}


# Marginal reliability of a unidimensional mirt model, integrated over the
# model's own latent distribution. mirt::marginal_rxx() computes
# I / (I + var) over N(0, 1), which is only right when the latent variance is
# fixed at 1; Rasch-family fits (Rasch, PCM, RSM) estimate it, so use
# I / (I + 1 / var) over N(mean, var) instead.
marginal_rxx_latent <- function(mod) {
  cfs <- coef(mod, simplify = TRUE)
  mu <- as.vector(cfs$means)
  var_theta <- as.vector(cfs$cov)
  fn <- function(theta) {
    TI <- mirt::testinfo(mod, matrix(theta))
    TI / (TI + 1 / var_theta) * stats::dnorm(theta, mu, sqrt(var_theta))
  }
  stats::integrate(fn, lower = -Inf, upper = Inf)$value
}
