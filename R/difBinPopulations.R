#' Per-group bin populations for a DIF item plot
#'
#' Returns the sample size of each empirical bin used by [plotDIFLogistic()]
#' or [plotDIFOrdExpected()], split by group. Useful as a diagnostic
#' alongside the DIF item plot — a bin with very few respondents (a common
#' rule of thumb is `n < 30` per group) should not be over-interpreted.
#'
#' @param match numeric vector of the matching variable (e.g. WLE θ).
#' @param group numeric vector of group membership coded 0/1 (reference/focal).
#' @param num.groups integer: number of bins.
#' @param bin.type character: either `"equal-dist"` (default) or
#'   `"equal-freq"`. See [plotDIFLogistic()] for details.
#' @param group.names character vector of length two giving names for the
#'   reference and focal groups. Default `c("Reference", "Focal")`.
#'
#' @return A data frame with columns `Bin`, `Lower`, `Upper`, `Midpoint`,
#'   one `n_<group>` column per group, and a `Flag` column that marks bins
#'   with fewer than 30 respondents in at least one group as `"sparse"`.
#'
#' @author Joshua McGrane
#'
#' @export
difBinPopulations <- function(match, group, num.groups = 10L,
                              bin.type = c("equal-dist", "equal-freq"),
                              group.names = c("Reference", "Focal")) {
  bin.type <- match.arg(bin.type)
  num.groups <- as.integer(num.groups)
  if (length(match) != length(group)) {
    stop("'match' and 'group' must have the same length.", call. = FALSE)
  }
  if (length(group.names) < 2L) group.names <- c("Reference", "Focal")
  match <- as.numeric(match)
  group <- as.integer(group)

  rng_all <- range(match, na.rm = TRUE)
  if (!isTRUE(diff(rng_all) > 0)) {
    return(data.frame(Bin = 1L, Lower = rng_all[1], Upper = rng_all[1],
                      Midpoint = rng_all[1],
                      setNames(list(sum(group == 0L), sum(group == 1L)),
                               paste0("n_", group.names)),
                      Flag = "single-value"))
  }

  brks <- if (bin.type == "equal-dist") {
    seq(rng_all[1], rng_all[2], length.out = num.groups + 1L)
  } else {
    unique(stats::quantile(match,
                           probs = seq(0, 1, length.out = num.groups + 1L),
                           na.rm = TRUE, type = 7))
  }
  if (length(brks) < 3L) {
    return(data.frame(Bin = 1L,
                      Lower = brks[1], Upper = utils::tail(brks, 1),
                      Midpoint = mean(brks),
                      setNames(list(sum(group == 0L), sum(group == 1L)),
                               paste0("n_", group.names)),
                      Flag = "merged"))
  }
  n_bins <- length(brks) - 1L
  bins <- cut(match, breaks = brks, include.lowest = TRUE)
  tbl_ref <- as.integer(table(bins[group == 0L]))
  tbl_foc <- as.integer(table(bins[group == 1L]))

  out <- data.frame(
    Bin      = seq_len(n_bins),
    Lower    = round(brks[-(n_bins + 1)], 3),
    Upper    = round(brks[-1], 3),
    Midpoint = round((brks[-(n_bins + 1)] + brks[-1]) / 2, 3)
  )
  out[[paste0("n_", group.names[1])]] <- tbl_ref
  out[[paste0("n_", group.names[2])]] <- tbl_foc
  sparse <- (tbl_ref < 30L) | (tbl_foc < 30L)
  empty  <- (tbl_ref + tbl_foc) == 0L
  out$Flag <- ifelse(empty, "empty", ifelse(sparse, "sparse", "ok"))
  out
}
