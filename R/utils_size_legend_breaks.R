# Three representative size breaks for the marker-count legend, anchored to
# the actual bin-count distribution in the data being analysed (smallest,
# median, largest), rounded to a sensible step size given the spread.
#' @keywords internal
#' @noRd
.size_legend_breaks <- function(counts) {
  counts <- counts[is.finite(counts)]
  if (length(counts) == 0L) return(NULL)
  if (length(counts) == 1L) return(counts)
  lo <- min(counts); hi <- max(counts); mid <- stats::median(counts)
  rng <- hi - lo
  step <- if (rng <= 0) 1 else
          if (rng <= 10) 1 else
          if (rng <= 50) 5 else
          if (rng <= 100) 10 else
          if (rng <= 500) 25 else
          if (rng <= 1000) 50 else 100
  brks <- unique(round(c(lo, mid, hi) / step) * step)
  if (length(brks) < 2L) brks <- unique(c(lo, hi))
  brks
}
