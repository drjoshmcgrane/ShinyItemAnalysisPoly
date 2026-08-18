#' Expected-score curve plot for ordinal DIF models
#'
#' Plots the model-implied expected item score curves for the reference and
#' focal groups from a polytomous DIF fit produced by [difNLR::difORD()]
#' (either the cumulative-logit or adjacent-category model). Optionally
#' overlays the empirical mean item score within equal-frequency bins of the
#' matching variable for each group.
#'
#' This is the polytomous analogue of the dichotomous item characteristic curve
#' plot used in the IRT model tabs: each curve is the model-implied
#' \eqn{E[Y \mid \mathrm{match}, \mathrm{group}]} as a function of the matching
#' variable, on the original item-score metric (0, 1, ..., K-1).
#'
#' @param x an object of class `"difORD"` from `difNLR::difORD()`.
#' @param item integer or character: index or name of the item to be plotted.
#' @param item.name character: title for the plot. If missing, the column name
#'   of the item in `x$Data` is used.
#' @param group.names character vector of length two: labels for the reference
#'   and focal groups. Defaults to `x$group.names` (falling back to
#'   `c("Reference", "Focal")`).
#' @param match.name character: label for the x-axis. Defaults to
#'   `x$match.name`.
#' @param draw.empirical logical: whether to overlay observed mean item scores.
#'   Default `TRUE`.
#' @param num.groups integer: number of equal-frequency bins to use for the
#'   empirical overlay, mirroring the convention used by the IRT model item
#'   plots in this package. Default `3`.
#' @param n.theta integer: number of points along the matching scale at which
#'   to evaluate the expected-score curve. Default `300`.
#' @param xlim numeric vector of length two: optional x-axis range. The
#'   default `NULL` uses the observed matching range.
#' @param bin.type character: how to define the empirical bins on the
#'   matching scale. `"equal-freq"` (default) uses quantile-based bins;
#'   `"equal-dist"` splits the matching range into bins of equal width.
#'
#' @return A `ggplot` object.
#'
#' @details Empirical bins are formed separately within each group using
#'   `quantile()` breaks of the matching variable, and the empirical y value
#'   is the mean of the item score within each bin. Bin sizes (counts) are
#'   encoded via the point area.
#'
#'   For the cumulative-logit and adjacent-category models, expected scores are
#'   computed as \eqn{\sum_k k \, P(Y = k \mid \mathrm{match}, \mathrm{group})}
#'   using [difNLR::predict.difORD()] with `type = "category"`.
#'
#' @author
#' Joshua McGrane, building on the difNLR DIF fitting infrastructure by
#' Adela Hladka and Patricia Martinkova (Institute of Computer Science, Czech
#' Academy of Sciences).
#'
#' @seealso [difNLR::difORD()], [difNLR::predict.difORD()]
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_colour_manual
#'   scale_linetype_manual scale_fill_manual scale_y_continuous xlab ylab
#'   ggtitle guides guide_legend theme unit
#' @importFrom stats quantile predict
#'
#' @export
plotDIFOrdExpected <- function(x, item = 1, item.name, group.names,
                               match.name, draw.empirical = TRUE,
                               num.groups = 3L, n.theta = 300L,
                               xlim = NULL,
                               bin.type = c("equal-dist", "equal-freq")) {
  bin.type <- match.arg(bin.type)
  if (!inherits(x, "difORD")) {
    stop("'x' must be an object of class 'difORD' (from difNLR::difORD()).",
         call. = FALSE)
  }

  # Resolve item index
  nams <- colnames(x$Data)
  if (is.character(item) || is.factor(item)) {
    i <- which(nams == as.character(item))
    if (length(i) == 0L) {
      stop("Item '", item, "' not found in x$Data.", call. = FALSE)
    }
  } else {
    i <- as.integer(item)
    if (is.na(i) || i < 1L || i > length(nams)) {
      stop("'item' index out of range.", call. = FALSE)
    }
  }
  if (missing(item.name) || is.null(item.name)) item.name <- nams[i]

  # Resolve group labels
  if (missing(group.names) || is.null(group.names)) {
    group.names <- x$group.names
    if (length(group.names) < 2L || all(group.names %in% c(0, 1))) {
      group.names <- c("Reference", "Focal")
    }
  }
  if (length(group.names) < 2L) {
    group.names <- c("Reference", "Focal")
  }

  # Resolve matching variable for this item
  match_full <- if (is.list(x$match)) x$match[[i]] else x$match
  if (is.null(match_full)) {
    stop("Matching variable not available on fit (x$match is NULL).",
         call. = FALSE)
  }
  if (missing(match.name) || is.null(match.name)) {
    match.name <- if (!is.null(x$match.name)) x$match.name else "Matching criterion"
  }

  # Determine category set and labels (use observed values so the y-axis
  # matches the data scale)
  item_y <- x$Data[, i]
  cats <- sort(unique(stats::na.omit(item_y)))
  cat_values <- as.numeric(cats)

  # Theta grid spanning the observed matching range (or user-supplied xlim)
  if (!is.null(xlim) && length(xlim) == 2L && all(is.finite(xlim))) {
    rng <- as.numeric(xlim)
  } else {
    rng <- range(match_full, na.rm = TRUE)
  }
  theta_grid <- seq(rng[1], rng[2], length.out = as.integer(n.theta))

  ## Expected-score curve per group ------------------------------------------
  exp_score <- function(g) {
    pr <- predict(x, item = i, match = theta_grid, group = g, type = "category")
    pr <- as.matrix(pr)
    # Align to observed categories (pr column order is sorted ascending by
    # the model already; we cap to length of cat_values defensively)
    K <- min(ncol(pr), length(cat_values))
    drop(pr[, seq_len(K), drop = FALSE] %*% cat_values[seq_len(K)])
  }

  curve_df <- rbind(
    data.frame(Match = theta_grid, Expected = exp_score(0L), Group = "gr1"),
    data.frame(Match = theta_grid, Expected = exp_score(1L), Group = "gr2")
  )

  ## Empirical mean per bin per group ----------------------------------------
  match_all <- match_full
  bin_means <- function(match_g, y_g, k = num.groups) {
    if (length(match_g) == 0L) {
      return(data.frame(Match = numeric(0), Expected = numeric(0),
                        Count = integer(0)))
    }
    rng_g <- range(match_g, na.rm = TRUE)
    if (rng_g[1] == rng_g[2]) {
      return(data.frame(Match = rng_g[1],
                        Expected = mean(y_g, na.rm = TRUE),
                        Count = length(match_g)))
    }
    brks <- if (bin.type == "equal-dist") {
      rng_all <- range(match_all, na.rm = TRUE)
      seq(rng_all[1], rng_all[2], length.out = k + 1L)
    } else {
      unique(stats::quantile(match_g,
                             probs = seq(0, 1, length.out = k + 1L),
                             na.rm = TRUE, type = 7))
    }
    if (length(brks) < 3L) {
      return(data.frame(Match = mean(match_g, na.rm = TRUE),
                        Expected = mean(y_g, na.rm = TRUE),
                        Count = length(match_g)))
    }
    bins <- cut(match_g, breaks = brks, include.lowest = TRUE)
    tbl <- table(bins)
    keep <- tbl > 0
    data.frame(
      Match = as.numeric(tapply(match_g, bins, mean, na.rm = TRUE))[keep],
      Expected = as.numeric(tapply(y_g, bins, mean, na.rm = TRUE))[keep],
      Count = as.integer(tbl)[keep]
    )
  }

  emp_df <- NULL
  if (isTRUE(draw.empirical)) {
    grp <- as.integer(x$group)
    e0 <- bin_means(match_full[grp == 0L], item_y[grp == 0L])
    e1 <- bin_means(match_full[grp == 1L], item_y[grp == 1L])
    if (nrow(e0) > 0L || nrow(e1) > 0L) {
      emp_df <- rbind(
        cbind(e0, Group = "gr1"),
        cbind(e1, Group = "gr2")
      )
    }
  }

  ## Plot --------------------------------------------------------------------
  col <- c(gr1 = "dodgerblue2", gr2 = "goldenrod2")
  linetype <- c(gr1 = "solid", gr2 = "dashed")
  y_min <- min(cat_values)
  y_max <- max(cat_values)

  g <- ggplot() +
    geom_line(
      data = curve_df,
      aes(x = .data$Match, y = .data$Expected,
          colour = .data$Group, linetype = .data$Group),
      linewidth = 0.8
    ) +
    scale_colour_manual(values = col, breaks = c("gr1", "gr2"),
                        labels = group.names) +
    scale_linetype_manual(values = linetype, breaks = c("gr1", "gr2"),
                          labels = group.names) +
    scale_fill_manual(values = col, breaks = c("gr1", "gr2"),
                      labels = group.names) +
    guides(colour = guide_legend(title = "Group", order = 2),
           linetype = guide_legend(title = "Group", order = 2)) +
    xlab(match.name) +
    ylab("Expected item score") +
    scale_y_continuous(limits = c(y_min, y_max)) +
    ggtitle(item.name) +
    theme_app() +
    theme(
      legend.box.just = "top",
      legend.position = c(0.01, 0.98),
      legend.justification = c(0, 1),
      legend.key.width = unit(1, "cm"),
      legend.box = "horizontal"
    )

  if (!is.null(emp_df) && nrow(emp_df) > 0L) {
    g <- g + geom_point(
      data = emp_df,
      aes(x = .data$Match, y = .data$Expected,
          colour = .data$Group, fill = .data$Group, size = .data$Count),
      alpha = 0.5, shape = 21
    ) +
      scale_size_continuous(breaks = .size_legend_breaks(emp_df$Count)) +
      guides(size = guide_legend(title = "Count", order = 1),
             fill = guide_legend(title = "Group", order = 2))
  }
  g
}
