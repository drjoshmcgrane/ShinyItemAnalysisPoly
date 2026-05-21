#' Function for characteristic curve of 2PL logistic DIF model
#'
#' @aliases plotDIFLogistic
#'
#' @description Plots characteristic curve of 2PL logistic DIF model
#'
#' @param x an object of `"Logistic"` class. See **Details**.
#' @param item numeric: number of item to be plotted
#' @param item.name character: the name of item to be used as title of plot.
#' @param group.names character: names of reference and focal group.
#' @param Data numeric: the data matrix. See **Details**.
#' @param group numeric: the vector of group membership. See **Details**.
#' @param match character or numeric: specifies observed score used for
#'   matching. Can be either `"score"`, or numeric vector of the same
#'   length as number of observations in `Data`. See **Details**.
#' @param draw.empirical logical: whether empirical probabilities should be
#'   calculated and plotted. Default value is `TRUE`.
#' @param num.groups integer or `NULL`: number of equal-frequency bins to use
#'   when aggregating the empirical proportions. The default `NULL` preserves
#'   the legacy behaviour for `match = "score"` (one bin per unique total
#'   score). For continuous matching variables (`"zscore"`, user-supplied
#'   numeric vectors such as IRT \eqn{\theta} or WLE estimates) it defaults to
#'   `3`. Ignored when `draw.empirical = FALSE`.
#' @param match.name character: label for the x-axis. If missing, an
#'   appropriate label is chosen from `match`.
#' @param xlim numeric vector of length two: optional x-axis range. The
#'   default `NULL` uses the data range (padded by 0.1 logits on each side).
#' @param bin.type character: how to define the empirical bins on the
#'   matching scale. `"equal-dist"` (default) splits the matching range
#'   into bins of equal width; bin midpoints sit at constant intervals on
#'   the x-axis and tail bins may have fewer respondents (be wary of bins
#'   with fewer than ~30 respondents). `"equal-freq"` uses quantile-based
#'   bins so each bin holds roughly the same number of respondents but at
#'   uneven x-axis intervals. Ignored when the matching variable is an
#'   integer total score (which is always grouped by unique value).
#'
#' @details This function plots characteristic curves of 2PL logistic DIF model
#' fitted by `difLogistic()` function from difR package using ggplot2.
#'
#' `Data` and `group` are used to calculate empirical probabilities
#' for reference and focal group. `match` should be the same as in
#' `x$match`. In case that an observed score is used as a matching variable
#' instead of the total score or the standardized score, `match` needs to
#' be a numeric vector of the same the same length as the number of observations
#' in `Data`.
#'
#' When the matching variable is continuous (anything other than a raw integer
#' total score), empirical proportions are aggregated into `num.groups`
#' equal-frequency bins on the matching scale, separately within each group.
#' Earlier versions of this function grouped by the exact value of the
#' matching variable, which produced a noisy point cloud when the matching
#' variable was continuous (e.g. a Rasch WLE \eqn{\theta}). Pass `num.groups`
#' explicitly to override the default.
#'
#' @author
#' Adela Hladka \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{hladka@@cs.cas.cz}
#'
#' Patricia Martinkova \cr
#' Institute of Computer Science of the Czech Academy of Sciences \cr
#' \email{martinkova@@cs.cas.cz} \cr
#'
#' @examples
#' # loading libraries
#' library(difR)
#'
#' # loading data based on GMAT
#' data(GMAT, package = "difNLR")
#' Data <- GMAT[, 1:20]
#' group <- GMAT[, 21]
#'
#' # DIF detection using difLogistic() function
#' x <- difLogistic(Data, group, focal.name = 1)
#' # Characteristic curve by logistic regression model
#' plotDIFLogistic(x, item = 1, Data = Data, group = group)
#'
#' # Using name of column as item identifier
#' plotDIFLogistic(x, item = "Item1", Data = Data, group = group)
#'
#' # Renaming reference and focal group
#' plotDIFLogistic(x, item = 1, group.names = c("Group 1", "Group 2"), Data = Data, group = group)
#'
#' # Not plotting empirical probabilities
#' plotDIFLogistic(x, item = 1, draw.empirical = FALSE)
#'
#' # Matching on Rasch WLE theta with five equal-frequency bins
#' theta <- as.numeric(mirt::fscores(mirt::mirt(Data, 1, "Rasch", verbose = FALSE),
#'                                    method = "WLE"))
#' xt <- difLogistic(Data, group, focal.name = 1, match = theta)
#' plotDIFLogistic(xt, item = 1, Data = Data, group = group,
#'                 match = theta, num.groups = 5)
#' @seealso [difR::difLogistic()], [ggplot2::ggplot()]
#'
#' @importFrom ggplot2 stat_function scale_colour_manual scale_linetype_manual
#'   guides guide_legend ggtitle scale_size_continuous
#' @importFrom stats quantile median
#'
#' @export
plotDIFLogistic <- function(x, item = 1, item.name, group.names = c("Reference", "Focal"),
                            Data, group, match, draw.empirical = TRUE,
                            num.groups = NULL, match.name, xlim = NULL,
                            bin.type = c("equal-dist", "equal-freq")) {
  bin.type <- match.arg(bin.type)
  res <- x
  i <- ifelse(is.character(item) | is.factor(item),
    (1:length(res$names))[res$names == item],
    item
  )
  if (missing(item.name)) {
    if (is.character(item) | is.factor(item)) {
      item.name <- paste(item)
    } else {
      item.name <- paste("Item", item)
    }
  }

  if (any(is.na(res$logitPar[i, ]))) {
    stop("Selected item is an anchor item!",
      call. = FALSE
    )
  }
  coef <- res$logitPar[i, ]

  if (missing(Data) & draw.empirical) {
    stop("'Data' needs to be specified! ", call. = FALSE)
  }
  if (missing(group) & draw.empirical) {
    stop("'group' needs to be specified! ", call. = FALSE)
  }

  if (missing(match)) {
    match <- res$match
  }

  if (res$purification & res$DIFitems[1] != "No DIF item detected") {
    ANCHOR <- c(1:nrow(res$logitPar))[-res$DIFitems]
  } else {
    ANCHOR <- c(1:nrow(res$logitPar))
  }

  # Resolve matching criterion ------------------------------------------------
  match_is_integer_score <- FALSE
  if (length(match) == 1 && match[1] == "score") {
    xlab <- "Total score"
    if (draw.empirical) {
      MATCHCRIT <- rowSums(Data[, ANCHOR])
    } else {
      MATCHCRIT <- c(0, nrow(res$logitPar))
    }
    match_is_integer_score <- TRUE
  } else if (length(match) == 1 && match[1] == "zscore") {
    xlab <- "Standardized total score"
    if (draw.empirical) {
      MATCHCRIT <- as.numeric(scale(apply(as.data.frame(Data[, ANCHOR]), 1, sum)))
    } else {
      MATCHCRIT <- c(0, nrow(res$logitPar))
    }
  } else if (length(match) != nrow(Data)) {
    stop("'match' needs to be either 'score', 'zscore' or numeric vector of the same length as number of observations in 'Data'. ",
         call. = FALSE)
  } else {
    MATCHCRIT <- as.numeric(match)
    xlab <- "Matching criterion"
  }
  if (!missing(match.name) && !is.null(match.name)) {
    xlab <- match.name
  }

  LR_plot <- function(x, group, b0, b1, b2, b3) {
    return(1 / (1 + exp(-(b0 + b1 * x + b2 * group + b3 * x * group))))
  }

  if (draw.empirical) {
    # Resolve the number of bins. NULL means "preserve legacy behaviour for
    # integer total scores", otherwise default to 3 equal-frequency bins.
    use_quantile_bins <- !match_is_integer_score
    if (is.null(num.groups)) {
      n_bins <- if (match_is_integer_score) NA_integer_ else 3L
    } else {
      n_bins <- as.integer(num.groups)
      if (is.na(n_bins) || n_bins < 2L) {
        stop("'num.groups' must be an integer >= 2.", call. = FALSE)
      }
      use_quantile_bins <- TRUE
    }

    bin_props <- function(match_g, y_g) {
      if (length(match_g) == 0L) {
        return(data.frame(Score = numeric(0), Probability = numeric(0), Count = integer(0)))
      }
      if (!use_quantile_bins) {
        lv <- as.factor(match_g)
        data.frame(
          Score = as.numeric(levels(lv)),
          Probability = as.numeric(tapply(y_g, lv, mean, na.rm = TRUE)),
          Count = as.integer(table(lv))
        )
      } else {
        rng <- range(match_g, na.rm = TRUE)
        if (rng[1] == rng[2]) {
          # all values equal -> single bin
          return(data.frame(
            Score = rng[1],
            Probability = mean(y_g, na.rm = TRUE),
            Count = length(match_g)
          ))
        }
        brks <- if (bin.type == "equal-dist") {
          # Equal-width bins across the matching range (use the COMBINED
          # range so reference and focal share the same x-axis breaks)
          rng_all <- range(MATCHCRIT, na.rm = TRUE)
          seq(rng_all[1], rng_all[2], length.out = n_bins + 1L)
        } else {
          unique(stats::quantile(match_g,
                                 probs = seq(0, 1, length.out = n_bins + 1L),
                                 na.rm = TRUE, type = 7))
        }
        # Tie-safe: cut needs at least one interval
        if (length(brks) < 3L) {
          return(data.frame(
            Score = mean(match_g, na.rm = TRUE),
            Probability = mean(y_g, na.rm = TRUE),
            Count = length(match_g)
          ))
        }
        bins <- cut(match_g, breaks = brks, include.lowest = TRUE)
        tbl <- table(bins)
        # Drop empty bins so they don't render as ghost points
        keep <- tbl > 0
        data.frame(
          Score = as.numeric(tapply(match_g, bins, mean, na.rm = TRUE))[keep],
          Probability = as.numeric(tapply(y_g, bins, mean, na.rm = TRUE))[keep],
          Count = as.integer(tbl)[keep]
        )
      }
    }

    score_R <- MATCHCRIT[group == 0]
    score_F <- MATCHCRIT[group == 1]
    item_y <- Data[, i]
    emp_R <- bin_props(score_R, item_y[group == 0])
    emp_F <- bin_props(score_F, item_y[group == 1])
    empirical <- rbind(
      cbind(emp_R, Group = "gr1"),
      cbind(emp_F, Group = "gr2")
    )
  }

  if (!is.null(xlim) && length(xlim) == 2L && all(is.finite(xlim))) {
    min_score <- xlim[1]; max_score <- xlim[2]
  } else {
    max_score <- max(MATCHCRIT, na.rm = TRUE) + 0.1
    min_score <- min(MATCHCRIT, na.rm = TRUE) - 0.1
  }

  col <- c("dodgerblue2", "goldenrod2")
  alpha <- .5
  shape <- 21
  size <- .8
  linetype <- c("solid", "dashed")

  g <- ggplot() +
    ### lines
    xlim(min_score, max_score) +
    stat_function(aes(colour = "gr1", linetype = "gr1"),
      fun = LR_plot,
      args = list(
        group = 0,
        b0 = coef[1],
        b1 = coef[2],
        b2 = coef[3],
        b3 = coef[4]
      ),
      linewidth = size, geom = "line"
    ) +
    stat_function(aes(colour = "gr2", linetype = "gr2"),
      fun = LR_plot,
      args = list(
        group = 1,
        b0 = coef[1],
        b1 = coef[2],
        b2 = coef[3],
        b3 = coef[4]
      ),
      linewidth = size, geom = "line"
    ) +
    ### style
    scale_colour_manual(
      values = col,
      breaks = c("gr1", "gr2"),
      labels = group.names
    ) +
    scale_linetype_manual(
      values = linetype,
      breaks = c("gr1", "gr2"),
      labels = group.names
    ) +
    guides(colour = guide_legend(title = "Group", order = 2)) +
    guides(linetype = guide_legend(title = "Group", order = 2)) +
    ### theme
    xlab(xlab) +
    ylab("Probability of correct answer") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_app() +
    theme(
      legend.box.just = "top",
      legend.position = c(0.01, 0.98),
      legend.justification = c(0, 1),
      legend.key.width = unit(1, "cm"),
      legend.box = "horizontal"
    ) +
    ggtitle(item.name)

  if (draw.empirical && nrow(empirical) > 0L) {
    g <- g +
      ### points
      geom_point(
        data = empirical,
        aes(x = .data$Score, y = .data$Probability, colour = .data$Group, fill = .data$Group, size = .data$Count),
        alpha = alpha, shape = shape
      ) +
      scale_size_continuous(breaks = .size_legend_breaks(empirical$Count)) +
      guides(size = guide_legend(title = "Count", order = 1)) +
      scale_fill_manual(
        values = col,
        breaks = c("gr1", "gr2"),
        labels = group.names
      ) +
      guides(fill = guide_legend(title = "Group", order = 2))
  }

  return(g)
}
