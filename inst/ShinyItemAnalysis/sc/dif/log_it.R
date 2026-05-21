library(difR)
library(ShinyItemAnalysisPoly)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
group <- GMAT[, "group"]

# logistic regression DIF detection method
(fit <- difLogistic(
  Data = data, group = group, focal.name = 1, match = "score",
  type = "both", p.adjust.method = "none", purify = FALSE
))

# plot of characteristic curve for item 1 (3 equal-frequency bins for the
# observed proportions; pass draw.empirical = FALSE to suppress the points)
plotDIFLogistic(fit, item = 1, Data = data, group = group,
                num.groups = 3, match.name = "Total score")

# estimated coefficients for item 1
fit$logitPar[1, ]
