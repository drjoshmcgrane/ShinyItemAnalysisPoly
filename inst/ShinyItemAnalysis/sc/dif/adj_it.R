library(difNLR)
library(ShinyItemAnalysisPoly)

# loading data
data(dataMedicalgraded, package = "ShinyItemAnalysisPoly")
data <- dataMedicalgraded[, 1:100]
group <- dataMedicalgraded[, 101]

# DIF with adjacent category logit regression model
(fit <- difORD(
  Data = data, group = group, focal.name = 1, model = "adjacent",
  type = "both", match = "zscore", p.adjust.method = "none", purify = FALSE
))

# plot of expected item score curves for item X2003
plotDIFOrdExpected(fit, item = "X2003", num.groups = 3,
                   match.name = "Standardized total score")

# estimated coefficients with SE in IRT parametrization for item X2003
coef(fit, SE = TRUE, IRTpars = TRUE, CI = 0)[["X2003"]]
# estimated coefficients with SE in intercept/slope parametrization for item X2003
coef(fit, SE = TRUE, IRTpars = FALSE, CI = 0)[["X2003"]]
