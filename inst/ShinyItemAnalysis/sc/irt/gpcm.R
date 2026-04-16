library(mirt)
library(ShinyItemAnalysis)

# loading data
data(Anxiety, package = "ShinyItemAnalysis")
Data <- Anxiety[, paste0("R", 1:29)]

# fitting Generalized Partial Credit Model (GPCM)
fit <- mirt(Data, model = 1, itemtype = "gpcm", SE = TRUE)

# item characteristic curves (category probability curves)
plot(fit, type = "trace", facet_items = FALSE)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters - IRT parametrization
coef(fit, IRTpars = TRUE, simplify = TRUE)
coef(fit, IRTpars = TRUE, printSE = TRUE)

# item fit statistics
itemfit(fit)

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(Data)))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# Wright map
pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
b <- pars[, grep("^b\\d", colnames(pars)), drop = FALSE]
ggWrightMap(fs, b)
