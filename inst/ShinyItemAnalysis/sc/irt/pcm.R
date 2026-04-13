library(mirt)
library(ShinyItemAnalysis)

# loading data
data(CZmaturaS, package = "ShinyItemAnalysis")
Data <- CZmaturaS[, grep("^b", names(CZmaturaS))]

# fitting Partial Credit Model (PCM)
# mirt fits PCM when itemtype = "Rasch" is applied to polytomous data
fit <- mirt(Data, model = 1, itemtype = "Rasch", SE = TRUE)

# item characteristic curves (category probability curves)
plot(fit, type = "trace", facet_items = FALSE)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters
coef(fit, simplify = TRUE)
coef(fit, printSE = TRUE)

# item fit statistics
itemfit(fit)

# infit and outfit statistics
itemfit(fit, fit_stats = "infit")

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(Data)))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# Wright map
b <- coef(fit, simplify = TRUE)$items
ggWrightMap(fs, b[, grep("^d", colnames(b))])
