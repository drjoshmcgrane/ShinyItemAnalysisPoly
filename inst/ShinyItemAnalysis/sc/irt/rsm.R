library(mirt)
library(ShinyItemAnalysis)

# loading data
data(Anxiety, package = "ShinyItemAnalysis")
Data <- Anxiety[, paste0("R", 1:29)]

# fitting Rating Scale Model (RSM)
fit <- mirt(Data, model = 1, itemtype = "rsm", SE = TRUE)

# item characteristic curves (category probability curves)
plot(fit, type = "trace", facet_items = FALSE)
# item information curves
plot(fit, type = "infotrace", facet_items = FALSE)
# test information curve
plot(fit, type = "infoSE")

# estimated parameters (IRT parametrization)
pars <- coef(fit, IRTpars = TRUE, simplify = TRUE)$items
b_cols <- grep("^b\\d", colnames(pars))

# item-specific thresholds b_ik = b_k(shared) - c_i (as shown in the app)
shared_b <- pars[1, b_cols]
c_vals   <- pars[, "c"]
thresholds <- t(outer(as.numeric(shared_b), as.numeric(c_vals), "-"))
colnames(thresholds) <- names(shared_b)
rownames(thresholds) <- rownames(pars)
thresholds

# SEs (printSE = TRUE gives SEs for the shared steps + item c values)
coef(fit, IRTpars = TRUE, printSE = TRUE)

# item fit statistics
itemfit(fit)

# infit and outfit statistics (RSM is Rasch-family)
itemfit(fit, fit_stats = "infit")

# factor scores vs standardized total scores
fs <- as.vector(fscores(fit))
sts <- as.vector(scale(rowSums(Data)))
plot(fs ~ sts, xlab = "Standardized total score", ylab = "Factor score")
cor(fs, sts)

# Wright map (uses derived item-specific thresholds)
ggWrightMap(fs, thresholds)
