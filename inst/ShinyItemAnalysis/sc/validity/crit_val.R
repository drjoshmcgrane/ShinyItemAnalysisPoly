library(ggplot2)
library(mirt)
library(psych)
library(ShinyItemAnalysisPoly)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
criterion <- GMAT[, "criterion"]   # criterion variable

# ---- Predictor (choose one) ---------------------------------------------
score  <- rowSums(data)                              # total score
zscore <- as.numeric(scale(score))                   # standardized total score
theta  <- as.numeric(mirt::fscores(                  # IRT theta (Rasch in this example)
  mirt::mirt(data, 1, "Rasch", verbose = FALSE),
  method = "WLE"
))

predictor       <- score          # <- pick total / zscore / theta here
predictor_label <- "Total score"  # <- update axis label accordingly

# ---- Descriptive plots --------------------------------------------------
criterionD <- round(criterion)    # treat as discrete for boxplot

# boxplot for a discrete criterion
df_box <- data.frame(predictor, criterion = as.factor(criterionD))
ggplot(df_box, aes(y = predictor, x = criterion, fill = criterion)) +
  geom_boxplot() +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  scale_fill_brewer(palette = "Blues") +
  xlab("Criterion group") +
  ylab(predictor_label) +
  coord_flip() +
  theme_app()

# scatterplot for a continuous criterion
df_sc <- data.frame(predictor, criterion)
ggplot(df_sc, aes(x = predictor, y = criterion)) +
  geom_point() +
  xlab(predictor_label) +
  ylab("Criterion variable") +
  geom_smooth(method = lm, se = FALSE, color = "red") +
  theme_app()

# ---- Correlation between predictor and criterion ------------------------
# Pearson (interval-by-interval)
cor.test(predictor, criterion, method = "pearson",  exact = FALSE)
# Spearman (rank, ordinal-friendly)
cor.test(predictor, criterion, method = "spearman", exact = FALSE)
# Kendall (rank alternative)
cor.test(predictor, criterion, method = "kendall",  exact = FALSE)
# Polychoric (both ordinal, ≤ ~12 categories each)
# psych::polychoric(cbind(predictor, criterion))$rho
# Polyserial (continuous predictor, ordinal criterion)
# psych::polyserial(predictor, criterion)
# Biserial (continuous predictor, binary criterion; assumes underlying normal)
# psych::biserial(predictor, criterion)
# Point-biserial (continuous predictor, binary criterion; no normality assumption)
# cor.test(predictor, criterion, method = "pearson")
