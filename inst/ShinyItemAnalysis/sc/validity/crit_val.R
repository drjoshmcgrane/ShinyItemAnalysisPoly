library(ggplot2)
library(mirt)
library(polycor)
library(psych)
library(ShinyItemAnalysisPoly)

# loading data
data(GMAT, package = "difNLR")
data <- GMAT[, 1:20]
criterion <- GMAT[, "criterion"]

# ---- Predictor (choose one) ---------------------------------------------
score  <- rowSums(data)                              # total score
zscore <- as.numeric(scale(score))                   # standardized total score
theta  <- as.numeric(mirt::fscores(                  # IRT theta (Rasch here)
  mirt::mirt(data, 1, "Rasch", verbose = FALSE),
  method = "WLE"
))

predictor       <- score
predictor_label <- "Total score"

# ---- Scale type decides the coefficient ---------------------------------
# binary     : 2 distinct values
# ordinal    : a few whole-numbered levels
# continuous : anything else
scale_type <- function(v) {
  u <- unique(v[!is.na(v)])
  if (length(u) < 2) "degenerate"
  else if (length(u) == 2) "binary"
  else if (all(abs(u - round(u)) < 1e-8) && length(u) <= 8) "ordinal"
  else "continuous"
}
scale_type(predictor)
scale_type(criterion)

# ---- Descriptive plot, matched to the criterion -------------------------
df <- data.frame(predictor, criterion)

# continuous criterion: scatterplot with a linear fit
ggplot(df, aes(x = predictor, y = criterion)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, se = FALSE, colour = "red") +
  xlab(predictor_label) + ylab("Criterion variable") +
  theme_app()

# ordinal criterion: boxplot across criterion levels
ggplot(df, aes(y = predictor, x = as.factor(criterion), fill = as.factor(criterion))) +
  geom_boxplot() +
  geom_jitter(shape = 16, position = position_jitter(0.2)) +
  scale_fill_brewer(palette = "Blues") +
  xlab("Criterion group") + ylab(predictor_label) +
  coord_flip() + theme_app()

# binary criterion: fitted probability of the higher category
ggplot(df, aes(x = predictor, y = as.numeric(criterion == max(criterion)))) +
  geom_jitter(height = 0.03, width = 0, shape = 16, alpha = 0.4) +
  geom_smooth(method = "glm", formula = y ~ x,
              method.args = list(family = "binomial"),
              colour = "red", fill = "red", alpha = 0.15) +
  xlab(predictor_label) + ylab("P(criterion = 1)") +
  theme_app()

# ---- Correlation, matched to the scale types ----------------------------
# continuous predictor x continuous criterion
cor.test(predictor, criterion, method = "pearson", exact = FALSE)

# continuous predictor x ordinal criterion (SE and test from polycor)
polycor::polyserial(predictor, round(criterion), std.err = TRUE)

# categorical predictor x categorical criterion (tetrachoric if both binary)
polycor::polychor(round(predictor / 5), round(criterion), std.err = TRUE)

# continuous predictor x binary criterion: biserial corrects the attenuation
# that Pearson's r suffers on a dichotomy. r_b = 0 exactly when r_pb = 0, so
# the point-biserial test is a valid test of the same null.
crit_bin <- as.numeric(criterion > median(criterion))
psych::biserial(predictor, crit_bin)
cor.test(predictor, crit_bin, method = "pearson", exact = FALSE)

# rank-based alternatives, robust to non-normality and monotone non-linearity
cor.test(predictor, criterion, method = "spearman", exact = FALSE)
cor.test(predictor, criterion, method = "kendall",  exact = FALSE)
