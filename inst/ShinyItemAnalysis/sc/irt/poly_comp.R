library(mirt)
library(ShinyItemAnalysis)

# loading data
data(Anxiety, package = "ShinyItemAnalysis")
Data <- Anxiety[, paste0("R", 1:29)]

# fitting polytomous IRT models
fitRSM <- mirt(Data, model = 1, itemtype = "rsm")
fitPCM <- mirt(Data, model = 1, itemtype = "Rasch")
fitGPCM <- mirt(Data, model = 1, itemtype = "gpcm")
fitGRM <- mirt(Data, model = 1, itemtype = "graded")

# information criteria for all models
anova(fitRSM, fitPCM, fitGPCM, fitGRM)

# likelihood ratio tests for nested models
anova(fitRSM, fitPCM) # RSM vs PCM
anova(fitPCM, fitGPCM) # PCM vs GPCM
