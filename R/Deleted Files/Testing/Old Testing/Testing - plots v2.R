library(snow)
library(foreach)
library(doParallel)
library(doSNOW)
library(tvcure)
library(smcure)
data(e1684)
source("./R/Testing/Testing - predict v2.R")

################################################################################
# Run test models
################################################################################

##### Testing plots binary, full model, without CIs
plot(testpreda, type = "uncureprob")
plot(testpreda, type = "basesurv")
plot(testpreda, type = "suncure")
plot(testpreda, type = "spop")

a <- plot(testpred, type = "basesurv")
b <- plot(testpred, type = "suncure")
c <- plot(testpred, type = "spop")

##### Testing plots continuous variables with CIs
plot(testpred2, type = "basesurv")
plot(testpred2, type = "suncure")
plot(testpred2, type = "spop")

plot(testpred4, type = "basesurv")
