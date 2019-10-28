library(snow)
library(foreach)
library(doParallel)
library(doSNOW)
library(tvcure)
library(smcure)
data(e1684)

################################################################################
# Run test models
################################################################################

### Run models
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
# Binary, full model
pd <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph", parallel = T)
Xmat <- tvsimx(pd, "TRT", c(0:1))
Zmat <- tvsimz(pd, "TRT", c(0:1))
# Binary, partial model
pd2 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX, cureform = ~ TRT + AGE, data = e1684, model = "ph", parallel = T)
Xmat2 <- tvsimx(pd2, "TRT", c(0:1))
Zmat2 <- tvsimz(pd2, "TRT", c(0:1))
# Continuous, full model
pd3 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph", parallel = T)
Xmat3 <- tvsimx(pd3, "AGE", seq(-30, 30, 10))
Zmat3 <- tvsimz(pd3, "AGE", seq(-30, 30, 10))
# Continuous, partial model
pd4 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX, cureform = ~ TRT + AGE, data = e1684, model = "ph", parallel = T)
Xmat4 <- tvsimx(pd4, "AGE", seq(-30, 30, 10))
Zmat4 <- tvsimz(pd4, "AGE", seq(-30, 30, 10))

### Predictions
# Binary variable, full model
testpreda <- predict.tvcure(pd, Xmat, Zmat, CI = F)
testpredb <- predict.tvcure(pd, Xmat, Zmat, CI = T, nsims = 100)
# Binary variable, partial model
testpred2a <- predict.tvcure(pd2, Xmat2, Zmat2, CI = F)
testpred2a <- predict.tvcure(pd2, Xmat2, Zmat2, CI = T, nsims = 100)
# Continuous variable, full model
testpred3a <- predict.tvcure(pd3, Xmat3, Zmat3, CI = F)
testpred3b <- predict.tvcure(pd3, Xmat3, Zmat3, CI = T, nsims = 100)
# Continuous variable, partial model, no cis
testpred4a <- predict.tvcure(pd4, Xmat4, Zmat4, CI = F)
testpred4b <- predict.tvcure(pd4, Xmat4, Zmat4, CI = T, nsims = 100)

testpreda <- prediction2(pd, "TRT", c(0, 1), "basesurv", CI = T)
testtestpreda <- prediction2(pd, "TRT", c(0, 1), "spop", CI = T)

testpreda <- prediction2(pd, "TRT", c(0, 1), "basesurv", CI = T)
testtestpreda <- prediction2(pd, "TRT", c(0, 1), "spop", CI = T)


cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
a <- makeCluster(4, "SOCK"); registerDoSNOW(a)

system.time(testpreda <- prediction2(pd, "TRT", c(0, 1), "suncure", CI = T, nsims = 10000))
stopCluster(cl)
system.time(testpreda <- prediction2(pd, "TRT", c(0, 1), "suncure", CI = T, nsims = 10000))


system.time(testpreda <- prediction2(pd, "TRT", c(0, 1), "suncure", CI = T, nsims = 100))


testpreda <- prediction2(pd, "TRT", c(0, 1), "suncure")
testpreda <- prediction2(pd, "TRT", c(0, 1), "spop", bw = T)
testpreda <- prediction2(pd, "TRT", c(0, 1), "spop", CI = T)

### Test population predictions

testpreda <- prediction2(pd, "TRT", c(0, 1), "suncure", CI = T, nsims = 100)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
testpreda <- prediction2(pd, "TRT", c(0, 1), "suncure", CI = T, nsims = 100)

