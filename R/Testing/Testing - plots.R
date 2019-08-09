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

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
pd <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph", parallel = T)
Xmat <- tvsimx(pd, "TRT", c(0:1))
Zmat <- tvsimz(pd, "TRT", c(0:1))
testpred <- predict.tvcure(pd, Xmat, Zmat, CI = F)
testpred4 <- predict.tvcure(pd, Xmat, Zmat, CI =  T, nsims = 100)

pd2 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX, cureform = ~ TRT + AGE, data = e1684, model = "ph", parallel = T)
Xmat2 <- tvsimx(pd, "AGE", seq(-30, 30, 10))
Zmat2 <- tvsimz(pd, "AGE", seq(-30, 30, 10))
testpred2 <- predict.tvcure(pd2, Xmat2, Zmat2, CI = F)

##### Testing plots binary variables without CIs
plot(testpred, type = "basesurv")
plot(testpred, type = "suncure")
plot(testpred, type = "spop")

a <- plot(testpred, type = "basesurv")
b <- plot(testpred, type = "suncure")
c <- plot(testpred, type = "spop")

##### Testing plots continuous variables with CIs
plot(testpred2, type = "basesurv")
plot(testpred2, type = "suncure")
plot(testpred2, type = "spop")

plot(testpred4, type = "basesurv")
