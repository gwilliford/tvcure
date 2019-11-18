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

# Binary variable, full model, no CIs
pd <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph", parallel = T)
Xmat <- tvsimx(pd, "TRT", c(0:1))
Zmat <- tvsimz(pd, "TRT", c(0:1))
testpred <- predict.tvcure(pd, Xmat, Zmat, CI = F)

# Binary variable, full model, with CIs
testpred4 <- predict.tvcure(pd, Xmat, Zmat, CI = T, nsims = 100)

# Binary variable, partial model, no

# Continuous variable, partial model
pd2 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX, cureform = ~ TRT + AGE, data = e1684, model = "ph", parallel = T)
Xmat2 <- tvXsim(pd, "AGE", seq(-30, 30, 10))
Zmat2 <- tvZsim(pd, "AGE", seq(-30, 30, 10))
# pd3 <- tvcure(Surv(FAILTIME, FAILCENS) ~ SEX, cureform = ~ TRT, data = e1684, model = "ph", parallel = T)

################################################################################
# Test prediction functions by plotting them in base R
################################################################################

################################################ Binary var without CIs - full model
testpred$uncureprob[, 1]
testpred$uncureprob[, 2]

plot(testpred$s0, type = "l")

plot(testpred$suncure[, 1], type = "l")
lines(testpred$suncure[, 2], type = "l", col = 2)

plot(testpred$spop[, 1], type = "l")
lines(testpred$spop[, 2], type = "l", col = 2)

################################################ Binary var with CIs - full model
with(testpred4, plot(testpred4$uncuremean, ylim = c(round(min(uncurelo), 2), round(max(uncurehi), 2)))))
points(testpred4$uncurehi)
points(testpred4$uncurelo)

plot(testpred4$s0mean, type = "l")
lines(testpred4$s0lo, type = "l", lty = 2)
lines(testpred4$s0hi, type = "l", lty = 2)
lines(testpred$s0, type = "l", lty = 3, col = 2)

plot(testpred4$suncuremean[, 1], type = "l")
lines(testpred4$suncurelo[, 1], type = "l", lty = 2)
lines(testpred4$suncurehi[, 1], type = "l", lty = 2)
lines(testpred4$suncuremean[, 2], type = "l", col = 2)
lines(testpred4$suncurelo[, 2], type = "l", lty = 2, col = 2)
lines(testpred4$suncurehi[, 2], type = "l", lty = 2, col = 2)
lines(testpred$suncure[, 1], type = "l", col = 3)
lines(testpred$suncure[, 2], type = "l", col = 3)

plot(testpred4$spopmean[, 1], type = "l")
lines(testpred4$spoplo[, 1], type = "l", lty = 2, col = 1))
lines(testpred4$spophi[, 1], type = "l", lty = 2, col = 1)
lines(testpred4$spopmean[, 2], type = "l", col = 2)
lines(testpred4$spoplo[, 2], type = "l", lty = 2, col = 2)
lines(testpred4$spophi[, 2], type = "l", lty = 2, col = 2)
lines(testpred$spop[, 1], type = "l", col = 3)
lines(testpred$spop[, 2], type = "l", col = 3)

############################################### Continuous var wihout CIs
testpred2 <- predict.tvcure(pd2, Xmat2, Zmat2, CI = F)
plot(testpred2, "basesurv")
plot(testpred2, "spop")
plot(testpred2, "suncure")

Xmat2 <- tvsimx(pd2, "AGE", seq(-30, 30, 10))
Zmat2 <- tvsimz(pd2, "AGE", seq(-30, 30, 10))

# TODO
a <- spline(testpred4$s0mean)
a <- loess(testpred4$s0mean ~ .)
a <- smooth.spline(testpred4$s0mean)

plot(testpred$s0, type = "l", lty = 3)
lines(testpred4$spopmean[, 1], type = "l", col = 2)
lines(a, type = "l", col = 3)


Test unfull models for both CI and no CI and binary and cts
