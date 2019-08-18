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
pd <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph", parallel = T, nboot = 10)
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
testpreda <- prediction2(pd, "TRT", c(0:1), "basesurv")
testpreda <- prediction2(pd, "TRT", c(0:1), "suncure")
testpreda <- prediction2(pd, "TRT", c(0:1), "spop")
pdf("plots.pdf")
testpreda <- prediction2(pd, "TRT", c(0:1), "uncureprob")
dev.off()

testpredb <- prediction2(pd, "TRT", c(0:1), "basesurv", CI = T, nsims = 100)
testpredc <- prediction2(pd, "TRT", c(0:1), "suncure", CI = T, nsims = 100)
testpredb <- prediction2(pd, "TRT", c(0:1), "spop", CI = T, nsims = 100)
testpredb <- prediction2(pd, "TRT", c(0:1), "uncureprob", CI = T, nsims = 100)

# Binary variable, partial model
testpred2a <- prediction2(pd2, Xmat2, Zmat2, CI =  F)
testpred2a <- prediction2(pd2, Xmat2, Zmat2, CI =  T, nsims = 100)
# Continuous variable, full model
testpredb <- prediction2(pd3, Xmat3, Zmat3, CI =  F)
testpredb <- prediction2(pd3, "AGE", seq(-30, 30, 10), CI =  T, nsims = 100)
# Continuous variable, partial model, no cis
testpred4a <- prediction2(pd4, Xmat4, Zmat4, CI =  F)
testpred4b <- prediction2(pd4, Xmat4, Zmat4, CI =  T, nsims = 100)


testpredc <- prediction2(pd, "TRT", c(0:1), type = "suncure")
CI = T, nsims = 100)


testpredc <- prediction2(pd, "TRT", c(0:1), type = "uncureprob", bw = T)
testpredc <- prediction2(pd, "TRT", c(0:1), type = "uncureprob", CI = T, nsims = 100)

testpredc <- prediction2(pd, "TRT", c(0:1), type = "uncureprob")
testpredc <- prediction2(pd, "TRT", c(0:1), type = "uncureprob", bw = T)
testpredc <- prediction2(pd, "TRT", c(0:1), type = "uncureprob", CI = T, nsims = 100)
testpredc <- prediction2(pd, "TRT", c(0:1), type = "uncureprob", CI = T, nsims = 100, bw = T)

testpredc <- prediction2(pd, "TRT", c(0:1), type = "suncure", CI = T, nsims = 100)


# Error testing
testpredc <- prediction2(pd, "TRT", c(0:1), type = "suncure", CI = T, nsims = 100)


# Test combined plotting functions (CIs + NO CIs)
testpredc <- prediction2(pd, "TRT", c(0:1), type = "basesurv")
testpredc <- prediction2(pd, "TRT", c(0:1), type = "basesurv", bw = T)
testpredc <- prediction2(pd, "TRT", c(0:1), type = "basesurv", CI = T, nsims = 100)
testpredc <- prediction2(pd, "TRT", c(0:1), type = "basesurv", CI = T, nsims = 100, bw = T)

testpredc <- prediction2(pd, "TRT", c(0:1), type = "suncure")
testpredc <- prediction2(pd, "TRT", c(0:1), type = "suncure", bw = T)
testpredc <- prediction2(pd, "TRT", c(0:1), type = "suncure", CI = T, nsims = 100)
testpredc <- prediction2(pd, "TRT", c(0:1), type = "suncure", CI = T, nsims = 100, bw = T)
