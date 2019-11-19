# testing file - 20191118

library(snow)
library(foreach)
library(doParallel)
library(doSNOW)
library(tvcure)
library(smcure)
library(ggpubr)
data(e1684)

################################################################################
# Run Models
################################################################################
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)

# Binary, full model
pd <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,
             cureform = ~ TRT + SEX + AGE,
             data = e1684, parallel = T, nboot = 10)

# Binary, partial model
pd2 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX,
              cureform = ~ TRT + AGE,
              data = e1684, parallel = T, nboot = 10)

# Continuous, full model
pd3 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,
              cureform = ~ TRT + SEX + AGE,
              data = e1684, parallel = T, nboot = 10)

# Continuous, partial model
pd4 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX,
              cureform = ~ TRT + AGE,
              data = e1684, parallel = T, nboot = 10)

################################################################################
# Test color plots
################################################################################

  ################################################################################
  # Predictions with CIs
  ################################################################################
  # Binary variable, full model
  testpred1a <- prediction2(pd, "TRT", c(0:1), "basesurv", CI = T, nsims = 100); testpred1a
  testpred1b <- prediction2(pd, "TRT", c(0:1), "suncure", CI = T, nsims = 100); testpred1b
  testpred1c <- prediction2(pd, "TRT", c(0:1), "spop", CI = T, nsims = 100); testpred1c
  testpred1d <- prediction2(pd, "TRT", c(0:1), "uncureprob", CI = T, nsims = 100); testpred1d

  # Binary variable, partial model
  testpred2a <- prediction2(pd2, "TRT", c(0:1), "basesurv", CI = T, nsims = 100); testpred2a
  testpred2b <- prediction2(pd2, "TRT", c(0:1), "suncure", CI = T, nsims = 100); testpred2b
  testpred2c <- prediction2(pd2, "TRT", c(0:1), "spop", CI = T, nsims = 100); testpred2c
  testpred2d <- prediction2(pd2, "TRT", c(0:1), "uncureprob", CI = T, nsims = 100); testpred2d

  # Continuous variable, full model
  testpred3a <- prediction2(pd3, "AGE", seq(-30, 30, 10), "basesurv", CI = T, nsims = 100); testpred3a
  testpred3b <- prediction2(pd3, "AGE", seq(-30, 30, 10), "suncure", CI = T, nsims = 100); testpred3b
  testpred3c <- prediction2(pd3, "AGE", seq(-30, 30, 10), "spop", CI = T, nsims = 100); testpred3c
  testpred3d <- prediction2(pd3, "AGE", seq(-30, 30, 10), "uncureprob", CI = T, nsims = 100); testpred3d

  # Continuous variable, partial model
  testpred4a <- prediction2(pd4, "AGE", seq(-30, 30, 10), "basesurv", CI = T, nsims = 100); testpred4a
  testpred4b <- prediction2(pd4, "AGE", seq(-30, 30, 10), "suncure", CI = T, nsims = 100); testpred4b
  testpred4c <- prediction2(pd4, "AGE", seq(-30, 30, 10), "spop", CI = T, nsims = 100); testpred4c
  testpred4d <- prediction2(pd4, "AGE", seq(-30, 30, 10), "uncureprob", CI = T, nsims = 100); testpred4d

  ################################################################################
  # Predictions without CIs
  ################################################################################
  # Binary variable, full model
  testpred1a <- prediction2(pd, "TRT", c(0:1), "basesurv", CI = F, nsims = 100); testpred1a
  testpred1b <- prediction2(pd, "TRT", c(0:1), "suncure", CI = F, nsims = 100); testpred1b
  testpred1c <- prediction2(pd, "TRT", c(0:1), "spop", CI = F, nsims = 100); testpred1c
  testpred1d <- prediction2(pd, "TRT", c(0:1), "uncureprob", CI = F, nsims = 100); testpred1d

  # Binary variable, partial model
  testpred2a <- prediction2(pd2, "TRT", c(0:1), "basesurv", CI = F, nsims = 100); testpred2a
  testpred2b <- prediction2(pd2, "TRT", c(0:1), "suncure", CI = F, nsims = 100); testpred2b
  testpred2c <- prediction2(pd2, "TRT", c(0:1), "spop", CI = F, nsims = 100); testpred2c
  testpred2d <- prediction2(pd2, "TRT", c(0:1), "uncureprob", CI = F, nsims = 100); testpred2d

  # Continuous variable, full model
  testpred3a <- prediction2(pd3, "AGE", seq(-30, 30, 10), "basesurv", CI = F, nsims = 100); testpred3a
  testpred3b <- prediction2(pd3, "AGE", seq(-30, 30, 10), "suncure", CI = F, nsims = 100); testpred3b
  testpred3c <- prediction2(pd3, "AGE", seq(-30, 30, 10), "spop", CI = F, nsims = 100); testpred3c
  testpred3d <- prediction2(pd3, "AGE", seq(-30, 30, 10), "uncureprob", CI = F, nsims = 100); testpred3d

  # Continuous variable, partial model
  testpred4a <- prediction2(pd4, "AGE", seq(-30, 30, 10), "basesurv", CI = F, nsims = 100); testpred4a
  testpred4b <- prediction2(pd4, "AGE", seq(-30, 30, 10), "suncure", CI = F, nsims = 100); testpred4b
  testpred4c <- prediction2(pd4, "AGE", seq(-30, 30, 10), "spop", CI = F, nsims = 100); testpred4c
  testpred4d <- prediction2(pd4, "AGE", seq(-30, 30, 10), "uncureprob", CI = F, nsims = 100); testpred4d

################################################################################
# Test bw plots
################################################################################

  # Binary variable, full model
  testpred1a <- prediction2(pd, "TRT", c(0:1), "basesurv", CI = T, nsims = 100, bw = T); testpred1a
  testpred1b <- prediction2(pd, "TRT", c(0:1), "suncure", CI = T, nsims = 100, bw = T); testpred1b
  testpred1c <- prediction2(pd, "TRT", c(0:1), "spop", CI = T, nsims = 100, bw = T); testpred1c
  testpred1d <- prediction2(pd, "TRT", c(0:1), "uncureprob", CI = T, nsims = 100, bw = T); testpred1d

  # Binary variable, partial model
  testpred2a <- prediction2(pd2, "TRT", c(0:1), "basesurv", CI = T, nsims = 100, bw = T); testpred2a
  testpred2b <- prediction2(pd2, "TRT", c(0:1), "suncure", CI = T, nsims = 100, bw = T); testpred2b
  testpred2c <- prediction2(pd2, "TRT", c(0:1), "spop", CI = T, nsims = 100, bw = T); testpred2c
  testpred2d <- prediction2(pd2, "TRT", c(0:1), "uncureprob", CI = T, nsims = 100, bw = T); testpred2d

  # Continuous variable, full model
  testpred3a <- prediction2(pd3, "AGE", seq(-30, 30, 10), "basesurv", CI = T, nsims = 100, bw = T); testpred3a
  testpred3b <- prediction2(pd3, "AGE", seq(-30, 30, 10), "suncure", CI = T, nsims = 100, bw = T); testpred3b
  testpred3c <- prediction2(pd3, "AGE", seq(-30, 30, 10), "spop", CI = T, nsims = 100, bw = T); testpred3c
  testpred3d <- prediction2(pd3, "AGE", seq(-30, 30, 10), "uncureprob", CI = T, nsims = 100, bw = T); testpred3d

  # Continuous variable, partial model
  testpred4a <- prediction2(pd4, "AGE", seq(-30, 30, 10), "basesurv", CI = T, nsims = 100, bw = T); testpred4a
  testpred4b <- prediction2(pd4, "AGE", seq(-30, 30, 10), "suncure", CI = T, nsims = 100, bw = T); testpred4b
  testpred4c <- prediction2(pd4, "AGE", seq(-30, 30, 10), "spop", CI = T, nsims = 100, bw = T); testpred4c
  testpred4d <- prediction2(pd4, "AGE", seq(-30, 30, 10), "uncureprob", CI = T, nsims = 100, bw = T); testpred4d

  ################################################################################
  # Predictions without CIs
  ################################################################################
  # Binary variable, full model
  testpred1a <- prediction2(pd, "TRT", c(0:1), "basesurv", CI = F, nsims = 100, bw = T); testpred1a
  testpred1b <- prediction2(pd, "TRT", c(0:1), "suncure", CI = F, nsims = 100, bw = T); testpred1b
  testpred1c <- prediction2(pd, "TRT", c(0:1), "spop", CI = F, nsims = 100, bw = T); testpred1c
  testpred1d <- prediction2(pd, "TRT", c(0:1), "uncureprob", CI = F, nsims = 100, bw = T); testpred1d

  # Binary variable, partial model
  testpred2a <- prediction2(pd2, "TRT", c(0:1), "basesurv", CI = F, nsims = 100, bw = T); testpred2a
  testpred2b <- prediction2(pd2, "TRT", c(0:1), "suncure", CI = F, nsims = 100, bw = T); testpred2b
  testpred2c <- prediction2(pd2, "TRT", c(0:1), "spop", CI = F, nsims = 100, bw = T); testpred2c
  testpred2d <- prediction2(pd2, "TRT", c(0:1), "uncureprob", CI = F, nsims = 100, bw = T); testpred2d

  # Continuous variable, full model
  testpred3a <- prediction2(pd3, "AGE", seq(-30, 30, 10), "basesurv", CI = F, nsims = 100, bw = T); testpred3a
  testpred3b <- prediction2(pd3, "AGE", seq(-30, 30, 10), "suncure", CI = F, nsims = 100, bw = T); testpred3b
  testpred3c <- prediction2(pd3, "AGE", seq(-30, 30, 10), "spop", CI = F, nsims = 100, bw = T); testpred3c
  testpred3d <- prediction2(pd3, "AGE", seq(-30, 30, 10), "uncureprob", CI = F, nsims = 100, bw = T); testpred3d

  # Continuous variable, partial model
  testpred4a <- prediction2(pd4, "AGE", seq(-30, 30, 10), "basesurv", CI = F, nsims = 100, bw = T); testpred4a
  testpred4b <- prediction2(pd4, "AGE", seq(-30, 30, 10), "suncure", CI = F, nsims = 100, bw = T); testpred4b
  testpred4c <- prediction2(pd4, "AGE", seq(-30, 30, 10), "spop", CI = F, nsims = 100, bw = T); testpred4c
  testpred4d <- prediction2(pd4, "AGE", seq(-30, 30, 10), "uncureprob", CI = F, nsims = 100, bw = T); testpred4d

################################################################################
# Combine plots
################################################################################
testpred1b <- prediction2(pd, "TRT", c(0:1), "suncure", CI = T, nsims = 100, ylab = ""); testpred1b
testpred1c <- prediction2(pd, "TRT", c(0:1), "spop", CI = T, nsims = 100, ylab = ""); testpred1c

splotc <- ggarrange(testpred1b, testpred1c, common.legend = T, legend = "bottom")
splotc <- annotate_figure(splotc, left = text_grob("Predicted Survival Probability", rot = 90));splotc

################################################################################
# Test internals output
################################################################################
testpred1a <- prediction2(pd, "TRT", c(0:1), "basesurv", CI = T, nsims = 100, internals = T); testpred1a
testpred1a <- prediction2(pd, "TRT", c(0:1), "basesurv", CI = F, nsims = 100, internals = T); testpred1a

