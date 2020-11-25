################################################################################
# Setup
################################################################################
setwd("C:/Users/gwill/Dropbox/Research/Dissertation/chapter3")
library(tvcure)
library(readstata13)
library(dplyr)
library(doSNOW)
library(compiler)
library(beepr)

options(scipen = 999)

# Parallel processing

peaceterm = read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/ICOWPeaceTerminationData.dta")
peaceterm = rename(peaceterm, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")

set_cox = coxph(Surv(start, stop, event) ~ ltradedep_geomean + lpchcap + caprat +
                  icowsal + riveriss + mariss +
                  recmidwt + recnowt + recyeswt + bdymid +
                  demdy + autdy + contdir + defense + igosum,
                data = peaceterm, x = T); summary(set_cox)
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
set_cure = tvcure(Surv(start, stop, event) ~ lpchcap + recmidwt + recnowt + recyeswt + bdymid,
                  cureform = ~ ltradedep_geomean + caprat +
                    icowsal + riveriss + mariss +
                    demdy + autdy + contdir + defense + igosum,
                  data = peaceterm,
                  link = "probit",
                  nboot = 10,
                  brglm = T)

################################################################################
# Test predictions with fitted values
################################################################################
z1 = tvpred(set_cure, "basesurv")





newX = apply(set_cure$X, 2, median)
newX = rbind(newX, newX)
newX[2, "bdymid"] = 1

newZ = apply(set_cure$Z, 2, median)
newZ = rbind(newZ, newZ)
newZ[2, "demdy"] = 1

newX2 = t(as.matrix(apply(set_cure$X, 2, median)))
newZ2 = t(as.matrix(apply(set_cure$Z, 2, median)))

##### Test basesurv
a1 = tvpred(set_cure, type = "basesurv", CI = F)
a2 = tvpred(set_cure, type = "basesurv", newX = newX, newZ = newZ, CI = F)
a3 = tvpred(set_cure, type = "basesurv", newX = newX, newZ = newZ, CI = T)
a4 = tvpred(set_cure, type = "basesurv", newX = newX2, newZ = newZ2, CI = T)

##### Test suncure
b1 = tvpred(set_cure, type = "suncure", CI = F) # should throw error
b2 = tvpred(set_cure, type = "suncure", newX = newX, newZ = newZ, CI = F)
b3 = tvpred(set_cure, type = "suncure", newX = newX, newZ = newZ, CI = T)
b4 = tvpred(set_cure, type = "suncure", newX = newX2, newZ = newZ2, CI = T)

##### Test spop
d1 = tvpred(set_cure, type = "spop", CI = F) # should throw error
d2 = tvpred(set_cure, type = "spop", newX = newX, newZ = newZ, CI = F)
d3 = tvpred(set_cure, type = "spop", newX = newX, newZ = newZ, CI = T)
d4 = tvpred(set_cure, type = "spop", newX = newX2, newZ = newZ2, CI = T)

##### Test uncureprob
e1 = tvpred(set_cure, type = "uncureprob", CI = F) # should throw error
e2 = tvpred(set_cure, type = "uncureprob", newX = newX, newZ = newZ, CI = F)
e3 = tvpred(set_cure, type = "uncureprob", newX = newX, newZ = newZ, CI = T)
e4 = tvpred(set_cure, type = "uncureprob", newX = newX2, newZ = newZ2, CI = T)

################################################################################
# Test residuals
################################################################################

resid_cs  = tvresid(set_cure, type = "Cox-Snell")
resid_mcs = tvresid(set_cure, type = "M-Cox-Snell")
resid_m   = tvresid(set_cure, type = "Martingale")
resid_mm  = tvresid(set_cure, type = "M-Martingale")

################################################################################
# Test PH survplots
################################################################################

newX = apply(set_cure$X, 2, median)
newX = rbind(newX, newX)
newX[2, "bdymid"] = 1

newZ = apply(set_cure$Z, 2, median)
newZ = rbind(newZ, newZ)
newZ[2, "demdy"] = 1

phtest_plot(set_cure, newX, newZ)

################################################################################
#
################################################################################
cox-snell residuals
