################################################################################
# Setup
################################################################################
library(tvcure)
library(readstata13)
library(readr)
library(dplyr)
library(doSNOW)
library(compiler)
library(beepr)
options(scipen = 999)
cl <- makeCluster(4, "SOCK")
registerDoSNOW(cl)
cmpfun(tvcure)
enableJIT(3)

################################################################################
# Peace Termination Test Models
################################################################################
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

# ################################################################################
# # Territorial claim onset test models
# ################################################################################
# terrstart = read_csv("C:/Users/gwill/Dropbox/Research/Dissertation/Data Management/outputdata/terrstart.csv")
#
# i0 = tvcure(Surv(start, stop, fail) ~ pchcap + bdymid + systchange + ww1 + ww2 + coldwar,
#                     cureform = ~ caprat + lnccdist + lagterrch + postcolonial + colonycontig +
#                       onemp + twomp + defense + demdy + trival,
#                     data = terrstart,
#                     brglm = F, var = T, nboot = 30)

################################################################################
# Test prediction function
################################################################################
newX = apply(set_cure$X, 2, median)
newX = rbind(newX, newX)
newX[2, "bdymid"] = 1

newZ = apply(set_cure$Z, 2, median)
newZ = rbind(newZ, newZ)
newZ[2, "demdy"] = 1

newX2 = t(as.matrix(apply(set_cure$X, 2, median)))
newZ2 = t(as.matrix(apply(set_cure$Z, 2, median)))

with(peaceterm, plot(survfit(Surv(start, stop, event) ~ 1)))

##### Test basesurv
a1 = prediction4(set_cure, type = "basesurv", variable = "contdir", values = c(0, 1), CI = F)
a2 = prediction4(set_cure, type = "basesurv", variable = "contdir", values = c(0, 1), CI = T)
a3 = prediction4(set_cure, type = "basesurv", variable = "contdir", values = 0, CI = F)
a4 = prediction4(set_cure, type = "basesurv", variable = "contdir", values = 0, CI = T)

##### Test suncure
b1 = prediction4(set_cure, type = "suncure", variable = "bdymid", values = c(0, 1), CI = F)
b2 = prediction4(set_cure, type = "suncure", variable = "bdymid", values = c(0, 1), CI = T)
b3 = prediction4(set_cure, type = "suncure", variable = "bdymid", values = 0, CI = F)
b4 = prediction4(set_cure, type = "suncure", variable = "bdymid", values = 0, CI = T)

##### Test spop
d1 = prediction4(set_cure, type = "spop", variable = "bdymid", values = c(0, 1), CI = F)
d2 = prediction4(set_cure, type = "spop", variable = "bdymid", values = c(0, 1), CI = T)
d3 = prediction4(set_cure, type = "spop", variable = "bdymid", values = 0, CI = F)
d4 = prediction4(set_cure, type = "spop", variable = "bdymid", values = 0, CI = T)

##### Test uncureprob
e1 = prediction4(set_cure, type = "uncureprob", variable = "riveriss", values = c(0, 1), CI = F)
e2 = prediction4(set_cure, type = "uncureprob", variable = "riveriss", values = c(0, 1), CI = T)
e3 = prediction4(set_cure, type = "uncureprob", variable = "riveriss", values = 0, CI = F)
e4 = prediction4(set_cure, type = "uncureprob", variable = "riveriss", values = 0, CI = T)

################################################################################
# Test predictions with single variable in each equation and single row in X/Zmat
################################################################################
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
onevar = tvcure(Surv(start, stop, event) ~ lpchcap,
                cureform = ~ ltradedep_geomean,
                data = peaceterm,
                link = "probit",
                nboot = 10,
                brglm = T)
newX3 = as.matrix(apply(onevar$X, 2, median))
newZ3 = as.matrix(apply(onevar$Z, 2, median))

onevar1 = prediction4(onevar, type = "basesurv", newX3, newZ3, CI = F)
onevar2 = prediction4(onevar, type = "suncure", newX3, newZ3, CI = F)
onevar3 = prediction4(onevar, type = "spop", newX3, newZ3, CI = F)
onevar4 = prediction4(onevar, type = "uncureprob", newX3, newZ3, CI = F)

onevar1 = prediction4(onevar, type = "basesurv", newX3, newZ3, CI = T)
onevar2 = prediction4(onevar, type = "suncure", newX3, newZ3, CI = T)
onevar3 = prediction4(onevar, type = "spop", newX3, newZ3, CI = T)
onevar4 = prediction4(onevar, type = "uncureprob", newX3, newZ3, CI = T)

################################################################################
# Test model with single variable in each equation and multiple rows in X/Zmat
################################################################################
newX4 = rbind(newX3, newX3)
newX4[1, "lpchcap"] = -69
newX4[2, "lpchcap"] = 247

newZ4 = rbind(newZ3, newZ3)
newZ4[1, "ltradedep_geomean"] = 0
newZ4[2, "ltradedep_geomean"] = 0.102

onevar1 = prediction4(onevar, type = "basesurv", newX4, newZ4, CI = F)
onevar2 = prediction4(onevar, type = "suncure", newX4, newZ4, CI = F)
onevar3 = prediction4(onevar, type = "spop", newX4, newZ4, CI = F)
onevar4 = prediction4(onevar, type = "uncureprob", newX4, newZ4, CI = F)
