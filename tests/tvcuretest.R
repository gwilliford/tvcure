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

onevar1 = tvpred(onevar, type = "basesurv", newX3, newZ3, CI = F)
onevar2 = tvpred(onevar, type = "suncure", newX3, newZ3, CI = F)
onevar3 = tvpred(onevar, type = "spop", newX3, newZ3, CI = F)
onevar4 = tvpred(onevar, type = "uncureprob", newX3, newZ3, CI = F)

onevar1 = tvpred(onevar, type = "basesurv", newX3, newZ3, CI = T)
onevar2 = tvpred(onevar, type = "suncure", newX3, newZ3, CI = T)
onevar3 = tvpred(onevar, type = "spop", newX3, newZ3, CI = T)
onevar4 = tvpred(onevar, type = "uncureprob", newX3, newZ3, CI = T)

################################################################################
# Test model with single variable in each equation and multiple rows in X/Zmat
################################################################################
newX4 = rbind(newX3, newX3)
newX4[1, "lpchcap"] = -69
newX4[2, "lpchcap"] = 247

newZ4 = rbind(newZ3, newZ3)
newZ4[1, "ltradedep_geomean"] = 0
newZ4[2, "ltradedep_geomean"] = 0.102

onevar1 = tvpred(onevar, type = "basesurv", newX4, newZ4, CI = F)
onevar2 = tvpred(onevar, type = "suncure", newX4, newZ4, CI = F)
onevar3 = tvpred(onevar, type = "spop", newX4, newZ4, CI = F)
onevar4 = tvpred(onevar, type = "uncureprob", newX4, newZ4, CI = F)




################################################################################
# Test residuals
################################################################################

resid_cs  = tvresid(set_cure, type = "Cox-Snell")
resid_mcs = tvresid(set_cure, type = "M-Cox-Snell")
resid_m   = tvresid(set_cure, type = "Martingale")
resid_mm  = tvresid(set_cure, type = "M-Martingale")

################################################################################
# Test schoenfeld residuals
################################################################################
schoenfeld = sch(set_cure)
plotsch(schoenfeld, variable = "recnowt")

cor(cbind(schoenfeld$sch[, "recnowt"], schoenfeld$failtime))
cor.test(schoenfeld$sch[, "recnowt"], schoenfeld$failtime)
cor.test(schoenfeld$sch[, "recyeswt"], schoenfeld$failtime)
cor.test(schoenfeld$sch[, "recmidwt"], schoenfeld$failtime)
cor.test(schoenfeld$sch[, "lpchcap"], schoenfeld$failtime)
cor.test(schoenfeld$sch[, "bdymid"], schoenfeld$failtime)

terr_sch = sch(i0)
plotsch(terr_sch, variable = "pchcap")
plotsch(terr_sch, variable = "bdymid")
plotsch(terr_sch, variable = "systchangeTRUE")
plotsch(terr_sch, variable = "ww1TRUE")
plotsch(terr_sch, variable = "ww2TRUE")
plotsch(terr_sch, variable = "coldwarTRUE")

################################################################################
# Test residuals survplots
################################################################################

newX = apply(set_cure$X, 2, median)
newX = rbind(newX, newX)
newX[2, "bdymid"] = 1

newZ = apply(set_cure$Z, 2, median)
newZ = rbind(newZ, newZ)
newZ[2, "demdy"] = 1

phtest_plot(set_cure, newX, newZ)

