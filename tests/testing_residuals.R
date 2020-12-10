library(mixcure)
library(tvcure)
library(smcure)
library(fmtr)

################################################################################
# Estimate models
################################################################################

data("goldman.data")

mc = mixcure(Surv(time, cens) ~ transplant, ~ transplant, data = goldman.data, debug = T)
mc = mixcureboot(mc, data = goldman.data)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl); tv = tvcure(Surv(time, cens) ~ transplant, ~ transplant, data = goldman.data, var = T, nboot = 10)

sm = smcure(Surv(time, cens) ~ transplant, ~ transplant, data = goldman.data, model = "ph", nboot = 10)

summary(mc)
summary(tv)
sm$beta
sm$logistfit$coefficients

################################################################################
# Calculate residuals
################################################################################
# Summarize to see if they appear similar

mc_cs = residuals(mc, data = goldman.data, type = "Cox-Snell")
tv_cs = tvresid(tv, type = "Cox-Snell")
summary(mc_cs$residuals)
summary(tv_cs$residuals)

mc_mcs = residuals(mc, data = goldman.data, type = "M-Cox-Snell")
tv_mcs = tvresid(tv, type = "M-Cox-Snell")
summary(mc_mcs$residuals)
summary(tv_mcs$residuals)

mc_m = residuals(mc, data = goldman.data, type = "Martingale")
tv_m = tvresid(tv, type = "Martingale")
summary(mc_m$residuals)
summary(tv_m$residuals)

mc_mm = residuals(mc, data = goldman.data, type = "M-Martingale")
tv_mm = tvresid(tv, type = "M-Martingale")
summary(mc_mm$residuals)
summary(tv_mm$residuals)

#
prop.coxph(tv)

################################################################################
# Testing Schoenfeld residuals
################################################################################
a = sch(set_cure)
b = residuals(set_cure$uncuremod, type = "schoenfeld")

plotsch(a, "recnowt")
plotsch(a, "recyeswt")
plotsch(a, "lpchcap")
plotsch(a, "recmidwt")
plotsch(a, "bdymid")

################################################################################
# Plot residuals
################################################################################

# Can't get proper quantities out of this
### Cox-snell residuals
# Used to address model misspecification - plot against themselves and examine whether they follow a 45-degree line

# # Calculate baseline hazard
# HHazard = -log(mc$survprob)
#
#
# # Ht = tv$BaseHaz
# Ht = -log(tv$Survival)
# Ht = Ht[!is.infinite(Ht)]
# exp1 = rep(exp(1), length(Ht))
# uexp = lm(Ht ~ exp1)
# plot(y = mc_cs$residuals, x = uexp)
# abline(a = 0, b = 1)
# plot(mc$)
#
#
# ### ???
# mc$cureformula
# mf = model.frame(formula = mc$survformula, data = goldman.data, na.action = na.pass)
# X = model.matrix(terms(mf), data = goldman.data)


