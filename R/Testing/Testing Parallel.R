library(snow)
library(foreach)
library(doParallel)
library(doSNOW)
library(tvcure)
library(smcure)
data(e1684)

# Run sequentially
pd <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph", parallel = F)

# Test if no parallel object registered - should throw error
pd <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph")

# Run in parallel
cl <- snow::makeCluster(4, "SOCK")
doSNOW::registerDoSNOW(cl)
pd <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph", nboot = 20)

# Logit
pd_logit <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,
                   cureform = ~ TRT + SEX + AGE,
                   data = e1684,
                   model = "ph",
                   emmax = 1000,
                   nboot = 10000)
pd_logit2 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,
                   cureform = ~ TRT + SEX + AGE,
                   data = e1684,
                   model = "ph",
                   emmax = 1000,
                   nboot = 10000)
pd_logit3 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,
                   cureform = ~ TRT + SEX + AGE,
                   data = e1684,
                   model = "ph",
                   emmax = 1000,
                   nboot = 10000)
cbind(pd_logit$b_pvalue, pd_logit2$b_pvalue, pd_logit3$b_pvalue)
cbind(pd_logit$g_pvalue, pd_logit2$g_pvalue, pd_logit3$g_pvalue)
pd_probit <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,
                   cureform = ~ TRT + SEX + AGE,
                   data = e1684,
                   model = "ph",
                   emmax = 1000,
                   link = "probit",
                   nboot = 10000)
pd_probit2 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,
                   cureform = ~ TRT + SEX + AGE,
                   data = e1684,
                   model = "ph",
                   emmax = 1000,
                   link = "probit",
                   nboot = 10000)
pd_probit3 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,
                   cureform = ~ TRT + SEX + AGE,
                   data = e1684,
                   model = "ph",
                   emmax = 1000,
                   link = "probit",
                   nboot = 10000)
cbind(pd_probit$b_pvalue, pd_probit2$b_pvalue, pd_probit3$b_pvalue)
cbind(pd_probit$g_pvalue, pd_probit2$g_pvalue, pd_probit3$g_pvalue)
saveRDS(pd_logit,"pd_logit.RDS")
saveRDS(pd_logit2,"pd_logit2.RDS")
saveRDS(pd_logit3,"pd_logit3.RDS")
saveRDS(pd_probit,"pd_probit.RDS")
saveRDS(pd_probit2,"pd_probit2.RDS")
saveRDS(pd_probit3,"pd_probit3.RDS")




# Probit
pd_probit4 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,
                   cureform = ~ TRT + SEX + AGE,
                   data = e1684,
                   model = "ph",
                   emmax = 1000,
                   nboot = 100,
                   link = "probit")


# Predict baseline survival function plot it
plot_basesurv_tvcure(pd)

# Test predict and plottinf function
pd.pred1 <- predict_tvcure(pd,
               newX = cbind(SEX = c(1), TRT = c(0), c(0.579)),
               newZ = cbind(SEX = c(1), TRT = c(0), c(0.579)),
               model="ph",
)
plot_predict_tvcure(pd.pred1, model = "ph", lwd = 2, basesurv = T, ylim=c(0, 1))

pd.pred <- predict_tvcure(pd2, model = "ph")
pred <- pd.pred$prediction
pred <- cbind(pd.pred$s0,pd.pred$prediction)

  pdsort <- pred[order(pred[, "Time"]), ]
  if (length(object$newuncureprob) == 1)
    plot(pdsort[, "Time"], pdsort[, 2], type = "S")
  else {
    matplot(pdsort[, "Time"], pdsort[, 1:(ncol(pred) -1)], type = "S",
            lty = 1:(ncol(pred) - 1))
  }




  # Create covariate profiles
    # Create one with median case
    # Create one with alternative profile
    # Plot

  # Predictions --- standard errors?

  # Plot legend automatically

