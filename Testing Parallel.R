library(snow)
library(foreach)
library(doParallel)
library(doSNOW)

cl <- makeCluster(3, "SOCK")
registerDoParallel(cl)


pd <- tvcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE,data=e1684,model="ph", parallel = F)

library(smcure); data(e1684)
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
pd <- tvcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE,data=e1684,model="ph")
pd.bases <- basesurv_tvcure(pd)
pd.pred <- predict_tvcure(pd2,
            newX = cbind(SEX = c(1, 0, 1), TRT = c(0, 0, 1), c(0.579, 0.579, 30)),
            newZ = cbind(SEX = c(1, 0, 1), TRT = c(0, 0, 1), c(0.579, 0.579, 30)),
            model="ph",
            main = "Predicted Survival Curves for Three Individuals"
)

pd.pred <- predict_tvcure(pd2,
              newX = cbind(SEX = c(1, 0, 1), TRT = c(0, 0, 1), c(0.579, 0.579, 30)),
              newZ = cbind(SEX = c(1, 0, 1), TRT = c(0, 0, 1), c(0.579, 0.579, 30)),
              model="ph",
              main = "Predicted Survival Curves for Three Individuals"
              )
par(mfrow=c(1,2))
plot_tvcure(pd.pred, model = "ph", lwd = 3, basesurv = T, ylim=c(0, 1))
plot_tvcure(pd.pred, model = "ph", lwd = 3, basesurv = F, ylim=c(0, 1))
plot_tvcure(pd.pred)
plot_tvcure(pd.pred, model = "ph")
legend('topright',legend = c("1","2","3"))

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

