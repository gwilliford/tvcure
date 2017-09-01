library(snow)
library(foreach)
library(doParallel)
library(doSNOW)

cl <- makeCluster(3, "SOCK")
registerDoParallel(cl)


system.time(pd <- tvcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE,data=e1684,model="ph"))

library(smcure); data(e1684)
cl <- makeCluster(3, "SOCK"); registerDoSNOW(cl)
system.time(pd2 <- tvcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE,data=e1684,model="ph"))

pd.pred <- predicttvcure(pd2,
              newX = cbind(c(1, 0, 1), c(0, 0, 1), c(0.579, 0.579, 30)),
              newZ = cbind(c(1, 0, 1), c(0, 0, 1), c(0.579, 0.579, 30)),
              model="ph",
              main = "Predicted Survival Curves for Three Individuals"
              )

  # Create covariate profiles
    # Create one with median case
    # Create one with alternative profile
    # Plot

  # Predictions --- standard errors?

  # Plot legend automatically

plotpredicttvcure(pd.pred, model = "ph", lwd = 3)
legend('topright',legend = c("1","2","3"))
