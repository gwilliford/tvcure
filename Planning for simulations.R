library(coreSim)
test1 <- list(time=c(4,3,1,1,2,2,3),
              status=c(1,1,1,0,1,1,0),
              x=c(0,2,1,1,1,0,0),
              sex=c(0,0,0,0,1,1,1))
a <- coxph(Surv(time, status) ~ x + sex, test1)
simdat <- expand.grid(sex = 0:1, x=0:2)
linear_qi <- qi_builder(a, simdat, slim = T)
predict_qi <- predict(a, simdat, type="lp")

linear_qi_gates <- b_sim(gates.tvsurv)

ggplot(linear_qi)

ggplot(linear_qi, aes(sex, qi_median))+geom_ribbon(aes(ymin = qi_min, ymax = qi_max), alpha = 0.3)
+
  geom_line()

# General appraoch
  A) Get Get a vcov matrix from each bootstrap (or from the original data?)
    Combine the bootstrap vcovs
  B) Take cov from bootstraps

  With a vcov matrix for gamma
    Use the vcov and the estimates to simulate gammas
    Use simulated gammas to produce simulated pps
  With vcov simulated pps and a vcov matrix for beta
    Get simulated survivor function
  Take min/max of simulations from vcov matrix to get confidence intervals

#
predict_g(pd, newX = cbind(SEX = c(1), TRT = c(0), c(0.579)),
              newZ = cbind(SEX = c(1), TRT = c(0), c(0.579)),
              model="ph",
)

a <- predict_g(pd, newX = cbind(SEX = c(1, 0), TRT = c(0, 0), AGE = c(0.579, 0.579)),
          newZ = cbind(SEX = c(1,0), TRT = c(0,0), AGE = c(0.579, 0.579)),
          model="ph",
)
b <- predict_tvcure(pd, newX = cbind(SEX = c(1, 0), TRT = c(0, 0), AGE = c(0.579, 0.579)),
          newZ = cbind(SEX = c(1,0), TRT = c(0,0), AGE = c(0.579, 0.579)),
          model="ph",
)
