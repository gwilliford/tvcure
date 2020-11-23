phtest_plot = function(model, newX, newZ) {
  su_pred = tvpred(model, "spop", newX, newZ, CI = F)
  Time = su_pred$Time
  surv = su_pred$spop
  nx = ncol(surv)

  p_pred = tvpred(model, "uncureprob", newX, newZ, CI = F)
  prob = p_pred$uncureprob

  qi = matrix(nrow = nrow(surv), ncol = nx)
  for (i in 1:nx) {
    qi[, i] = surv[, i] - prob[i]
    qi[, i] = log(-log(qi[, i]))
  }

  splot = matplot(qi, Time, type = "l")
  return(splot)
}
