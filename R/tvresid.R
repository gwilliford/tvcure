tvresid = function(model, type = c("Cox-Snell", "M-Cox-Snell", "Martingale", "M-Martingale")) {

  type = match.arg(type)

  if (class(model) != "tvcure") stop("Model must be a tvcure object")

  # Data
  Time = model$Time
  Status = model$Status
  X = model$X
  Z = model$Z[, -1]

  uncureprob = model$uncureprob
  survprob = model$Survival
  weight = model$w
  beta = model$beta
  cox_xb = as.vector(X %*% beta)

  # glm parameters
  gamma = model$gamma[-1]

  if (type == "Cox-Snell") {
    residuals = -log(uncureprob * survprob + 1 - uncureprob)
    resid.uncure = -log(survprob)
    out = list(type = type, Status = Status, time = Time, weight = model$w,
         residuals = residuals, resid.uncure = resid.uncure,
         X = X, Z = Z)
  }
  else if (type == "M-Cox-Snell") {
    residuals = -log(survprob)
    resid.dist = (function(resids, Status, weight) {
      design = survey::svydesign(id = ~1, weights = weight[weight > 0])
      survey::svykm(Surv(resids[weight > 0], Status[weight > 0]) ~ 1, design = design)
    })(residuals, Status, weight)
    out = list(type = type, Status = Status, time = Time, weight = weight,
         residuals = residuals, resid.dist = resid.dist,
         X = X, Z = Z)
  }
  else if (type == "M-Martingale") {
    keep = weight != 0
    val = Status + weight * log(survprob)
    val2 = residuals(model$curemod$latency_fit, weighted = T)
    deviance = sign(val) * sqrt(-2 * (val + Status * log(Status - val)))
    out = list(type = type, xm = X[keep, ], keep = keep, cox_xb = cox_xb[keep],
         residuals = val[keep], deviance = deviance[keep],
         residuals2 = val2,
         X = X, Z = Z)
  }
  else if (type == "Martingale") {
    val = Status + log(uncureprob * (survprob - 1) + 1)
    out = list(type = type, xm = X, cox_xb = cox_xb, residuals = val,
         deviance = sign(val) * sqrt(-2 * (val + Status * log(Status - val))),
         X = X, Z = Z)
  }
  return(out)
}
