tvresid = function(model, type = c("Cox-Snell", "M-Cox-Snell", "Martingale", "M-Martingale")) {

  type = match.arg(type)

  if (class(model) != "tvcure") stop("Model must be a tvcure object")

  # Data
  ttime = model$Time
  cens = model$Status
  X = model$X
  Z = model$Z[, -1]

  uncureprob = model$uncureprob
  survprob = model$Survival
  weight = model$w
  beta_uncure = model$uncuremod$coefficients
  unculp = as.vector(X %*% beta_uncure)

  # glm parameters
  gamma = model$gamma[-1]

  if (type == "Cox-Snell") {
    residuals = -log(uncureprob * survprob + 1 - uncureprob)
    resid.uncure = -log(survprob)
    out = list(type = type, cens = cens, time = ttime, weight = model$w,
         residuals = residuals, resid.uncure = resid.uncure)
  }
  else if (type == "M-Cox-Snell") {
    residuals = -log(survprob)
    resid.dist = (function(resids, cens, weight) {
      design = survey::svydesign(id = ~1, weights = weight[weight > 0])
      survey::svykm(Surv(resids[weight > 0], cens[weight > 0]) ~ 1, design = design)
    })(residuals, cens, weight)
    out = list(type = type, cens = cens, time = ttime, weight = weight,
         residuals = residuals, resid.dist = resid.dist)
  }
  else if (type == "M-Martingale") {
    keep = weight != 0
    val = cens + weight * log(survprob)
    val2 = residuals(model$uncuremod, weighted = T)
    deviance = sign(val) * sqrt(-2 * (val + cens * log(cens - val)))
    out = list(type = type, xm = X[keep, ], keep = keep, unculp = unculp[keep],
         residuals = val[keep], deviance = deviance[keep],
         residuals2 = val2)
  }
  else if (type == "Martingale") {
    val = cens + log(uncureprob * (survprob - 1) + 1)
    out = list(type = type, xm = X, unculp = unculp, residuals = val,
         deviance = sign(val) * sqrt(-2 * (val + cens * log(cens - val))))
  }
  return(out)
}
