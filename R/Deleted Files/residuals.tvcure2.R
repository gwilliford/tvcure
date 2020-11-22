# delete formula from tvcure - contained in call function
# pass out weights from tvcure

residuals.tvcure <- function (model, data, type = c("Cox-Snell",
                                 "M-Cox-Snell", "Martingale", "M-Martingale"))
{
  type = match.arg(type)
  if (class(model) != "tvcure")
    stop("tvcure model is missing")
  if (is.null(data))
    stop("data is missing")
  mf = model.frame(formula = model$call$formula, data = data,
                   na.action = na.pass)
  ttime = model$Time
  cens = model$Status
  uncureprob = model$uncureprob
  survival = model$Survival
  w = model$w
  beta = model$beta
  X = model$X
  #model.matrix(terms(mf), data = model$terms)[, -1]
  # if (length(beta) == ncol(X)) {
  unculp = as.vector(X %*% beta)
  # } else {
  #   unculp = as.vector(X[, -1, drop = F] %*% beta)
  # }
  # mf = model.frame(formula = model$call$cureform, data = data,
  #                  na.action = na.pass)
  # Z = model.matrix(terms(mf), data = data)[, -1, drop = F]
  browser()
  # gm = model$gamma[-1]
  if (type == "Cox-Snell") {
    residuals = -log(uncureprob * survival + 1 - uncureprob)
    resid.uncure = -log(survival)
    list(type = type, cens = cens, time = ttime, weight = w,
         residuals = residuals, resid.uncure = resid.uncure)
  } else if (type == "M-Cox-Snell") {
    residuals = -log(survival)
    weight = w
    resid.dist = (function(resids, cens, weight) {
      design = svydesign(id = ~1, weights = weight[weight > 0])
      svykm(Surv(resids[weight > 0], cens[weight > 0]) ~ 1, design = design)
    })(residuals, cens, weight)
    list(type = type, cens = cens, time = ttime, weight = weight,
         residuals = residuals, resid.dist = resid.dist)
  } else if (type == "M-Martingale") {
    keep = w != 0
    val = cens + w * log(survival)
    # if (model$survmodel$fun == "coxph")
    val2 = residuals(model$latency_fit, weighted = T)
    # else val2 = NULL
    deviance = sign(val) * sqrt(-2 * (val + cens * log(cens - val)))
    list(type = type, xm = X[keep, ], keep = keep, unculp = unculp[keep],
         residuals = val[keep], deviance = deviance[keep],
         residuals2 = val2)
  } else if (type == "Martingale") {
    val = cens + log(uncureprob * (survival - 1) + 1)
    list(type = type, xm = X, unculp = unculp, residuals = val,
         deviance = sign(val) * sqrt(-2 * (val + cens * log(cens - val))))
  }
}
