residuals.tvcure = function (model, type = c("WLCH", "Cox-Snell", "M-Cox-Snell", "Martingale", "M-Martingale")) {

  type = match.arg(type)

  if (class(model) != "tvcure") stop("Model must be a tvcure object")

  # Response variable
  mfx = model.frame(formula = model$call$survform, data = data, na.action = na.pass)
  survtime = model.response(mfx)
  ttime = model$Time
  cens = model$Status

  uncureprob = model$uncureprob
  beta_uncure = coef(model$uncufit$fit)
  X = model$X
  unculp = as.vector(X %*% beta_uncure)
  else unculp = as.vector(X[, -1, drop = F] %*% beta_uncure)

  # glm parameters
  mfz = model.frame(formula = model$call$cureform, data = data, na.action = na.pass)
  Z = model$Z[, -1]
  gamma = model$gamma[-1]

  if (type == "WLCH") {
    if (class(model$uncufit) != "surv.survreg")
      stop("WLCH residuals is only defined for parametric models")
    fit = model$uncufit$fit
    n = nrow(data)
    hazard = outer(ttime, 1:n, FUN = function(x, y) {
      dval = dsurvreg(x, mean = unculp[y], scale = fit$scale,
                      dist = fit$dist, parms = fit$parms)
      sval = 1 - psurvreg(x, mean = unculp[y], scale = fit$scale,
                          dist = fit$dist, parms = fit$parms)
      uncureprob[y] * dval/(uncureprob[y] * sval + 1 -
                              uncureprob[y])
    })
    etime = NULL
    val = NULL
    XZ = cbind(X[, -1, drop = F], Z)
    for (i in 1:n) {
      if (cens[i] == 1) {
        tt = ttime[i]
        etime = c(etime, tt)
        rset = ttime >= tt
        hrset = hazard[i, rset]
        val = rbind(val, XZ[i, , drop = F] - colSums(sweep(XZ[rset,
                                                              , drop = F], 1, hrset, FUN = "*"))/sum(hrset))
      }
    }
    list(eventtime = etime, residuals = val)
  }
  else if (type == "Cox-Snell") {
    residuals = -log(uncureprob * model$survprob + 1 - uncureprob)
    resid.uncure = -log(model$survprob)
    list(type = type, cens = cens, time = ttime, weight = model$postuncure,
         residuals = residuals, resid.uncure = resid.uncure)
  }
  else if (type == "M-Cox-Snell") {
    residuals = -log(model$survprob)
    weight = model$postuncure
    resid.dist = (function(resids, cens, weight) {
      design = svydesign(id = ~1, weights = weight[weight >
                                                     0])
      svykm(Surv(resids[weight > 0], cens[weight > 0]) ~
              1, design = design)
    })(residuals, cens, weight)
    list(type = type, cens = cens, time = ttime, weight = weight,
         residuals = residuals, resid.dist = resid.dist)
  }
  else if (type == "M-Martingale") {
    keep = model$postuncure != 0
    val = cens + model$postuncure * log(model$survprob)
    if (model$survmodel$fun == "coxph")
      val2 = residuals(model$uncufit$fit, weighted = T)
    else val2 = NULL
    deviance = sign(val) * sqrt(-2 * (val + cens * log(cens -
                                                         val)))
    list(type = type, xm = X[keep, ], keep = keep, unculp = unculp[keep],
         residuals = val[keep], deviance = deviance[keep],
         residuals2 = val2)
  }
  else if (type == "Martingale") {
    val = cens + log(uncureprob * (model$survprob - 1) +
                       1)
    list(type = type, xm = X, unculp = unculp, residuals = val,
         deviance = sign(val) * sqrt(-2 * (val + cens * log(cens -
                                                              val))))
  }
  else stop("The type of residuals are not defined yet")
}
