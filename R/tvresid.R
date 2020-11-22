residuals.tvcure = function (object, data, type = c("WLCH", "Cox-Snell", "M-Cox-Snell", "Martingale", "M-Martingale")) {

  type = match.arg(type)

  if (class(object) != "mixcure")
    stop("mixcure object is missing")
  if (is.null(data))
    stop("data is missing")

  mfx = model.frame(formula = object$survformula, data = data, na.action = na.pass)
  survtime = model.response(mfx)
  ttime = survtime[, 1]
  cens = survtime[, 2]

  uncureprob = object$curefit$uncureprob
  bt = coef(object$uncufit$fit)
  X = model.matrix(terms(mf), data = data)
  if (length(bt) == ncol(X))
    unculp = as.vector(X %*% bt)
  else unculp = as.vector(X[, -1, drop = F] %*% bt)

  mfz = model.frame(formula = object$cureformula, data = data,
                   na.action = na.pass)
  Z = model.matrix(terms(mfz), data = data)[, -1, drop = F]
  gm = coef(object$curefit$fit)[-1]

  if (type == "WLCH") {
    if (class(object$uncufit) != "surv.survreg")
      stop("WLCH residuals is only defined for parametric models")
    fit = object$uncufit$fit
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
    residuals = -log(uncureprob * object$survprob + 1 - uncureprob)
    resid.uncure = -log(object$survprob)
    list(type = type, cens = cens, time = ttime, weight = object$postuncure,
         residuals = residuals, resid.uncure = resid.uncure)
  }
  else if (type == "M-Cox-Snell") {
    residuals = -log(object$survprob)
    weight = object$postuncure
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
    keep = object$postuncure != 0
    val = cens + object$postuncure * log(object$survprob)
    if (object$survmodel$fun == "coxph")
      val2 = residuals(object$uncufit$fit, weighted = T)
    else val2 = NULL
    deviance = sign(val) * sqrt(-2 * (val + cens * log(cens -
                                                         val)))
    list(type = type, xm = X[keep, ], keep = keep, unculp = unculp[keep],
         residuals = val[keep], deviance = deviance[keep],
         residuals2 = val2)
  }
  else if (type == "Martingale") {
    val = cens + log(uncureprob * (object$survprob - 1) +
                       1)
    list(type = type, xm = X, unculp = unculp, residuals = val,
         deviance = sign(val) * sqrt(-2 * (val + cens * log(cens -
                                                              val))))
  }
  else stop("The type of residuals are not defined yet")
}
