#' @return sch A matrix of Schoenfeld residuals for each variable. Sorted by failure time.
#' @return failtime A vector of ordered failure times
sch = function(model) {
  X = model$X
  beta = model$beta
  Time = model$Time
  Status = model$Status
  death_point <- sort(unique(subset(Time, Status == 1)))
  basehaz = tvbh(model)

  ht = numeric()
  xbar = matrix(nrow = length(death_point), ncol = ncol(X))
  sch  = matrix(nrow = sum(Status), ncol = ncol(X))

  for (i in 1:length(death_point)) {
    subs = X[Time >= death_point[i], ]
    coxexp = exp((beta) %*% t(subs))
    ht = basehaz[i] * drop(coxexp)
    for (k in 1:ncol(X)) {
      xvec = subs[, k]
      xbar[i, k] = sum(xvec[i] * ht[i]) / sum(ht[i])
     # xres = cbind(Time[Status == 1], X[Status == 1, ])
    }
  }

  xbar = cbind(death_point, xbar)
  ind = Time %in% death_point & Status == 1
  xres = cbind(Time[ind], X[ind, ])
  colnames(xres)[1] = "death_point"
  xres = merge(xres, xbar)
  xres = xres[, 2:ncol(xres)]
  ncx = ncol(X)
  for (i in 1:ncx) {
    sch[, i] = xres[, i] - xres[, i + ncx]
  }
  colnames(sch) = colnames(X)
  browser()
  sch = list(sch = sch, failtime = Time[ind])
  sch
}

plotsch = function(schres, variable, zeroline = T) {
  variable = variable
  y = schres$sch[, variable]
  x = schres$failtime
  lws = loess(y ~ x, span = 20)
  j = order(x)
  #lws = loess(y ~ x)
  lws = predict(lws, se = T)
  u = lws$fit[j] + qt(0.975, lws$df) * lws$se
  l = lws$fit[j] - qt(0.975, lws$df) * lws$se
  plot(y ~ x)
  lines(lws$fit[order(lws$fit)], col = "blue")
  lines(u[order(lws$fit)], lty = 2)
  lines(l[order(lws$fit)], lty = 2)
  if (zeroline) abline(h = 0, lty = 2, col = "dark gray")
}
