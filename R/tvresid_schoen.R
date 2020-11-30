tvbh = function(model) {
  X = model$X
  beta = model$beta
  Time = model$Time
  Status = model$Status

  death_point <- sort(unique(subset(Time, Status == 1)))
  coxexp = exp((beta) %*% t(X))

  lambda <- numeric()
  event <- numeric()

  # Estimate the baseline hazard
  for (i in 1:length(death_point)) {
    # Total # deaths at deathpoint
    event[i] <- sum(Status * as.numeric(Time == death_point[i]))
    # risk of death for each individual in subset of individuals still alive (done by Time >= death_point)
    temp <- sum(as.numeric(Time >= death_point[i]) * drop(coxexp))
    temp1 <- event[i]
    # no. events / total risk = baseline hazard
    lambda[i] <- temp1/temp
  }
  # print(death_point)
  # print(lambda)
  return(lambda)
}

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

  for (k in 1:ncol(X)) {
    for (i in 1:length(death_point)) {
      subs = X[Time >= death_point[i], ]
      coxexp = exp((beta) %*% t(subs))
      ht = basehaz[i] * drop(coxexp)
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
}

sch(set_cure)
