#' Calculate baseline hazard
#'
#' Calculates baseline hazard for uncured subjects
#' @param model A tvcure model
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
  return(lambda)
}
