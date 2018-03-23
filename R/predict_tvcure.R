#' Predict
#'
#' @param model Model returned from tvcure function.
#' @param newX Values for covariates in hazard formula (X).
#' @param nexZ Values for covariates in glm formula (Z). This matrix should not include a constant.
predict_tvcure <- function(model, newX, newZ, link, ...) {
  call <- match.call()
  if (!inherits(model, "tvcure"))
    stop("Model must be a tvcure object")
  s0 = as.matrix(model$Survival, ncol = 1)
  n = nrow(s0)
  if (is.vector(newZ))
    newZ = as.matrix(newZ)
  newZ = cbind(1, newZ)
  if (is.vector(newX))
    newX = as.matrix(newX)
  if (link == "logit") {
    uncureprob <- exp(model$gamma %*% t(newZ)) /
                (1 + exp(model$gamma %*% t(newZ)))
  }
  if (link == "probit") {
    uncureprob <- pnorm(model$gamma %*% t(newZ))
  }
  suncure = array(0, dim = c(n, nrow(newX)))
  # t = array(0, dim = c(n, nrow(newX)))
  spop = array(0, dim = c(n, nrow(newX)))
  #if (model == "ph") {
  ebetaX = exp(model$beta %*% t(newX))
  for (i in 1:nrow(newZ)) {
    suncure[, i] = s0 ^ ebetaX[i]
  }
  for (i in 1:n) {
    for (j in 1:nrow(newX)) {
      spop[i, j] = uncureprob[j] * suncure[i, j] + (1 - uncureprob[j])
    }
  }
  #scure = cbind(scure, Time = model$Time)
  #spop = cbind(spop, Time = model$Time)
  #sbase = cbind(s0, Time = model$Time)
  #}
  # if (model == "aft") {
  #   newX = cbind(1, newX)
  #   ebetaX = exp(model$beta %*% t(newX))
  #   for (i in 1:nrow(newX)) {
  #     t[, i] = ebetaX[i] * exp(model$error)
  #   }
  #   for (i in 1:n) {
  #     for (j in 1:nrow(newX)) {
  #       spop[i, j] = uncureprob[j] * s0[i] + (1 - uncureprob[j])
  #     }
  #   }
  #   prd = cbind(spop = spop, Time = t)
  #   sur = cbind(s0, Time = t)
  # }
  structure(list(call = call, uncureprob = uncureprob, Time = model$Time, suncure = suncure, spop = spop, sbase = s0, class = "predicttvcure"))
}
