#' Predict
#'
#' @param model Model returned from tvcure function.
#' @param newX Values for covariates in hazard formula (X).
#' @param nexZ Values for covariates in glm formula (Z). This matrix should not include a constant.
predict.tvcure <- function(model, newX = NULL, newZ = NULL, ...) {
  call <- match.call()
  if (!inherits(model, "tvcure"))
    stop("Model must be a tvcure object")
  s0 = as.matrix(model$Survival, ncol = 1)
  nobs = nrow(s0)
  if (is.vector(newZ))
    newZ = as.matrix(newZ)
  if (is.vector(newX))
    newX = as.matrix(newX)
  link <- model$link
  if (link == "logit") {
    uncureprob <- exp(model$gamma %*% t(newZ)) /
                (1 + exp(model$gamma %*% t(newZ)))
  }
  if (link == "probit") {
    uncureprob <- pnorm(model$gamma %*% t(newZ))
  }
  suncure = array(0, dim = c(nobs, nrow(newX)))
  spop = array(0, dim = c(nobs, nrow(newX)))
  ebetaX = exp(model$beta %*% t(newX))
  for (i in 1:nrow(newZ)) {
    suncure[, i] = s0^ebetaX[i]
  }
  for (i in 1:nobs) {
    for (j in 1:nrow(newX)) {
      spop[i, j] = uncureprob[j] * suncure[i, j] + (1 - uncureprob[j])
    }
  }
  structure(list(call = call, uncureprob = uncureprob, Time = model$Time,
                 suncure = suncure, spop = spop, sbase = s0, newZ = newZ,
                 newX = newX, beta = model$beta, gamma = model$gamma,
                 link = model$link, vcovb = model$vcovb, vcovg = model$vcovg,
                 Survival = model$Survival),
            class = "predicttvcure")
}
