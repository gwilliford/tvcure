#' Predict
#'
#' @param model Model returned from tvcure function.
#' @param newX Values for covariates in hazard formula (X).
#' @param nexZ Values for covariates in glm formula (Z). This matrix should not include a constant.
predict.tvcure <- function(model, newX = NULL, newZ = NULL, CI = F, nsims = 1000, ...) {
  call <- match.call()
  if (!inherits(model, "tvcure"))
    stop("Model must be a tvcure object")
  s0 = as.matrix(model$Survival, ncol = 1)
  nobs = nrow(s0)
  beta <- model$beta
  gamma <- model$gamma
  link <- model$link
  Status <- model$Status
  Time <- model$Time[order(model$Time)]
  if (is.vector(newZ))
    newZ = as.matrix(newZ)
  if (is.vector(newX))
    newX = as.matrix(newX)

  # No CI
  if (CI == F) {
    # Estimate uncureprob for simulated observations
    if (link == "logit") {
      uncureprob <- exp(model$gamma %*% t(newZ)) /
                  (1 + exp(model$gamma %*% t(newZ)))
    }
    if (link == "probit") {
      uncureprob <- pnorm(model$gamma %*% t(newZ))
    }

    # Estimate survival for uncured patients for simulated observations
    suncure = array(0, dim = c(nobs, nrow(newX)))
    ebetaX = exp(model$beta %*% t(newX))
    for (i in 1:nrow(newX)) {
      suncure[, i] = s0^ebetaX[i]
    }

    # Estimate survival for all patients for simulated observations
    spop = array(0, dim = c(nobs, nrow(newX)))
    for (i in 1:nobs) {
      for (j in 1:nrow(newX)) {
        spop[i, j] = uncureprob[j] * suncure[i, j] + (1 - uncureprob[j])
      }
    }
  } else {
    Coef_smplb <- MASS::mvrnorm(n = nsims, mu = beta,  Sigma = model$vcovb)
    Coef_smplg <- MASS::mvrnorm(n = nsims, mu = gamma, Sigma = model$vcovg)

    # Estimate nsims simulated values of uncureprob
    if (link == "logit") {
      uncureprobsims <- exp(Coef_smplg %*% t(newZ)) / (1 + exp(Coef_smplg %*% t(newZ)))
    }
    if (link == "probit") {
      uncureprob <- pnorm(Coef_smplg %*% t(newZ))
    }

    w <- Status
    # Calculate simulated values of s0
    s0sim <- matrix(nrow = nsims, ncol = model$nobs)
    for (j in 1:nsims) {
      s0temp <- tvsurv(Time, Status, cbind(1, newX),
                       Coef_smplb[j, ], w, model$model) # 1 X 284
      s0sim[j, ] <- s0temp$survival
    }
    s0mean <- apply(s0sim, 2, mean)[order(Time)]
    s0lo   <- apply(s0sim, 2, quantile, 0.05)[order(Time)]
    s0hi   <- apply(s0sim, 2, quantile, 0.95)[order(Time)]

    # suncuresim = array(0, dim = c(nobs, nrow(newX), nsims)) # i = 284, j = 2, k = 1000
    # for (k in 1:nsims){
    #   for (j in 1:nrow(newX)){
    #     for (i in 1:nobs) {
    #       suncuresim[i, j, k] = s0sim[i, j]^ebetaXsim[i, j, k]
    #     }
    #   }
    # }
    ebetaXsim <- exp(Coef_smplb %*% t(newX))
    suncuresims <- array(NA, dim = c(nsims, model$nobs, nrow(newX)))
    spopsims    <- array(NA, dim = c(nsims, model$nobs, nrow(newX)))
    for(i in 1:nsims) {
      for(k in 1:nrow(newX)) {
        suncuresims[i, , k] <- s0sim[i, ]^ebetaXsim[i, k]
        for (j in 1:nobs) {
          spopsims[, j, k]    <- uncureprobsims[i, ] * suncuresims[i, j, k]
          + (1 - uncureprobsims[i, ])
        }
      }
    }
    suncuremean <- matrix(nrow = nobs, ncol = dim(newZ)) # 284 x 2
    suncurelo   <- matrix(nrow = nobs, ncol = dim(newZ))
    suncurehi   <- matrix(nrow = nobs, ncol = dim(newZ))
    spopmean    <- matrix(nrow = nobs, ncol = dim(newZ))
    spoplo      <- matrix(nrow = nobs, ncol = dim(newZ))
    spophi      <- matrix(nrow = nobs, ncol = dim(newZ))
    for (i in 1:nrow(newX)) {
      suncuremean[, i] <- apply(suncuresims[, , i], 2, mean)
      suncurelo[, i]   <- apply(suncuresims[, , i], 2, quantile, 0.05)
      suncurehi[, i]   <- apply(suncuresims[, , i], 2, quantile, 0.95)
      spopmean[, i]    <- apply(spopsims[, , i], 2, mean)
      spoplo[, i]      <- apply(spopsims[, , i], 2, quantile, 0.05)
      spophi[, i]      <- apply(spopsims[, , i], 2, quantile, 0.95)
    }
  lapply(list(suncuremean, suncurelo, suncurehi, spopmean, spoplo, spophi), function(x) x[order(Time), ])
  }

  if (CI == F) {
    structure(list(uncureprob = uncureprob,
                   s0 = s0, suncure = suncure, spop = spop,
                   Survival = model$Survival,
                   link = link, Time = Time, CI = CI),
              class = "predicttvcure")

  } else {
    structure(list(s0mean = s0mean, s0lo = s0lo, s0hi = s0hi,
                   suncuremean = suncuremean, suncurelo = suncurelo, suncurehi = suncurehi,
                   spopmean = spopmean, spoplo = spoplo, spophi = spophi,
                   link = link, Time = Time, CI = CI),
              class = "predicttvcure")
  }
# TODO Rewrite s0sim a pbapply function
}
