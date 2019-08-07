#' Predict
#'
#' @param model Model returned from tvcure function.
#' @param newX Values for covariates in hazard formula (X).
#' @param nexZ Values for covariates in glm formula (Z). This matrix should not include a constant.
#' @param CI Logical value indicating whether predictions and confidence intervals should be estimated using maximum simulated likelihood
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

  if (CI == F) {
    if (link == "logit") {
      uncureprob <- exp(model$gamma %*% t(newZ)) / (1 + exp(model$gamma %*% t(newZ)))
    }
    if (link == "probit") {
      uncureprob <- pnorm(model$gamma %*% t(newZ))
    }

    suncure = array(0, dim = c(nobs, nrow(newX)))
    ebetaX = exp(model$beta %*% t(newX))
    for (i in 1:nrow(newX)) {
      suncure[, i] = s0^ebetaX[i]
    }

    spop = array(0, dim = c(nobs, nrow(newX)))
    for (i in 1:nobs) {
      for (j in 1:nrow(newX)) {
        spop[i, j] = uncureprob[j] * suncure[i, j] + (1 - uncureprob[j])
      }
    }
    s0      <- s0[order(s0, decreasing = T)]
    suncure <- suncure[order(suncure[, 1], decreasing = T), ]
    spop    <- spop[order(spop[, 1], decreasing = T), ]
    # suncure <- sort(suncure, decreasing = T)
    # spop <- sort(spop, decreasing = T)

  } else {
    Coef_smplb <- MASS::mvrnorm(n = nsims, mu = beta,  Sigma = model$vcovb)
    Coef_smplg <- MASS::mvrnorm(n = nsims, mu = gamma, Sigma = model$vcovg)

    # Estimate nsims simulated values of uncureprob
    if (link == "logit") {
      uncureprobsims <- exp(Coef_smplg %*% t(newZ)) / (1 + exp(Coef_smplg %*% t(newZ)))
    }
    if (link == "probit") {
      uncureprobsims <- pnorm(Coef_smplg %*% t(newZ))
    }

    # Obtain simulated values of s0
    w <- Status
    s0sim <- matrix(nrow = nsims, ncol = model$nobs)
    for (j in 1:nsims) {
      s0temp <- tvsurv(Time, Status, cbind(1, newX),
                       Coef_smplb[j, ], w, model$model) # 1 X 284
      s0sim[j, ] <- s0temp$survival
    }
    s0mean <- apply(s0sim, 2, mean)[order(Time)]
    s0lo   <- apply(s0sim, 2, quantile, 0.05)[order(Time)]
    s0hi   <- apply(s0sim, 2, quantile, 0.95)[order(Time)]

    # Obtain simulated values of suncure and spop
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
}
# Export uncureprobsims
# TODO Rewrite s0sim a pbapply function
# TODO Order singulate s0 in a logical way
