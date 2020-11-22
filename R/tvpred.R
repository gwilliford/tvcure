#' Prediction function for tvcure models
#'
#' Computes various quantities, including the baseline survivor function, conditional survivor function, population survivor function, and probability of being at risk. Output is typically passed to \link{plot.tvpred}.
#' @param model A \link{tvcure} object.
#' @param type A character vector indicating values to predict. Choices include the baseline survivor function (\"basesurv\"), the conditional survivor function for uncured individuals (\"suncure\"), the population survivor function (\"spop\"), or the probability that an observation is at risk (\"uncureprob\").
#' @param newX An n x k matrix containing values of X to use, where n is the number of observations and k is the number of variables in the hazard equation. Must contain values for all variables in the hazard equation. Must be included unless type = \"basesurv\." Must be passed as a matrix even if it only contains 1 row.
#' @param newZ An n x k matrix containing values of Z to use. Must contain values for all variables in the cure equation equation. Must be included unless type = \"basesurv\." Must be passed as a matrix even if it only contains 1 row.
#' @param CI Logical value indicating whether confidence intervals should be estimated by simulation.
#' @param nsims Number of simulations for estimating confidence intervals.
#'
#' @return Returns a list of quantities necessary to generate plots using \link{plot.tvpred}.

tvpred = function(model, type = c("basesurv", "spop", "suncure", "uncureprob"),
                  newX = NULL, newZ = NULL,
                  CI = T, nsims = 1000) {

  # Setup -------------------------------------------------------------------

  call = match.call()
  type = match.arg(type)

  # Parameters
  beta = model$beta
  gamma = model$gamma
  vcovb = model$vcovb
  vcovg = model$vcovg

  # Varnames
  bnames = model$bnames
  gnames = model$gnames

  # Data
  Status = model$Status
  Time   = model$Time[order(model$Time)]
  X = model$X
  Z = model$Z

  # survival and hazard estimates
  s0 = as.matrix(model$Survival, ncol = 1)
  H0 = as.matrix(model$BaseHaz, ncol = 1)

  # Summary
  nobs = nrow(s0)

  # Options
  link = model$link




  # Error messages -------------------------------------------------------------
  if (!inherits(model, "tvcure"))
    stop("Model must be a tvcure object")
  if (type != "basesurv" & (is.null(newX) | is.null(newZ)))
    stop("newX and newZ must be specified unless type = \"basesurv\"")




  # Format data ----------------------------------------------------------------

  if (is.null(newX)) {
    newX = apply(X, 2, median)
    newX = matrix(rep(newX, 1), ncol = length(newX), byrow = T)
    colnames(newX) = bnames
  }
  newX = as.matrix(newX)
  nx = nrow(newX)

  if (is.null(newZ)) {
    newZ = apply(Z, 2, median)
    newZ = matrix(rep(newZ, 1), ncol = length(newZ), byrow = T)
    colnames(newZ) = gnames
    newZ = as.matrix(newZ)
  }
  newZ = as.matrix(newZ)
  nz = nrow(newZ)

  # Create predictions without CIs ---------------------------------------------
  if (CI == F) {

    # Baseline hazard
    s0 = s0[order(s0, decreasing = T)]

    if (type != "basesurv") {
      # Uncureprob
      if (link == "logit")  uncureprob = exp(gamma %*% t(newZ)) / (1 + exp(gamma %*% t(newZ)))
      if (link == "probit") uncureprob = pnorm(gamma %*% t(newZ))

      # Suncure
      suncure = array(0, dim = c(nobs, nx))
      ebetaX = exp(model$beta %*% t(newX))
      for (i in 1:nx) {
        suncure[, i] = s0^ebetaX[i]
      }
      suncure = suncure[order(suncure[, 1], decreasing = T), ]

      # Spop
      spop = array(0, dim = c(nobs, nrow(newX)))
      for (i in 1:nobs) {
        for (j in 1:nrow(newX)) {
          spop[i, j] = uncureprob[j] * suncure[i, j] + (1 - uncureprob[j])
        }
      }
      spop    = spop[order(spop[, 1], decreasing = T), ]
    }
  }



  # Predictions with CIs -------------------------------------------------------
  if (CI == T) {

    # Simulate coefficients
    Coef_smplb = MASS::mvrnorm(n = nsims, mu = beta, Sigma = vcovb)
    Coef_smplg = MASS::mvrnorm(n = nsims, mu = gamma, Sigma =  vcovg)

    # obtain simulated values of baseline hazard
    s0sim = matrix(nrow = nsims, ncol = nobs)
    for (j in 1:nsims) {
      s0sim[j, ] = as.vector(s0)^exp(Coef_smplb[j, ] %*% t(model$X))
    }
    s0mean = sort(apply(s0sim, 2, mean), decreasing = T)
    s0lo   = sort(apply(s0sim, 2, quantile, 0.05), decreasing = T)
    s0hi   = sort(apply(s0sim, 2, quantile, 0.95), decreasing = T)

    browser()
    if (type != "basesurv") {

      # # turn newX and Z into row vector if only one observation
      # if (ncol(newX) == 1) newX = t(newX)
      # if (ncol(newZ) == 1) newZ = t(newZ)

      # obtain simulated values of uncureprob

      if (link == "logit")
        uncureprobsims = exp(Coef_smplg %*% t(newZ)) / (1 + exp(Coef_smplg %*% t(newZ)))
      if (link == "probit") uncureprobsims = pnorm(Coef_smplg %*% t(newZ))

      uncuremean = apply(uncureprobsims, 2, mean)
      uncurelo   = apply(uncureprobsims, 2, quantile, 0.05)
      uncurehi   = apply(uncureprobsims, 2, quantile, 0.95)

      # Obtain simulated values of suncure and spop

      if (type == "suncure" | type == "spop") {
        ebetaXsim   = exp(Coef_smplb %*% t(newX))
        suncuresims = array(NA, dim = c(nsims, nobs, nrow(newX)))
        spopsims    = array(NA, dim = c(nsims, nobs, nrow(newX)))

        for (i in 1:nsims) {
          for (k in 1:nrow(newX)) {
            suncuresims[i, , k] = s0sim[i, ]^ebetaXsim[i, k]
            spopsims[i, , k] = uncureprobsims[i, k] * suncuresims[i, , k] + (1 - uncureprobsims[i, k])
          }
        }

        suncuremean = matrix(nrow = nobs, ncol = dim(newZ))
        suncurelo   = matrix(nrow = nobs, ncol = dim(newZ))
        suncurehi   = matrix(nrow = nobs, ncol = dim(newZ))
        spopmean    = matrix(nrow = nobs, ncol = dim(newZ))
        spoplo      = matrix(nrow = nobs, ncol = dim(newZ))
        spophi      = matrix(nrow = nobs, ncol = dim(newZ))

        for (k in 1:nrow(newX)) {
            suncuremean[, k] = sort(apply(suncuresims[, , k], 2, mean), decreasing = T)
            suncurelo[, k]   = sort(apply(suncuresims[, , k], 2, quantile, 0.05), decreasing = T)
            suncurehi[, k]   = sort(apply(suncuresims[, , k], 2, quantile, 0.95), decreasing = T)
            spopmean[, k]    = sort(apply(spopsims[, , k], 2, mean), decreasing = T)
            spoplo[, k]      = sort(apply(spopsims[, , k], 2, quantile, 0.05), decreasing = T)
            spophi[, k]      = sort(apply(spopsims[, , k], 2, quantile, 0.95), decreasing = T)
        }
      }
    }
  }



  # Output ---------------------------------------------------------------------
  if (CI == F) {
    if (type == "basesurv")   return(structure(list(s0 = s0, CI = CI, type = type),
                                               class = "tvpred"))
    if (type == "suncure")    return(structure(list(suncure = suncure, Time = Time, CI = CI, type = type),
                                               class = "tvpred"))
    if (type == "spop")       return(structure(list(spop = spop, Time = Time, CI = CI, type = type),
                                               class = "tvpred"))
    if (type == "uncureprob") return(structure(list(uncureprob = uncureprob, Time = Time, CI = CI, type = type),
                                               class = "tvpred"))
  } else {
    if (type == "basesurv")   return(structure(list(
      s0mean = s0mean, s0lo = s0lo, s0hi = s0hi,
      CI = CI, type = type),
      class = "tvcure"))
    if (type == "suncure")    return(structure(list(
      suncuremean = suncuremean,
      suncurelo = suncurelo, suncurehi = suncurehi,
      Time = Time, CI = CI, type = type),
      class = "tvcure"))
    if (type == "spop")       return(structure(list(
      spopmean = spopmean,
      spoplo = spoplo, spophi = spophi,
      Time = Time, CI = CI, type = type),
      class = "tvcure"))
    if (type == "uncureprob") return(structure(list(
      uncuremean = uncuremean, uncurelo = uncurelo, uncurehi = uncurehi,
      Time = Time, CI = CI, type = type),
      class = "tvcure"))
  }
}
