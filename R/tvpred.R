tvpred <- function(model, newX = NULL, newZ = NULL,
                        type = c("basesurv", "spop", "suncure", "uncureprob"),
                        CI = T, nsims = 1000) {

  # Setup -------------------------------------------------------------------

  call <- match.call()
  type = match.arg(type)

  # Parameters
  beta <- model$beta
  gamma <- model$gamma
  vcovb <- model$vcovb
  vcovg <- model$vcovg

  # Varnames
  bnames <- model$bnames
  gnames <- model$gnames

  # Data
  Status <- model$Status
  Time   <- model$Time[order(model$Time)]
  X <- model$X
  Z <- model$Z

  browser()
  # survival and hazard estimates
  s0 = as.matrix(model$Survival, ncol = 1)
  H0 = as.matrix(model$BaseHaz, ncol = 1)

  # Summary
  nobs = nrow(s0)

  # Options
  link <- model$link



  # Error messages -------------------------------------------------------------
  if (!inherits(model, "tvcure")) stop("Model must be a tvcure object")



  # Format data ----------------------------------------------------------------
  if (is.null(newX)) {
    newX <- apply(X, 2, median)
    newX <- matrix(rep(newX, length(values)), ncol = length(newX), byrow = T)
    colnames(newX) <- bnames
    if (variable %in% bnames) newX[, variable] <- values
  }
  newX <- as.matrix(newX)
  nx <- nrow(newX)

  if (is.null(newZ)) {
    newZ <- apply(Z, 2, median)
    newZ <- matrix(rep(newZ, length(values)), ncol = length(newZ), byrow = T)
    colnames(newZ) <- gnames
    if (variable %in% gnames) newZ[, variable] <- values
  }
  newZ <- as.matrix(newZ)
  nz <- nrow(newZ)



  # Create predictions without CIs ---------------------------------------------
  if (CI == F) {

    # Baseline hazard
    s0 = s0[order(s0, decreasing = T)]

    # Uncureprob
    if (link == "logit")  uncureprob <- exp(gamma %*% t(newZ)) / (1 + exp(gamma %*% t(newZ)))
    if (link == "probit") uncureprob <- pnorm(gamma %*% t(newZ))

    # Suncure
    suncure = array(0, dim = c(nobs, nx))
    ebetaX = exp(model$beta %*% t(newX))
    for (i in 1:nx) {
      suncure[, i] = s0^ebetaX[i]
    }
    suncure <- suncure[order(suncure[, 1], decreasing = T), ]

    # Spop
    spop = array(0, dim = c(nobs, nrow(newX)))
    for (i in 1:nobs) {
      for (j in 1:nrow(newX)) {
        spop[i, j] = uncureprob[j] * suncure[i, j] + (1 - uncureprob[j])
      }
    }
    spop    <- spop[order(spop[, 1], decreasing = T), ]
  }


  # Output ------------------------------------------------------------------
  if (CI == F) {
    if (type == "basesurv")   structure(list(basesurv = s0), class = "tvpred")
    if (type == "suncure")    structure(list(suncure = suncure), class = "tvpred")
    if (type == "spop")       structure(list(spop = spop), class = "tvpred")
    if (type == "uncureprob") structure(list(uncureprob = uncureprob), class = "tvpred")
  }
}
