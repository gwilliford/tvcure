tvpred_noci = function(model, type = c("basesurv", "suncure", "spop", "uncureprob"),
         newX = NULL, newZ = NULL, variable = NULL, values = NULL) {

  if (!inherits(model, "tvcure")) stop("Model must be a tvcure object")
  s0 = as.matrix(model$Survival, ncol = 1)
  nobs = nrow(s0)
  beta <- model$beta
  gamma <- model$gamma
  bnames <- model$bnames
  gnames <- model$gnames
  link <- model$link
  Status <- model$Status
  Time <- model$Time[order(model$Time)]
  type = match.arg(type)
  browser()

  if (is.null(newX)) {
    newX = matrix(apply(model$X, 2, median), nrow = 1, ncol = ncol(model$X))
    colnames(newX) = model$bnames
    # newX <- matrix(rep(newX, length(values)), ncol = length(newX), byrow = T)
    # colnames(newX) <- bnames
    # if (!is.null(variable) & !is.null(values))
    #   if (variable %in% bnames) newX[, variable] <- values
  }
  nx <- nrow(newX)

  if (is.null(newZ)) {
    newZ = matrix(apply(model$Z, 2, median), nrow = 1, ncol = ncol(model$Z))
    colnames(newZ) = model$gnames
    # newZ <- matrix(rep(newZ, length(values)), ncol = length(newZ), byrow = T)
    # colnames(newZ) <- gnames
    # if (!is.null(variable) & !is.null(values))
    #   if (variable %in% gnames) newZ[, variable] <- values
  }
  nz <- nrow(newZ)

  if (link == "logit")  uncureprob <- exp(newZ %*% gamma) / (1 + exp(newZ %*% gamma))
  if (link == "probit") uncureprob <- pnorm(newZ %*% gamma)

  suncure = array(0, dim = c(nobs, nx))
  ebetaX = exp(model$beta %*% t(newX))
  for (i in 1:nx) {
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
  list(basesurv = s0, suncure = suncure, spop = spop, uncureprob = uncureprob, time = time)
}
