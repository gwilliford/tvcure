function(predobj, nsims = 1000)
# if (ci == TRUE) {
 #x <- model.coups

# Errors
  if (!inherits(model, "tvcure"))
    stop("Model must be a tvcure object")

# Assign shortcut values

# Draw samples of beta and gamma
  nsims <- 1000
  beta <- model$beta
  gamma <- model$gamma
  X <- model$X
  Z <- model$X
  Status <- model$Status
  Coef_smplb <- MASS::mvrnorm(n = nsims, mu = beta,  Sigma = model$vcovb)
  Coef_smplg <- MASS::mvrnorm(n = nsims, mu = gamma, Sigma = model$vcovg)
  # b_idx <- 1:x$n.terms$duration
  # g_idx <- (max(b_idx) + 1):(max(b_idx) + x$n.terms$risk)

# Calculate uncureprob for simulated values
  if (link == "logit") {
    # simulated based on old data
    # uncureprobsims <- exp(Coef_smplg %*% t(model$Z)) / (1 + exp(Coef_smplg %*% t(model$Z)))
    uncureprobsims <- exp(Coef_smplg %*% t(newZ)) / (1 + exp(Coef_smplg %*% t(newZ)))
  }
  if (link == "probit") {
    #uncureprob <- pnorm(Coef_smplg %*% t(model$Z))
    uncureprob <- pnorm(Coef_smplg %*% t(newZ))
  }

  # Calculate w using the simulated coefficients
  w <- Status
  #wsim <- model$Status + (1 - model$Status) * (uncureprobsims * model$Survival)/((1 - uncureprobsims) + uncureprobsims * model$Survival)

  # Recalculate s0 from simulations
    # TODO Rewrite as a pbapply function
  s0sim <- matrix(nrow = nsims, ncol = model$nobs)
    # Each row is a simulation, each column is a vector of s0sims
  for (j in 1:nsims) {
    s0sim[j, ] <- tvsurv(model$Time, model$Status, cbind(1, newX), Coef_smplb[j, ], w, model$model)$survival
  }
  # S0 - from model
  s0sim_mean <- apply(s0sim, 2, mean)
  s0sim_05 <- apply(s0sim, 2, quantile, 0.05)
  s0sim_95 <- apply(s0sim, 2, quantile, 0.95)

  plot(a[order(Time)], type = "l")
  lines(b[order(Time)], type = "l")
  lines(c[order(Time)], type = "l")
  lines(testpred$sbase[order(Time)], type = "l", col = 3)

  # Calculate suncure
  # ebetaXsim <- exp(Coef_smplb)
  # ebetaXsim <- exp(Coef_smplb * newX)
  # suncuresim = s0sim^ebetaXsim
  #s0sim_mean % ebetaXsim
  #ebetaXmean <- exp(a * t(newX))
  #ebetaX = exp(model$beta %*% t(newX))
  # 1 X 3 by 3 x 1
  ###### I think this works correctly, there's just no variation in teh ddta b/c nonstat sif
  ebetaXsim <- exp(Coef_smplb %*% t(newX))
  suncuresim = array(0, dim = c(nobs, nrow(newX), nsims)) # i = 284, j = 2, k = 1000
  for (k in 1:nsims){
    for (j in 1:nrow(newX)){
      for (i in 1:nobs) {
        suncuresim[i, , k] = s0sim[i, j]^ebetaXsim[i, ]
        s0sim[i, j]^ebetaXsim[i, ]
      }
    }
  }
#
#   sunlist <-
#   for (i in 1:nrow(newX)) {
#     sunlist[i] <- unlist(suncuresim[, , i])
#   }
#
#   suncuresim = array(NA, dim = c(nsims, nobs, nrow(newX))) # i = 1000, j = 284, k = 2
#   for (k in 1:nrow(newX)){
#     for (j in 1:nobs){
#       for (i in 1:nsims) {
#         suncuresim[i, j, k] = s0sim[i, ]^ebetaXsim[i, k]
#         # suncuresim[i, k] = s0sim[i, ]^ebetaXsim[i, k]
#       }
#     }
#   }
#       # # Suncure loop - original
#       # suncure = array(0, dim = c(nobs, nrow(newX)))
#       # for ( in 1:nrow(newX)) {
#       #   suncure[, i] = s0^ebetaX[i]
#       #   # Each row s0 = 1 obs (284 X 1)
#       #   # Each column ebetaX = 1 fakeobs (1 x 2)
#       #   # Output 284 X 2
#       # }
#
#   suncuresim
#
#   spopsims = array(0, dim = c(nobs, nrow(newX), nsims))
#   for (k in 1:nsims) {
#     for (j in 1:nrow(newZ)) {
#       for (i in 1:nobs) {
#         spopsims[i, j, k] = uncureprobsims[, j] * suncuresim[i, j, k] + (1 - uncureprobsims[, j])
#       }
#     }
#   }
#
#
#
#   s0sim[i, ] ^ ebetaXsim[i, ] # produces 284 length vector
#
#
#
#   # spop = array(0, dim = c(nsims, nrow(newX)))
#   # # For each row in the simulation,
#   # for (i in 1:nsims) {
#   #   #for (j in 1:nrow(X)) {
#   #     spop = t(uncureprobsims[i, ]) %*% suncure[i, ] + (1 - uncureprobsims[i, ])
#   #   }
#   # }
#   spopsim <- uncureprobsims * suncuresim + (1 - uncureprobsims)
#
#   # for (i in 1:nsims) {
#   #     sims[, i] <- hazard(ti = t, lambda = lambda[i], cure = cure[i],
#   #         alpha = Alpha[i], out = NULL, dist = x$distr)
#   # }
#   #
#
#   spopmat <- matrix(nrow = nobs, ncol = 3)
#   spopmat[, 2] <- apply(spopsim, 2, mean)
#   spopmat[, 1] <- apply(spopsim, 2, quantile, probs = 0.05)
#   spopmat[, 3]  <- apply(spopsim, 2, quantile, probs = 0.95)
#   colnames(spopmat) <- c("mean", "hi", "lo")
#
# # hmat[, 1] <- ht
# #   hmat[, 2] <- apply(sims, 1, quantile, probs = 0.05)
#   hmat[, 3] <- apply(sims, 1, quantile, probs = 0.95)
#   plot(t, hmat[, 1], type = "l", xlab = "Time",
#       ylab = "Conditional Hazard", ylim = c(0, max(hmat[,
#           3])), ...)
#   lines(t, hmat[, 2], lty = 2)
#   lines(t, hmat[, 3], lty = 2)
# } else {
#     plot(t, ht, type = "l", xlab = "Time", ylab = "Conditional hazard",
#         ylim = c(0, 1.2 * max(ht)), ...)

  }
