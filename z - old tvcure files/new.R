predict_g <- function (object, newX, newZ, ci = .95, nsim = 1000, model = c("ph", "aft"), ...) {
  call <- match.call()
  if (!inherits(object, "tvcure"))
    stop("Object must be results of tvcure")
  s0 = as.matrix(object$s, ncol = 1)
  n = nrow(s0)
  if (is.vector(newZ))
    newZ = as.matrix(newZ)
  #newZ = cbind(1, newZ)
  if (is.vector(newX))
    newX = as.matrix(newX)
  names(object$gamma)[1] <- "intercept_"
  #g_sims           <- coreSim::b_sim(mu = object$gamma, Sigma = object$vcovg, nsim = nsim)
  #g_zg_sims        <- coreSim::linear_systematic(g_sims, newZ)
  pr_function       <- function(x) 1 / (1 + exp(-x))
  g_pp              <- qi_builder(mu = object$gamma, Sigma = object$vcovg,
                                 ci = ci, nsim = nsim, FUN = pr_function,
                                 newdata = newZ, slim = T, original_order = T)
  #pr_sims          <- pr_function(g_zg_sims$ls_)
  # pr_sims_mat      <- matrix(pr_sims, nrow = nsim, ncol = nrow(newZ))
  # pr_median        <- apply(pr_sims_mat, 2, mean)
  # pr_min           <- apply(pr_sims_mat, 2, min)
  # pr_max           <- apply(pr_sims_mat, 2, max)
  # pr_slim          <- cbind(newZ, pr_min, pr_median, pr_max)
  uncureprob       <- g_pp$qi_median
  # uncureprobb = exp(object$gamma %*% t(cbind(1,newZ)))/(1 + exp(object$gamma %*% t(cbind(1,newZ))))
  # uncureprobc = 1/(1+exp(-(object$gamma %*% t(cbind(1,newZ)))))
  scure = array(0, dim = c(n, nrow(newX)))
  t = array(0, dim = c(n, nrow(newX)))
  spop = array(0, dim = c(n, nrow(newX)))
  if (model == "ph") {
    exp_function      <- function(x) exp(x)
    ebetax            <- qi_builder(mu = object$beta, Sigma = object$vcovb,
                                   ci = ci, nsim = nsim, FUN = exp_function,
                                   newdata = newX, slim = T, original_order = T)

    #ebetaX2 = exp(object$beta %*% t(newX))
    for (i in 1:nrow(newZ)) {
      scure[, i] = s0^ebetax$qi_median[i]
    }
    for (i in 1:n) {
      for (j in 1:nrow(newX)) {
        spop[i, j] = uncureprob[j] * scure[i, j] + (1 - uncureprob[j])
      }
    }
    pred = cbind(spop, Time = object$Time)
  }
  #browser()
  # if (model == "aft") {
  #   newX = cbind(1, newX)
  #   ebetaX = exp(object$beta %*% t(newX))
  #   for (i in 1:nrow(newX)) {
  #     t[, i] = ebetaX[i] * exp(object$error)
  #   }
  #   for (i in 1:n) {
  #     for (j in 1:nrow(newX)) {
  #       spop[i, j] = uncureprob[j] * s0[i] + (1 - uncureprob[j])
  #     }
  #   }
  #   prd = cbind(spop = spop, Time = t)
  # }
  structure(list(call = call, uncureprob = uncureprob, uncureprob_l = g_pp$qi_min, uncureprob_u = g_pp$qi_max, pred = pred, pred_l = , pred_u =  s0 = s0),
            class = "predicttvcure")
}
