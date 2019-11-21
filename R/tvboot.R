tvboot <- function(nboot, nbeta, ngamma, survtype, Time, Start, Stop, Status, X,
                   Z, gnames, bnames, offset, gamma, beta, link, emmax,
                   eps, brglm, firthcox, survobj, n, parallel) {

  # Progress Bar
    pb <- txtProgressBar(max = nboot, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

  # Format data for bootstrap function
    if (survtype=="right") {
      tempdata <- cbind(Time, Status, 1, X, Z)
    }
    if (survtype=="counting") {
      tempdata <- cbind(Start, Stop, Status, 1, X, Z)
    }
    data1 <- subset(tempdata, Status == 1)
    data0 <- subset(tempdata, Status == 0)
    n1 <- nrow(data1)
    n0 <- nrow(data0)

  # Set up parallel processing
  if (parallel == F) {
    registerDoSEQ()
  }

  # Sample and Estimate
  bootres <- foreach(i = 1:nboot, .packages = c('survival','logistf'),
                     .options.snow = opts, .errorhandling = 'remove') %dopar% {
    for(i in 1:nboot) {
      try({
      id1 <- sample(1:n1, n1, replace = T)
      id0 <- sample(1:n0, n0, replace = T)
      bootdata <- rbind(data1[id1, ], data0[id0, ])
      bootZ <- bootdata[, gnames]
      bootX <- as.matrix(cbind(bootdata[, bnames]))
      if (survtype=="right") {
        bootsurv <- Surv(bootdata[, 1], bootdata[, 2])
        boottime <- bootdata[, 1]
        bootstatus <- bootdata[, 2]
      }
      if (survtype=="counting") {
        bootsurv <- Surv(bootdata[, 1], bootdata[, 2], bootdata[, 3])
        bootstart <- bootdata[, 1]
        boottime <- bootdata[, 2]
        bootstatus <- bootdata[, 3]
      }

      bootfit <- tvem(Time = boottime, Status = bootstatus,
                      X = bootX, Z = bootZ, offset,
                      gamma, beta, link, emmax,
                      eps, brglm, firthcox,
                      survobj = bootsurv, survtype)#, error = function(e) NULL)
      break
      }, silent = F) #close try function
    } # close for loop
    list(bootfitg = bootfit$gamma, bootfitb = bootfit$latencyfit)
  } # close foreach loop

  # Combine results from bootstraps into matrices
  g_boot <- matrix(rep(0, nboot * ngamma), nrow = nboot)
  b_boot <- matrix(rep(0, nboot * nbeta), nrow = nboot)
  for (i in 1:length(bootres)) {
    g_boot[i,] <- bootres[[i]]$bootfitg
    b_boot[i,] <- bootres[[i]]$bootfitb
  }

  # Create vcov matrix
  vcovg <- cov(g_boot)
  colnames(vcovg) <- gnames
  rownames(vcovg) <- gnames
  vcovb <- cov(b_boot)
  colnames(vcovb) <- bnames
  rownames(vcovb) <- bnames

  # Calculate variance and standard errors
  g_var <- apply(g_boot, 2, var)
  b_var <- apply(b_boot, 2, var)
  g_sd <- sqrt(g_var)
  b_sd <- sqrt(b_var)
  varout <- list(g_var = g_var, b_var = b_var, g_sd = g_sd, b_sd = b_sd,
                 vcovg = vcovg, vcovb = vcovb, bootcomp = length(bootres))
}
