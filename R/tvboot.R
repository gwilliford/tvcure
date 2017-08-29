tvboot <- function(nboot, nbeta, ngamma, survtype, Time, Start, Stop, Status, X,
                   Z, gnames, bnames, offsetvar, g, beta, model, link, emmax,
                   eps, firthlogit, firthcox, survobj, n, parallel) {

  # Progress Bar
    #bootpb <- progress_bar$new(
    #  format = "Bootstrap progress [:bar] :percent, :elapsed elapsed, approximately :eta remaining",
    #  total = nboot, clear = F, width= 100)

  # Format data for bootstrap function
    if (survtype=="right") {
      tempdata <- cbind(Time, Status, X, Z)
    }
    if (survtype=="counting") {
      tempdata <- cbind(Start, Stop, Status, X, Z)
    }
    data1 <- subset(tempdata, Status == 1)
    data0 <- subset(tempdata, Status == 0)
    n1 <- nrow(data1)
    n0 <- nrow(data0)

  #bootpb$tick(0)
  #Sys.sleep(3)

  if (parallel == T) {
    bootres <- foreach(i = 1:nboot, .packages = c("survival")) %dopar% {
      set.seed(i)
      id1 <- sample(1:n1, n1, replace = T)
      id0 <- sample(1:n0, n0, replace = T)
      bootdata <- rbind(data1[id1, ], data0[id0, ])
      bootZ <- bootdata[, gnames]
      bootX <- as.matrix(cbind(rep(1, n), bootdata[, bnames]))
      if (survtype=="right") {
        bootsurv <- Surv(bootdata[, 1], bootdata[, 2])
        boottime <- bootdata[, 1]
        bootstatus <- bootdata[, 2]
      } # close right loop
      if (survtype=="counting") {
        bootsurv <- Surv(bootdata[, 1], bootdata[, 2], bootdata[, 3])
        bootstart <- bootdata[, 1]
        bootstop <- bootdata[, 2]
        bootstatus <- bootdata[, 3]
        boottime <- bootstop
      } # close counting loop

      #tryCatch(
        bootfit <- tvem(Start=bootstart, Stop=bootstop, Status=bootstatus,
                        Time=boottime, X=bootX, Z=bootZ, offsetvar=offsetvar,
                        g=g, beta=beta, model=model, link=link, emmax=emmax,
                        eps=eps, firthlogit=firthlogit, firthcox=firthcox,
                        survobj=bootsurv, survtype=survtype)#,
      #  error = function(e) e
      #)

    # Export results
    list(bootfitg = bootfit$g, bootfitb = bootfit$latencyfit)
    } # Close foreach loop
  } # close if bracket

  # Combine results from bootstraps into matrices
  g_boot <- matrix(rep(0, nboot * ngamma), nrow = nboot)
  b_boot <- matrix(rep(0, nboot * (nbeta)), nrow = nboot)
  for (i in 1:nboot) {
    g_boot[i,] <- bootres[[i]]$bootfitg
    b_boot[i,] <- bootres[[i]]$bootfitb
  } # close matrix loop

  # Calculate variance and standard errors
  cat("Variance estimation completed, wrapping up...\n")
  g_var <- apply(g_boot, 2, var)
  b_var <- apply(b_boot, 2, var)
  g_sd <- sqrt(g_var)
  b_sd <- sqrt(b_var)
  varout <- list(g_var=g_var, b_var=b_var, g_sd=g_sd, b_sd=b_sd)
} # close function
