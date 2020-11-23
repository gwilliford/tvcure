summary.tvcure <- function(model) {
  o <- model
  if (model$var == T) {
    gmat <- cbind(model$gamma, model$g_sd, model$g_zvalue, model$g_pvalue)
    bmat <- cbind(model$beta, model$b_sd, model$b_zvalue, model$b_pvalue)
    rownames(gmat) <- model$gnames
    rownames(bmat) <- model$bnames
    colnames(gmat) <- c("GLM Coefficients", "Standard Error", "Z-Score", "p-value")
    colnames(bmat) <- c("Hazard Coefficients", "Standard Error", "Z-Score", "p-value")
    model$bootcomp
  } else {
    gmat <- as.matrix(model$gamma)
    bmat <- as.matrix(model$beta)
    rownames(gmat) <- model$gnames
    rownames(bmat) <- model$bnames
    colnames(gmat) <- "GLM Coefficients"
    colnames(bmat) <- "Hazard Coefficients"
  }

  cat("Call:\n")
  print(model$call); cat("", sep="\n\n")
  print(gmat)  ; cat("", sep="\n\n")
  print(bmat)  ; cat("", sep="\n\n")
  cat("n =", model$nobs, "with", model$nfail, "failures\n", sep = " ")
  cat("Model converged in", model$emrun, "iterations\n", sep = " ")
  if (model$var == T) cat("Variance estimates based on", model$bootcomp, "successful boostrap replications\n", sep = " ")
}
