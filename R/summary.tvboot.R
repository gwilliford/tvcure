summary.tvcure <- function(object) {
  o <- object
  gmat <- cbind(o$gamma, o$g_sd, o$g_zvalue, o$g_pvalue)
  bmat <- cbind(o$beta, o$b_sd, o$b_zvalue, o$b_pvalue)
  rownames(gmat) <- o$gnames
  rownames(bmat) <- o$bnames
  colnames(gmat) <- c("GLM Coefficients", "Standard Error", "Z-Score", "p-value")
  colnames(bmat) <- c("Hazard Coefficients", "Standard Error", "Z-Score", "p-value")

  o$nfail
  o$nobs

  o$nboot
  o$bootcomp

  o$emmax
  o$emrun

cat("Call:\n")
print(o$call); cat("", sep="\n\n")
print(gmat)  ; cat("", sep="\n\n")
print(bmat)  ; cat("", sep="\n\n")
cat("n =", o$nobs, "with", o$nfail, "failures\n", sep = " ")
cat("Model converged in", o$emrun, "iterations\n", sep = " ")
cat("Variance estimates based on", o$nboot, "boostrap replications\n", sep = " ")
}
