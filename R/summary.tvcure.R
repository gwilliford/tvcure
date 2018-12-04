summary.tvcure <- function(object) {
o <- object
if (object$var == T) {
  gmat <- cbind(o$gamma, o$g_sd, o$g_zvalue, o$g_pvalue)
  bmat <- cbind(o$beta, o$b_sd, o$b_zvalue, o$b_pvalue)
  rownames(gmat) <- o$gnames
  rownames(bmat) <- o$bnames
  colnames(gmat) <- c("GLM Coefficients", "Standard Error", "Z-Score", "p-value")
  colnames(bmat) <- c("Hazard Coefficients", "Standard Error", "Z-Score", "p-value")
  o$nboot
  o$bootcomp
} else {
  gmat <- as.matrix(o$gamma)
  bmat <- as.matrix(o$beta)
  rownames(gmat) <- o$gnames
  rownames(bmat) <- o$bnames
  colnames(gmat) <- "GLM Coefficients"
  colnames(bmat) <- "Hazard Coefficients"
}

cat("Call:\n")
print(o$call); cat("", sep="\n\n")
print(gmat)  ; cat("", sep="\n\n")
print(bmat)  ; cat("", sep="\n\n")
cat("n =", o$nobs, "with", o$nfail, "failures\n", sep = " ")
cat("Model converged in", o$emrun, "iterations\n", sep = " ")
if (object$var == T) cat("Variance estimates based on", o$bootcomp, "successful boostrap replications\n", sep = " ")
}
