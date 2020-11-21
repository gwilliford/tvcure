# summary.tvcure <- function(object) {
#   o <- object
#   if (object$var == T) {
#     gmat <- cbind(o$parametersgamma, o$parametersg_sd, o$parametersg_zvalue, o$parametersg_pvalue)
#     bmat <- cbind(o$parametersbeta, o$parametersb_sd, o$parametersb_zvalue, o$parametersb_pvalue)
#     rownames(gmat) <- o$varnames$gnames
#     rownames(bmat) <- o$varnames$bnames
#     colnames(gmat) <- c("GLM Coefficients", "Standard Error", "Z-Score", "p-value")
#     colnames(bmat) <- c("Hazard Coefficients", "Standard Error", "Z-Score", "p-value")
#     o$nboot
#     o$bootcomp
#   } else {
#     gmat <- as.matrix(o$parametersgamma)
#     bmat <- as.matrix(o$parametersbeta)
#     rownames(gmat) <- o$varnames$gnames
#     rownames(bmat) <- o$varnames$bnames
#     colnames(gmat) <- "GLM Coefficients"
#     colnames(bmat) <- "Hazard Coefficients"
#   }
#
#   cat("Call:\n")
#   print(o$call); cat("", sep="\n\n")
#   print(gmat)  ; cat("", sep="\n\n")
#   print(bmat)  ; cat("", sep="\n\n")
#   cat("n =", o$nobs, "with", o$nfail, "failures\n", sep = " ")
#   cat("Model converged in", o$emrun, "iterations\n", sep = " ")
#   if (o$options$var == T) cat("Variance estimates based on", o$bootcomp, "successful boostrap replications\n", sep = " ")
# }
