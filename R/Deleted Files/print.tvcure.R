print.summary.tvcure <- function(model) {

  if (model$var) {
    gamma <- array(model$gamma, c(length(model$gamma), 4))
    rownames(gamma) <- model$gnames
    colnames(gamma) <- c("Estimate", "Std.Error", "Z value", "Pr(>|Z|)")
    gamma[, 2] <- model$g_sd
    gamma[, 3] <- model$g_zvalue
    gamma[, 4] <- model$g_pvalue

    beta <- array(model$beta, c(length(model$beta), 4))
    rownames(beta) <- model$bnames
    colnames(beta) <- c("Estimate", "Std.Error", "Z value", "Pr(>|Z|)")
    beta[, 2] <- model$b_sd
    beta[, 3] <- model$b_zvalue
    beta[, 4] <- model$b_pvalue
  } else {
    gamma <- array(model$gamma, c(length(model$gamma), 1))
    rownames(gamma) <- model$gnames
    colnames(gamma) <- "Estimate"

    beta <- array(model$beta, c(length(model$beta), 1))
    rownames(beta) <- model$bnames
    colnames(beta) <- "Estimate"
  }

  cat("Call:\n")
  cat("\nCure probability model$:\n")
  print(gamma)
  cat("\n")
  cat("\nFailure time distribution model$:\n")
  print(beta)
  invisible(model)
}
