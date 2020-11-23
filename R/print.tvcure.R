print.tvcure <- function (x, var, ...) {
  if (!is.null(cl <- x$call)) {
    cat("Call:\\n")
    dput(cl)
  }
  cat("\\nCure probability model:\\n")
  if (var) {
    gamma <- array(x$gamma, c(length(x$gamma), 4))
    rownames(gamma) <- x$gnames
    colnames(gamma) <- c("Estimate", "Std.Error", "Z value", "Pr(>|Z|)")
    gamma[, 2] <- x$g_sd
    gamma[, 3] <- x$g_zvalue
    gamma[, 4] <- x$g_pvalue
  }
  if (!var) {
    gamma <- array(x$gamma, c(length(x$gamma), 1))
    rownames(gamma) <- x$gnames
    colnames(gamma) <- "Estimate"
  }
  print(gamma)
  cat("\\n")
  cat("\\nFailure time distribution model:\\n")
  if (var) {
    beta <- array(x$beta, c(length(x$beta), 4))
    rownames(beta) <- x$bnames
    colnames(beta) <- c("Estimate", "Std.Error", "Z value", "Pr(>|Z|)")
    beta[, 2] <- x$b_sd
    beta[, 3] <- x$b_zvalue
    beta[, 4] <- x$b_pvalue
  }
  if (!var) {
    beta <- array(x$beta, c(length(x$beta), 1))
    rownames(beta) <- x$bnames
    colnames(beta) <- "Estimate"
  }
  print(beta)
  invisible(x)
}
