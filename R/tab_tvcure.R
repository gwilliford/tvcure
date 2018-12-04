#' @param model Model of class tvcure
#' @param format Specifies format of the resulting table. Wide presents incidence and latency results side-by-side with standard errors presented in separate columns. Long presents coefficient estimates side-by-side with standard errors beneath them.
#' @param digits Number of digits to round coefficients and standard errors to
tvtable <- function(model, qi = "se", stars = T, digits = 3) {
  gamma <- round(model$gamma, digits)
  beta  <- round(model$beta, digits)
  gse   <- round(model$g_sd, digits)
  bse   <- round(model$b_sd, digits)
  gz    <- round(model$g_zvalue, digits)
  bz    <- round(model$b_zvalue, digits)
  gpval <- round(model$g_pvalue, digits)
  bpval <- round(model$b_pvalue, digits)
  if (stars == T) {
    gstar <- gtools::stars.pval(gpval)
    bstar <- gtools::stars.pval(bpval)
  } else {
    gstar <- NULL
    bstar <- NULL
  }
  gnames <- model$gnames
  bnames <- model$bnames
  allnames <- unique(c(model$gnames, model$bnames))

  gsevec <- vector(length = length(gamma))
  bsevec <- vector(length = length(beta))
  gzvec <- vector(length = length(gamma))
  bzvec <- vector(length = length(beta))
  gpvalvec <- vector(length = length(gamma))
  bpvalvec <- vector(length = length(beta))
  for (i in 1:length(gamma)) {
    gsevec[i]   <- capture.output(cat("(", gse[i], ")", gstar[i], sep = ""))
    gzvec[i]    <- capture.output(cat("(", gz[i], ")", gstar[i], sep = ""))
    gpvalvec[i] <- capture.output(cat("(", gpval[i], ")", gstar[i], sep = ""))
  }
  for (i in 1:length(beta)) {
    bsevec[i]   <- capture.output(cat("(", bse[i], ")", bstar[i], sep = ""))
    bzvec[i]    <- capture.output(cat("(", bz[i], ")", bstar[i], sep = ""))
    bpvalvec[i] <- capture.output(cat("(", bpval[i], ")", bstar[i], sep = ""))
  }

  if (qi == "se") {
    gmat <- as.data.frame(rbind(gamma, gsevec))
    bmat <- as.data.frame(rbind(beta, bsevec))
  }
  if (qi == "zscore") {
    gmat <- as.data.frame(rbind(gamma, gzvec))
    bmat <- as.data.frame(rbind(beta, bzvec))
  }
  if (qi == "pvalue") {
    gmat <- as.data.frame(rbind(gamma, gpval))
    bmat <- as.data.frame(rbind(beta, bpval))
  }

  colnames(gmat) <- gnames
  colnames(bmat) <- bnames

  fullmat <- merge(gmat, bmat, all = T, sort = F)
  fullmat <- fullmat[c(gnames)]
  fullmat <- t(fullmat)
  colnames(fullmat) <- c("Incidence Coef.", "S.E.", "Hazard Coef.", "S.E.")
  #fullmat <- round(fullmat, digits)
  for (i in 1:nrow(fullmat)) {
    fullmat[i, 1] <- ifelse(is.na(fullmat[i, 1]), "", fullmat[i, 1])
    fullmat[i, 2] <- ifelse(is.na(fullmat[i, 2]), "", fullmat[i, 2])
    fullmat[i, 3] <- ifelse(is.na(fullmat[i, 3]), "", fullmat[i, 3])
    fullmat[i, 4] <- ifelse(is.na(fullmat[i, 4]), "", fullmat[i, 4])
  }

  #long1 <- as.vector(rbind(fullmat[, 1], fullmat[, 2]))
  #long2 <- as.vector(rbind(fullmat[, 3], fullmat[, 4]))
  #long  <- cbind(long1, long2)
  #colnames(long) <- c("GLM", "Hazard")
  #rownames(long) <- as.vector(rbind(rownames(fullmat), rep("", length(allnames))))

  #stacked <- c("GLM Estimates", long1, "Hazard Estimates", long2)
  #rownames(stacked) <-

  #browser()
  #if (format == "wide")    return(fullmat)
  #if (format == "long")    return(long)
  #if (format == "stacked") return(stacked)
  # Todo
    # option for sorting variables
    # option for multiple models
    # option for qi
    # stars on standard errors
  return(fullmat)
}
