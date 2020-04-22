tvtable_coxph <- function(model, format = c("wide", "long"),
                          qi = c("se", "pvalue", "zscore"), stars = T, digits = 3,
                          varlist = NULL)
{
  format <- match.arg(format)
  beta  <- round(coef(model), digits)
  bse   <- round(sqrt(diag(model$var)), digits)
  bz    <- round(beta/bse, digits)
  bpval <- round((1 - pnorm(abs(bz))) * 2, digits)

  # Variable names
  bnames <- attr(terms(model), "term.labels")
  allnames <- bnames
  if (is.null(varlist)) varnames <- allnames
  else {
    i2 <- match(varlist, allnames)
    if (sum(is.na(i2)) > 0) stop(paste0("\n\tVariable ", varlist[is.na(i2)], " not found."))

    if (length(allnames) == length(varlist)) {
      allnames <- allnames[i2]
    } else if (length(allnames) > length(varlist)) {
      allnames <- c(allnames[i2], allnames[-i2])
    }
    if (length(allnames) < length(varlist)) {
      notfound <- varlist[(varlist %in% allnames == F) == T]
    } else notfound <- NULL
    if (!is.null(notfound)) stop(paste0("\n\tVariable ", notfound, " not found."))
    names(varlist)[names(varlist) == ""] <- varlist[names(varlist) == ""]
    varnames <- allnames
    varnames[allnames %in% varlist] <- names(varlist)
  }

  # Summary stats
  nobs = nrow(model$y)
  nfail = model$nevent
  emat <- matrix(ncol = 3, nrow = 2)
  emat[ , 1] <- c("Number of Observations", "Number of Failures")
  emat[ , 2] <- c(nobs, nfail)

  # Beta Coefficients and QI
  qi <- match.arg(qi)
  if (stars == T) bstar <- gtools::stars.pval(bpval)
  else bstar <- NULL
  bsevec <- vector(length = length(beta))
  bzvec <- vector(length = length(beta))
  bpvalvec <- vector(length = length(beta))
  for (i in 1:length(beta)) {
    bsevec[i]   <- capture.output(cat("(", bse[i], ")", bstar[i], sep = ""))
    bzvec[i]    <- capture.output(cat("(", bz[i], ")", bstar[i], sep = ""))
    bpvalvec[i] <- capture.output(cat("(", bpval[i], ")", bstar[i], sep = ""))
  }
  if (qi == "se") {
    bmat <- as.data.frame(rbind(beta, bsevec))
  }
  if (qi == "zscore") {
    bmat <- as.data.frame(rbind(beta, bzvec))
  }
  if (qi == "pvalue") {
    bmat <- as.data.frame(rbind(beta, bpval))
  }

  # Combine matrices
  # colnames(bmat) <- bnames
  # fullmat <- bmat
  fullmat <- t(bmat)
  # fullmat <- fullmat[allnames, ]
  # rownames(fullmat)[nrow(fullmat) - 1] <- "Number of Observations"
  # rownames(fullmat)[nrow(fullmat)] <- "Number of Failures"
  fullmat <- fullmat[allnames, ]
  fullmat <- cbind(varnames, fullmat)
  fullmat <- rbind(as.matrix(fullmat), emat)
  rownames(fullmat) <- NULL
  if (qi == "se") {
    colnames(fullmat) <- c("", "Hazard Coef.", "Std. Error")
  }
  if (qi == "pvalue") {
    colnames(fullmat) <- c("", "Hazard Coef.", "P-value")
  }
  if (qi == "zscore") {
    colnames(fullmat) <- c("", "Hazard Coef.", "Z-score")
  }
  for (i in 1:nrow(fullmat) - 2) {
    fullmat[i, 1] <- ifelse(is.na(fullmat[i, 1]), "", fullmat[i, 1])
    fullmat[i, 2] <- ifelse(is.na(fullmat[i, 2]), "", fullmat[i, 2])
  }

  # Create long matrix
  if (format == "long") {
    long <- as.vector(rbind(fullmat[, 2], fullmat[, 3]))
    long <- long[1:(length(long) - 4)]
    long <- matrix(long, ncol = 1, nrow = length(long))
    long <- rbind(long, emat[1, 2], emat[2, 2])
    longnames <- as.vector(rbind(varnames, rep("", length(varnames))))
    long <- cbind(c(longnames, "Number of Obs.", "Number of Failures"), long)
    colnames(long) <- c("", "Hazard Coef.")
  }

  if (format == "wide") return(fullmat)
  if (format == "long") return(long)
}

