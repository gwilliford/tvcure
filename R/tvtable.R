#' @param model A model of class tvcure
#' @param format Specifies format of the resulting table. Wide presents incidence and latency results side-by-side with standard errors presented in separate columns. Long presents incidence and latency coefficients side-by-side side-by-side with standard errors beneath them.
#' @param qi Specifies the quantity of interest to display in addition to coefficients (standard errors, p-values, or z-scores).
#' @param stars Logical value indicating whether significance stars should be printed
#' @param digits Number of digits to display past the decimal point
tvtable <- function(model, format = c("wide", "long"), qi = c("se", "pvalue", "zscore"), stars = T, digits = 3)
    if (class == "tvcure") {
      gamma <- round(model$gamma, digits)
      beta  <- round(model$beta, digits)
      gse   <- round(model$g_sd, digits)
      bse   <- round(model$b_sd, digits)
      gz    <- round(model$g_zvalue, digits)
      bz    <- round(model$b_zvalue, digits)
      gpval <- round(model$g_pvalue, digits)
      bpval <- round(model$b_pvalue, digits)
      gnames <- model$gnames
      bnames <- model$bnames
      allnames <- unique(c(model$gnames, model$bnames))
    }
    qi <- match.arg(qi)
    if (stars == T) {
      gstar <- gtools::stars.pval(gpval)
      bstar <- gtools::stars.pval(bpval)
    } else {
      gstar <- NULL
      bstar <- NULL
    }
    nobs = model$nobs
    nfail = model$nfail
    emat <- matrix(ncol = 4, nrow = 2)
    emat[ , 1] <- c(nobs, nfail)
    format <- match.arg(format)

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
    # fullmat <- fullmat[c(gnames)]
    # fullmat <- fullmat[,allnames]
    fullmat <- t(fullmat)
    fullmat <- fullmat[allnames, ]
    fullmat <- rbind(as.matrix(fullmat), emat)
    rownames(fullmat)[nrow(fullmat) - 1] <- "Number of Observations"
    rownames(fullmat)[nrow(fullmat)] <- "Number of Failures"

    # if (rownames(test) == rownames(a)) {
    #   for (n in 1:nrow(test)) {
    #     rownames[n](test) <- a[n]
    #   }
    # }
    # for (n in 1:nrow(test)) {
    #   if (rownames(test)[n] %in% rownames(a)) {
    #     rownames(test)[n] <- a[1, rownames(test)[n] == rownames(a)]
    #   }
    # }

    if (qi == "se") {
      colnames(fullmat) <- c("Incidence Coef.", "Std. Error", "Hazard Coef.", "Std. Error")
    }
    if (qi == "pvalue") {
      colnames(fullmat) <- c("Incidence Coef.", "P-value", "Hazard Coef.", "P-value")
    }
    if (qi == "zscore") {
      colnames(fullmat) <- c("Incidence Coef.", "Z-score", "Hazard Coef.", "Z-score")
    }
    #fullmat <- round(fullmat, digits)
    for (i in 1:nrow(fullmat) - 2) {
      fullmat[i, 1] <- ifelse(is.na(fullmat[i, 1]), "", fullmat[i, 1])
      fullmat[i, 2] <- ifelse(is.na(fullmat[i, 2]), "", fullmat[i, 2])
      fullmat[i, 3] <- ifelse(is.na(fullmat[i, 3]), "", fullmat[i, 3])
      fullmat[i, 4] <- ifelse(is.na(fullmat[i, 4]), "", fullmat[i, 4])
    }

    long1 <- as.vector(rbind(fullmat[, 1], fullmat[, 2]))
    long1 <- long1[1:(length(long1) - 4)]
    long2 <- as.vector(rbind(fullmat[, 3], fullmat[, 4]))
    long2 <- long2[1:(length(long2) - 4)]
    long <- cbind(long1, long2)
    matrix(long, ncol = 2)
    long <- rbind(long, emat[, 1:2])
    colnames(long) <- c("Incidence Coef.", "Hazard Coef.")
    longnames <- as.vector(rbind(rownames(fullmat), rep("", length(allnames) + 2)))
    rownames(long) <- longnames[-c(length(longnames) - 2, length(longnames))]
    # long <- long[-c(nrow(long), nrow(long) - 2), ]

    # stacked <- as.matrix(c("", long1, "", long2[3:length(long2)], emat[, 1:2]))
    # # stackmat <- matrix(ncol = 1, nrow = length(stacked) + 2)
    # stackmat <- as.data.frame(stacked[1:(nrow(stacked) - 2), 1])
    # # colnames(stacked) <- c("Incidence Coef.", "Hazard Coef.")
    # stacknames <-
    # rownames(stackmat) <- c("Incidence Coef.",
    #                         as.vector(rbind(rownames(fullmat), rep("", length(allnames)))),
    #                         "Hazard Coef.",
    #                         as.vector(rbind(rownames(fullmat)[2:length(allnames)],
    #                         rep("", length(allnames) - 1))), "Number of Observations",
    #                         "Number of Failures")
    # stacknames <- c("Incidence Coefficients", longnames[1:(nrow(long) - 4)], "Hazard Coefficients", longnames[1:(nrow(long) - 4)], "Number of Observations", "Number of Failures")

  if (format == "wide")    return(fullmat)
  if (format == "long")    return(long)
  if (format == "stacked") return(stackmat)
}

