#' @param modname modname of class tvcure
#' @param format Specifies format of the resulting table. Wide presents incidence and latency results side-by-side with standard errors presented in separate columns. Long presents coefficient estimates side-by-side with standard errors beneath them.
#' @param digits Number of digits to display past the decimal point
tvtable <- function(model, format = c("wide"), qi = "se", stars = T, digits = 3) {
  #modellist <- list(...)
  #for (j in 1:length(modellist)) {
    #modname <- modellist[j][[1]]
    modname <- model
    gamma <- round(modname$gamma, digits)
    beta  <- round(modname$beta, digits)
    gse   <- round(modname$g_sd, digits)
    bse   <- round(modname$b_sd, digits)
    gz    <- round(modname$g_zvalue, digits)
    bz    <- round(modname$b_zvalue, digits)
    gpval <- round(modname$g_pvalue, digits)
    bpval <- round(modname$b_pvalue, digits)
    if (stars == T) {
      gstar <- gtools::stars.pval(gpval)
      bstar <- gtools::stars.pval(bpval)
    } else {
      gstar <- NULL
      bstar <- NULL
    }
    gnames <- modname$gnames
    bnames <- modname$bnames
    allnames <- unique(c(modname$gnames, modname$bnames))

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
    #fullmat <- fullmat[c(gnames)]
    #fullmat <- fullmat[,allnames]
    fullmat <- t(fullmat)
    fullmat <- fullmat[allnames,]
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
    for (i in 1:nrow(fullmat)) {
      fullmat[i, 1] <- ifelse(is.na(fullmat[i, 1]), "", fullmat[i, 1])
      fullmat[i, 2] <- ifelse(is.na(fullmat[i, 2]), "", fullmat[i, 2])
      fullmat[i, 3] <- ifelse(is.na(fullmat[i, 3]), "", fullmat[i, 3])
      fullmat[i, 4] <- ifelse(is.na(fullmat[i, 4]), "", fullmat[i, 4])
    }

    long1 <- as.vector(rbind(fullmat[, 1], fullmat[, 2]))
    long2 <- as.vector(rbind(fullmat[, 3], fullmat[, 4]))
    long  <- cbind(long1, long2)
    colnames(long) <- c("Incidence Coef.", "Hazard Coef.")
    rownames(long) <- as.vector(rbind(rownames(fullmat), rep("", length(allnames))))

    stacked <- c("", long1, "", long2[3:length(long2)])
    # stackmat <- matrix(ncol = 1, nrow = length(stacked) + 2)
    stackmat <- matrix(stacked)
    # colnames(stacked) <- c("Incidence Coef.", "Hazard Coef.")
    rownames(stackmat) <- c("Incidence Coef.",
                            as.vector(rbind(rownames(fullmat), rep("", length(allnames)))),
                            "Hazard Coef.",
                            as.vector(rbind(rownames(fullmat)[2:length(allnames)], rep("", length(allnames) - 1))))
  #}
  # browser()
  #   if (format == "wide") {
  #         matname <- paste("fullmat",j, sep = "")
  #         matname0 <- paste("fullmat",j-1, sep = "")
  #     #eval(call("<-", as.name(matname), fullmat))
  #     #if (j > 1) matname <- cbind(matname, matname0)
  #     #fullmat <- matname
  #   }
  #   if (format == "long") {
  #     if (j > 1) {
  #       longold <- longnew
  #     }
  #     longnew <- long
  #     #matname <- paste("rllongmat", j, sep = "")
  #     #eval(call("<-", as.name(matname), longnew))
  #     if (j > 1) {
  #       #matname0 <- paste("longmat", j-1, sep = "")
  #       #eval(call("<-", as.name(matname0), ))
  #       #longfull <- merge(as.data.frame(longold), as.data.frame(longnew), all = T)
  #       #longfull <- Matrix.utils::merge.Matrix(longold, longnew)
  #       cbind(longold, longnew)
  #
  #     }
  #     browser()
  #   }
  # }
  #
  # # if length(modellist) > 1 {
  #   # for(k in 1:length(modellist)) {
  #
  #     # fullmat <- cbind()
  #
  # #}

  if (format == "wide")    return(fullmat)
  if (format == "long")    return(long)
  if (format == "stacked") return(stackmat)
}
