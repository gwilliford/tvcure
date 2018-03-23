 tab_tvcure <- function(object, qi = c("se", ""), digits = 3) {
  o <- object
  gamma <- o$gamma
  beta  <- o$beta
  gse   <- o$g_sd
  bse   <- o$b_sd
  glen  <- length(gamma)
  blen  <- length(beta)
  gnames <- o$gnames
  bnames <- o$bnames
  #allnames <- unique(c(o$gnames, o$bnames))
  #tab      <- matrix(nrow = max(glen, blen) * 2, ncol = 2)

  #blanks   <- rep("", length(allnames))
  #tabnames <- as.vector(rbind(allnames, blanks))
  #rownames(tab) <- tabnames

  gmat <- as.data.frame(rbind(gamma, gse))
  colnames(gmat) <- gnames
  #gmat <- round(gmat, digits)

  bmat <- as.data.frame(rbind(beta, bse))
  colnames(bmat) <- bnames
  #bmat <- round(bmat, digits)

  fullmat <- merge(gmat, bmat, all = T, sort  = F)
  fullmat <- fullmat[c(gnames)]
  fullmat <- t(fullmat)
  colnames(fullmat) <- c("GLM Coefficients", "GLM S.E.", "Hazard Coefficients", "Hazard S.E.")
  #fullmat <- apply(fullmat, 2, sprintf, fmt = "%0.3f")
  #fullmat <- sprintf("%0.3f", fullmat)
  fullmat <- round(fullmat, digits)
  for (i in 1:nrow(fullmat)) {
    fullmat[i, 1] <- ifelse(is.na(fullmat[i, 1]), "", fullmat[i, 1])
    fullmat[i, 2] <- ifelse(is.na(fullmat[i, 2]), "", paste0("(", fullmat[i, 2], ")"))
    fullmat[i, 3] <- ifelse(is.na(fullmat[i, 3]), "", fullmat[i, 3])
    fullmat[i, 4] <- ifelse(is.na(fullmat[i, 4]), "", paste0("(", fullmat[i, 4], ")"))
  }

  #fullmat[, 4] <- paste0("(", format(unlist(fullmat[, 4])),")")
  #fullmat <- ifelse("NA", "", fullmat)
  #fullmat <- ifelse("(NA)", "", fullmat)
  #long1 <- as.vector(cbind(fullmat[, 1], fullmat[, 2]), length = nrow(fullmat) * 2)
  long1 <- as.vector(rbind(fullmat[, 1], fullmat[, 2]))
  long2 <- as.vector(rbind(fullmat[, 3], fullmat[, 4]))
  long  <- cbind(long1, long2)
  colnames(long) <- c("GLM", "Hazard")
  rownames(long) <- as.vector(rbind(rownames(fullmat), rep("", length(allnames))))
  #long2 <- matrix(as.vector(rbind(fullmat[, 3], fullmat[, 4])), nrow = nrow(fullmat) * 2, ncol = 1)
  #gvec <- vector(length = length(gamma) * 2)
  #bvec <- vector(length = length(beta) * 2)
  #while (i in 1:length(gvec)) {
    #gvec[i] <- gamma
    #gvec[i + 1] <- gse
    #gvec <- cat("(", gvec, ")", sep = "")

  #}

  #for (i in 1:length(bvec)) {
    # bvec[i] <- beta
    # bvec[i + 1] <- bse
    # bvec <- cat("(", bvec, ")", sep = "")

  #}

  # option for qi
  # stars on standard errors
  return(list(widetab = fullmat, longtab = long))
}
