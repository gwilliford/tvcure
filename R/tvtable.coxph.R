b <- function(model, format = c("wide", "long"), qi = c("se", "pvalue", "zscore"), stars = T, digits = 3) {
  beta  <- round(coef(model), digits)
  bse   <- round(sqrt(diag(model$var)), digits)
  bz    <- round(beta/bse, digits)
  bpval <- round((1 - pnorm(abs(bz))) * 2, digits)
  bnames <- attr(terms(model), "term.labels")
  allnames <- bnames
  nobs = nrow(model$y)
  nfail = model$nevent
  qi <- match.arg(qi)
  if (stars == T) {
    bstar <- gtools::stars.pval(bpval)
  } else {
    bstar <- NULL
  }

emat <- matrix(ncol = 2, nrow = 2)
emat[ , 1] <- c(nobs, nfail)
format <- match.arg(format)

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

colnames(bmat) <- bnames
fullmat <- bmat
fullmat <- t(fullmat)
fullmat <- fullmat[allnames, ]
fullmat <- rbind(as.matrix(fullmat), emat)
rownames(fullmat)[nrow(fullmat) - 1] <- "Number of Observations"
rownames(fullmat)[nrow(fullmat)] <- "Number of Failures"
browser()
if (qi == "se") {
  colnames(fullmat) <- c("Hazard Coef.", "Std. Error")
}
if (qi == "pvalue") {
  colnames(fullmat) <- c("Hazard Coef.", "P-value")
}
if (qi == "zscore") {
  colnames(fullmat) <- c("Hazard Coef.", "Z-score")
}
for (i in 1:nrow(fullmat) - 2) {
  fullmat[i, 1] <- ifelse(is.na(fullmat[i, 1]), "", fullmat[i, 1])
  fullmat[i, 2] <- ifelse(is.na(fullmat[i, 2]), "", fullmat[i, 2])
}

long1 <- as.vector(rbind(fullmat[, 1], fullmat[, 2]))
long1 <- long1[1:(length(long1) - 4)]
long <- long1
long <- matrix(long, ncol = 1, nrow = length(long1))
long <- rbind(long, emat[1, 1], emat[2, 1])
colnames(long) <- "Hazard Coef."
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

