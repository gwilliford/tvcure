tvtable <- function(..., qi = c("se", "pvalue", "zscore"), stars = T, digits = 3,
                    modnames = NULL, varlist = NULL, footnote = NULL)
{
  # Set up
  # if (format != "long") stop("Only one model may be specified if format is set to wide. /n")
  if (sum(names(varlist) == "") > 0) stop("Names must be supplied for all variables contained in varlist.")
  models <- list(...)
  len = length(models)
  `%notin%` <- Negate(`%in%`)
  class <- lapply(models, class)
  if (sum(class %notin% c("tvcure", "coxph")) > 0) stop("Models must be of class coxph or tvcure.")
  qi <- match.arg(qi)

  tabnames <- list()
  mn <- list()
  # Extract cell entries and variable names
  for (i in 1:length(models)) {

    # Extract coefficients and qis
    model <- models[[i]]
    if (class(model) == "coxph") {
      beta  <- round(coef(model), digits)
      bse   <- round(sqrt(diag(model$var)), digits)
      bz    <- round(beta/bse, digits)
      bpval <- round((1 - pnorm(abs(bz))) * 2, digits)
      bnames <- attr(terms(model), "term.labels")
      # assign(paste0("tabnames", i), bnames)
      nc = 1
    } else {
      beta  <- round(model$beta, digits)
      bse   <- round(model$b_sd, digits)
      bz    <- round(model$b_zvalue, digits)
      bpval <- round(model$b_pvalue, digits)
      bnames <- model$bnames

      gamma <- round(model$gamma, digits)
      gse   <- round(model$g_sd, digits)
      gz    <- round(model$g_zvalue, digits)
      gpval <- round(model$g_pvalue, digits)
      gnames <- model$gnames

      if (qi == "se") qivec2 <- round(model$g_sd, digits)
      if (qi == "zscore") qivec2 <- round(model$g_pvalue, digits)
      if (qi == "pvalue") qivec2 <- round(model$g_zvalue, digits)
      nc = 2
    }
    if (qi == "se")     qivec <- bse
    if (qi == "zscore") qivec <- bz
    if (qi == "pvalue") qivec <- bpval

    # Stars
    browser()
    if (stars) {
      bstar <- gtools::stars.pval(bpval)
      if (nc == 2) gstar <- gtools::stars.pval(gpval)
      for (j in 1:length(qivec)) {
        qivec[j] <- capture.output(cat("(", qivec[j], ")", bstar[j], sep = ""))
        if (nc == 2) qivec2[j] <- capture.output(cat("(", qivec2[j], ")", gstar[j], sep = ""))
      }
    }

    # Create list of variable names in model
    bn = as.vector(rbind(bnames, ""))
    for (j in seq(2, length(bn), 2)) {
      bn[j] <- as.vector(rbind(paste0("qi_", bn[j - 1])))
    }
    if (nc == 2) {
      gn = as.vector(rbind(gnames, ""))
      for (j in seq(2, length(gn), 2)) {
        gn[j] <- as.vector(rbind(paste0("qi_", gn[j - 1])))
      }
    }

    # Create table
    tab <- cbind(bn, as.vector(rbind(beta, qivec)))
    colnames(tab)[1] <- "vn"
    colnames(tab)[2] <- "Hazard Coef."
    tabnames[[i]] <- bn
    if (nc == 2) {
      g <- cbind(gn, as.vector(rbind(gamma, qivec2)))
      colnames(g)[1] <- "vn"
      tab <- merge(g, tab, by = "vn", sort = F, all = T)
      tabnames[[i]] <- c(bn, gn)
      colnames(tab)[2:3] <- c("Incidence Coef.", "Hazard Coef.")
    }
    colnames(tab)[1] <- "vn"
    assign(paste0("tab", i), tab)

    # Create model names
    if (is.null(modnames)) {
      if (nc == 1) {
        mn[[i]] <- paste0("Model " , i,)
      } else {
        mn[[i]] <- paste0("\\", "multicolumn{2}{c}{", "Model ", i, "}")
      }
    } else {
      if (nc == 1) {
        mn[[i]] <- modnames[i]
      } else {
        mn[[i]] <- c(modnames[i], "blank")
      }
    }

    # Create summary stats
    if (nc == 1) {
      svec = vector(length = 2)
      svec[1] <- nrow(model$y)
      svec[2] <- model$nevent
    } else {
      svec = vector(length = 4)
      svec[1] <- model$nobs
      svec[2] <- model$nfail
    }
    assign(paste0("svec", i), svec)
  }

  # Order by varlist
  allnames <- unique(unlist(tabnames))
  if (is.null(varlist)) {
    varnames <- allnames
    varlabs <- allnames
  } else {
    index <- match(varlist, allnames)
    index2 <- !is.na(index)
    index3 <- names(varlist[index2])
    index <- index[!is.na(index)]
    vn <- allnames[index]
    if (length(vn) == 1) c(vn, "")
    vn <- as.vector(rbind(vn, ""))
    for (j in seq(2, length(vn), 2)) {
      vn[j] <- as.vector(rbind(paste0("qi_", vn[j - 1])))
    }
    nout <- allnames[allnames %notin% vn]
    varnames <- c(vn, nout)

    varlabs <- varnames
    dex <- as.vector(rbind(index3, ""))
    varlabs[1:length(dex)] <- dex
  }

  # Combine tables
  tdf  <- matrix(varnames, ncol = 1)
  colnames(tdf)[1] <- "vn"
  # colnames(ttab)[1] <- names(tab[i])
  for (i in 1:len) {
    tab <- eval(parse(text = paste0("tab", i)))
    tdf <- merge(tdf, tab, by = "vn", all.x = T, sort = F)
  }
  ind <- match(varnames, tdf[, 1])
  tdf <- tdf[ind, ]
  tdf[, 1] <- varlabs
  colnames(tdf)[1] <- c(" ")
  cn <- colnames(tdf)
  ind <- startsWith(cn, "Hazard")
  cn <- replace(cn, ind, "Hazard")
  ind2 <- startsWith(cn, "Incidence")
  cn <- replace(cn, ind2, "Incidence")
  cn[1] <- ""
  colnames(tdf) <- NULL
  for (j in seq(2, nrow(tdf), 2)) {
    tdf[j, 1] <- ""
  }
  tdf <- as.matrix(tdf)

  mn2 <- c("", unlist(mn))
  mn2 <- replace(mn2, mn2 == "blank", "")

  # test <- c(1, 2, 3, 4)
  svec <- vector()
  for (i in 1:len) {
    svec <- c(svec, eval(parse(text = paste0("svec", i))))
  }
  sumtab <- matrix(svec, nrow = 2)
  sumtab <- cbind(c("Number of Observations", "Number of Failures"), sumtab)

  ftab <- rbind(mn2, cn, tdf, sumtab)
  rownames(ftab) <- NULL
  ftab
}
