tvtable <- function(..., qi = c("se", "pvalue", "zscore"), stars = T, digits = 3,
                    modnum = F, varlist = NULL, footnote = NULL)
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
  # Extract cell entries and variable names
  for (i in 1:length(models)) {
    model <- models[[i]]
    if (class(model) == "coxph") {
      beta  <- round(coef(model), digits)
      bse   <- round(sqrt(diag(model$var)), digits)
      bz    <- round(beta/bse, digits)
      bpval <- round((1 - pnorm(abs(bse))) * 2, digits)
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
      gz    <- round(model$g_pvalue, digits)
      gpval <- round(model$g_zvalue, digits)
      if (qi == "se") qivec2 <- round(model$g_sd, digits)
      if (qi == "zscore") qivec2 <- round(model$g_pvalue, digits)
      if (qi == "pvalue") qivec2 <- round(model$g_zvalue, digits)
      gnames <- model$gnames
      # assign(paste0("tabnames", i), unique(c(model$gnames, model$bnames)))
      nc = 2
    }

    if (qi == "se")     qivec <- bse
    if (qi == "zscore") qivec <- bz
    if (qi == "pvalue") qivec <- bpval
    bn = as.vector(rbind(bnames, ""))
    for (j in seq(2, length(bn), 2)) {
      bn[j] <- as.vector(rbind(paste0("qi_", bn[j - 1])))
    }
    if (nc == 2) {
      gn = as.vector(rbind(gnames[[i]], ""))
      for (j in seq(2, length(gn), 2)) {
        gn[j] <- as.vector(rbind(paste0("qi_", gn[j - 1])))
      }
    }
    # tab <- as.data.frame(as.vector(rbind(beta, qivec)))
    # rownames(tab) <- bn
    # colnames(tab) <-;
    tab <- cbind(bn, as.vector(rbind(beta, qivec)))
    colnames(tab)[2] <- "hazard"
    tabnames[[i]] <- bn
    if (nc == 2) {
      g <- cbind(gn, as.vector(rbind(gamma, qivec2)))
      tab <- merge(tab, g)
      tabnames[[i]] <- c(bn, gn)
      colnames(tab)[2:3] <- c("incidence", "hazard")
    }
    colnames(tab)[1] <- "vn"
    assign(paste0("tab", i), tab)

    #
    # # sumtab <- matrix(nrow = 2, ncol = nc)
    # if (nc == 1) {
    #   sumtab[1, ] <- nrow(model$y)
    #   sumtab[2, ] <- model$nevent
    # } else {
    #   sumtab[, 1] <- model$nobs
    #   sumtab[, 2] <- model$nobs
    #   assign(paste0("st", i)) <- c(nobs, nfail)
    # }

    if (nc == 1) {
      svec = vector(length = 2)
      svec[1] <- nrow(model$y)
      svec[2] <- model$nevent
      # assign(paste0("nobs", i), nrow(model$y))
      # assign(paste0("nfail", i), model$nevent)
      # assign(paste0("st", i)) <- c(nobs, nfail)
    } else {
      svec = vector(length = 4)
      svec[1] <- model$nobs
      svec[2] <- model$nfail
      # assign(paste0("nobs", i), model$nobs)
      # assign(paste0("nfail", i), model$nfail)
      # assign(paste0("st", i)) <- c(nobs, nfail, "", "")
    }
    assign(paste0("svec", i), svec)
    # sumtab[i, ] <- cbind("Number of Observations")
  }

  # Order by varlist
  allnames <- unique(unlist(tabnames))
  browser()
  if (is.null(varlist)){
    varnames <- allnames
    varlabs <- allnames
  } else {
    index <- match(varlist, allnames)
    vn <- allnames[index]
    if (length(vn) == 1) c(vn, "")
    vn <- as.vector(rbind(vn, ""))
    for (j in seq(2, length(vn), 2)) {
      vn[j] <- as.vector(rbind(paste0("qi_", vn[j - 1])))
    }
    nout <- allnames[allnames %notin% vn]
    varnames <- c(vn, nout)
    ind <- names(varlist[match(vn, varlist)])
    varlabs <- varnames
    names(varlist[ind])
    varlabs[match(varlist, vn)] <- na.omit(names(varlist[ind]))
      # vn[match(varlist, vn)]
  }

  browser()
  # Combine tables
  tdf  <- matrix(varnames, ncol = 1)
  colnames(tdf)[1] <- "vn"
  # colnames(ttab)[1] <- names(tab[i])
  #
  for (i in 1:len) {
    tab <- eval(parse(text = paste0("tab", i)))
    #colnames(tab)[-1] <- c("vn", rep(i)
    # tab <- cbind(rownames(tab), as.matrix(tab))
    # colnames(tab)[1] <- "vn"
    # rownames(tab) <- NULL
    # tab <- eval(parse(text = paste0("tab", i)))
    # colnames(tab) <- NULL
    # fulltab[] <- merge(tdf, ttab,  all = T)\
    tdf <- merge(tdf, tab, by = "vn", all.x = T, sort = F)
  }
  # ind <- match(tdf[, 1], varnames)
  ind <- match(varnames, tdf[, 1])
  tdf <- tdf[ind, ]
  tdf[, 1] <- varlabs
  colnames(tdf)[startsWith(colnames(tdf), "hazard")] <- "Hazard"
  colnames(tdf)[startsWith(colnames(tdf), "incidence")] <- "Incidence"
  colnames(tdf)[1] <- c(" ")
  rownames(tdf) <- NULL
  for (j in seq(2, nrow(tdf), 2)) {
    tdf[j, 1] <- ""
  }

  # test <- c(1, 2, 3, 4)
  svec <- vector()
  for(i in 1:len) {
    svec <- c(svec, eval(parse(text = paste0("svec", i))))
  }
  sumtab <- matrix(svec, nrow = 2)
  sumtab <- cbind(c("Number of Observations", "Number of Failures"), sumtab)
  colnames(sumtab) <- colnames(tdf)
  tdf <- rbind(tdf, sumtab)

  ftab <- list(tab = tdf, class = "tvtable")
  print(ftab$tab)
}
