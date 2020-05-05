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
      gn = as.vector(rbind(gnames, ""))
      for (j in seq(2, length(gn), 2)) {
        gn[j] <- as.vector(rbind(paste0("qi_", gn[j - 1])))
      }
    }
    # tab <- as.data.frame(as.vector(rbind(beta, qivec)))
    # rownames(tab) <- bn
    # colnames(tab) <-;
    tab <- cbind(bn, as.vector(rbind(beta, qivec)))
    colnames(tab)[2] <- "Hazard Coef."
    tabnames[[i]] <- bn
    if (nc == 2) {
      g <- cbind(gn, as.vector(rbind(gamma, qivec2)))
      colnames(g)[1] <- "bn"
      tab <- merge(tab, g, sort = F, all = T)
      tabnames[[i]] <- c(bn, gn)
      colnames(tab)[2:3] <- c("Incidence Coef.", "Hazard Coef.")
    }
    colnames(tab)[1] <- "vn"
    assign(paste0("tab", i), tab)

    if (is.null(modnames)) {
      if (nc == 1) {
        mn[[i]] <- paste0("Model " , i)
      } else {
        mn[[i]] <- c(paste0("Model ", i), "blank")
      }
    } else {
      if (nc == 1) {
        mn[[i]] <- modnames[i]
      } else {
        mn[[i]] <- c(modnames[i], "blank")
      }
    }
    # if (is.null(modnames)) {
    #   if (nc == 1) {
    #     mn[[i]] <- paste0("Model ", i)
    #   } else {
    #     mn[[i]] <- c(paste0("Model ", i), "")
    #   }
    # } else if (modnames == T) {
    #     mn[[i]] <- modnames[i]
    # } else if (modnames == F) {
    #   mn[[i]] <- NULL
    # }

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
  if (is.null(varlist)){
    varnames <- allnames
    varlabs <- allnames
  } else {
    index <- match(varlist, allnames)
    index2 <- !is.na(index)
    index3 <- names(varlist[index2])
    index <- index[!is.na(index)]
    vn <- allnames[index]

    #
    # names(varlist[index])
    #

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
    # for (i in f:length(dex)) {
    #
    # }
    # index2 <- match(varnames, varlist)
    # names(varlist)
    # varlabs[length(vn)] <- n
    # varnames[vn] <-
    #   ind <- names(varlist[
    #     match(vn, varlist)]
    #     ff)
    #   ind[is.na(ind)]
    #   # vn[match(varlist, vn)]
  }

  # Combine tables
  tdf  <- matrix(varnames, ncol = 1)
  colnames(tdf)[1] <- "vn"
  # colnames(ttab)[1] <- names(tab[i])
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
  # colnames(tdf)[1] <- c(" ")
  colnames(tdf)[1] <- c(" ")
  cn <- colnames(tdf)
  ind <- startsWith(cn, "Hazard")
  cn <- replace(cn, ind, "Hazard")
  ind2 <- startsWith(cn, "Infcidence")
  cn <- replace(cn, ind2, "Incidence")
  cn[1] <- ""
  # colnames(tdf)[startsWith(colnames(tdf), "incidence")] <- "Incidence"
  colnames(tdf) <- NULL
  for (j in seq(2, nrow(tdf), 2)) {
    tdf[j, 1] <- ""
  }
  tdf <- as.matrix(tdf)

  mn2 <- c("", unlist(mn))
  mn2 <- replace(mn2, mn2 == "blank", "")

  # test <- c(1, 2, 3, 4)
  svec <- vector()
  for(i in 1:len) {
    svec <- c(svec, eval(parse(text = paste0("svec", i))))
  }
  sumtab <- matrix(svec, nrow = 2)
  sumtab <- cbind(c("Number of Observations", "Number of Failures"), sumtab)
  # colnames(sumtab) <- colnames(tdf)

  ftab <- rbind(mn2, cn, tdf, sumtab)
  rownames(ftab) <- NULL
  tabout <- list(tab = ftab, class = "tvtable")
  print(tabout$tab)
}
