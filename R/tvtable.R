#' Create tables for tvcure and related models
#'
#' Returns a table stored as a matrix that can be passed to \link[xtable]{xtable}. Currently only supports models with variance estimates
#'
#\link{[xtable]{xtable}}
#' @param ... One or more models to include in the table. Models may be of class tvcure, coxph, coxphf, or glm.
#' @param qi The quantity of interest to be presented below the coefficients. Can be one of "se" for standard errors, "zscore", or "pvalue".
#' @param varlist An optional named list containing variable names for each variable. Variables will be presented in the order of the list.
#' @param digits The number of digits to which quantities are rounded
#' @param stars Logical value indicating whether to include significance stars. By default, * p < 0.1, ** p < 0.05, *** p < 0.01.
#' @param siglevel If stars = T, specifies significance levels and their associated symbols. See \code{\link[modelsummary]{msummary}} for details
#' @export




tvtable <- function(..., qi = c("se", "zscore", "pvalue"), varlist = NULL,
                    digits = 3, stars = T, siglevel = NULL)
{
  # Set up ---------------------------------------------------------------------
  models <- list(...)
  len = length(models)
  tabnames <- list()
  qi <- match.arg(qi)

  `%notin%` <- Negate(`%in%`)

  class = character(length = length(models))
  for (i in 1:length(models)) {
    if (class(models[[i]])[1] == "glm")    class[i] = "glm"
    if (class(models[[i]])[1] == "coxphf") class[i] = "coxphf"
    if (class(models[[i]])[1] == "tvcure") class[i] = "tvcure"
    if (class(models[[i]])[1] == "coxph")  class[i] = "coxph"
  }

  # Error checking -------------------------------------------------------------
  if (sum(names(varlist) == "") > 0)
    stop("Names must be supplied for all variables contained in varlist.")
  if (sum(class %notin% c("tvcure", "coxph", "coxphf", "glm")) > 0)
    stop("Models must be of class tvcure, coxph, coxphf, or glm.")

  # Extract cell entries and variable names
  for (i in 1:length(models)) {

    # Extract coefficients and qis ---------------------------------------------
    model <- models[[i]]
    if (class[i] == "coxph") {
      beta  <- round(coef(model), digits)
      bse   <- round(sqrt(diag(model$var)), digits)
      bz    <- round(beta/bse, digits)
      bpval <- round((1 - pnorm(abs(bz))) * 2, digits)
      bnames <- attr(terms(model), "term.labels")
      nc = 1
    } else if (class[i] == "coxphf") {
      beta   = round(coef(model), digits)
      bse    = diag(model$var)
      bse    = round(sqrt(bse), digits)
      bz     = round(beta/bse, digits)
      bpval  = round(model$prob, digits)
      bnames = names(model$coefficients)
      nc = 1
    } else if (class[i] == "glm") {
      beta  <- round(coef(model), digits)
      bse   <- round(summary(model)$coefficients[, 2], 3)
      bz    <- round(beta/bse, digits)
      bpval <- round((1 - pnorm(abs(bz))) * 2, digits)
      bnames <- c("Intercept", attr(terms(model), "term.labels"))
      nc = 1
    } else {
      beta   <- round(model$beta, digits)
      bse    <- round(model$b_sd, digits)
      bz     <- round(model$b_zvalue, digits)
      bpval  <- round(model$b_pvalue, digits)
      bnames <- model$bnames

      gamma  <- round(model$gamma, digits)
      gse    <- round(model$g_sd, digits)
      gz     <- round(model$g_zvalue, digits)
      gpval  <- round(model$g_pvalue, digits)
      gnames <- model$gnames

      nc = 2
    }

    if (qi == "se")     qivec <- paste0("(", round(bse,   digits), ")")
    if (qi == "zscore") qivec <- paste0("(", round(bz,    digits), ")")
    if (qi == "pvalue") qivec <- paste0("(", round(bpval, digits), ")")
    if (nc == 2) {
      if (qi == "se")     qivec2 <- paste0("(", round(model$g_sd,     digits), ")")
      if (qi == "zscore") qivec2 <- paste0("(", round(model$g_pvalue, digits), ")")
      if (qi == "pvalue") qivec2 <- paste0("(", round(model$g_zvalue, digits), ")")
    }

    # Add significance stars ---------------------------------------------------
    if (stars) {
      if (is.null(siglevel)) siglevel = c(`*` = 0.1, `**` = 0.05, `***` = 0.01)
      beta = modelsummary:::make_stars(beta, bpval, siglevel)
      if (nc == 2) gamma = modelsummary:::make_stars(gamma, gpval, siglevel)
    }

    # Create list of variable names in model -----------------------------------
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

    # Create tables for each model ---------------------------------------------
    tab <- cbind(bn, as.vector(rbind(beta, qivec)))
    colnames(tab)[1] <- "vn"
    colnames(tab)[2] <- "Hazard Coef."
    tabnames[[i]] <- bn
    if (nc == 2) {
      g <- cbind(gn, as.vector(rbind(gamma, qivec2)))
      colnames(g)[1] <- "vn"
      tab <- merge(g, tab, by = "vn", sort = F, all = T)
      tabnames[[i]] <- c(bn, gn)
      colnames(tab)[2:3] <- c("Risk Coef.", "Hazard Coef.")
    }
    colnames(tab)[1] <- "vn"
    assign(paste0("tab", i), tab)


    # Order by varlist -----------------------------------------------------------
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


    # Create summary stats -------------------------------------------------------
    if (nc == 1) {
      svec = character(length = 2)
      if (class[i] == "coxph") svec[1] <- nrow(model$y)
      if (class[i] == "glm" | class[i] == "coxphf")   svec[1] <- length(model$y)
      if (class[i] == "coxph") svec[2] <- model$nevent
      if (class[i] == "glm") svec[2] <- sum(model$y, na.rm = T)
      if (class[i] == "coxphf") svec[2] <- sum(model$y[, "status"])
    } else {
      svec = character(length = 4)
      svec[1] <- c(model$nobs, "")
      svec[2] <- c(model$nfail, "")
    }
    assign(paste0("svec", i), svec)
  }


  # Combine tables -------------------------------------------------------------
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
  ind2 <- startsWith(cn, "Risk")
  cn <- replace(cn, ind2, "Risk")
  cn[1] <- ""
  colnames(tdf) <- NULL
  for (j in seq(2, nrow(tdf), 2)) {
    tdf[j, 1] <- ""
  }
  tdf <- as.matrix(tdf)


  # # Create model names
  # mn <- list()
  # if (is.null(modnames)) {
  #   if (nc == 1) {
  #     mn[[i]] <- paste0("\\multicolumn{1}{c}{", "Model " , i, "}", sep = "")
  #   } else {
  #     mn[[i]] <- paste0("\\multicolumn{2}{c}{", "Mode l", i, "}", sep = "")
  #   }
  # } else {
  #   if (nc == 1) {
  #     mn[[i]] <- modnames[i]
  #   } else {
  #     mn[[i]] <- c(modnames[i], "blank")
  #   }
  # }
  # mn <- c("", unlist(mn))
  # mn <- replace(mn, mn == "blank", "")
  # test <- c(1, 2, 3, 4)

  svec <- vector()
  for (i in 1:len) {
    svec <- c(svec, eval(parse(text = paste0("svec", i))))
  }
  sumtab <- matrix(svec, nrow = 2)
  sumtab <- cbind(c("Number of Observations", "Number of Failures"), sumtab)
  # sumtab[1, 3] = NA
  # sumtab[2, 3] = NA

  ftab <- rbind(cn, tdf, sumtab) #Mn first if used
  rownames(ftab) <- NULL
  ftab
}
