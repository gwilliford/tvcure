# tables <- c("cox", "curepart")
# t1 <- tvtable.coxph(cox, format = "long")
# t2 <- tvtable(curepart, format = "long")
# tvtable.combine(c("t1", "t2"))

tvtable_combine <- function(tables, format = c("wide", "long"),
                          qi = c("se", "pvalue", "zscore"), stars = T, digits = 3,
                          modnum = T, varlist = NULL) {
  require(dplyr)
  `%notin%` <- Negate(`%in%`)
  len <- length(tables)
  # nr <- vector(length = len)
  # tcol <- vector()
  allnames <- vector()
  for (i in 1:len) {
    tab <- eval(parse(text = tables[i]))
    assign(paste0("stats", i), tab[c(nrow(tab) - 1):nrow(tab), ])
    tab <- tab[-c(c(nrow(tab) - 1):nrow(tab)), ]
    assign(paste0("tab", i), tab)
    assign(paste0("varnames", i), tab[, 1])
    if (i == 1) allnames <- tab[, 1]
    else allnames <- c(allnames, tab[, 1])
    # nr[i] <- nrow(tab)
    # tcol[i] <- ncol(tab) - 1
  }
  # maxrow <- max(nr)
  allnames <- unique(allnames)
  allnames <- allnames[allnames != ""]
  allnames <- as.vector(rbind(allnames, rep("", length(allnames))))
  for (i in seq(2, length(allnames), by = 2)) {
    allnames[i] <- paste0("sd_", allnames[i - 1])
  }
  mout1 <- tab1
  for (i in 2:len) {
    tab <- eval(parse(text = paste0("tab", i)))
    # Get the indices of variables in 2 that are in 1
    index <- match(eval(parse(text = paste0("varnames", i - 1))), eval(parse(text = paste0("varnames", i))))
    index <- index[index != 2]
    index2 <- index + 1
    vec <- as.vector(rbind(index, index2))
    # vec <- vec[-length(vec)]
    matin <- tab[vec, ]
    indout <- c(1:nrow(tab))
    indout <- indout[indout %notin% vec == T]
    matout <- tab[indout, ]
    assign(paste0("mout", i), rbind(matin, matout))
  }

browser()

  rn <- as.vector("")
  for (i in 1:len) {

    # Assign model numbers
    tab <- eval(parse(text = paste0("mout", i)))
    if (modnum) {
      if (ncol(tab) == 2) rni <- paste0("(", i, ") \\newline")
      if (ncol(tab) == 3) rni <- c(paste0("(", i, ") \\newline"), "")
      # if (ncol(tab) == 2) {
      #   colnames(tab)[2] <- paste0("(", i, ") \\newline ", colnames(tab)[2])
      # } else {
      #   colnames(tab)[2] <- paste0("(", i, ") \\newline ", colnames(tab)[2])
      #   colnames(tab)[3] <- paste0("\\hspace{0cm} \\newline ", colnames(tab)[3])
      # }
      # cn <- colnames(tab)
      # tab <- rbind(cn, tab)
      # row.names(tab) <- NULL
      # rbind("", tab[, 2]
      # if (ncol(tab == 2)) {
      #   rbind(c(paste0("(", i, "))
      # }
      rn <- c(rn, rni)
    }


    addmat <- matrix("", nrow = length(allnames) + 2, ncol = ncol(tab))
    addmat[, 1] <- c(allnames, "Number of Obs.", "Number of Failures")
    # index <- match(eval(parse(text = paste0("varnames", i - 1))), eval(parse(text = paste0("varnames", i))))

    # tab <- rbind(tab, addmat)
    index <- match(addmat[, 1], tab[, 1])
    addmat <- tab[index, ]
    addmat[(nrow(addmat) - 1):nrow(addmat), ] <- eval(parse(text = paste0("stats", i)))
    # tab <- rbind(tab, eval(parse(text = paste0("stats", i))))
    assign(paste0("ftab", i), addmat)
    if (i == 1) {
      ftab1 <- cbind(c(allnames, "Number of Obs.", "Number of Failures."), addmat)
      ftabfinal <- ftab1[, -2]
    } else {
      ftabfinal <- cbind(ftabfinal, eval(parse(text = paste0("ftab", i)))[, -1])
    }
  }
  if (modnum) {
    cn <- colnames(ftabfinal)
    ftabfinal <- rbind(cn, ftabfinal)
    rownames(ftabfinal) <- NULL
    colnames(ftabfinal) <- rn
  }
  # browser()
  index <- ftabfinal[, 1]
  ftabfinal[, 1] <- replace(ftabfinal[, 1], startsWith(ftabfinal[, 1], "sd_"), "")

    # Get the indices of variables in 1 that are not in 2
    # Get the indices of variables in 2 that are not in 1
        #match(eval(parse(text = paste0("varnames", i - 1))), allnames)
  return(ftabfinal)
}



#### Code for parsing both model types at once

  #   } else stop("Models must be a coxph or tvcure object.")
  #   assign(paste0("table", i), tabx)
  #   assign(paste0("varnames", i), tabx[, 1])
  #   if (i == 1) allnames <- tabx[, 1]
  #   else allnames <- c(allnames, tabx[, 1])
  # }
  # allnames <- unique(allnames)
  # allnames
  # browser()
  #
  # allnames[order(varlist)]
  # allnames <- dplyr::select(allnames, -c('""'))
  # allnames <- allnames[allnames != ""]
  # index <- allnames %notin% eval(parse(text = paste0("varnames", i)))
  # missnames <- allnames[index]

# varlist <- c("capchange", "tie", "onedem5")
# varnames <- c("Capability Change", "Tie", "Joint Democracy")
#
#   allnames <- vector()
#   for (i in 1:len) {
#     for (i == 1) {
#       allnames1 <- eval(parse(text = paste0("varnames", i)))
#     } for (i = 2:len) {
#       assign(paste0("allnames", i), c(allnames, eval(parse(text = paste0("varnames", i))))
#       allnames[i] <- c(allnames[i - 1], allnames[i])
#     }
#
#   for (i in 1:len) cbind(eval(parse(text = paste0("allnames", i))))
#
#   }
#
#
#
#
#
#   }
