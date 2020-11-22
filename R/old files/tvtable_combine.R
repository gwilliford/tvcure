# tables <- c("cox", "curepart")
# t1 <- tvtable.coxph(cox, format = "long")
# t2 <- tvtable(curepart, format = "long")
# tvtable.combine(c("t1", "t2"))

tvtable_combine <- function(tables, format = c("wide", "long"),
                          qi = c("se", "pvalue", "zscore"), stars = T, digits = 3,
                          modnum = T, modlist = NULL, varlist = NULL,
                          footnote = NULL) {

  allnames <- vector()
  for (i in 1:len) {
      # *** {This code is an artifact of having other functions put obs and fialures in
    tab <- eval(parse(text = tables[i]))
    # tab <- tables[i]
    assign(paste0("stats", i), tab[c(nrow(tab) - 1):nrow(tab), ])
    tab <- tab[-c(c(nrow(tab) - 1):nrow(tab)), ]
      # } ***
    tabnames <- tab[, 1]
    for (j in seq(2, nrow(tab), by = 2)) {
      tabnames[j] <- paste0("sd_", tabnames[j-1])
    }
    tab[, 1] <- tabnames
    assign(paste0("tabnames", i), tabnames)
    # assign(paste0("varnames", i), tab[, 1])
    # if (i == 1) allnames <- tab[, 1]
    # else allnames <- c(allnames, tab[, 1])
    allnames <- c(allnames, tabnames)
    # nr[i] <- nrow(tab)
    # tcol[i] <- ncol(tab) - 1
    assign(paste0("tab", i), tab)
  }
  # maxrow <- max(nr)
  allnames <- unique(allnames)

  # if (is.null(varlist)) {
  #   varnames <- allnames
  # } else {
  #   i2 <- match(varlist, allnames)
  #     if (length(allnames) < length(varlist)) {
  #       notfound <- varlist[(varlist %in% allnames == F) == T]
  #     } else notfound <- NULL
  # }
  # else {
  #   i2 <- match(varlist, allnames)

  #   #if (sum(is.na(i2)) > 0) stop(paste0("\n\tVariable ", varlist[is.na(i2)], " not found."))
  #   if (length(allnames) == length(varlist)) {
  #     allnames <- allnames[i2]
  #   } else if (length(allnames) > length(varlist)) {
  #     allnames <- c(allnames[i2], -allnames[i2])
  #   }
  #   if (length(allnames) < length(varlist)) {
  #     notfound <- varlist[(varlist %in% allnames == F) == T]
  #   } else notfound <- NULL
  #   if (!is.null(notfound)) stop(paste0("\n\tVariable ", notfound, " not found."))
  #   names(varlist)[names(varlist) == ""] <- varlist[names(varlist) == ""]
  #   varnames <- allnames
  #   varnames[allnames %in% varlist] <- names(varlist)
  # }
  tab <- eval(parse(text = paste0("tab", i)))
  # assign(paste0("tabnames", i), tab)
  # Get the indices of variables in 2 that are in 1
  index <- match(eval(parse(text = paste0("varnames", i - 1))), eval(parse(text = paste0("varnames", i))))
  # index <- index[index != 2]
  # index2 <- index + 1
  # vec <- as.vector(rbind(index, index2))
  # vec <- vec[-length(vec)]
  matin <- tab[index, ]
  indout <- c(1:nrow(tab))
  indout <- indout[indout %notin% index == T]
  matout <- tab[indout, ]
  assign(paste0("mout", i), rbind(matin, matout))


  # allnames <- allnames[allnames != ""]
  # allnames <- as.vector(rbind(allnames, rep("", length(allnames))))
  # for (i in seq(2, length(allnames), by = 2)) {
  #   allnames[i] <- paste0("sd_", allnames[i - 1])
  # }
  mout1 <- tab1
  for (i in 2:len) {
    tab <- eval(parse(text = paste0("tab", i)))
    # assign(paste0("tabnames", i), tab)
    # Get the indices of variables in 2 that are in 1
    index <- match(eval(parse(text = paste0("tabnames", i - 1))), eval(parse(text = paste0("tabnames", i))))
    # index <- index[index != 2]
    # index2 <- index + 1
    # vec <- as.vector(rbind(index, index2))
    # vec <- vec[-length(vec)]
    matin <- tab[index, ]
    indout <- c(1:nrow(tab))
    indout <- indout[indout %notin% index == T]
    matout <- tab[indout, ]
    assign(paste0("mout", i), rbind(matin, matout))
  }

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
    colnames(ftabfinal) <- paste(rn, ftabfinal[1, ])
    ftabfinal <- ftabfinal[-1, ]
  }
  # browser()
  # index <- ftabfinal[, 1]
  # ftabfinal[1, 1] <- replace(ftabfinal[1, 1], startsWith(ftabfinal[1, 1], "sd_"), "")

    # Get the indices of variables in 1 that are not in 2
    # Get the indices of variables in 2 that are not in 1
    index <- match(varlist, allnames)
    for (i in length(index)) ftabfinal[index, 1] <- names(varlist)[i]
    allnames %in% varlist

        #match(eval(parse(text = paste0("varnames", i - 1))), allnames)
  if (stars) {
    footnote = paste0(footnote, " *** \n p < .001, ** p < .01, * p < .05, . < .10.")
  }

  ftabfinal[, 1] <- varnames
  for (i in seq(2, nrow(ftabfinal) - 2, by = 2)) {
    ftabfinal[i , 1] <- paste0("")
  }
browser()
  out <- list(table = ftabfinal, footnote = footnote)
  return(out)
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
