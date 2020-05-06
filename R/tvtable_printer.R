footnote <- function(x) {
  fn <- c("here is a really long example footnote that should stretch across the rows", rep("", ncol(x) - 1))
  fn <- rbind(x, fn)
  rownames(fn) <- NULL
  return(fn)
}

tvtable_xtable <- function(tab, ...) {
  require(xtable)
  t <- as.data.frame(tab)
  # if (!is.null(footnote)) {
  #   fn <- list(pos = list(0), command = tab$footnote)
  #   fn$pos[[1]] <- c(nrow(t))
  #   fn$command = paste("\\hline \\footnotesize{", tab$footnote, "}\n", sep = " ")
  # }
  tvxtab <- xtable(t, ...)
  # tvxtab <- list(tvxtab = tvxtab, fn = fn)
}
printer <- function(x, ...) {
  xtable::print.xtable(x,
                       booktabs = T,
                       sanitize.text.function = identity,
                       include.rownames = F,
                       include.colnames = F,
                       # add.to.row = x$fn,
                       hline.after = c(-1, 2, nrow(x) - 2), ...)
}
