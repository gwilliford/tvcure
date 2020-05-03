footnote <- function(x) {
  fn <- c("here is a really long example footnote that should stretch across the rows", rep("", ncol(x) - 1))
  fn <- rbind(x, fn)
  rownames(fn) <- NULL
  return(fn)
}
tvtable_xtable <- function(tvtable, ...) {
  require(xtable)
  t <- as.data.frame(tvtable$table)
  if (!is.null(footnote)) {
    fn <- list(pos = list(0), command = tvtable$footnote)
    fn$pos[[1]] <- c(nrow(t))
    fn$command = paste("\\hline \\footnotesize{", tvtable$footnote, "}\n", sep = " ")
  }
  x <- list(xtab = xtable(t, ...), fn = fn)
}
printer <- function(x, ...) {
  xtable::print.xtable(x$xtab, include.rownames = F, add.to.row = x$fn, hline.after = c(-1,0), ...)
}
