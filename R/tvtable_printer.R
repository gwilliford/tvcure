footnote <- function(x) {
  fn <- c("here is a really long example footnote that should stretch across the rows", rep("", ncol(x) - 1))
  fn <- rbind(x, fn)
  rownames(fn) <- NULL
  return(fn)
}
printer <- function(tvtable, ...) {
  require(xtable)
  t <- as.data.frame(tvtable$table)
  if (!is.null(footnote)) {
    fn <- list(pos = list(0), command = tvtable$footnote)
    fn$pos[[1]] <- c(nrow(t))
    fn$command = paste("\\hline \\footnotesize{", tvtable$footnote, "}\n", sep = " ")
  }
  x <- xtable(t)
  print(x, include.rownames = F, add.to.row = fn, hline.after = c(-1,0), ...)
}
