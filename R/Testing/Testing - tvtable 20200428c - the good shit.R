#### Tvtable
# put in error for 1 model for wide

at1_t <- tvtable_tvcure(at1, format = "long")
at2_t <- tvtable_tvcure(at2, format = "long")
at3_t <- tvtable_tvcure(at3, format = "long")
at4_t <- tvtable_tvcure(at4, format = "long")
at_table <- tvtable_combine(c("t1", "t2", "t3", "t4"), format = "long")
printer(at_table, file = "./tables/at_table.tex")

ag1_t <- tvtable_tvcure(ag1, format = "long")
ag2_t <- tvtable_tvcure(ag2, format = "long")
ag3_t <- tvtable_tvcure(ag3, format = "long")
ag4_t <- tvtable_tvcure(ag4, format = "long")
ag_table <- tvtable_combine(c("ag1_t", "ag2_t", "ag3_t", "ag4_t"), format = "long")
printer(ag_table, file = "./tables/ag_table.tex")

agiss1_t <- tvtable_tvcure(agiss1, format = "long")
agiss2_t <- tvtable_tvcure(agiss2, format = "long")
agiss3_t <- tvtable_tvcure(agiss3, format = "long")
agiss4_t <- tvtable_tvcure(agiss4, format = "long")
agiss_table <- tvtable_combine(c("agiss1_t", "agiss2_t", "agiss3_t", "agiss4_t"), format = "long")
printer(agiss_table, file = "./tables/agiss_table.tex")

clterm1_t <- tvtable_tvcure(clterm1, format = "long")
clterm2_t <- tvtable_tvcure(clterm2, format = "long")
clterm3_t <- tvtable_tvcure(clterm3, format = "long")
clterm4_t <- tvtable_tvcure(clterm4, format = "long")
clterm_table <- tvtable_combine(c("clterm1_t", "clterm2_t", "clterm3_t", "clterm4_t"), format = "long")
printer(clterm_table, file = "./tables/clterm_table.tex")

midterm1_t <- tvtable_tvcure(midterm1, format = "long")
midterm2_t <- tvtable_tvcure(midterm2, format = "long")
midterm3_t <- tvtable_tvcure(midterm3, format = "long")
midterm4_t <- tvtable_tvcure(midterm4, format = "long")
midterm_table <- tvtable_combine(c("midterm1_t", "midterm2_t", "midterm3_t", "midterm4_t"), format = "long")
printer(midterm_table, file = "./tables/midterm_table.tex")

tvtable_combine(c("t1", "t2", "t3", "t4"), format = "long")
t <- tvtable_combine(c("t1", "t2"), format = "long")

at_table <- tvtable_combine(c("t1", "t2", "t3", "t4"), format = "long")
ag_table <- tvtable_combine(c("ag1", "ag2", "ag3", "ag4"), format = "long")
at_table <- tvtable_combine(c("t1", "t2", "t3", "t4"), format = "long")
at_table <- tvtable_combine(c("t1", "t2", "t3", "t4"), format = "long")
at_table <- tvtable_combine(c("t1", "t2", "t3", "t4"), format = "long")

# library(xtable)
# print(xtable(t), 
#       sanitize.rownames.function = getOption("xtable.sanitize.rownames.function",
#                                              sanitize.text.function),
#       
# varnames <- t[, 1]  
# a <- t[, -1]     
# rownames(a) <- varnames
# a <- as.data.frame(a)

z
san <- function(x) {
  beta <- replace(rownames(x)[seq(1:nrow(x))], startsWith(alpha, "sd_"), "")
  # rownames(x) <- beta
  rownames(x) <- beta
}
  
y <- xtable(z)
print(y)
san(y)

v <- print(y, sanitize.rownames.function = names(y) <- NULL)

u <- xtableMatharray(t)
rownames(u) <- beta

xtable.tvtable <- function(table) {
  print(xtable(table$table), include.rownames = F)
}
xtable.tvtable(y)

charlie <- print(xtable(data.frame(row = beta, data.frame(x)),include.rownames = FALSE))

footnote <- function(x) {
  fn <- c("here is a really long example footnote that should stretch across the rows", rep("", ncol(x) - 1))
  fn <- rbind(x, fn)
  rownames(fn) <- NULL
  return(fn)
}
fn <- footnote(t)
fn <- list(pos = list(0), footnote = NULL)

tz <- list(table = t, footnote =  c("here is a really long example footnote that should stretch across the rows", rep("", ncol(t) - 1)))


tz$footnote <- "here is a really long example footnote that should stretch across the rows"
t2 <- tvtable_tvcure(at2, format = "long", )
t <- tvtable_combine(c("t1", "t2"), format = "long", footnote = "abcd")

printer <- function(tvtable, ...) {
  t <- as.data.frame(tvtable$table)
  browser()
  if (!is.null(fn)) {
    fn <- list(pos = list(0), command = tvtable$footnote)
    fn$pos[[1]] <- c(nrow(t))
    fn$command = paste("\\hline \\footnotesize{", tvtable$footnote, "}\n", sep = " ")
  }
  x <- xtable(t)
  print(x, include.rownames = F, add.to.row = fn, hline.after = c(-1,0), ...)
}
printer(tz)
printer(t)

comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(indPctChgCC))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize Note: * signifies number of 
properties used was 35 or less.}\n", sep = ""))
