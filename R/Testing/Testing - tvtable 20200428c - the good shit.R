#### Tvtable
# put in error for 1 model for wide

atgap1_t <- tvtable.tvcure(atgap1, format = "long")
atgap2_t <- tvtable.tvcure(atgap2, format = "long")
atgap3_t <- tvtable.tvcure(atgap3, format = "long")
atgap4_t <- tvtable.tvcure(atgap4, format = "long")
atgap_table <- tvtable_combine(c("atgap1_t", "atgap2_t", "atgap3_t", "atgap4_t"), format = "long")
printer(atgap_table, file = "./tables/atgap_table.tex")

ag1_t <- tvtable.tvcure(ag1, format = "long")
ag2_t <- tvtable.tvcure(ag2, format = "long")
ag3_t <- tvtable.tvcure(ag3, format = "long")
ag4_t <- tvtable.tvcure(ag4, format = "long")
ag_table <- tvtable_combine(c("ag1_t", "ag2_t", "ag3_t", "ag4_t"), format = "long")
printer(ag_table, file = "./tables/ag_table.tex")

agiss1_t <- tvtable.tvcure(agiss1, format = "long")
agiss2_t <- tvtable.tvcure(agiss2, format = "long")
agiss3_t <- tvtable.tvcure(agiss3, format = "long")
agiss4_t <- tvtable.tvcure(agiss4, format = "long")
agiss_table <- tvtable_combine(c("agiss1_t", "agiss2_t", "agiss3_t", "agiss4_t"), format = "long")
printer(agiss_table, file = "./tables/agiss_table.tex")

clterm1_t <- tvtable.tvcure(clterm1, format = "long")
clterm2_t <- tvtable.tvcure(clterm2, format = "long")
clterm3_t <- tvtable.tvcure(clterm3, format = "long")
clterm4_t <- tvtable.tvcure(clterm4, format = "long")
clterm_table <- tvtable_combine(c("clterm1_t", "clterm2_t", "clterm3_t", "clterm4_t"), format = "long")
printer(clterm_table, file = "./tables/clterm_table.tex")

midterm1_t <- tvtable.tvcure(midterm1, format = "long")
midterm2_t <- tvtable.tvcure(midterm2, format = "long")
midterm3_t <- tvtable.tvcure(midterm3, format = "long")
midterm4_t <- tvtable.tvcure(midterm4, format = "long")
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

fn <- footnote(t)
fn <- list(pos = list(0), footnote = NULL)

tz <- list(table = t, footnote =  c("here is a really long example footnote that should stretch across the rows", rep("", ncol(t) - 1)))


tz$footnote <- "here is a really long example footnote that should stretch across the rows"
t2 <- tvtable.tvcure(at2, format = "long", )
t <- tvtable_combine(c("t1", "t2"), format = "long", footnote = "abcd")


printer(tz)
printer(t)

comment <- list(pos = list(0), command = NULL)
comment$pos[[1]] <- c(nrow(indPctChgCC))
comment$command <- c(paste("\\hline\n",
                           "{\\footnotesize Note: * signifies number of
properties used was 35 or less.}\n", sep = ""))
