a <- cbind(c(matrix(c("", ""), ncol = 2), b(cox, format = "long")), tvtable(pd, format = "long"))
rownames(a)[rownames(a) == ""] <- "blank"
colnames(a)[1] <- "Hazard Coefficient"
d <- as.data.frame(a)
dplyr::rename_at(d, vars(starts_with("blank"), function(x))

stargazer(a, single.row = F, covariate.labels = c("a", "b", "c", "d"))
rownames(d)[2] <- NULL
rownames(d)[4] <- NULL
e <- stargazer(a, single.row = F, covariate.labels = c("a", "b", "c", "d"))
e <- xtable(a)
names(e[, 1]) <- rename_at(as.tbl(e), contains("blank"), "a")


x <- b(cox)
y <- tvtable(pd)

g <- as.tbl(d)



g <- tibble::rownames_to_column(d, var = "rowname")
xtable(g[, 1:2])



rownames(a) <- rownames

.rowNamesDF(d)
make.names = F) <- c("a", "", "b", "c")



h <- tvtable(pd)
xtable(h)

m <- tvtable(pd, format = "long")
n <- as.data.frame(m, check.names = F, fix.empty.names = F, optional = T)
z <- xtable(a)
rownames(z)[2] <- "NA"
rownames(z)[4] <- "NA"

j <- b(cox)
i <- merge(h, j, by = "row.names", suffixes = c("", ""))
j <- as.data.frame(i, check.names = F)
rownames(j) <- NULL
xtable(j, rownames = F)

xtableMatharray(a)


stargazer(a, covariate.labels = c("a", "", "b", "c", "f", "m"))

matrix --> Xtable -- matrix
z <- xtable(a)
z2 <- as.matrix

print.
