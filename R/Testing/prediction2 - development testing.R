Time <- testpredb$Time
basesurv <- testpredb$s0mean
bslo <- testpredb$s0lo
bshi <- testpredb$s0hi
a <- ggplot(mapping = aes(x = Time, y = basesurv)) + geom_line()
b <- a + geom_ribbon(aes(ymin=bslo, ymax=bshi), lty = 2, alpha=0.1, col = "black");b



scm <- testpredb$suncuremean
sclo <- testpredb$suncurelo
schi <- testpredb$suncurehia
nmax <- ncol(testpredb$suncuremean)
a <- ggplot(mapping = aes(x = Time, y = suncuremean[, 1])) + geom_line();a
b <- a + geom_ribbon(aes(ymin=sclo[, 1], ymax=schi[, 1]), lty = 2, alpha=0.1, col = "black");b

geom_blank
a <- ggplot(mapping = aes(x = Time, y = scm)) + geom_line()

a <- ggplot(mapping = aes(x = Time, y = scm[, 1])) + geom_line()
a <- a + geom_ribbon(aes(ymin=sclo[, 1], ymax=schi[, 1]), lty = 2, alpha=0.1, colour = i)
for (i in 2:nmax) {
  a <- a + geom_line(aes(x = Time, y = scm[, i]))
  a <- a + geom_line(aes(x = Time, y = scm[, 5]))
  a <- a + geom_ribbon(aes(ymin = sclo[, i], ymax = schi[, i]), lty = 2, alpha = 0.1, col = i)
}
  a <- a + geom_line(aes(x = Time, y = scm[, 3]))
  a <- a + geom_ribbon(aes(ymin = sclo[, 3], ymax = schi[, 3]), lty = 2, alpha = 0.1, col = i)

i <- 2
a <- ggplot(mapping = aes(x = Time, y = scm[, 1])) + geom_line() + geom_ribbon(aes(ymin=sclo[, 1], ymax=schi[, 1]), lty = 2, alpha=0.1)
while (i < nmax) {
  a[i] <- a + geom_line(aes(x = Time, y = scm[, i])) + geom_ribbon(aes(ymin = sclo[, i], ymax = schi[, i]), lty = 2, alpha = 0.1)
  i <- i + 1
}


# p1 <- ggplot(mapping = aes(x = predobj$Time, y = predobj$spop[, 1])) + geom_line() +


a <- ggplot(mapping = aes(x = Time, y = t(scm), colour = )) + geom_line() + geom_ribbon(aes(ymin=sclo[, 1], ymax=schi[, 1]), lty = 2, alpha=0.1)
a

ecomomics2 <- economics
econ <- list(e1=economics, e2=economics, e3=economics)
df   <- cbind(cat=rep(names(econ),sapply(econ,nrow)),do.call(rbind,econ))
ggplot(scm2, aes(Time, unemploy, color=cat)) + geom_line()

nobs <- 284
scm2 <- split(scm, rep(1:ncol(scm), each = nrow(scm)))
t2 <- t(scm2)
t2 <- matrix(nrow = nmax, ncol = nobs)
for (i in 1:nmax) {
  t2[i, ] <- scm[, i]
}
scm3 <- cbind(cat = rep(names(scm2), sapply(scm2, nrow)), do.call(rbind, scm2))
ggplot(scm2)

ggplot(t2, aes(Time, t2)) + geom_line()


x = Time
y = probability

for (i in 1:7) {
  colnames(scm)[i] <- paste("v", i, sep = "")
}

tidyr::

scm <- testpredb$suncuremean
scm <- as.data.frame(scm)
scm$T <- as.vector(Time)
# scm2 <- split(scm, rep(1:ncol(scm), each = nrow(scm)))
sc2 <- list(scm)
scm3 <- cbind(cat = rep(names(scm2), sapply(scm2, nrow))



ecomomics2 <- economics
econ <- list(e1=economics, e2=economics, e3=economics)
df   <- cbind(cat=rep(names(econ),sapply(econ,nrow)),do.call(rbind,econ))
ggplot(df, aes(date, unemploy, color=cat)) + geom_line()
ggplot(df, aes(date, unemploy)) + geom_line() + facet_wrap(~ cat)


nobs <- 284



ggplot(scm, aes(Time, unemploy, color=cat)) + geom_line()
df   <- cbind(cat=rep(names(econ),sapply(econ,nrow)),do.call(rbind,econ))
a <- do.call(rbind, scm2)


b <- ggplot()
b <- b + geom_line(aes(x=Time,y=unemploy,colour=cat),



b=ggplot()
for(i in 1:3){
  b <- b + geom_line(aes(x=date,y=unemploy,colour=cat),
                     data=cbind(cat=as.character(i),econ[[i]]))
}
print(b)


scm <- testpredb$suncuremean
scm <- as.data.frame(scm)
sc2 <- split(scm, rep(1:ncol(scm), each = nrow()))
sc2 <- as.list(scm)

df   <- cbind(cat=rep(names(sc2),sapply(sc2,nrow(sc2))),do.call(rbind,sc2))

a <- rep(1:ncol(scm), sapply(sc2, nrow))


tfun <- function(x) {
  x$T <- Time
  x$cat <- x[[]]
}
lapply(sc2, fun(x) = x$T <- Time)

scm <- testpredb$suncuremean
 b <- t(scm)
sc2 <- as.list(b)

sc2 <- split(scm)


cbind(sc2[[1]], Time)
df <- cbind(sapply(sc2,nrow(sc2))),do.call(rbind,sc2))
rbind(sc2)
a <- unlist(sc2)
sapply(sc2, nrow(sc2))

a <- as.data.frame(do.call(rbind, sc2))

ggplot(b, aes(Time, pr)) + geom_line()
b <- a[1:284, ]
ggplot(df, aes(date,unemploy, color=cat)) + geom_line()



scm2 <- split(scm, rep(1:ncol(scm), each = nrow(scm)))
for (i in 1:7) {
  scm2[[i]] <- cbind(scm2[[i]], Time, num = i)
}
a <- as.matrix(scm2[[1]])
for (i in 2:7) {
  a <- rbind(a, scm2[[i]])
}
colnames(a)[1] <- "pr"
a <- as.data.frame(a)

ggplot()
x <- ggplot(b, aes(Time, pr)) + geom_line()
for (i in 1:7) {
  b <- a["num" == i, ]
  x <- ggplot(b, aes(Time, pr)) + geom_line()
  with(b, x <- geom_line(data = b, mapping = aes(Time, pr))
}



b
  ggplot(a, aes(Time, pr)) + geom_line()
  ggplot(a, aes(Time, pr)) + geom_line() + facet_wrap(~ num)

ggplot(a[1:284, ], aes(Time, pr)) + geom_line() + geom_line(a[285:568, ], mapping = aes(Time, pr))

ggplot(a, aes(Time, pr)) + geom_line() + facet_wrap(~ num)

scm <- testpredc$suncuremean
scm <- as.data.frame(scm)
sc2 <- split(scm, rep(1:ncol(scm), each = nrow()))
sc2 <- as.list(scm)
df <- cbind(sapply(sc2,nrow(sc2))),do.call(rbind,sc2))


with(test)
aplpha <_

ggplot(a, aes(Time, pr)) + geom_line() + facet_wrap(~ num)

scm <- split(scm, rep(1:ncol))
schi <- split(scm, rep(1:ncol(scm), each = nrow(scm)))

scm  <- testpredb$suncuremean
sclo <- testpredb$suncurelo
schi <- testpredb$suncurehi
scm  <- split(scm, rep(1:ncol(scm), each = nrow(scm)))
sclo <- split(sclo, rep(1:ncol(sclo), each = nrow(sclo)))
schi <- split(schi, rep(1:ncol(schi), each = nrow(schi)))
for (i in 1:length(scm)) {
  scm[[i]] <- cbind(scm[[i]], Time, num = i, sclo[[i]], schi[[i]])
}
scf <- as.data.frame(do.call(rbind, scm))
colnames(scf) <- c("scm", "Time", "num", "sclo", "schi")


# scm <- testpredb$suncuremean
# scm2 <- split(scm, rep(1:ncol(scm), each = nrow(scm)))
# for (i in 1:length(scm2)) {
#   scm2[[i]] <- cbind(scm2[[i]], Time, num = i)
# }
# scm2 <- do.call(rbind, scm2)
# scm2 <- as.data.frame(scm2)


scm  <- testpredb$suncuremean
sclo <- testpredb$suncurelo
schi <- testpredb$suncurehi
scm  <- split(scm, rep(1:ncol(scm), each = nrow(scm)))
sclo <- split(sclo, rep(1:ncol(sclo), each = nrow(sclo)))
schi <- split(schi, rep(1:ncol(schi), each = nrow(schi)))
for (i in 1:length(scm)) {
  scm[[i]] <- cbind(scm[[i]], Time, num = i, sclo[[i]], schi[[i]])
}
scf <- as.data.frame(do.call(rbind, scm))
colnames(scf) <- c("scm", "Time", "num", "sclo", "schi")


ggplot(scf, aes(Time, scm, col = as.factor(num))) + geom_line() + geom_ribbon(aes(ymin = sclo, ymax = schi, col = as.factor(num)), alpha=0.1)



testpredc <- prediction2(pd, "TRT", c(0:1), "suncure", CI = T, nsims = 100)
testpredc <- prediction2(pd, "TRT", c(0:1), "suncure", CI = T, nsims = 100)
testpredc[[19]]

testpredb <- prediction2(pd3, "AGE", seq(-30, 30, 30), "spop", CI =  T, nsims = 100)
testpredb[[19]]


testpred2a <- prediction2(pd2, "TRT", c(0, 1), "spop", CI = T, nsims = 100)

testpred2a <- prediction2(pd2, "TRT", c(0, 1), "basesurv", CI = T, nsims = 100)
testpred2a <- prediction2(pd2, "TRT", c(0, 1), "suncure", CI = T, nsims = 100)
testpred2a <- prediction2(pd2, "TRT", c(0, 1), "spop", CI = T, nsims = 100)


testpred2a <- prediction2(pd2, "TRT", c(0, 1), "suncure", legendtitle = "Treatment", CI = T, nsims = 100)


a <- testpred2a$splot
a + theme(legend.title = element_text("TRT"))


testpred <- prediction2(pd2, "TRT", c(0, 1), "basesurv");testpred
testpred <- prediction2(pd2, "TRT", c(0, 1), "suncure");testpred
testpred <- prediction2(pd2, "TRT", c(0, 1), "spop");testpred


testpred <- prediction2(pd2, "TRT", c(0, 1), "uncureprob");testpred
a <- testpred$splot
a <- a + coord_fixed()

a <- splot
ggsave("test.png", a, "png")

testpred <- prediction2(pd2, "TRT", c(0, 1), "uncureprob");testpred



testpred <- prediction2(pd, "TRT", c(0, 1), "suncure")
testpred <- prediction2(pd, "TRT", c(0, 1), "suncure", bw = T)
testpred <- prediction2(pd, "TRT", c(0, 1), "suncure", CI = T, nsims = 100)
testpred <- prediction2(pd, "TRT", c(0, 1), "suncure", bw = T, CI = T, nsims = 100)


testpred <- prediction2(pd, "TRT", c(0, 1), "spop")
testpred <- prediction2(pd, "TRT", c(0, 1), "spop", bw = T)
testpred <- prediction2(pd, "TRT", c(0, 1), "spop", CI = T, nsims = 100)
testpred <- prediction2(pd, "TRT", c(0, 1), "spop", bw = T, CI = T, nsims = 100)
