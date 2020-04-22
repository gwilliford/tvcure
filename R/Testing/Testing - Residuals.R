library(tvcure)
library(mixcure)
data(goldman.data)
# cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
a <- mixcure(Surv(time, cens) ~ transplant, ~ transplant, data = goldman.data)
b <- tvcure(Surv(time, cens) ~ transplant, ~ transplant, data = goldman.data, var = T, parallel = F)
d <- residuals(b, data = goldman.data)
e <- residuals(b, data = goldman.data, type = "M-Martingale")
plot(e$xm, e$residuals)


#### LHR Tests
library(haven)
library(dplyr)
library(matrixStats)
# plyr package also required - called directly
options(scipen = 999)
setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Article 1 - Cure Models/V10 Replication")
lhr <- read_dta("LHRIOOct08replication.dta")
lhr <- plyr::rename(lhr, replace = c("_st" = "st", "_d" = "event", "_t" = "stop", "_t0" = "start"))
lhr$lnt <- log(lhr$stop)
lhr$indexlnt <- lhr$lnt * lhr$index

m2 <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths + contiguity + index + cfhist,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire +
               stakes +
               twodem5 + index + cfhist + contiguity,
             data = lhr, var = T, parallel = F, brglm = T, nboot = 30); summary(m2)
r2 <- residuals(m2, data = lhr, "Cox-Snell")
plot(r2$time, r2$residuals)
r2b <- residuals(m2, data = lhr, "M-Martingale")
plot(r2b$xm[, 5], r2b$residuals)


m2$coxmod
m2$curemod$latency_fit
cox.zph(m2$curemod$latency_fit, terms = F)



# Calculate schoenfeld residuals
time <- m2$Time
z <- cbind(m2$X, time)
alpha <- as.data.frame(z) %>% group_by(time) %>% summarize(
  meanlndeaths = mean(lndeaths, na.rm = T),
  meanindex = mean(index, na.rm = T),
  meantwodem5 = mean(twodem5, na.rm = T),
  meancapchange = mean(capchange, na.rm = T)
); X <- merge(z, alpha, by = 'time')

X$charlie <- m2$uncureprob * (X$lndeaths - X$meanlndeaths)
X$dee <-m2$uncureprob * (X$index - X$meanindex)
X$frank <- m2$uncureprob * (X$twodem5 - X$meantwodem5)
X$mac <- m2$uncureprob * (X$capchange - X$meancapchange)

# Plot schoenfeld residuals v. time
plot(X$charlie ~ X$time)
plot(X$frank ~ X$time)
plot(X$dee ~ X$time)
lowdee = lowess(X$dee ~ X$time)
lowcharlie = lowess(X$charlie ~ X$time)
lowfrank = lowess(X$frank ~ X$time)
lowmac = lowess(X$mac ~ X$time)
lowmac = lowess(X$mac ~ X$time)

X$lowdee = lowdee$y
X$lowcharlie = lowcharlie$y
X$lowfrank = lowfrank$y
X$lowmac = lowmac$y

ggplot(aes(y = dee, x = time), data = X) + geom_point() + geom_line(aes(x = time, y = lowdee, col = "red", lwd = 1.5))
ggplot(aes(y = charlie, x = time), data = X) + geom_point() + geom_line(aes(x = time, y = lowcharlie, col = "red", lwd = 2))
ggplot(aes(y = frank, x = time), data = X) + geom_point() + geom_line(aes(x = time, y = lowfrank, col = "red"))
ggplot(aes(y = mac, x = time), data = X) + geom_point() + geom_line(aes(x = time, y = lowmac, col = "red"))

plot(t, type = "l")
points(X$dee ~ X$index)

t <- lowess(X$dee ~ X$index)

ks.test(lndeaths, X$meanlndeaths)

m2 <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths + contiguity + index + cfhist + twodem5,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire +
               stakes +
               twodem5 + index + cfhist + contiguity,
             data = lhr, var = T, parallel = F, brglm = T, nboot = 30); summary(m2)
s <- cox.zph(m2$curemod$latency_fit, terms = F)
library(survminer)
ggcoxzph(s)

c1 <- coxph(Surv(start, stop, event) ~ lndeaths + tie + battletide + thirdpartycfire + archigosFIRC + stakes + onedem5 + twodem5 + index + cfhist + contiguity + capchange, data = lhr, x = T); summary(c1)
c1res <- cox.zph(c1)
ggcoxzph(c1res)
ggcoxfunctional(fit = c1, data = lhr)


# Modified martingale - general test of model fit



a <- mixcure(Surv(time, cens) ~ transplant, ~ transplant, data = goldman.data)
g <- residuals(a, data = goldman.data, type = "Cox-Snell")
# a <- mixcureboot(a, data = goldman.data)
b <- tvcure(Surv(time, cens) ~ transplant, ~ transplant, data = goldman.data, var = F, parallel = F)


d <- residuals(b, data = goldman.data, type = "Cox-Snell")
summary(d)
plot(d$time, d$residuals)
goldman.data$lnt <- log(goldman.data$time)
goldman.data$transplantlnt <- as.numeric(goldman.data$transplant) * goldman.data$lnt
e <- tvcure(Surv(time, cens) ~ transplant + transplantlnt, ~ transplant, data = goldman.data, var = F)
f <- residuals(e, data = goldman.data, type = "Cox-Snell")
plot(f$time, f$residuals)


h <- residuals(e, data = goldman.data, type = "M-Martingale")
matplot(h$xm[, -1])
i <- residuals(b, data = goldman.data, type = "M-Martingale")
matplot(i$xm[, -1])

plot(i$xm[, 2], i$residuals2)
plot(i$residuals, i$xm[, 2])
lines(lowess(i$residuals ~ i$xm[, 2]))
plot(h$xm[, 2], h$residuals2)
plot(h$residuals, h$xm[, 3])
lines(lowess(h$residuals ~ h$xm[, 3]))


j <- coxph(Surv(time, cens) ~ transplant, data = goldman.data)

goldman2 <- goldman.data
goldman2[1:5, "transplant"] <- NA
k <- mixcure(Surv(time, cens) ~ transplant, ~ transplant, data = goldman2)
