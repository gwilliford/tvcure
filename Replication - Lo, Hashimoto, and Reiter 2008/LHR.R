# Setup
library(readstata13)
library(plyr)
library(tvcure)
library(coxphf)
options(scipen = 999)

# Load data
lhr <- read.dta13("C:/Users/gwill/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR 2008/lhrIOOct08replication.dta")
lhr <- rename(lhr, replace = c("_st" = "st", "_d" = "event", "_t" = "stop", "_t0" = "start"))
#lhr <- lhr[,c("archigosFIRC", "archigosFIRClnt", "wernerFIRC", "wernerFIRClnt", "capchange", "battletide", "thirdpartycfire", "index", "onedem5", "twodem5", "twodem5lnt", "tie", "lndeaths", "cfhist", "stakes", "contiguity", "contiguitylnt", "LHRcluster", "start", "stop", "event", "newwar", "date1")]
#lhr <- na.omit(lhr)
#coxphf(Surv(start, stop, event) ~ wernerFIRC + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr)
#b <- with(lhr, cbind(wernerFIRC, capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity))
#cor(b)

#
a <- table(lhr$event, lhr$wernerFIRC)
rownames(a) <- c("No Event", "Event")
colnames(a) <- c("No FIRC", "FIRC")
a

c <- tvcure(Surv(start, stop, event) ~ wernerFIRC, cureform = ~ wernerFIRC, data = lhr, link = "probit")
Xnew <- median(lhr$wernerFIRC)
Znew <- median(lhr$wernerFIRC)
d <- predict_tvcure(c, Xnew, Znew, model = "ph")

plot(d$sur[, 2], d$sur[, 1])
plot(d$prediction[, 2], d$prediction[, 1])

# plot
plot_basesurv_tvcure(c)
plot_predict_tvcure(d)

# Correlation matrix
cor(with(lhr, cbind(event, wernerFIRC, capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity)), use = "complete.obs")

# Model
cl <- makeCluster(3, "SOCK")
registerDoSNOW(cl)
lhr_tvcurefull <- tvcure(Surv(start, stop, event) ~ capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, cureform = ~ wernerFIRC + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr, model = "ph", link = "probit", nboot = 1000)
saveRDS(lhr_tvcurefull, "lhr_tvcurefull_probit")


lhr_tvcurefull <- readRDS("lhr_tvcurefull_probit")
summary(lhr_tvcurefull)

lhr_tvcurefull <- tvcure(Surv(start, stop, event) ~ capchange + battletide + thirdpartycfire + index + tie + lndeaths + cfhist + stakes + contiguity, cureform = ~ wernerFIRC + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr, model = "ph", link = "logit", nboot = 1000)
lhr_tvcurefull3 <- tvcure(Surv(start, stop, event) ~ capchange + battletide + thirdpartycfire + index + tie + lndeaths + cfhist + stakes + contiguity, cureform = ~ capchange + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + contiguity, data = lhr, model = "ph", link = "logit", nboot = 1000)
lhr_tvcurefull5 <- tvcure(Surv(start, stop, event) ~ capchange + index + tie + lndeaths + cfhist + stakes, cureform = ~  thirdpartycfire + index + twodem5 + tie + lndeaths + cfhist + contiguity, data = lhr, model = "ph", link = "logit", nboot = 1000)
lhr_tvcurefull6 <- tvcure(Surv(start, stop, event) ~ capchange + index + twodem5 + tie + lndeaths + cfhist + stakes, cureform = ~  thirdpartycfire + index + twodem5 + tie + lndeaths + cfhist + contiguity, data = lhr, model = "ph", link = "logit", nboot = 1000)



tvcure.full.standard2 <- tvcure(Surv(start, stop, event) ~ capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, cureform = ~ archigosFIRC + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr, model = "ph", link = "probit", nboot = 1000)
stopCluster(cl)

Xnew <- with(lhr, t(as.matrix(apply(cbind(capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity), 2, median, na.rm = T))))
Xnew <- rbind(Xnew, Xnew)
Xnew[2, "tie"] <- 1
Znew <- with(lhr, t(as.matrix(apply(cbind(wernerFIRC, capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity), 2, median, na.rm = T))))
Znew <- rbind(Znew, Znew)
Znew[2, "tie"] <- 1
d <- predict_tvcure(lhr, newX = Xnew, newZ = Znew, link = "probit")
plot_predict_tvcure(d, type = "basesurv")
plot_predict_tvcure(d, type = "suncure")
plot_predict_tvcure(d, type = "spop")

# Models
X <- with(cbind(capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity), data = lhr)
Z <- with(cbind(wernerFIRC, capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity), data = lhr)
X = t(as.matrix(colMeans(X, na.rm = T)))
Z = t(as.matrix(colMeans(Z, na.rm = T)))
predict_tvcure(tvcure.full.standard, newX = X, newZ = Z, model = "ph")
plot(z$)

table(lhr$wernerFIRC, lhr$event)

m_reduced <- tvcure(Surv(start, stop, event) ~ thirdpartycfire,
                      cureform = ~ wernerFIRC,
                      data = lhr,
                      model = "ph",
                      link = "probit", var = F)

# Simple tests
tvcure.logit  <- tvcure(Surv(start, stop, event) ~ twodem5, cureform = ~ wernerFIRC, data = lhr, var = T)
tvcure.probit <- tvcure(Surv(start, stop, event) ~ twodem5, cureform = ~ wernerFIRC, data = lhr, var = T, link = "probit", parallel = F)

tvcure.full.logit <- tvcure(Surv(start, stop, event) ~ capchange +
                        battletide + thirdpartycfire + index + onedem5 + twodem5
                      + tie + lndeaths + cfhist + stakes + contiguity,
                      cureform = ~ wernerFIRC + capchange
                      + battletide + thirdpartycfire + index + onedem5 + twodem5
                      + tie + lndeaths + cfhist + stakes + contiguity,
                      data = lhr,
                      model = "ph",
                      link = "logit", nboot = 1000)
tvcure.full.probit <- tvcure(Surv(start, stop, event) ~ capchange +
                        battletide + thirdpartycfire + index + onedem5 + twodem5
                      + tie + lndeaths + cfhist + stakes + contiguity,
                      cureform = ~ wernerFIRC + capchange
                      + battletide + thirdpartycfire + index + onedem5 + twodem5,
                      data = lhr,
                      model = "ph",
                      link = "probit", nboot = 1000, parallel = F)

a <- glm(event ~ wernerFIRC, family = binomial(link = "logit"), data = lhr)
b <- glm(event ~ wernerFIRC, family = quasibinomial(link = "probit"), data = lhr)
b$fitted.values
summary(pnorm(b$linear.predictors))
c <- predict(b, type = "response")

# Predicted surival
plot(sort(tvcure.probit$Survival, decreasing = T), type = "l")

# Predicted probabilities
Xtest <- t(with(lhr, colMeans(cbind(capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity))))
Ztest <- t(with(lhr, colMeans(cbind(wernerFIRC, capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity))))
logit.base <- predict_tvcure(tvcure.full.logit, Xtest, Ztest)
plot_predict_tvcure(logit.base)
probit.base <- predict_tvcure(tvcure.full.probit, Xtest, Ztest)
plot_predict_tvcure(probit.base)
Xtest2 <- matrix(nrow = 2, ncol = ncol(Xtest))
Xtest2[1, ] <- Xtest
Xtest2[2, ] <- Xtest
Ztest2 <- matrix(nrow = 2, ncol = ncol(Ztest))
Ztest2[1, ] <- Ztest
Ztest2[2, ] <- Ztest
Ztest2[1, 1] <- 0
Ztest2[2, 1] <- 1
logit.base2 <- predict_tvcure(tvcure.full.probit, Xtest2, Ztest2)
plot_predict_tvcure(logit.base2)


logit.base.hi <- predict_tvcure(tvcure.full.probit, Xtesthi, Ztesthi)
plot_predict_tvcure(logit.base.hi)

Xtestlo1 <- with(lhr, cbind(5, 1, 1, 0, 0, 0, 1, 16.2, 1.6, 1, 1))
Xtestlo2 <- with(lhr, cbind(5, 1, 1, 0, 0, 0, 1, 16.2, 1.6, 1, 1))
Xtestlo <- rbind(Xtestlo1, Xtestlo2)

Ztestlo1 <- with(lhr, cbind(0, 5, 1, 1, 0, 0, 0, 1, 16.2, 1.6, 1, 1))
Ztestlo2 <- with(lhr, cbind(1, 5, 1, 1, 0, 0, 0, 1, 16.2, 1.6, 1, 1))
Ztestlo <- rbind(Ztestlo1, Ztestlo2)

logit.base.lo <- predict_tvcure(tvcure.full.probit, Xtestlo, Ztestlo)
plot_predict_tvcure(logit.base.lo)

# High risk scenario
Xtest.dem.hi1 <- with(lhr, cbind(5, 1, 1, 0, 1, 1, 1, 16.2, 1.6, 1, 1))
Xtest.dem.hi2 <- with(lhr, cbind(5, 1, 1, 0, 0, 0, 1, 16.2, 1.6, 1, 1))
Xtest.dem.hi <- rbind(Xtest.dem.hi1, Xtest.dem.hi2)

Ztest.dem.hi1 <- with(lhr, cbind(0, 5, 1, 1, 0, 1, 1, 1, 16.2, 1.6, 1, 1))
Ztest.dem.hi2 <- with(lhr, cbind(0, 5, 1, 1, 0, 0, 0, 1, 16.2, 1.6, 1, 1))
#Ztest.dem.hi1 <- rep(0, 12)
#Ztest.dem.hi2 <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
Ztest.dem.hi <- rbind(Ztest.dem.hi1, Ztest.dem.hi2)

logit.base.dem.hi <- predict_tvcure(tvcure.full.standard2, Xtest.dem.hi, Ztest.dem.hi, link = "probit")
plot_predict_tvcure(logit.base.dem.hi, type = "suncure")

# Low risk scenario
Xtest.dem.lo1 <- with(lhr, cbind(0, 0, 0, 10, 1, 1, 0, 0, 0, 0, 0))
Xtest.dem.lo2 <- with(lhr, cbind(0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0))
Xtest.dem.lo <- rbind(Xtest.dem.lo1, Xtest.dem.lo2)

Ztest.dem.lo1 <- with(lhr, cbind(1, 0, 0, 0, 10, 1, 1, 0, 0, 0, 0, 0))
Ztest.dem.lo2 <- with(lhr, cbind(1, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0))
Ztest.dem.lo <- rbind(Ztest.dem.lo1, Ztest.dem.lo2)

logit.base.dem.lo <- predict_tvcure(tvcure.full.standard2, Xtest.dem.lo, Ztest.dem.lo, link = "probit")
plot_predict_tvcure(logit.base.dem.lo, type = "suncure")
matplot(logit.base.dem.lo$scure[, 3], logit.base.dem.lo$scure[, 1], type = "l")
matplot(logit.base.dem.lo$scure[, 3], logit.base.dem.lo$scure[, 2], add = T, col = 2, type = "l")
exp(tvcure.full.logit$gamma %*% c(1, 0, 0, 0, 0, 10, 1, 0, 0, 0, 0, 0, 0))/(1 + exp(tvcure.full.logit$gamma %*% c(1, 0, 0, 0, 0, 10, 1, 0, 0, 0, 0, 0, 0)))

exp(tvcure.full.logit$gamma %*% c(1, 0, 5, 1, 1, 0, 1, 0, 1, 16.2, 1.6, 1, 1))/(1 + exp(tvcure.full.logit$gamma %*% c(1, 0, 5, 1, 1, 0, 1, 0, 1, 16.2, 1.6, 1, 1)))



logit.base.lo <- predict_tvcure(tvcure.full.probit, Xtestd, Ztestd)
plot_predict_tvcure(logit.base.lo)


tvcure.institutions.flogit <- tvcure(Surv(start, stop, event) ~ archigosFIRC + capchange +
                               battletide + thirdpartycfire + formal + withdraw +
                               dmz + ac + pk + ext_inv + internal + detail +
                               info + disp_res +index + onedem5 + twodem5 + tie +
                               lndeaths + cfhist + stakes + contiguity,
                             cureform = ~ archigosFIRC + capchange +
                               battletide + thirdpartycfire + formal + withdraw +
                               dmz + ac + pk + ext_inv + internal + detail +
                               info + disp_res +index + onedem5 + twodem5 + tie +
                               lndeaths + cfhist + stakes + contiguity,
                             data = lhr,
                             model = "ph",
                             firthlogit = T)


# Coxph
cox <- coxph(Surv(start, stop, event) ~ capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr)
plot(survfit(Surv(start, stop, event)))
plot(survfit(cox))

Zt <- with(lhr, cbind(archigosFIRC, capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity))
Zt <- t(as.matrix(apply(Zt, 2, median, na.rm = T)))
Zt <- rbind(Zt, Zt)
Zt[2, 5] <- 10
Xt <- Zt[, -1]
p <- predict_tvcure(tvcure.full.standard2, Xt, Zt, link = "probit")
plot_predict_tvcure(p)
p$newuncureprob
