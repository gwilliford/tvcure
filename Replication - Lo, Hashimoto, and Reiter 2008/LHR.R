# Setup
library(readstata13)
library(plyr)
library(tvcure)
library(coxphf)
cl <- makeCluster(3, "SOCK")
registerDoSNOW(cl)
options(scipen = 999)

# Load data
lhr <- read.dta13("C:/Users/gwill/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR 2008/lhrIOOct08replication.dta")
lhr <- rename(lhr, replace = c("_st" = "st", "_d" = "event", "_t" = "stop", "_t0" = "start"))
lhr <- lhr[,c("archigosFIRC", "archigosFIRClnt", "wernerFIRC", "wernerFIRClnt", "capchange", "battletide", "thirdpartycfire", "index", "onedem5", "twodem5", "twodem5lnt", "tie", "lndeaths", "cfhist", "stakes", "contiguity", "contiguitylnt", "LHRcluster", "start", "stop", "event", "newwar", "date1")]
lhr <- na.omit(lhr)
coxphf(Surv(start, stop, event) ~ wernerFIRC + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr)
b <- with(lhr, cbind(wernerFIRC, capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity))
cor(b)

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


# Models
X <- with(cbind(capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity), data = lhr)
Z <- with(cbind(wernerFIRC, capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity), data = lhr)
X2 = as.matrix(colMeans(X, na.rm = T))
Z2 = as.matrix(colMeans(Z, na.rm = T))
tvcure.full.standard <- tvcure(Surv(start, stop, event) ~ archigosFIRC + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, cureform = ~ archigosFIRC + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr, model = "ph", link = "probit")
z <- predict_tvcure(tvcure.full.standard, newX = X2, newZ = Z2, model = "ph")
plot(z$)

table(lhr$wernerFIRC, lhr$event)

m_reduced <- tvcure(Surv(start, stop, event) ~ thirdpartycfire,
                      cureform = ~ wernerFIRC,
                      data = lhr,
                      model = "ph",
                      link = "probit", var = F)
tvcure.logit  <- tvcure(Surv(start, stop, event) ~ twodem5, cureform = ~ wernerFIRC, data = lhr)
tvcure.probit <- tvcure(Surv(start, stop, event) ~ twodem5, cureform = ~ wernerFIRC, data = lhr, var = F, link = "probit")

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
                      + battletide + thirdpartycfire + index + onedem5 + twodem5
                      + tie + lndeaths + cfhist + stakes + contiguity,
                      data = lhr,
                      model = "ph",
                      link = "probit", nboot = 1000)

a <- glm(event ~ wernerFIRC, family = binomial(link = "logit"), data = lhr)
b <- glm(event ~ wernerFIRC, family = quasibinomial(link = "probit"), data = lhr)

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
