# Basic Model
library(smcure); data(e1684); cl <- makeCluster(3, "SOCK"); registerDoSNOW(cl)
tmod <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph", link = "logit", var = F)
tmod.brglm <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph", link = "logit", var = F, brglm = T)

logit <- glm(as.integer(FAILCENS) ~ TRT + SEX + AGE, family = binomial(link = 'logit'), data = e1684)
logit2 <- glm(FAILCENS ~ TRT + SEX + AGE, family = quasibinomial(link = "logit"), data = e1684)
logit3 <- brglm::brglm(FAILCENS ~ TRT + SEX + AGE, family = binomial(link = "logit"), data = e1684)


# Model 1 - full model
library(readstata13)
library(splitstackshape)
library(plyr)
library(tvcure)
library(coxphf)
library(xtable)
library(stargazer)
library(coefplot)
options(scipen = 999)
# Load data
lhr <- read.dta13("C:/Users/gwill/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR 2008/lhrIOOct08replication.dta")
lhr <- rename(lhr, replace = c("_st" = "st", "_d" = "event", "_t" = "stop", "_t0" = "start"))
lhrna <- as.data.frame(na.omit(with(lhr, cbind(start, stop, event, archigosFIRC, wernerFIRC, capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity, warnumb))))

cl <- makeCluster(4, "SOCK")
registerDoSNOW(cl)
a <- tvcure(Surv(start, stop, event) ~ twodem5, cureform = ~ archigosFIRC, data = lhr, model = "ph", nboot = 100)
b <- tvcure(Surv(start, stop, event) ~ twodem5, cureform = ~ archigosFIRC, data = lhr, model = "ph", brglm = T, nboot = 100)
c <- tvcure(Surv(start, stop, event) ~ twodem5, cureform = ~ archigosFIRC + twodem5, data = lhr, model = "ph", nboot = 100)
d <- tvcure(Surv(start, stop, event) ~ twodem5, cureform = ~ archigosFIRC + twodem5, data = lhr, model = "ph", brglm = T, nboot = 100)


a <- tvcure(Surv(start, stop, event) ~ capchange + battletide + thirdpartycfire + twodem5 + tie + lndeaths, cureform = ~ wernerFIRC + capchange + battletide + thirdpartycfire + index + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr, model = "ph", nboot = 100, brglm = T)
b <- tvcure(Surv(start, stop, event) ~ capchange + battletide + thirdpartycfire + twodem5 + tie + lndeaths, cureform = ~ archigosFIRC + capchange + battletide + thirdpartycfire + index + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr, model = "ph", nboot = 100, brglm = T)

# Check against smcure
smod <- smcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph", link = "logit", Var = F)

# Prediction function with 1 covariate profile
preds = smcuretest(smod,newX=cbind(c(1),c(0),c(0.579)),
     newZ=cbind(c(1),c(0),c(0.579)),model="ph")
predt = predict_tvcure(tmod, cbind(c(1),c(0),c(0.579)),
     newZ=cbind(c(1),c(0),c(0.579)))

# Prediction function with 2 covariate profiles
preds2 = smcuretest(smod,newX=cbind(c(1,0),c(0,0),c(0.579,0.579)),
     newZ=cbind(c(1,0),c(0,0),c(0.579,0.579)),model="ph")
predt2 = predict_tvcure(tmod,newX=cbind(c(1,0),c(0,0),c(0.579,0.579)),
     newZ=cbind(c(1,0),c(0,0),c(0.579,0.579)))

# Plotting function with 1 covariate profile
plotpredictsmcure(preds)
plot_predict_tvcure(predt)

# Plotting function with 2 covariate profiles
plotpredictsmcure(preds2)
plot_predict_tvcure(predt2)










#
e <- predictsmcure(smod, cbind(0, 0, .579), cbind(0, 0, .579), model = "ph")
plotpredictsmcure(e)

#
f <- predictsmcure(smod, cbind(0, 0, .579), cbind(0, 0, .579), model = "ph")


predm = predictsmcure(smod,newX=cbind(c(1,0),c(0,0),c(0.579,0.579)),
     newZ=cbind(c(1,0),c(0,0),c(0.579,0.579)),model="ph")
plotpredictsmcure(predm)
predm = smcuretest(smod,newX=cbind(c(1,0),c(0,0),c(0.579,0.579)),
     newZ=cbind(c(1,0),c(0,0),c(0.579,0.579)),model="ph")
predt = predict_tvcure(tmod,newX=cbind(c(1,0),c(0,0),c(0.579,0.579)),
     newZ=cbind(c(1,0),c(0,0),c(0.579,0.579)))


coxtest <- coxph(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, data = e1684)
head(basehaz(coxtest))
head(tmod$Basehaz)
a <- survfit(coxtest, data = e1684)

# Test that survival function contains one entry for every observation
identical(length(tmod$Survival), length(tmod$Basehaz), tmod$nobs)
