# Basic Model
library(smcure); data(e1684); cl <- makeCluster(3, "SOCK"); registerDoSNOW(cl)
tmod <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph", link = "logit", var = F)
tmod.brglm <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph", link = "logit", var = F, brglm = T)

logit <- glm(as.integer(FAILCENS) ~ TRT + SEX + AGE, family = binomial(link = 'logit'), data = e1684)
logit2 <- glm(FAILCENS ~ TRT + SEX + AGE, family = quasibinomial(link = "logit"), data = e1684)
logit3 <- brglm::brglm(FAILCENS ~ TRT + SEX + AGE, family = binomial(link = "logit"), data = e1684)


su# Check against smcure
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
