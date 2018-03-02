library(smcure); data(e1684)
cl <- makeCluster(3, "SOCK"); registerDoSNOW(cl)
smod <- smcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph")
tmod <- tvcure(Surv(FAILTIME, FAILCENS) ~ SEX, cureform = ~ AGE + TRT + SEX, data = e1684, model = "ph", nboot = 100, parallel = T, link = "logit")

#
e <- predictsmcure(smod, cbind(0, 0, .579), cbind(0, 0, .579), model = "ph")
plotpredictsmcure(e)

#
f <- predictsmcure(smod, cbind(0, 0, .579), cbind(0, 0, .579), model = "ph")


predm = predictsmcure(smod,newX=cbind(c(1,0),c(0,0),c(0.579,0.579)),
     newZ=cbind(c(1,0),c(0,0),c(0.579,0.579)),model="ph")
plotpredictsmcure(predm)

coxtest <- coxph(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, data = e1684)
head(basehaz(coxtest))
head(tmod$Basehaz)
a <- survfit(coxtest, data = e1684)

# Test that survival function contains one entry for every observation
identical(length(tmod$Survival), length(tmod$Basehaz), tmod$nobs)
