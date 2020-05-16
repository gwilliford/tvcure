1/(1 + exp(-1.72))
1/(1 + exp(-1.72) - 1.96*.14)
1/(1 + exp(-1.72) + 1.96*.14)

exp(1.72) - 1.96 *.14
exp(1.72) + 1.96 *.14
fit <- 1.72
se.fit <- .14
exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit))
exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit))

.8471765 + 1.96*.1469992
.8471765 - 1.96*.1469992

.9603465 + 1.96*1.9323163
.9603465 - 1.96*1.9323163

.5755773 + 1.96*.1512412
.5755773 - 1.96*.1512412

.8666896 + 1.96*1.9423487
.8666896 - 1.96*1.9423487

cifun <- function(pp, se) {
  lowerci <- pp - 1.96 * se
  upperci <- pp + (1.96 * se)
  ppout <- cbind(ppvec, sevec, lowerci, upperci)
}

ppvec <- c(.8471765, .9603465, .5755773, .8555898)
sevec <- c(.1469992, 1.9323163, .1512412, 1.9423487)
ppout <- cifun(ppvec, sevec)




library(logistf)
data(sex2)

fit <- logistf(case ~ age + oc + vic + vicl + vis + dia, data = sex2, profile = F)
logistfpp <- cifun(fit$predict, fit$hat.diag)
C <- coef(fit)
se <- sqrt(t(C) %*% vcov(fit) %*% C)
lowerci <- fit$predict - 1.96 * fit$se
upperci <- fit$predict + 1.96 * fit$se


xb <- -1.72
se_lin <- fit$hat.diag
inv.logit(xb)*inv.logit(-xb)*se_lin

# brglm_pred2 <- predict(brglm_model, type = "link", se.fit = T)
# lowerci2 <- 1 / (1 + exp(-brglm_pred2$fit) - 1.96 * brglm_pred2$se.fit)

# load sample data from logistf 
library(logistf)
data(sex2)

# run model in brglm
library(brglm)
brglm_model <- brglm(case ~ age + oc + vic + vicl + vis + dia, family = "binomial", data = sex2, pl = T)
brglm_pred  <- predict(object = brglm_model, type = "response", se.fit = T)
lowerci     <- brglm_pred$fit - 1.96 * brglm_pred$se.fit
upperci     <- brglm_pred$fit + 1.96 * brglm_pred$se.fit
cbind(brglm_pred$fit, brglm_pred$se.fit, lowerci, upperci)

#brglmpp <- cifun(brglm_pred$fit, brglm_pred$se.fit)
#brglmpp2 <- cbind(brglm_pred$fit, brglm_pred$se.fit, lowerci, upperci)
