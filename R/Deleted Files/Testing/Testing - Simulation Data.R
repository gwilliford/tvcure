###Set up
rm(list=ls())
library(survival)
library(eha)
library(pscl)
library(plyr)

###Simulate data
syndat<-function(nobs){
#generate covariates
  x1 <- runif(nobs,0,2)
  x2 <- rnorm(nobs,0,2)
  x3 <- runif(nobs,0,5)

#generate duration as a function of x1 and x2
  xmat <- cbind(1,x1,x2)
  b <- rbind(1,1.5,.1)
  duration <- rweibull(nobs, scale=exp(xmat %*% b), shape=2)

#time dependent censoring
  #summary(dur1)
  #s0<-Surv(dur1)
  #c0<-coxph(s0~x1+x2);c0
  #dur2 <- dur1
  #dur2[dur2>quantile(dur1,.9)] <- quantile(dur1,.9)
  #s1<-Surv(dur2)
  #c1<-coxph(s1~x1+x2);c1

#generate failure probability as a function of x1 and x3
  zmat <-cbind(1,x1,x3)
  g <-c(2,-1,.2)
  p<-1/(1+exp(zmat %*% -g))
  y<-rbinom(nobs,1, prob=p)

#generate censoring
  cens <- rbinom(nobs,1,.9)

#bind into dataframe
  df<-as.data.frame(cbind(y,duration,cens,x1,x2,x3))

#gen failure indicator equal to 1 if not censored or cured
  df$fail<-1
  df$fail[df$cens==0]<-0
  df$fail[df$y==0]<-0

#tobinary
  df$ent<-0
  dfbin<-toBinary(df, surv=c("ent","duration","fail"))

#return
  return(list(df=df, dfbin=dfbin))
}

x <- syndat(1000)

m <- tvcure(Surv(duration, fail) ~ x1 + x2 + x3, cureform = ~ x1, data = x$df, model = "ph", parallel = T)

