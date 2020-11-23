###Set up
rm(list=ls())
library(survival)
library(purrr)

###Simulate data
syndat<-function(nobs = 200){

  #generate covariates
  x <- as.numeric(rbernoulli(nobs))
  beta = log(0.1)
  xbeta = x * beta

  z <- as.numeric(rbernoulli(nobs))
  gamma = c(2, -1)



  #generate duration as a function of x1 and x2
  # pdf <- rweibull(nobs, scale=exp(x * beta), shape=2)
  # cdf <- integrate(dweibull(200, scale = expxbeta, shape = 2), 0, 200)
  shape = 2
  scale = exp(x * beta)
  cdf = 1 - exp(-(x/scale)^shape)

  mat = as.data.frame(cbind(cdf, seq(1, 200)))
  colnames(mat) = c("cdf", "time")
  mat = mat[order(mat$time), ]
  mat$runsum = cumsum(mat$cdf)
  mat$Ft = mat$runsum/nobs
  #plot(mat$Ft, mat$time)
  mat$St = 1 - mat$Ft
  #plot(mat$St, mat$time)

  #generate failure probability
  zmat <- cbind(1, z)
  p <- 1/(1 + exp(zmat %*% -gamma))
  y <- rbinom(nobs, 1, prob = p)

  #generate censoringm
  cens <- runif(0, 10)

  #bind into dataframe
  df <- as.data.frame(cbind(y,duration,cens,x1,x2,x3))

  #gen failure indicator equal to 1 if not censored or cured
  df$fail <- 1
  df$fail[df$cens==0] <- 0
  df$fail[df$y==0] <- 0

  #tobinary
  df$ent <- 0
  dfbin <- toBinary(df, surv=c("ent","duration","fail"))

  #return
  return(list(df=df, dfbin=dfbin))
}
