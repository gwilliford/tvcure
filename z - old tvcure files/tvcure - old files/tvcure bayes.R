tvcuretest <- function() {
  for (i in 1:n) {
    for (T in 1:Tmax) {
	    pi[i,time[i]] <- invlogit(inprod(Z[i,time[i]], Gamma[]))
	    Spop[i,time[i]] <- pi[i,time[i]]*Snc + 1 - pi[i,time[i]]
	    Snc  <- S0^(exp(inprod(X[i,time[i]],Beta)))
    }
  }
  for (t in 1:Tmax) {
	  S0[t] <- exp(-sum(d[t]/sum(pi[i,time[i]]*exp(X[i,time[i]] * Beta[]))))
  }
  for (i in 1:nbeta) {
	  Beta[i] ~ dnorm(0, .01)
  }
  for (i in 1:ngamma) {
	  Gamma[i] ~ dnorm(0, .01)
  }
}
  
library(smcure)
data(e1684)

X <- cbind(e1684$TRT, e1684$SEX, e1684$AGE)
Z <- cbind(1, e1684$TRT, e1684$SEX, e1684$AGE)
dtab <- table(e1684$FAILTIME)
e1684d <- merge(dtab, e1684, by.x = )
n <- nrow(X)
nbeta <- ncol(X)
ngamma <- ncol(Z)
datfin <- list(X = X, Z = Z, )