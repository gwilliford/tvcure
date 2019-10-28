###Setup
rm(list=ls())
setwd("C:/Users/gwill_000/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR")
setwd("C:/Users/gww17580/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/replication - lhr")
library(readstata13)
library(plyr)
library(eha)
library(R2jags)

###Load data, rename surv variables, create survival object, create binary data
lhr<-read.dta13("lhrIOOct08replication.dta")
lhr<-rename(lhr,c("_t0"="start"))
lhr<-rename(lhr,c("_t"="stop"))
lhr<-rename(lhr,c("_d"="event"))
lhr.surv<-Surv(lhr$start, lhr$stop, lhr$event)
lhr2<-lhr[,c("archigosFIRC", "archigosFIRClnt", "capchange", "battletide", "thirdpartycfire", "index", "onedem5", "twodem5", "twodem5lnt", "tie", "lndeaths", "cfhist", "stakes", "contiguity", "contiguitylnt", "LHRcluster", "start", "stop", "event", "id")]
lhr.bin<-na.omit(toBinary(lhr2, surv=c("start", "stop", "event")))

###Run frequentist cox model
lhr.cox<-coxph(lhr.surv~archigosFIRC + archigosFIRClnt + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + twodem5lnt + tie + lndeaths + cfhist + stakes + contiguity + contiguitylnt+cluster(LHRcluster), data=lhr, ties=c("breslow"))
summary(lhr.cox)

###Format data for use in stan
nrow<-nrow(lhr.bin)
lhr.bin$id<-as.numeric(droplevels(as.factor(lhr.bin$id)))
nsub<-length(unique(lhr.bin$id))
idx <- sort(unique(lhr.bin$riskset))
dummy <- matrix(NA, nrow = nrow, ncol = length(idx))
for (j in 1:length(idx)) {dummy[,j] <- as.integer(lhr.bin$riskset == idx[j])}
X<-with(lhr.bin,as.matrix(cbind(rep(1,nrow),archigosFIRC, archigosFIRClnt,capchange, battletide, thirdpartycfire, index, onedem5, twodem5, twodem5lnt, tie, lndeaths, cfhist, stakes, contiguity, contiguitylnt,dummy)))
Z<-with(lhr.bin,as.matrix(cbind(rep(1,nrow),archigosFIRC,capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity)))
ncovx<-ncol(X)
ncovz<-ncol(Z)
K<-10000
zeros<-rep(0,nrow)
datfin<-list(y=lhr.bin$event, X=X, Z=Z, nrow=nrow, nsub=nsub, ncovx=ncovx, ncovz=ncovz, id=lhr.bin$id, K=K, zeros=zeros)

m1mod<-function(){
  for(i in 1:nrow){
    #Define zero indicator
    d[i]<-step(y[i]-1)
    #Define inflation equation
    logit(p[i])<-inprod(Z[i,],g)+r1[id[i]]
    #Define incidence equation
    log(mu[i])<-inprod(X[i,],b)+r2[id[i]]
    #Define probabiity of zero
    thtau[i]<-p[i]*(1-exp(-mu[i]))
    #Define log-likelihood
    ll[i]<-(1-d[i])*log(1-thtau[i]) + d[i]*(log(thtau[i])+y[i]*log(mu[i]) -mu[i] - loggam(y[i]+1)-log(1-exp(-mu[i]))) 
    #zeros
    zeros[i]~dpois(phi[i])
    phi[i]<- -ll[i]+K
  }
  
  ###Coefficient priors
  #beta
  for(i in 1:ncovx){b[i]~dnorm(0,.01)}
  #gamma
  for(i in 1:ncovz){g[i]~dnorm(0,.01)}
  
  ####Random effects
  for (i in 1:nsub){
    r1[i]~dnorm(0,tau1)
    m[i]<-psi*r1[i]	  
    r2[i]~dnorm(m[i],tau2) 
  }
  psi~dnorm(0,.001)
  tau1~dgamma(.001,.001)							# 1/eta^2_1 
  tau2~dgamma(.001,.001)							# 1/eta^2_2
  sigma1<-1/sqrt(tau1) 								   # SD of b1_i 
  denom1<-pow(psi,2)/tau1+1/tau2
  sigma2<-sqrt(denom1)							    # SD of b2_i
  denom2<-sqrt(denom1/tau1)
  num<-psi/tau1
  rho<-num/denom2										# corr(b1_i,b2_i)  
}

system.time(m1fit<-jags(data=datfin, parameters.to.save = c("b","g"), model.file = m1mod, n.chains=2, n.iter=2000))
#run time 6.509167 hours
m1fit<-update(m1fit, n.iter=5000)
m1mcmc<-as.mcmc(m1fit)
write.csv(m1mcmc[[1]], "m1fit.csv")
write.csv(m1mcmc[[2]], "m1fitb.csv")
m1fitu2<-update(m1fit, n.iter=10000)
m2mcmc<-as.mcmc(m1fitu2)
write.csv(m2mcmc[[1]], "m2fita.csv")
write.csv(m2mcmc[[2]], "m2fitb.csv")