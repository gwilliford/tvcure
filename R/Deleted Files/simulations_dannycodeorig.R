syndata <- function(nobs, b=c(.5,1.5), g=c(1,.75), sh=2){
x1 <- runif(nobs,0,2)
z1 <- runif(nobs,0,1)
infl.lat <- cbind(1,z1) %*% g + rlogis(nobs)
infl <- as.numeric(infl.lat > 0)
mean.infl <- mean(infl)
outcome <- rweibull(nobs, scale=exp(cbind(1,x1) %*% b), shape=sh)*infl
y<-outcome
y[y>25]<-25
y[y==0]<-25
#<-recode(outcome, "25:hi=25; 0=25")
ct<-as.numeric(y<25)
dvs<-cbind(ct,y)
return(list(X=cbind(1,x1), Z=cbind(1,z1), dvs=dvs,
infl=infl, mean.infl=mean.infl))
}

weib.lik <- function(theta,y,X,Z){
beta<-theta[1:2]
gamma<-theta[3:4]
p<-theta[5]
d<-y[,1]
ti<-y[,2]
lambda<-exp(X%*%beta)
su<-pweibull(ti, scale=lambda, shape=p, lower.tail=F)
haz<-dweibull(ti, scale=lambda, shape=p)
pr1<-plogis(Z%*%gamma)
pr0<-plogis(Z%*%gamma, lower.tail=F)
lls<-log(pr0+pr1*su)
lld<-log(pr1*haz)
cens<-lls*(1-d)
nocens<-lld*d
logl<-sum(cens+nocens)
return(-logl)
}

reps<-100
obs<-1000
ests<-matrix(nrow=reps, ncol=5)
inflate.mean<-matrix(nrow=reps, ncol=1)

i<-1

while (i <= reps) {

dat<-syndata(obs)
est<-optim(c(1, 1, 1, 1, 1), weib.lik, method="BFGS", hessian=T, y=dat$dvs, X=dat$X, Z=dat$Z)
ests[i,]<-est$par
inflate.mean[i,]<-dat$mean.infl

i<-i+1
}

for (i in 1:5) {
print(mean(ests[,i]))
}

b0<-as.matrix(rep(.5, reps), byrow=T)
b0bias<-as.matrix(ests[,1]-b0)

b1<-as.matrix(rep(1.5, reps), byrow=T)
b1bias<-as.matrix(ests[,2]-b1)

g0<-as.matrix(rep(1, reps), byrow=T)
g0bias<-as.matrix(ests[,3]-g0)

g1<-as.matrix(rep(.75, reps), byrow=T)
g1bias<-as.matrix(ests[,4]-g1)

a<-as.matrix(rep(2, reps), byrow=T)
abias<-as.matrix(ests[,5]-a)

results<-cbind(ests, b0bias, b1bias, g0bias, g1bias, abias, inflate.mean)

summary(results)

par(mfrow=c(2, 2))
plot(density(b0bias), main="Bias in Estimates (b0)", xlab="")
plot(density(b1bias), main="Bias in Estimates (b1)", xlab="")
plot(density(g0bias), main="Bias in Estimates (g0)", xlab="")
plot(density(g1bias), main="Bias in Estimates (g1)", xlab="")

par(mfrow=c(3, 2))
plot(density(ests[,1]), main="Estimates (b0)", xlab="")
abline(v=.5)
plot(density(ests[,2]), main="Estimates (b1)", xlab="")
abline(v=1.5)
plot(density(ests[,3]), main="Estimates (g0)", xlab="")
abline(v=1)
plot(density(ests[,4]), main="Estimates (g1)", xlab="")
abline(v=.75)
plot(density(ests[,5]), main="Estimates (a)", xlab="")
abline(v=2)
