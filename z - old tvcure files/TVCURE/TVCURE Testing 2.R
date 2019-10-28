# Right censored dataset -------------------------------------------------------
testdata.right <- as.data.frame(list(time=c(4,3,1,1,2,2,3,4),
                            status=c(1,1,1,0,1,1,0,1),
                            x=c(0,2,1,1,1,0,0,2),
                            sex=c(0,0,0,0,1,1,1,1)))
testsurv.right<-with(testdata.right,Surv(time,status))
cox.right <- coxph(testsurv.right~x+sex, data=testdata.right)

# Counting process dataset -----------------------------------------------------
testdata.counting <- as.data.frame(cbind(start=c(1, 2, 5, 2, 1, 7, 3, 4, 8, 8),stop =c(2, 3, 6, 7, 8, 9, 9, 9,14,17), event=c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0), x=c(1, 0, 0, 1, 0, 1, 1, 1, 0, 0)))
testsurv.counting<-with(testdata.counting,Surv(start,stop,event))
cox.counting <- coxph(testsurv.counting~x, data=testdata.counting)

tvcure.counting <- tvcure(Surv(start,stop,event)~x,cureform=~x, model="ph",data=testdata.counting)

# smcure test data
library(smcure)
data(e1684)
e1684$start<-rep(0,nrow(e1684))

# Test that tvcure produces accurate estimates of smcure test data and function
set.seed(1684)
test.sm <- smcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE, data=e1684,model="ph",Var = T)
set.seed(1684)
test.tvright <- tvcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE, data=e1684,model="ph",firthlogit=T,firthcox=T,Var = F)
set.seed(1684)
test.tvcount <- tvcure(Surv(start,FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE, data=e1684,model="ph",Var = T,nboot=10)

testbeta<-cbind(test.sm$beta,test.tvright$beta,test.tvcount$beta);testbeta
testgamma<-cbind(test.sm$b,test.tvright$g,test.tvcount$g);testgamma
testbetavar<-cbind(test.sm$beta_var, test.tvright$b_var, test.tvcount$b_var);testbetavar
testgammavar<-cbind(test.sm$b_var, test.tvright$g_var, test.tvcount$g_var);testgammavar

  #length(unique(as.list(testbeta)))
  #all(sapply(df, identical, testbeta[,1]))

# Test tvcure with firthlogit ------------------------------------------------------------------
set.seed(1684)
test.tvright.firth <- tvcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE, data=e1684,model="ph",Var = T, firthlogit=T)
testfirthbeta <- cbind(test.tvright$beta,test.tvright.firth$beta);testfirthbeta
testfirthgamma <- cbind(test.tvright$g,test.tvright.firth$g);testfirthgamma
testfirthbvar <- cbind(test.tvright$b_var,test.tvright.firth$b_var);testfirthbvar
testfirthgvar <- cbind(test.tvright$g_var,test.tvright.firth$g_var);testfirthgvar

# Test tvcure with firthcox
set.seed(1684)
test.tvright.firthcox <- tvcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE, data=e1684,model="ph",Var = T, firthcox=T)
testfirthbeta <- cbind(test.tvright$beta,test.tvright.firth$beta);testfirthbeta
testfirthgamma <- cbind(test.tvright$g,test.tvright.firth$g);testfirthgamma
testfirthbvar <- cbind(test.tvright$b_var,test.tvright.firth$b_var);testfirthbvar
testfirthgvar <- cbind(test.tvright$g_var,test.tvright.firth$g_var);testfirthgvar

a<-na.omit(e1684)
b<-coxphf(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,data=a,pl=F)




smtest<-smcure(test1.surv~x+sex,data=test1,~x+sex,model="ph",Var=F)
tvtest1<-tvcure(testsurv.right~x+sex,data=testdata.right,~x+sex,model="ph",Var=F)

coxphf(testsurv.right~x+offset(sex),data=testdata.right, pl=F)

coxtest2<-coxph(test2.surv~x,data=test2)
tvtest2<-tvcure(testsurv.counting~x,data=testdata.counting,~x,model="ph",Var=F)

####Test 3 - take test1 data and format as counting
test3 <- as.data.frame(list(start=c(0,0,0,0,0,0,0,0),stop=c(4,3,1,1,2,2,3,4),
status=c(1,1,1,0,1,1,0,1), x=c(0,2,1,1,1,0,0,2),    sex=c(0,0,0,0,1,1,1,1)))
test3.surv<-with(test3,Surv(start,stop,status))
tvtest3<-tvcure(test3.surv~x+sex,cureform=~x+sex,data=test3,model="ph",Var=F)

####Test 4 - take example data from smcure and use as example
data(e1684)
e1684$start<-rep(0,nrow(e1684))
#Original function with original data
set.seed(1684)
pdorig <- smcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE, data=e1684,model="ph",Var = F)
#New function with original data
set.seed(1684)
pdtv1 <- tvcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE, data=e1684,model="ph",Var = F)
#New function with counting process data
set.seed(1684)
pdtv2 <- tvcure(Surv(start,FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE, data=e1684,model="ph",Var = T, nboot=10)
cbind(pdorig$beta,pdtv1$beta,pdtv2$beta)
cbind(pdorig$b,pdtv1$b,pdtv2$b)

####Test 5 - test firthlogit
set.seed(1684)
test5 <- tvcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE, data=e1684,model="ph",Var = F,firthlogit = TRUE)

test6<-tvcure(Surv(FAILTIME,FAILCENS)~TRT+SEX+AGE,cureform=~TRT+SEX+AGE, data=e1684,model="ph",Var = T,firthlogit = TRUE)

#library(flexmix)
#boot(test5,100,"ordinary")

####Testing logistsf
flogtest<-logistf(status~x+sex,test1)

