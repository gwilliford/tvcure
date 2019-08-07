library(readstata13)
library(plyr)
library(tvcure)

hks <- read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/tvcure/Replication Files/Replication - HKS/ReplicationHSK2016CMPS.dta")
hks <- plyr::rename(hks, replace = c("_st" = "st", "_d" = "event","_t" = "stop","_t0" = "start"))

#### These Models has a stalled bootstrap replication
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
hkstest <- tvcure(Surv(start, stop, event) ~ troop + polity2 + lntpop + lnrgdppc,
      cureform = ~ polity2,
      model = "ph",
      data = hks,
      emmax = 1000,
      nboot = 100); summary(hkstest)

hkstest <- tvcure(Surv(start, stop, event) ~ troop + lntpop + lnrgdppc,
      cureform = ~ polity2,
      brglm = T,
      model = "ph",
      data = hks,
      emmax = 1000,
      nboot = 100); summary(hkstest)

hkstest <- tvcure(Surv(start, stop, event) ~ lntpop + lnrgdppc,
      cureform = ~ polity2,
      brglm = T,
      model = "ph",
      data = hks,
      emmax = 1000,
      nboot = 100); summary(hkstest)



