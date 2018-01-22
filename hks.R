library(readstata13)
library(plyr)
library(tvcure)

hks <- read.dta13("C:/Users/gwill/Dropbox/Research/Peace Settlement Conceptulization Paper/Replication Analysis/Replication - HKS 2016 CMPS/ReplicationHSK2016CMPS.dta")
hks <- plyr::rename(hks, replace = c("_st" = "st", "_d"="event","_t"="stop","_t0"="start"))

a <- coxph(Surv(start, stop, event) ~ troop + police + militaryobservers + wardur + brv_warAgg + osvAll + lntpop + polity2 + polity2Sq + rebpolwing + numprevepisodes + lnrgdppc + troop * lnrgdppc, data = hks);a
cox.a <- coxsimInteract(cox, "troop", "lnrgdppc", qi = "Hazard Rate", X1 = 1000, X2 = 4)

cl <- makeCluster(4, "SOCK")
registerDoSNOW(cl)
hks_tvcure <- tvcure(Surv(start, stop, event) ~ troop + police + militaryobservers,
      cureform = ~ troop + police + militaryobservers + ,
      model = "ph",
      data = hks,
      emmax = 1000,
      nboot = 100)

hks_tvcure_novar <- tvcure(Surv(start, stop, event) ~ troop + police + militaryobservers,
      cureform = ~ troop + police + militaryobservers,
      model = "ph",
      data = hks,
      emmax = 1000,
      var = F)

hks_tvcure_firth <- tvcure(Surv(start, stop, event) ~ troop + police + militaryobservers,
      cureform = ~ troop + police + militaryobservers + lntpop + polity2 +
        polity2Sq + rebpolwing + numprevepisodes,
      model = "ph",
      data = hks,
      emmax = 1000,
      firthlogit = T,
      nboot = 100)

hks_tvcure_probit <- tvcure(Surv(start, stop, event) ~ troop + police + militaryobservers,
      cureform = ~ wardur + brv_warAgg + osvAll + lntpop + polity2 + polity2Sq + rebpolwing + numprevepisodes + lnrgdppc + negsettleFull + lowactFull,
      model = "ph",
      data = hks,
      emmax = 1000,
      nboot = 500,
      link = "probit")

hkstvcure <- tvcure(Surv(start, stop, event) ~ troop + police + militaryobservers + polity + victoryFull,
      cureform = ~ troop + police + militaryobservers + victoryFull,
      model = "ph",
      data = hks,
      emmax = 100,
#      firthlogit = T,
      nboot = 100)


#wardur + brv_warAgg + osvAll + lntpop + polity2 + polity2Sq + rebpolwing + numprevepisodes + lnrgdppc + negsettleFull +  + lowactFull,
# robust
# stcox troop police militaryobservers wardur brv_warAgg osvAll lntpop polity2 polity2Sq rebpolwing numprevepisodes lnrgdppc if negsettleFull==1 | lowactFull==1 , robust nohr
