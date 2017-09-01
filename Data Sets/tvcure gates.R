library(readstata13)
library(plyr)
library(tvcure)

gates <- read.dta13("C:/Users/gwill/Dropbox/Dissertation/tvcure/Data Sets/AOWYDataGates2016Replication.dta")
gates <- plyr::rename(gates, replace = c("_st" = "st", "_d"="event","_t"="stop","_t0"="start"))

cl <- makeCluster(3, "SOCK")
registerDoSNOW(cl)

gates.tvsurv <- tvcure(Surv(start, stop, event) ~ constraining + dispersive +
                         inclusive + growth + democ_aclp_gwf + lnpop + lngdpcap
                       + cen_elf + prox_pschange_10 + interregnum + missing +
                         prior_conflict_intensity + duration + pk_dum,
                       cureform = ~ constraining + dispersive + inclusive + growth
                       + democ_aclp_gwf + lnpop + lngdpcap + cen_elf +
                         prox_pschange_10 + interregnum + missing +
                         prior_conflict_intensity + duration + pk_dum,
                       data = gates,
                       firthlogit = T,
                       emmax = 50,
                       model = "ph")

#gates.cox<-coxph(Surv(start,stop,event)~constraining+dispersive+inclusive+growth+democ_aclp_gwf+lnpop+lngdpcap+cen_elf+prox_pschange_10+interregnum+missing+prior_conflict_intensity+duration+pk_dum, data=gates)

#gates.cox.ridge<-coxph(Surv(start,stop,event)~ridge(constraining)+ridge(dispersive)+ridge(inclusive)+ridge(growth)+ridge(democ_aclp_gwf)+ridge(lnpop)+ridge(lngdpcap)+ridge(cen_elf)+ridge(prox_pschange_10)+ridge(interregnum)+ridge(missing)+ridge(prior_conflict_intensity)+ridge(duration)+ridge(pk_dum), data=gates)
