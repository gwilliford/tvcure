setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data - Police Reform Data")
library(readstata13)
library(plyr)
library(tvcure)
cl <- makeCluster(3, "SOCK")
registerDoSNOW(cl)
options(scipen = 999)

hks <- read.dta13("PRPA HKS CID Dummies.dta")
hks <- plyr::rename(hks, replace = c("_st" = "st", "_d"="event","_t"="stop","_t0"="start"))
hks$index <- with(hks, accountability + training + capacity + human_rights + composition_all + int_monitoring)
hks2 <- hks[hks$agreement == 1, ]

m1 <- tvcure(Surv(start, stop, event) ~ accountability + agreement, cureform = ~ accountability + agreement, data = hks, var = F)
m2 <- tvcure(Surv(start, stop, event) ~ index + polity2 + wardur + brv_warAgg + numprevepisodes + lntpop + lnrgdppc, cureform = ~ index + lntpop + polity2 + victoryFull + wardur + brv_warAgg + numprevepisodes + lnrgdppc, data = hks2, var = T)
with(hks2, cor(cbind(accountability, lntpop, polity2, victoryFull, wardur, brv_warAgg, numprevepisodes, lnrgdppc, troop, police, militaryobservers)))


coxph(Surv(start, stop, event) ~ index + polity2 + victoryFull + wardur + brv_warAgg + numprevepisodes + lntpop + lnrgdppc + troop + police + militaryobservers, data = hks)
d3 <- as.data.frame(na.omit(hks2[, c("start", "stop", "event", "index", "lntpop", "polity2", "victoryFull", "wardur", "brv_warAgg", "numprevepisodes", "lnrgdppc", "police", "militaryobservers", "troop")]))
coxphf(Surv(start, stop, event) ~ index + polity2 + victoryFull + wardur + brv_warAgg + numprevepisodes + lntpop + lnrgdppc + troop + police + militaryobservers, data = d3)

