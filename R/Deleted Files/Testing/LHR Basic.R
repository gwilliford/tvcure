library(readstata13)
library(splitstackshape)
library(plyr)
library(tvcure)
library(coxphf)
library(xtable)
library(stargazer)
library(coefplot)
options(scipen = 999)

# Load data
lhr <- read.dta13("C:/Users/gwill/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR 2008/lhrIOOct08replication.dta")
lhr <- rename(lhr, replace = c("_st" = "st", "_d" = "event", "_t" = "stop", "_t0" = "start"))
lhrna <- as.data.frame(na.omit(with(lhr, cbind(start, stop, event, archigosFIRC, wernerFIRC, capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity, warnumb))))

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
a <- tvcure(Surv(start, stop, event) ~ archigosFIRC, cureform = ~ archigosFIRC, data = lhr, model = "ph", firthlogit = T, nboot = 100)
a <- tvcure(Surv(start, stop, event) ~  capchange + battletide + index + tie + lndeaths + cfhist, cureform = ~ wernerFIRC + capchange + battletide + thirdpartycfire + index + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr, model = "ph", nboot = 100)
