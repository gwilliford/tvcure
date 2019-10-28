library(readstata13)
library(tvcure)
library(dplyr)

# library(splitstackshape)
# library(plyr)
# library(coxphf)
# library(xtable)
# library(stargazer)
# library(coefplot)
# options(scipen = 999)

# Load data
lhr <- read.dta13("C:/Users/gwill/Dropbox/Research/Dissertation/tvcure/PaperV9ReplicationAnalysis/LHR20191028.R")
lhr <- rename(lhr, replace = c("_st" = "st", "_d" = "event", "_t" = "stop", "_t0" = "start"))


a <- tvcure(Surv(start, stop, event) ~  capchange + battletide + index + tie + lndeaths + cfhist, cureform = ~ wernerFIRC + capchange + battletide + thirdpartycfire + index + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr, model = "ph", nboot = 100)

# Information - Yes/No
# Commitment -
