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
lhr <- read.dta13("C:/Users/gwill/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR 2008/lhrIOOct08replication.dta")
lhr <- rename(lhr, replace = c("_st" = "st", "_d" = "event", "_t" = "stop", "_t0" = "start"))
