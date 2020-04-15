setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Article 1 - Cure Models/V10 Replication")
library(haven)
library(dplyr)
library(tvcure)
library(matrixStats)
options(scipen = 999)
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)

# Load data
lhr <- read_dta("LHRIOOct08replication.dta")
lhr <- rename(lhr, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")

##### Models ---------------------------------------------------------------------------

# Cox model - full
cox <- coxph(Surv(start, stop, event) ~ lndeaths + tie + battletide + thirdpartycfire + archigosFIRC + stakes + onedem5 + twodem5 + index + cfhist + contiguity + capchange, data = lhr); summary(cox)

# Cure model - full
curefull <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths +
                     battletide + tie + thirdpartycfire + stakes +
                     twodem5 + onedem5 + index + cfhist + contiguity,
             cureform = ~ archigosFIRC + capchange + lndeaths + battletide +
               tie + thirdpartycfire + stakes + onedem5 + twodem5 + index +
               cfhist + contiguity,
             data = lhr, var = T, nboot = 100, brglm = T); summary(curefull)

# Cure model - partial
curepart <- tvcure(Surv(start, stop, event) ~  capchange + index +
                     stakes + cfhist + contiguity,
             cureform = ~ archigosFIRC + capchange + lndeaths + battletide +
               tie + thirdpartycfire + stakes + onedem5 + twodem5 + index +
               cfhist + contiguity,
             data = lhr, var = T, nboot = 30, brglm = T); summary(m1)
