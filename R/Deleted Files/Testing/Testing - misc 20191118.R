library(snow)
library(foreach)
library(doParallel)
library(doSNOW)
library(tvcure)
library(smcure)
library(ggpubr)
data(e1684)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)

# Full model
pd <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,
             cureform = ~ TRT + SEX + AGE,
             data = e1684, parallel = T, nboot = 10)

# Partial model
pd2 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX,
              cureform = ~ TRT + AGE,
              data = e1684, parallel = T, nboot = 10)

# Basic testing
tvtable(pd)
tvtable(pd2)
tvtable(pd, qi = "pvalue")
tvtable(pd, qi = "zscore")

# Testing long format
tvtable(pd, "long")


#
