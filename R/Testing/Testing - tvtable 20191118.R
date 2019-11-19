library(snow)
library(foreach)
library(doParallel)
library(doSNOW)
library(tvcure)
library(smcure)
library(ggpubr)
data(e1684)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
pd <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,
             cureform = ~ TRT + SEX + AGE,
             data = e1684, parallel = T, nboot = 10)

# Test subset functionality
pda <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE,
              cureform = ~ TRT + SEX + AGE,
              data = e1684, parallel = T, nboot = 10, subset = e1684$AGE > 0)

# missing variable message
pda <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEXY + AGE,
             cureform = ~ TRT + SEX + AGE,
             data = e1684, parallel = T, nboot = 10)
