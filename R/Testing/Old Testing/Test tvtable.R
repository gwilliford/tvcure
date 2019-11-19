library(snow)
library(foreach)
library(doParallel)
library(doSNOW)
library(tvcure)
library(smcure)
data(e1684)

# Run model sequentially

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
pd <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX + AGE, cureform = ~ TRT + SEX + AGE, data = e1684, model = "ph", parallel = T)
summary(pd)
tvtable(pd)

# Test qi functionality
tvtable(pd)
tvtable(pd, qi = "pvalue")
tvtable(pd, qi = "zscore")

# Test stars functionality
tvtable(pd, stars = F)
tvtable(pd, qi = "zscore", stars = F)

# Test digits functionality
tvtable(pd, digits = 5)

# Test format functionality
tvtable(pd, format = "long")
tvtable(pd, qi = "pvalue", format = "long")
tvtable(pd, qi = "zscore", format = "long")

tvtable(pd, format = "stacked")

# Test ability to create tables with different variables in each eq
pd2 <- tvcure(Surv(FAILTIME, FAILCENS) ~ TRT + SEX, cureform = ~ TRT + AGE, data = e1684, model = "ph", parallel = F)
tvtable(pd2)
tvtable(pd2, format = "long")
tvtable(pd2, format = "stacked")

library(sjlabelled)
e1684$SEX <- set_label(e1684$SEX, "sex")
e1684$AGE <- set_label(e1684$AGE, "Age")
e1684$TRT <- set_label(e1684$TRT, "Treatment")
get_label(e1684)

