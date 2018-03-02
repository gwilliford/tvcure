setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Data - Police Reform Data")
library(readstata13)
library(plyr)
library(tvcure)
cl <- makeCluster(3, "SOCK")
registerDoSNOW(cl)
options(scipen = 999)

hks <- read.dta13("PRPA HKS CID Dummies.dta")
hks <- plyr::rename(hks, replace = c("_st" = "st", "_d"="event","_t"="stop","_t0"="start"))
hks2 <- hks[hks$agreement == 1, ]

m1 <- tvcure(Surv(start, stop, event) ~ accountability + agreement, cureform = ~ accountability + agreement, data = hks)
m2 <- tvcure(Surv(start, stop, event) ~ accountability, cureform = ~ accountability, data = hks2)
