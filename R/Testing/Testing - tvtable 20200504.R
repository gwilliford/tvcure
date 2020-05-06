setwd("C:/Users/gwill/Dropbox/Research/Dissertation/tvcure/V10 Replication")
library(haven)
library(dplyr)
library(tvcure)
library(matrixStats)
library(xtable)
options(scipen = 999)
# clusterSetupSPRNG(cl, seed = round(2^32 * runif(1)))
# clusterSetupRNG(cl)
# cl <- makeCluster(4, "SOCK"); registerDoParallel(cl)
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
clusterSetupRNG (cl, type = "RNGstream")


#rlecuyer
#doRNG

# Load data
lhr <- read_dta("LHRIOOct08replication.dta")
lhr <- rename(lhr, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")
lhr$io <- ifelse(lhr$index > 0, 1, 0)
lhr$lnt <- log(lhr$stop)
lhr$caplnt <- lhr$capchange * lhr$lnt
lhr$samereg <- ifelse(lhr$twodem5 == 0 & lhr$onedem5 == 0, 1, 0)
##### Models ---------------------------------------------------------------------------

cox <- coxph(Surv(start, stop, event) ~ lndeaths + tie + battletide + thirdpartycfire +
               stakes + onedem5 + twodem5 + index + cfhist + archigosFIRC +
               contiguity + capchange, data = lhr, x = T); summary(cox)

cp <- tvcure(Surv(start, stop, event) ~ lndeaths + tie,
             cureform = ~ battletide + thirdpartycfire + tie,
             data = lhr, nboot = 10, seed = 5)
cp2 <- tvcure(Surv(start, stop, event) ~ lndeaths + tie,
             cureform = ~ battletide + thirdpartycfire + tie,
             data = lhr, nboot = 10)
summary(cp); summary(cp2)

cox2 <- cox
tvtable(cox, cox2, format = "long")
t1 <- tvtable.coxph(cox)
t2 <- tvtable.coxph(cox2)
tvtable_combine(c("t1", "t2"), format = "long")
c1 <- coxph(Surv(start, stop, event) ~ lndeaths + tie + battletide + thirdpartycfire, data = lhr)
c2 <- coxph(Surv(start, stop, event) ~ stakes + onedem5 + tie + twodem5 + index + cfhist + archigosFIRC, data = lhr)

varlist = list("Tie" = "tie",
               "Joint Democracy" = "twodem5",
               "alpha" = "stakes",
               "Agreement Strength" = "index")

tab <- tvtable(c1, cureph2, varlist = varlist)
tab2 <- tvtable_xtable(tab)
printer(tab2)
tab <- tvtable(c1, cureph2, varlist = varlist)
