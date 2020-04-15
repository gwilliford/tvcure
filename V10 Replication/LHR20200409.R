library(haven)
library(dplyr)
library(tvcure)
library(matrixStats)
# plyr package also required - called directly
options(scipen = 999)
setwd("C:/Users/gwill/Dropbox/Research/Dissertation/Article 1 - Cure Models/V10 Replication")

# Load data
lhr <- read_dta("LHRIOOct08replication.dta")
lhr <- plyr::rename(lhr, replace = c("_st" = "st", "_d" = "event", "_t" = "stop", "_t0" = "start"))
# lhr$capmin <- rowMins(cbind(lhr$cap_1, lhr$cap_2))
# lhr$capmax <- rowMaxs(cbind(lhr$cap_1, lhr$cap_2))
# lhr$caprat <- lhr$capmin / lhr$capmax
# lhr <- lhr %>%
#   group_by(id) %>%
#   mutate(
#     minyear = min(year),
#     #capminyr = caprat[year == minyear]
#   ) %>%
#   ungroup(lhr) %>%
#   arrange(id, year)
#
#
# lhr <- lhr %>%
#   group_by(id, year) %>%
#   mutate(
#     mincapyr = mean(caprat, na.rm = T)
#   ) %>%
#   ungroup(lhr) %>%
#   group_by(id) %>%
#   mutate(
#     mincapid = if_else(year == minyear, mincapyr, 0),
#     mincapid = max(mincapid, na.rm = T),
#     capchbeg = (caprat - mincapid)/mincapid
#   )
# id <- unique(lhr$id)
# mincapyr <- lhr$caprat[lhr$year == lhr$minyear]
# mat <- cbind(id, mincapyr)
#
#
# a <- duplicated(lhr[, c("id", "year")])
# b <- lhr[a, ] %>% arrange(id, year)
# flhrcapbet = lhr %>%
#   group_by(id) %>%
#   mutate(
#     ,
#   )
# )



cox <- coxph(Surv(start, stop, event) ~ lndeaths + tie + battletide + thirdpartycfire + archigosFIRC + stakes + onedem5 + twodem5 + index + cfhist + contiguity + capchange, data = lhr); summary(cox)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
m1 <- tvcure(Surv(start, stop, event) ~  capchange + onedem5,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire +
               stakes +
               twodem5 + index + cfhist + contiguity,
             data = lhr, var = T, nboot = 30, brglm = T); summary(m1)




m2 <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths + battletide + tie + thirdpartycfire +
               stakes +
               twodem5 + onedem5 + index + cfhist + contiguity,
             cureform = ~ archigosFIRC + lndeaths + battletide + tie + thirdpartycfire +
               stakes + onedem5 +
               twodem5 + index + cfhist + contiguity,
             data = lhr, var = T, nboot = 100, brglm = T); summary(m2)



m3 <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths + battletide + tie + thirdpartycfire +
               stakes +
               twodem5 + onedem5 + index + cfhist + contiguity,
             cureform = ~ archigosFIRC + capchange + lndeaths + battletide + tie + thirdpartycfire +
               stakes + onedem5 +
               twodem5 + index + cfhist + contiguity,
             data = lhr, var = T, nboot = 100, brglm = T); summary(m3)





cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
m1 <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths + contiguity + index + cfhist,
             cureform = ~ archigosFIRC + capchange +  lndeaths + battletide + tie + thirdpartycfire +
               stakes +
               onedem5 + twodem5 + index + cfhist + contiguity,
             data = lhr, var = T, nboot = 100, brglm = T); summary(m1)

m2 <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths + contiguity + index + cfhist + twodem5,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire +
               stakes +
               twodem5 + index + contiguity,
             data = lhr, var = T, parallel = T, brglm = T, nboot = 30); summary(m2)


m1 <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths + contiguity,
             cureform = ~ archigosFIRC + lndeaths + battletide + tie + thirdpartycfire +
               stakes +
               onedem5 + twodem5 + index + cfhist + contiguity,
             data = lhr, var = T, nboot = 100, brglm = T); summary(m1)




prediction3(m1, "capchange", c(), type = "spop")

m2 <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths + contiguity + index + cfhist,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire +
               stakes +
               twodem5 + index + cfhist + contiguity,
             data = lhr, var = F, nboot = 100, brglm = T); summary(m2)




m3 <- tvcure(Surv(start, stop, event) ~  archigosFIRC + capchange +  lndeaths + battletide + tie + thirdpartycfire +
               stakes +
               onedem5 + twodem5 + index + cfhist + contiguity,
             cureform = ~ archigosFIRC + capchange +  lndeaths + battletide + tie + thirdpartycfire +
               stakes +
               onedem5 + twodem5 + index + cfhist + contiguity,
             data = lhr, var = T, nboot = 100, brglm = T); summary(m3)

m3 <- tvcure(Surv(start, stop, event) ~  capchange +  lndeaths + battletide + tie + thirdpartycfire +
               stakes +
               onedem5 + twodem5 + index + cfhist + contiguity,
             cureform = ~ archigosFIRC + lndeaths + battletide + tie + thirdpartycfire +
               stakes +
               onedem5 + twodem5 + index + cfhist + contiguity,
             data = lhr, var = T, nboot = 100, brglm = T); summary(m3)




cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
m1 <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths + battletide + contiguity +
               index + stakes + cfhist + onedem5 + archigosFIRC,
            cureform = ~ lndeaths + battletide + tie + thirdpartycfire + twodem5 +
              cfhist + index + archigosFIRC + contiguity + onedem5 + stakes,
             data = lhr, var = F, brglm = T)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
m1 <- tvcure(Surv(start, stop, event) ~  twodem5 + lndeaths,
             cureform = ~ cfhist,
             data = lhr, var = F, brglm = T)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
m2a <- tvcure(Surv(start, stop, event) ~  capchange + contiguity +
               index + stakes + cfhist + onedem5,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire + twodem5 +
               cfhist + index + archigosFIRC + contiguity + onedem5 + stakes,
             data = lhr, nboo = 10)
m2b <- tvcure(Surv(start, stop, event) ~  capchange + contiguity +
               index + stakes + cfhist + onedem5,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire + twodem5 +
               cfhist + index + archigosFIRC + contiguity + onedem5 + stakes,
             data = lhr, var = T, brglm = T, nboot = 10)


m3 <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths + battletide + contiguity +
               index + stakes + cfhist + onedem5,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire + twodem5 +
               cfhist + index + archigosFIRC + contiguity + onedem5 + stakes,
             data = lhr, brglm = T, nboot = 1000)


m4 <- tvcure(Surv(start, stop, event) ~  capchange,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire + twodem5 +
               cfhist + index + archigosFIRC + contiguity + onedem5 + stakes,
             data = lhr, brglm = T, nboot = 100)

m4 <- tvcure(Surv(start, stop, event) ~  capchange,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire + twodem5 +
               cfhist + index + archigosFIRC + contiguity + onedem5 + stakes,
             data = lhr, brglm = T, nboot = 100)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
m5 <- tvcure(Surv(start, stop, event) ~  capchange,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire + twodem5 +
               cfhist + index + archigosFIRC + contiguity + onedem5 + stakes,
             data = lhr,
             firthcox = T, brglm = T,
             nboot = 100)

survobj <- Surv(lhr$start, lhr$stop, lhr$event)
survobj2 <- subset


cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
m6 <- tvcure(Surv(start, stop, event) ~  capchange,
             cureform = ~ lndeaths,
             data = lhr,
             firthcox = T, brglm = T,
             var = F)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
m7 <- tvcure(Surv(start, stop, event) ~  capchange,
             cureform = ~ lndeaths,
             data = lhr,
             brglm = T,
             nboot = 100)

m8 <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths + battletide + contiguity +
               index + stakes + cfhist + onedem5,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire + twodem5 +
               cfhist + index + archigosFIRC + contiguity + onedem5 + stakes,
             data = lhr,
             var = T, nboot = 100,
             brglm = T)

m9 <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths + battletide + contiguity +
               index + cfhist + onedem5,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire + twodem5 +
               cfhist + index + archigosFIRC + contiguity + stakes,
             data = lhr,
             var = T, nboot = 100,
             brglm = T)

m10 <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths +
               index + cfhist + onedem5,
             cureform = ~ lndeaths + battletide + tie + thirdpartycfire + twodem5 +
               cfhist + index + archigosFIRC + contiguity + stakes,
             data = lhr,
             var = T, nboot = 100,
             brglm = F)

