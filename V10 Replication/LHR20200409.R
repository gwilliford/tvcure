setwd("C:/Users/gwill/Dropbox/Research/Dissertation/tvcure/V10 Replication")
library(haven)
library(dplyr)
library(tvcure)
library(matrixStats)
library(xtable)
options(scipen = 999)
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)

# Load data
lhr <- read_dta("LHRIOOct08replication.dta")
lhr <- rename(lhr, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")
lhr$io <- ifelse(lhr$index > 0, 1, 0)
lhr$lnt <- log(lhr$stop)
lhr$capchangelnt <- lhr$capchange * lhr$lnt
lhr$samereg <- ifelse(lhr$twodem5 == 0 & lhr$onedem5 == 0, 1, 0)
lhr$battletidelnt <- lhr$battletide * lhr$lnt
lhr$twoaut5 = ifelse(lhr$twodem5 == 0 & lhr$onedem5 == 0, 1, 0)
lhr$twodem5t = lhr$twodem5 * lhr$stop
##### Models ---------------------------------------------------------------------------

### Standard cox models
cox <- coxph(Surv(start, stop, event) ~ archigosFIRC + capchange + battletide +
                   thirdpartycfire + index + twoaut5 + twodem5 + tie + lndeaths +
                   cfhist + stakes + contiguity,
                 data = lhr,
                 x = T); summary(cox)
cox.zph(cox)
cox2 <- coxph(Surv(start, stop, event) ~ capchange + capchangelnt + battletide +
               thirdpartycfire + index + twoaut5 + twodem5 + tie + lndeaths +
               cfhist + cfhistlnt + stakes + contiguity,
             data = lhr,
             x = T); summary(cox2)
cox.zph(cox2, function(x) {log(x)})


cure1 <- tvcure(Surv(start, stop, event) ~ capchange + contiguity +
              stakes + cfhist + thirdpartycfire,
            cureform = ~ tie + battletide + lndeaths + thirdpartycfire + stakes +
              index + cfhist + twodem5 + twoaut5 + archigosFIRC,
            data = lhr,
            var = T, nboot = 30,
            brglm = T); summary(cure1)
cure2 <- tvcure(Surv(start, stop, event) ~ capchange + capchangelnt + contiguity +
              stakes + cfhist + cfhistlnt + thirdpartycfire,
            cureform = ~ tie + battletide + lndeaths + thirdpartycfire + stakes +
              index + cfhist + twodem5 + twoaut5,
            data = lhr,
            var = T, nboot = 30,
            brglm = T); summary(cure2)

##### PLOTS --------------------------------------------------------------------

breaks <- seq(0, 150, 10) * 365
labs <- seq(0, 150, 10)

### Tie variable
# Cox pred
# newdata1 <- apply(cox2$x, 2, median, na.rm = T)
# newdata1 <- as.data.frame(rbind(newdata1, newdata1))
# newdata1[2, "tie"] <- 1
# pcox <- ggsurvplot(
#   a <- surv_fit(cox, data = newdata1, conf.int = 0.90)
#   , data = newdata1)
# plot(pcox, T, col = c(1, 2))
# survpred.coxph(pcox)

# Surv uncure
tpred1 <- prediction4(cure2, "tie", c(0, 1), type = "uncureprob")
tplot1 <- tpred1 + xlab("Tie")

# Surv cure
tpred2 <- prediction4(cure2, "tie", c(0, 1), type = "spop")
tplot2 <- tpred2 + xlab("Tie")

### Deaths variable
# Cox pred
lnmin <- min(lhr$lndeaths, na.rm = T)
lnmax <- max(lhr$lndeaths, na.rm = T)
# newdata2 <- apply(cox2$x, 2, median, na.rm = T)
# newdata2 <- as.data.frame(rbind(newdata2, newdata2))
# newdata2[2, "lndeaths"] <- lnmin
# newdata2[2, "lndeaths"] <- lnmax
# pcox2 <- survfit(cox2, newdata2, conf.int = 0.90)
# plot(pcox2, T, col = c(1, 2))

dpred1 <- prediction4(cure2, "lndeaths", c(lnmin, lnmax), type = "uncureprob")
dpred1 <- dpred1 + xlab("Log Battle Deaths")
dpred2 <- prediction4(cure2, "lndeaths", c(lnmin, lnmax), type = "spop", legendtitle = "Log Battle Deaths")
dpred2 <- dpred2 + xlab("Log Battle Deaths") + xlab("Time (in years)") + scale_x_continuous(breaks = breaks, labels = labs)

### Battle consistency variable
# newdata3 <- apply(cox2$x, 2, median, na.rm = T)
# newdata3 <- as.data.frame(rbind(newdata3, newdata3))
# newdata3[1, "battletide"] <- 0
# newdata3[2, "battletide"] <- 1
# pcox3 <- survfit(cox2, newdata3, conf.int = 0.90)
# plot(pcox3, T, col = c(1, 2))

bpred1 <- prediction4(cure2, "battletide", c(0, 1), type = "uncureprob")
bpred1 <- bpred1 + xlab("Battle Consistency")
bpred2 <- prediction4(cure2, "battletide", c(0, 1), type = "spop", legendtitle = "Battle Consistency")
bpred2 <- bpred2 + xlab("Time (in years)") + scale_x_continuous(breaks = breaks, labels = labs)

##### Tables --------------------------------------------------------------------
vl <- list("Battle Deaths" = "lndeaths",
     "Tie" = "tie",
     "Battle Consistency" = "battletide",
     "Third Party Intervention" = "thirdpartycfire",
     "Capability Change" = "capchange",
     "Capability Change $\\times \\ln(t)$" = "capchangelnt",
     "Existential Stakes" = "stakes",
     "Agreement Strength" = "index",
     "Foreign-Imposed Regime Change" = "archigosFIRC",
     "Mixed Regime Type" = "onedem5",
     "Joint Democracy" = "twodem5",
     "Joint Autocracy" = "twoaut5",
     "Conflict History" = "cfhist",
     "Conflict History $\\times \\ln(t)$" = "cfhistlnt",
     "Contiguity" = "contiguity")

t1 <- tvtable(cox2, cure2, varlist = vl)
x1 <- tvtable_xtable(t1)
printer(x1)

