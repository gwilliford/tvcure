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

##### Coxph diagnostics
#
# coxfull <- coxph(Surv(start, stop, event) ~ archigosFIRC + capchange + battletide +
#                thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths +
#                cfhist + stakes + contiguity,
#              data = lhr,
#              x = T); summary(coxfull)
# coxfullph <- coxph(Surv(start, stop, event) ~ archigosFIRC + archigosFIRClnt + battletide + capchange +
#                      thirdpartycfire + index + onedem5 + twodem5 + twodem5lnt + tie + lndeaths +
#                      cfhist + stakes + contiguity + contiguitylnt,
#                    data = lhr,
#                    x = T); summary(coxfullph)

##### PH Diagnostics
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
#### cf hist --- capchange -- archigosFIRC ----- tie(ish)
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
b <- prediction3(a, variable = "twodem5", values = c(0, 1), type = "uncureprob", CI = F)



####

c1 <- survfit(cox2)

##### PLOTS --------------------------------------------------------------------

### Tie variable
# Cox pred
newdata1 <- apply(cox2$x, 2, median, na.rm = T)
newdata1 <- as.data.frame(rbind(newdata1, newdata1))
newdata1[2, "tie"] <- 1
pcox <- survfit(cox, newdata1, conf.int = 0.90)
plot(pcox, T, col = c(1, 2))

# Surv uncure
tpred1 <- prediction4(cure2, "tie", c(0, 1), type = "uncureprob")
plot(tpred1)

# Surv cure
tpred2 <- prediction4(cure2, "tie", c(0, 1), type = "spop")
plot(tpred2)

### Deaths variable
# Cox pred
lnmin <- min(lhr$lndeaths, na.rm = T)
lnmax <- max(lhr$lndeaths, na.rm = T)
newdata2 <- apply(cox2$x, 2, median, na.rm = T)
newdata2 <- as.data.frame(rbind(newdata2, newdata2))
newdata2[2, "lndeaths"] <- lnmin
newdata2[2, "lndeaths"] <- lnmax
pcox <- survfit(cox2, newdata2, conf.int = 0.90)
plot(pcox, T, col = c(1, 2))

dpred1 <- prediction4(cure2, "lndeaths", c(lnmin, lnmax), type = "uncureprob")
plot(dpred1)
dpred2 <- prediction4(cure2, "lndeaths", c(lnmin, lnmax), type = "spop", legendtitle = "Log Battle Deaths")
plot(dpred2)

### Battle consistency variable
newdata3 <- apply(cox2$x, 2, median, na.rm = T)
newdata3 <- as.data.frame(rbind(newdata3, newdata3))
newdata3[1, "battletide"] <- 0
newdata3[2, "battletide"] <- 1
pcox <- survfit(cox2, newdata3, conf.int = 0.90)
plot(pcox, T, col = c(1, 2))

bpred1 <- prediction4(cure2, "battletide", c(0, 1), type = "uncureprob")
plot(bpred1)
bpred2 <- prediction4(cure2, "battletide", c(0, 1), type = "spop", legendtitle = "Battle Consistency")
plot(bpred2)

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
     "Conflict History" = "cfhist",
     "Conflict History $\\times \\ln(t)$" = "cfhistlnt",
     "Contiguity" = "contiguity")
# parttab <- tvtable_tvcure(curepart, format = "long", varlist = vl)
# fulltab <- tvtable_tvcure(curefull, format = "long", varlist = vl)
# coxtab <- tvtable_coxph(cox, format = "long", varlist = vl)

tvtable(cox2, cure2)

# tvtable_tvcure(curepart, format = "long", varlist = vl)

# Combined Table
combtab <- tvtable_combine(c("coxtab", "parttab"))
print(xtable(combtab, align = "llXXX"),
      booktabs = T,
      tabular.environment = "tabularx",
      width="\\textwidth",
      include.rownames = F,
      sanitize.text.function=identity,
      hline.after = getOption("xtable.hline.after", c(-1,0,nrow(x))),
) #file = "./tab/combtab.tex")
combtab2 <- tvtable_combine(c("fulltab", "parttab"), modnum = T)
print(xtable(combtab2, align = "llXXXX"),
      booktabs = T,
      tabular.environment = "tabularx",
      width="\\textwidth",
      include.rownames = F,
      sanitize.text.function=identity,
      hline.after = getOption("xtable.hline.after", c(-1,0,nrow(x))),
) #file = "./tab/combtab.tex")
combtab3 <- tvtable_combine(c("coxtab", "fulltab", "parttab"), modnum = T)
print(xtable(combtab3, align = "llXXXXX"),
      booktabs = T,
      tabular.environment = "tabularx",
      width="\\textwidth",
      include.rownames = F,
      sanitize.text.function=identity,
      hline.after = getOption("xtable.hline.after", c(-1,0,nrow(x))),
) #file = "./tab/combtab.tex")


newdata <- apply(cox$x, 2, median, na.rm = T)
newdata <- as.data.frame(rbind(newdata, newdata))
newdata[2, "tie"] <- 1
pcox <- survfit(cox, newdata, conf.int = 0.90)
plot(pcox, T, col = c(1, 2))
# pcox <- predict(cox, type = "expected", data = lhr)
# scox <- exp(-pcox)
# lattice(x = lhr$stop, x = )
# library(survminer)
ggsurvplot(cox, newdata)
newdata2 <- as.data.frame(rbind(newdata, newdata))
newdata2[2, "twodem5"] <- 1
pcox <- survfit(cox, newdata2, conf.int = 0.90)
plot(pcox, T, col = c(1, 2))

ppart <- prediction3(curefull, "twodem5", c(0, 1), "uncureprob")
plot(ppart)
ppart <- prediction3(curepart, "twodem5", c(0, 1), "spop")
plot(ppart)
ppart <- prediction3(curepart, "twodem5", c(0, 1), "suncure")
plot(ppart)

ppart <- prediction3(curepart, "contiguity", c(0, 1), "uncureprob")
plot(ppart)
ppart <- prediction3(curepart, "contiguity", c(0, 1), "spop")
plot(ppart)

ppart <- prediction3(curepart, "tie", c(0, 1), "uncureprob")
plot(ppart)
ppart <- prediction3(curepart, "tie", c(0, 1), "spop")
plot(ppart)
ppart <- prediction3(curepart, "tie", c(0, 1), "suncure")
plot(ppart)


ppart <- prediction3(curepart, "lndeaths", seq(5, 16, 1), "uncureprob"); plot(ppart)
ppart <- prediction3(curepart, "lndeaths", c(5, 10, 15), "spop")
a <- plot(ppart)
a <- a + geom_vline(aes(xintercept = 8789))
newdata <- apply(cox$x, 2, median, na.rm = T)
newdata4 <- as.data.frame(rbind(newdata, newdata))
newdata4[2, "lndeaths"] <- 16
newdata4[2, "lndeaths"] <- 5
alpha <- predict(cox, newdata4, "risk", se.fit = T)



plot(survfit(cox, newdata4), conf.int = T, col = c(1:2))
ppart <- prediction3(curepart, "lndeaths", c(5, 10, 15), "suncure")



a <- survminer::ggcoxzph(cox.zph(cox))



ggcoxdiagnostics(cox, type = "deviance", linear.predictions = F)
