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

##### Models ---------------------------------------------------------------------------


##### set.seed

# Cox model - full
cox <- coxph(Surv(start, stop, event) ~ lndeaths + tie + battletide + thirdpartycfire +
               archigosFIRC + stakes + onedem5 + twodem5 + index + cfhist +
               contiguity + capchange, data = lhr, x = T); summary(cox)
saveRDS(coxtab, file = "./res/coxtab.RDS")

# Cure model - full
curefull <- tvcure(Surv(start, stop, event) ~  capchange + lndeaths +
                     battletide + tie + thirdpartycfire + stakes +
                     twodem5 + onedem5 + index + cfhist + contiguity,
             cureform = ~ archigosFIRC + capchange + lndeaths + battletide +
               tie + thirdpartycfire + stakes + onedem5 + twodem5 + index +
               cfhist + contiguity,
             data = lhr, var = T, nboot = 1000, brglm = T); summary(curefull)
saveRDS(curefull, file = "./res/curefull.RDS")


# Cure model - partial
curepart <- tvcure(Surv(start, stop, event) ~  capchange + index +
                     stakes + cfhist + contiguity,
             cureform = ~ archigosFIRC + capchange + lndeaths + battletide +
               tie + thirdpartycfire + stakes + onedem5 + twodem5 + index +
               cfhist + contiguity,
             data = lhr, var = T, nboot = 1000, brglm = T); summary(curepart)
saveRDS(curepart, file = "./res/curepart.RDS")


##### Tables --------------------------------------------------------------------
vl <- list("Battle Deaths" = "lndeaths",
     "Tie" = "tie",
     "Battle Tide" = "battletide",
     "Third Party Intervention" = "thirdpartycfire",
     "Capability Change" = "capchange",
     "Existential Stakes" = "stakes",
     "Agreement Strength" = "index",
     "Foreign-Imposed Regime Change" = "archigosFIRC",
     "Mixed Regime Type" = "onedem5",
     "Joint Democracy" = "twodem5",
     "Conflict History" = "cfhist",
     "Contiguity" = "contiguity")
parttab <- tvtable_tvcure(curepart, format = "long", varlist = vl)
fulltab <- tvtable_tvcure(curefull, format = "long", varlist = vl)
coxtab <- tvtable_coxph(cox, format = "long", varlist = vl)



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


# Footnote - pvalues/FIRC stands for foreign imposed regime change.

# Good list
tvtable.coxph(cox, format = "long",
        varlist = list("Battle Deaths" = "lndeaths",
                       "Tie" = "tie",
                       "Battle Tide" = "battletide",
                       "Third Party Intervention" = "thirdpartycfire",
                       "Capability Change" = "capchange",
                       "Existential Stakes" = "stakes",
                       "Agreement Strength" = "index",
                       "Foreign-Imposed Regime Change" = "archigosFIRC",
                       "Mixed Regime Type" = "onedem5",
                       "Joint Democracy" = "twodem5",
                       "Conflict History" = "cfhist",
                       "Contiguity" = "contiguity")
)




# Bad List
tvtable.coxph(cox, varlist = list("$\\ln$ Battle Deaths" = "lndeaths",
  "Tie" = "tie",
  "Battle Tide" = "battletide",
  "Third Party Intervention" = "thirdpartycfire",
  "Capability Change",
  "Existential Stakes" = "stakes",
  "Agreement Strength" = "agree",
  "Mixed Regime Type" = "onedem5",
  "Joint Democracy" = "twodem5",
  "Conflict History" = "cfhist",
  "Contiguity" = "contiguity",
  "Intercept"))

# Shortlist
tvtable.coxph(cox, format = "long",
              varlist = list("Battle Deaths" = "lndeaths",
                             "Tie" = "tie",
                             "Battle Tide" = "battletide",
                             "Foreign-Imposed Regime Change" = "archigosFIRC",
                             "Mixed Regime Type" = "onedem5",
                             "Joint Democracy" = "twodem5",
                             "Conflict History" = "cfhist",
                             "Contiguity" = "contiguity")
)


# Good list
tvtable(curepart, format = "long",
              varlist = list("Battle Deaths" = "lndeaths",
                             "Tie" = "tie",
                             "Battle Tide" = "battletide",
                             "Third Party Intervention" = "thirdpartycfire",
                             "Capability Change" = "capchange",
                             "Existential Stakes" = "stakes",
                             "Agreement Strength" = "index",
                             "Foreign-Imposed Regime Change" = "archigosFIRC",
                             "Mixed Regime Type" = "onedem5",
                             "Joint Democracy" = "twodem5",
                             "Conflict History" = "cfhist",
                             "Contiguity" = "contiguity")
)

# Bad list
tvtable(curepart, format = "long",
        varlist = list("$\\ln$ Battle Deaths" = "lndeaths",
                       "Tie" = "tie",
                       "Battle Tide" = "battletide",
                       "Third Party Intervention" = "thirdpartycfire",
                       "Capability Change",
                       "Existential Stakes" = "stakes",
                       "Agreement Strength" = "agree",
                       "Mixed Regime Type" = "onedem5",
                       "Joint Democracy" = "twodem5",
                       "Conflict History" = "cfhist",
                       "Contiguity" = "contiguity",
                       "Intercept")
)


# Bad list 2
tvtable(curepart, format = "long",
        varlist = list("Battle Deaths" = "lndeaths",
                       "Tie" = "tie",
                       "Battle Tide" = "battletide",
                       "Third Party Intervention" = "thirdpartycfire",
                       "Capability Change" = "capchange",
                       "Existential Stakes" = "stakes",
                       "Agreement Strength" = "index",
                       "Foreign-Imposed Regime Change" = "archigosFIRC",
                       "Mixed Regime Type" = "onedem5",
                       "Joint Democracy" = "twodem5",
                       "Conflict History" = "cfhist",
                       "Contiguity" = "contiguity",
                       "alpha" = "alpha")
)

tvtable(curepart, format = "long",
        varlist = list("$\\ln$ Battle Deaths" = "lndeaths",
                       "Tie" = "tie",
                       "Battle Tide" = "battletide",
                       "Third Party Intervention" = "thirdpartycfire",
                       "Capability Change",
                       "Existential Stakes" = "stakes",
                       "Agreement Strength" = "agree",
                       "Mixed Regime Type" = "onedem5",
                       "Joint Democracy" = "twodem5",
                       "Conflict History" = "cfhist",
                       "Contiguity" = "contiguity",
                       "Intercept")
)

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

