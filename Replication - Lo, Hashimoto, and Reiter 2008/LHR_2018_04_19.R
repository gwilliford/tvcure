# Setup
library(readstata13)
library(splitstackshape)
library(plyr)
library(tvcure)
library(coxphf)
library(xtable)
library(stargazer)
library(coefplot)
options(scipen = 999)

# Load data
lhr <- read.dta13("C:/Users/gwill/Dropbox/Methods Notes/Survival Analysis/Cure Models Paper/Replication - LHR 2008/lhrIOOct08replication.dta")
lhr <- rename(lhr, replace = c("_st" = "st", "_d" = "event", "_t" = "stop", "_t0" = "start"))
lhrna <- as.data.frame(na.omit(with(lhr, cbind(start, stop, event, archigosFIRC, wernerFIRC, capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity, warnumb))))

# Correlation matrix
cor(with(lhr, cbind(event, wernerFIRC, capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity)), use = "complete.obs")

# Model 1 - full model
cl <- makeCluster(4, "SOCK")
registerDoSNOW(cl)
a <- tvcure(Surv(start, stop, event) ~ archigosFIRC, cureform = ~ archigosFIRC, data = lhr, model = "ph", firthlogit = T, nboot = 100)

a <- tvcure(Surv(start, stop, event) ~  capchange + battletide + index + tie + lndeaths + cfhist, cureform = ~ wernerFIRC + capchange + battletide + thirdpartycfire + index + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr, model = "ph", nboot = 100)
saveRDS(lhr_tvcurefull, "lhr_tvcurefull_probit.RDS")
lhr_tvcurefull <- tvcure(Surv(start, stop, event) ~ capchange + battletide + thirdpartycfire + index + tie + lndeaths + cfhist + stakes + contiguity, cureform = ~ archigosFIRC + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr, model = "ph", link = "probit", nboot = 1000)
saveRDS(lhr_tvcurefull, "wernerfirc.RDS")

# Model 2 - comparable cox model
mcox <- coxph(Surv(start, stop, event) ~ wernerFIRC + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr)
mcoxb <- coxph(Surv(start, stop, event) ~ archigosFIRC + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity, data = lhr)

mcoxhf <- coxphf(Surv(start, stop, event) ~ archigosFIRC + capchange + battletide + thirdpartycfire + index + onedem5 + twodem5 + tie + lndeaths + cfhist + stakes + contiguity + twodem5lnt + cfhistlnt + cluster(warnumb), data = lhrna)

b <- rbind(Zmed, Zmed)
b[1, "lndeaths"] <- min(lhr$lndeaths, na.rm = T)
b[2, "lndeaths"] <- max(lhr$lndeaths, na.rm = T)
mcox.preda <- survfit(mcox, as.data.frame(b))
plot(mcox.preda)
plot(survfit(mcox, as.data.frame(Zmed)))

# Load Model
lhr_tvcurefull <- readRDS("lhr_tvcurefull_probit")
summary(lhr_tvcurefull)

#
summary(lhr_tvcurefull)
tab.full <- tvtable(lhr_tvcurefull)

  #stargazer(mcox, cbind(summary(mcox)$coefficients[, 4], gtools::stars.pval(summary(mcox)$coefficients[, 6])), style = "apsr", out = "C:/Users/gwill/Dropbox/Research/Dissertation/Cure Models Article/coxtab.tex", booktabs = T, float = F)
# Cox table
coxtab <- cbind(round(coef(mcox), 3), paste(round(summary(mcox)$coefficients[, 4],3), gtools::stars.pval(summary(mcox)$coefficients[, 6]), sep = ""))
colnames(coxtab) <- c("Cox Coefficients", "Standard Errors")
rownames(coxtab) <- c("Foreign Imposed Regime Change", "Capability Change", "Battletide", "Third Party Ceasefire", "Agreement Strength", "One Democracy", "Two Democracy", "Tie", "Battle Deaths", "Conflict History", "Existential Stakes", "Contiguity")
# Merge tables
# mergetab <- merge(coxtab, tab.full, by = 0, all = T, sort = F)
# mergetab <- mergetab[, -1]
# rbind(c("\multicolumn{2}{"Cox Model"}", "\multicolumn{4}{"Cure Model"}"}
# addtorow <- list()
# addtorow$pos <- list(0)
# addtorow$command <- paste('& \\multicolumn{2}{c}{Cox Model\\cmidrule}', '\\multicolumn{4}{c}{Cure Model\\cmidrule}',  '\\\\', '& Cox Coef.&(S.E.)&Probit Coef.&(S.E.)&Hazard Coef.&(S.E.)\\\\', collapse='')
# # Output to latex
rownames(tab.full) <- c("Intercept", "Foreign Imposed Regime Change", "Capability Change", "Battletide", "Third Party Ceasefire", "Agreement Strength", "One Democracy", "Two Democracy", "Tie", "Battle Deaths", "Conflict History", "Existential Stakes", "Contiguity")
#colnames(tab.full) <- NULL
#rbind(as.matrix(t(c(, mergetab)
print(xtable(tab.full, align = "lcccc"), file = "C:/Users/gwill/Dropbox/Research/Dissertation/Cure Models Article/lhrtab.tex", floating = F, booktabs = T)#, tabular.environment = "tabular*", width = "6.5 in", add.to.row = addtorow)
print(xtable(coxtab, align = "lcc"), file = "C:/Users/gwill/Dropbox/Research/Dissertation/Cure Models Article/coxtab.tex", floating = F, booktabs = T)



# OS predictions with tie = c(0, 1) with all variables held at median
Xmed <- with(lhr, t(as.matrix(apply(cbind(capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity), 2, median, na.rm = T))))
Xnew <- rbind(Xmed, Xmedin)
Xnew[1, "lndeaths"] <- min(lhr$lndeaths, na.rm = T)
Xnew[2, "lndeaths"] <- max(lhr$lndeaths, na.rm = T)
Zmed <- with(lhr, t(as.matrix(apply(cbind(wernerFIRC, capchange, battletide, thirdpartycfire, index, onedem5, twodem5, tie, lndeaths, cfhist, stakes, contiguity), 2, median, na.rm = T))))
Znew <- rbind(Zmed, Zmed)
Znew[1, "lndeaths"] <- min(lhr$lndeaths, na.rm = T)
Znew[2, "lndeaths"] <- max(lhr$lndeaths, na.rm = T)
d <- predict(lhr_tvcurefull, newX = Xnew, newZ = Znew, link = "probit")

# Survivor function plots
plot(d, type = "basesurv")
plot(d, type = "suncure")
plot(d, type = "spop")

# OS predictions with tie = c(0, 1) in low risk cases
Xtestlo1 <- with(lhr, cbind(0, 0, 0, 10, 1, 1, 0, 0, 5.28, 0, 0))
Xtestlo2 <- with(lhr, cbind(0, 0, 0, 10, 1, 1, 0, 0, 16.2, 0, 0))
Xtestlo <- rbind(Xtestlo1, Xtestlo2)

Ztestlo1 <- with(lhr, cbind(0, 0, 0, 0, 10, 1, 1, 0, 0, 5.28, 0, 0))
Ztestlo2 <- with(lhr, cbind(0, 0, 0, 0, 10, 1, 1, 0, 0, 16.2, 0, 0))
Ztestlo <- rbind(Ztestlo1, Ztestlo2)

predlo <- predict(lhr_tvcurefull, Xtestlo, Ztestlo, link = "probit")
plot(predlo, type = "spop")
plot(predlo, type = "suncure")

# OS predictions with tie = c(0, 1) in high risk cases
Xtesthi1 <- with(lhr, cbind(.05, 1, 1, 0, 0, 0, 1, 5.28, 1.6, 1, 1))
Xtesthi2 <- with(lhr, cbind(.05, 1, 1, 0, 0, 0, 1, 16.2, 1.6, 1, 1))
Xtesthi <- rbind(Xtesthi1, Xtesthi2)

Ztesthi1 <- with(lhr, cbind(0, .05, 1, 1, 0, 0, 0, 1, 5.28, 1.6, 1, 1))
Ztesthi2 <- with(lhr, cbind(0, .05, 1, 1, 0, 0, 0, 1, 16.2, 1.6, 1, 1))
Ztesthi <- rbind(Ztesthi1, Ztesthi2)

predhi <- predict(lhr_tvcurefull, Xtesthi, Ztesthi)
plot(predhi, type = "basesurv")
plot(predhi, type = "spop")
plot(predhi, type = "suncure")

# OS predictions with capchange = c(0:4.5) by .5 in median case
Xcap <- expandRows(as.data.frame(Xmed), 6, F)
Xcap$capchange <- seq(0, 5, 1)
Zcap <- expandRows(as.data.frame(Zmed), 6, F)
Zcap$capchange <- seq(0, 5, 1)


Xcap <- expandRows(as.data.frame(Xmed), 6, F)
Xcap$capchange <- c(seq(0, 1, .2), 5)
Xcap$capchange[7] <- 2
Zcap <- expandRows(as.data.frame(Zmed), 7, F)
Zcap$capchange <- c(seq(0, 1, .2), 2)

setEPS()
postscript("../Cure Models Article/cappred.eps")
cappred <- predict(lhr_tvcurefull, Xcap, Zcap, link = "probit")
plot(Zcap$capchange, cappred$uncureprob, type = "l", ylab = "Probability of Failure", xlab = "Capability Change", cex = 1.5)
dev.off()

setEPS()
postscript("../Cure Models Article/capsurv.eps")
plot(cappred, type = "suncure", xaxt="n", xlab = "Days at Peace (at 10 year intervals)", lwd = 2, col = c(1:6, 8))
  ?#title("Conditional Survivor Function for Various Levels of Capability Change")
  legend("topright", legend = c("0.0", "0.2", ".4", ".6", ".8", "1", "2"), col = c(1:6, 8), lty = 1:10, title = "Capchange", lwd = 2)
   axis(side = 1, at=seq(0, 30000, by = 3650), cex = .5)
dev.off()
#fplot(cappred, type = "spop")

