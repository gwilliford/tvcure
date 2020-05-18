setwd("C:/Users/gwill/Dropbox/Research/Dissertation/tvcure/V10 Replication")
library(haven)
library(dplyr)
library(tvcure)
library(matrixStats)
library(xtable)
library(gridExtra)
library(ggpubr)
options(scipen = 999)
cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)

##### Load data
lhr <- read_dta("LHRIOOct08replication.dta")
lhr <- rename(lhr, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")
lhr$io <- ifelse(lhr$index > 0, 1, 0)
lhr$lnt <- lhr$stop^2 / 1000
lhr$lnt <- log(lhr$stop)
lhr$capchangelnt <- lhr$capchange * lhr$lnt
lhr$lncapchangelnt <- log(lhr$capchange) * lhr$lnt
lhr$lncapchange <- log(lhr$capchange)
lhr$samereg <- ifelse(lhr$twodem5 == 0 & lhr$onedem5 == 0, 1, 0)
lhr$battletidelnt <- lhr$battletide * lhr$lnt
lhr$twoaut5 = ifelse(lhr$twodem5 == 0 & lhr$onedem5 == 0, 1, 0)
lhr$twodem5t = lhr$twodem5 * lhr$stop

##### Notes
# Cure equation - positive coefficients increase probability of failure, negative coefficients decrease probability of failure

##### Descriptive Statistics
lhr$one <- 1
a <- lhr %>%
        group_by(id) %>%
        summarize(sum1 = sum(one))


##### Models ---------------------------------------------------------------------------

### Standard cox models
cox <- coxph(Surv(start, stop, event) ~ archigosFIRC + lncapchange + battletide +
                   thirdpartycfire + index + twoaut5 + twodem5 + tie + lndeaths +
                   cfhist + stakes + contiguity,
                 data = lhr,
                 x = T); summary(cox)
cox.zph(cox)
cox2 <- coxph(Surv(start, stop, event) ~ lncapchange + lncapchangelnt + battletide +
               thirdpartycfire + index + twoaut5 + twodem5 + tie + lndeaths +
               cfhist + cfhistlnt + stakes + contiguity,
             data = lhr,
             x = T); summary(cox2)
# cox.zph(cox2, function(x) {log(x)})
# cox.zph(cox2)
# cox.zph(cox2, "identity")
# plot(cox.zph(cox2))[1]

cure1 <- tvcure(Surv(start, stop, event) ~ capchange + contiguity +
              stakes + cfhist + thirdpartycfire,
            cureform = ~ tie + battletide + lndeaths + thirdpartycfire + stakes +
              index + cfhist + twodem5 + twoaut5 + archigosFIRC,
            data = lhr,
            var = T, nboot = 30,
            brglm = T); summary(cure1)
cure2 <- tvcure(Surv(start, stop, event) ~ lncapchange + lncapchange:lnt + contiguity + #t^2
              stakes + cfhist + cfhistlnt + thirdpartycfire,
            cureform = ~ tie + battletide + lndeaths + thirdpartycfire + stakes +
              index + cfhist + twodem5 + twoaut5,
            data = lhr,
            var = T, nboot = 1000,
            brglm = T); summary(cure2)
# cure3 <- tvcure(Surv(start, stop, event) ~ lncapchange + lncapchange:lnt + contiguity + #lnt
#                         stakes + cfhist + cfhistlnt + thirdpartycfire,
#                 cureform = ~ tie + battletide + lndeaths + thirdpartycfire + stakes +
#                         index + cfhist + twodem5 + twoaut5,
#                 data = lhr,
#                 var = T, nboot = 100,
#                 brglm = T); summary(cure3)
# cure4 <- tvcure(Surv(start, stop, event) ~ capchange + capchange:lnt + contiguity + #lnt
#                         stakes + cfhist + cfhistlnt + thirdpartycfire,
#                 cureform = ~ tie + battletide + lndeaths + thirdpartycfire + stakes +
#                         index + cfhist + twodem5 + twoaut5,
#                 data = lhr,
#                 var = T, nboot = 1000,
#                 brglm = T); summary(cure4)

##### PLOTS --------------------------------------------------------------------
cn <- c("time", "surv", "lower", "upper", "num")
breaks <- seq(0, 150, 10) * 365
labs <- seq(0, 150, 10)

### Baseline hazard Comparison -------------------------------------------
#cox basesurv
coxbase <- survfit(cox2)
coxbasepdat <- as.data.frame(cbind(coxbase$time, coxbase$surv, coxbase$lower, coxbase$upper))
colnames(coxbasepdat) <- cn[-5]
coxbase <- ggplot(mapping = aes(x = time, y = surv), data = coxbasepdat) +
        geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
        ylab("Probability of Survival") + xlab("Time in years") +
        scale_x_continuous(breaks = breaks, labels = labs) +
        theme(panel.border = element_rect(colour = "black", fill = NA)) + ylim(0, 1)


#cure basesurv
curebase <- prediction4(cure2, "tie", c(0, 1), type = "basesurv")
curebase <- curebase + scale_x_continuous(breaks = breaks, labels = labs) +
        xlab("Time in years") + ylab("Probability of Survival") +
        theme(axis.title.y=element_blank())

# combine
base <- grid.arrange(coxbase, curebase + theme(axis.title.y=element_blank()), ncol = 2); base
ggsave("C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter2/img/base.png", plot = base, width = 5.5, height = 3, units = "in")


### Tie comparison ---------------------------------------------------------------
legendtitle <- "Tie"
vals <- c(0, 1)

# Cox plot
newdata1 <- apply(cox2$x, 2, mean, na.rm = T)
newdata1 <- as.data.frame(rbind(newdata1, newdata1))
newdata1[2, "tie"] <- 1
pcoxtie1 <- survfit(cox2, newdata = newdata1[1, ], se.fit = T)
pcoxtie2 <- survfit(cox2, newdata = newdata1[2, ], se.fit = T)

pcoxtie1pdat <- cbind(as.data.frame(cbind(pcoxtie1$time, pcoxtie1$surv, pcoxtie1$lower, pcoxtie1$upper)), 1)
pcoxtie2pdat <- cbind(as.data.frame(cbind(pcoxtie2$time, pcoxtie2$surv, pcoxtie2$lower, pcoxtie2$upper)), 2)
colnames(pcoxtie1pdat) <- cn
colnames(pcoxtie2pdat) <- cn
pcoxtiepdat <- rbind(pcoxtie1pdat, pcoxtie2pdat)


pcoxtieplot <- ggplot(mapping = aes(x = time, y = surv, col = as.factor(num), linetype = as.factor(num)), data = pcoxtiepdat) +
        geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper,
                                      col = as.factor(num), linetype = as.factor(num), fill = as.factor(num)), alpha = 0.2) +
        labs(fill = legendtitle, linetype = legendtitle, col = legendtitle) +
        scale_linetype_discrete(labels = vals)  +
        scale_color_discrete(labels = vals) +
        scale_fill_discrete(labels = vals) +
        ylab("Probability of Survival") + xlab("Time in years") +
        scale_x_continuous(breaks = breaks, labels = labs) +
        theme(panel.border = element_rect(colour = "black", fill = NA))

# Cureplot
tpred1 <- prediction4(cure2, "tie", c(0, 1), type = "uncureprob")
tplot1 <- tpred1 + xlab("Tie")
tpred2 <- prediction4(cure2, "tie", c(0, 1), type = "spop", legendtitle = "Tie")
tplot2 <- tpred2 + xlab("Time in years") + theme(legend.position = "none") +
        ylab("Probability of Survival") + xlab("Time in years") +
        scale_x_continuous(breaks = breaks, labels = labs)

# combine
legend <- get_legend(pcoxtieplot)
pcoxtieplot <- pcoxtieplot + theme(legend.position = "none")
b <- grid.arrange(
        grobs = list(legend, pcoxtieplot, tplot1, tplot2),
        ncol = 2, nrow = 2,
        # widths = c(0.1, 1, 1),
        layout_matrix = rbind(c(1, 2),
                              c(3, 4))
)
ggsave("C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter2/img/tie.png",
       plot = b, width = 5.5, height = 4, units = "in")


### Deaths comparison
legendtitle <- "ln(Battle Deaths)"
deathmin <- round(min(lhr$lndeaths, na.rm = T), 1)
deathmax <- round(max(lhr$lndeaths, na.rm = T), 1)
vals <- c(deathmin, deathmax)

# Cox plot
newdata2 <- apply(cox2$x, 2, median, na.rm = T)
newdata2 <- as.data.frame(rbind(newdata2, newdata2))
newdata2[1, "lndeaths"] <- deathmin
newdata2[2, "lndeaths"] <- deathmax
pcoxdeaths1 <- survfit(cox2, newdata = newdata2[1, ], se.fit = T)
pcoxdeaths2 <- survfit(cox2, newdata = newdata2[2, ], se.fit = T)

pcoxdeaths1pdat <- cbind(as.data.frame(cbind(pcoxdeaths1$time, pcoxdeaths1$surv, pcoxdeaths1$lower, pcoxdeaths1$upper)), 1)
pcoxdeaths2pdat <- cbind(as.data.frame(cbind(pcoxdeaths2$time, pcoxdeaths2$surv, pcoxdeaths2$lower, pcoxdeaths2$upper)), 2)
colnames(pcoxdeaths1pdat) <- cn
colnames(pcoxdeaths2pdat) <- cn
pcoxdeathspdat <- rbind(pcoxdeaths1pdat, pcoxdeaths2pdat)

pcoxdeathsplot <- ggplot(mapping = aes(x = time, y = surv, col = as.factor(num), linetype = as.factor(num)), data = pcoxdeathspdat) +
        geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper,
                                      col = as.factor(num), linetype = as.factor(num), fill = as.factor(num)), alpha = 0.2) +
        labs(fill = legendtitle, linetype = legendtitle, col = legendtitle) +
        scale_linetype_discrete(labels = vals)  +
        scale_color_discrete(labels = vals) +
        scale_fill_discrete(labels = vals) +
        ylab("Probability of Survival") + xlab("Time in years") +
        scale_x_continuous(breaks = breaks, labels = labs) +
        theme(panel.border = element_rect(colour = "black", fill = NA))

# Cureplots
tpred1 <- prediction4(cure2, "lndeaths", c(deathmin, deathmax), type = "uncureprob")
tplot1 <- tpred1 + xlab("ln(Battle Deaths)")
tpred2 <- prediction4(cure2, "lndeaths", c(deathmin, deathmax), type = "spop", legendtitle = "ln(Battle Deaths)")
tplot2 <- tpred2 + xlab("Time in years") + theme(legend.position = "none") +  ylab("Probability of Survival") + xlab("Time in years") + scale_x_continuous(breaks = breaks, labels = labs)

# combine
legend <- get_legend(pcoxdeathsplot)
pcoxdeathsplot <- pcoxdeathsplot + theme(legend.position = "none")
d <- grid.arrange(
        grobs = list(legend, pcoxdeathsplot, tplot1, tplot2),
        ncol = 2, nrow = 2,
        # widths = c(0.1, 1, 1),
        layout_matrix = rbind(c(1, 2),
                              c(3, 4))
)
ggsave("C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter2/img/deaths.png", plot = d, width = 5.5, height = 4, units = "in")



### Battletide
bpred1 <- prediction4(cure2, "battletide", c(0, 1), type = "uncureprob")
bplot1 <- bpred1 + xlab("Battle Consistency")
bpred2 <- prediction4(cure2, "battletide", c(0, 1), type = "spop", legendtitle = "Battle Consistency")
bplot2 <- bpred2 + xlab("Time in years") + theme(legend.position = "none") +
        ylab("Probability of Survival") + xlab("Time in years") +
        scale_x_continuous(breaks = breaks, labels = labs)


#### Low risk cases 1st row - high risk cases second column
sim <- apply(cox2$x, 2, median, na.rm = T)
sim <- rbind(sim,sim)
sim[,"lncapchange"] <- c(-11.7, 1.5)
sim[,"lncapchange:lnt"] <- c(-11.7 * log(21), 1.5 * log(21))
sim[,"battletide"] <- c(1, 0)
sim[,"thirdpartycfire"] <- c(1, 0)
sim[,"index"] <- c(12, 0)
sim[,"lndeaths"] <- c(15.6, 5.3)
sim[,"cfhist"] <- c(0, 1.6)
sim[,"cfhistlnt"] <- c(0, 1.6 * log(21))
sim[, "stakes"] <- c(0, 1)
sim[,"contiguity"] <- c(0, 1)
sim[, "twodem5"] <- c(1, 0)
sim[,"twoaut5"] <- c(0, 1)
sim[,"tie"] <- c(0, 1)
sim <- cbind(sim, c(0, 1))
colnames(sim)[ncol(sim)] <- "Intercept"

simlo <- sim[, cure2$bnames]
simhi <- sim[, cure2$gnames]
prediction4(cure2, newX = g1, newZ = g2, type = "suncure")


### High risk cases (second row of g)


##### Tables --------------------------------------------------------------------
vl <- list("Battle Deaths" = "lndeaths",
     "Tie" = "tie",
     "Battle Consistency" = "battletide",
     "Third Party Intervention" = "thirdpartycfire",
     "Capability Change" = "capchange",
     "Capability Change $\\times \\ln(t)$" = "capchangelnt",
     "$\\ln(\\text{Capability Change})$" = "lncapchange",
     "$\\ln(\\text(Capability Change)) \\times \\ln(\\text(T))$" = "lncapchangelnt",
     "Existential Stakes" = "stakes",
     "Agreement Strength" = "index",
     "Foreign-Imposed Regime Change" = "archigosFIRC",
     "Mixed Regime Type" = "onedem5",
     "Joint Democracy" = "twodem5",
     "Joint Autocracy" = "twoaut5",
     "Conflict History" = "cfhist",
     "Conflict History $\\times \\ln(t)$" = "cfhistlnt",
     "Contiguity" = "contiguity")

t1 <- tvtable(cox2, cure2, varlist = vl, label = "tab:res",
              caption = "Cox proportional hazards and cure model estimates of ceasefire duration"
              modnum = F)
x1 <- tvtable_xtable(t1)
# printer(x1)
addtorow <- list()
addtorow$pos <- as.list(c(0, nrow(t1)))
addtorow$command <- as.list(c("\& \\multicolumn{1}{c}{Model 1} & \\multicolumn{2}{c}{Model 2} \\\\ \\cmidrule(lr){2-2}\\cmidrule(lr){3-4}",
                              "\\bottomrule"
                              "\\bottomrule \\n \\footnote{\\footnotesize Note: $'***' p leq 0.001, '**' p \leq 0.01, '*' p \\leq 0.5, + \\leq 0.1$}\n"
                              ))

print(t1,
     booktabs = T,
     sanitize.text.function = identity,
     include.rownames = F,
     include.colnames = F,
     add.to.row = addtorwo
     hline.after = c(-1, 2, nrow(x) - 2), ...)



#assign a position argument to addtorow
#rws are the row indexes for the row to be colored,
#0 is the row index for longtable argument

#assign corresponding commands to addtorow

