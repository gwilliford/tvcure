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

### Tie comparison ---------------------------------------------------------------
legendtitle <- "Tie"
vals <- c(0, 1)

# Cox plot
newdata1 <- apply(cox2$x, 2, median, na.rm = T)
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
tplot2 <- tpred2 + xlab("Time in years") + theme(legend.position = "none") +  ylab("Probability of Survival") + xlab("Time in years") + scale_x_continuous(breaks = breaks, labels = labs)

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

#save
ggsave("C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter2/img/tie.png", plot = b, width = 5.5, height = 4, units = "in")


### Deaths comparison
legendtitle <- "Log(Battle Deaths)"
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

# Cureplot
tpred1 <- prediction4(cure2, "lndeaths", c(deathmin, deathmax), type = "uncureprob")
tplot1 <- tpred1 + xlab("Log(Battle Deaths)")
tpred2 <- prediction4(cure2, "lndeaths", c(deathmin, deathmax), type = "spop", legendtitle = "Log(Battle Deaths)")
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




# ggarrange(a, tplot1, tplot2, ncol = 2, nrow = 2)
# b <- grid.arrange(
#         grobs = list(a, tplot1, tplot2),
#         ncol = 2, nrow = 2,
#         # widths = c(0.1, 1, 1),
#         layout_matrix = rbind(c(1, 2),
#                               c(1, 3))
# )


# ) + plot.background()
# grid.rect(width = .98, height = .98, gp = gpar(lwd = 2, col = "blue", fill = NA))

ggsave("C:/Users/gwill/Dropbox/Research/Dissertation/Manuscript/chapter2/img/tie.png", plot = b, width = 5.5, height = 4, units = "in")

# pcox1b <- surv_fit(cox2, data = newdata1[2, ], se.fit = T)

# pcox1b <- survfit(cox2, newdata = newdata2, se.fit = T)
plot(pcox1a)
plot(pcox1b)

autoplot(pcox1a)

# Surv uncure
tplot1 <- tpred1 + xlab("Tie")

# Surv cure
+ ylim(0.5, 1)

# Combine and save
ggarrange(tpred1, tpred2, ncol = 2,)
ggsave("./figures/attempts.png", width = 3, height = 3, units = "in")


### Deaths variable
# Cox pred
# lnmin <- min(lhr$lndeaths, na.rm = T)
# lnmax <- max(lhr$lndeaths, na.rm = T)
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

d <- prediction4(cure2, "battletide", c(0, 1), type = "suncure")

g <- apply(cox2$x, 2, median, na.rm = T)
g <- rbind(g,g)
g[,"capchange"] <- c(0, 4.5)
g[,"capchangelnt"] <- c(0, 4.5 * log(21))
g[,"battletide"] <- c(1, 0)
g[,"thirdpartycfire"] <- c(1, 0)
g[,"index"] <- c(12, 0)
g[,"lndeaths"] <- c(15.6, 5.3)
g[,"cfhist"] <- c(0, 1.6)
g[,"cfhistlnt"] <- c(0, 1.6 * log(21))
g[, "stakes"] <- c(0, 1)
g[,"contiguity"] <- c(0, 1)
g[, "twodem5"] <- c(1, 0)
g[,"twoaut5"] <- c(0, 1)
g[,"tie"] <- c(0, 1)
g <- cbind(g, c(0, 1))
colnames(g)[ncol(g)] <- "Intercept"

g1 <- g[, cure2$bnames]
g2 <- g[, cure2$gnames]


f <- g[1,]
f <- rbind(f, f)
f[2, "tie"] <- 1
f1 <- f[, cure2$bnames]
f2 <- f[, cure2$gnames]

h <- g[2,]
h <- rbind(h, h)
h[2, "tie"] <- 1
h1 <- h[, cure2$bnames]
h2 <- h[, cure2$gnames]


prediction4(cure2, newX = f1, newZ = f2, type = "spop")
# g1hi <- g1[2, cure2$bnames]
# g2lo <- g2[1, c(cure2$gnames[-1])]
# g2hi <- g2[2, c(cure2$gnames[-1])]
# g <- prediction4(cure2, "battletide", c(0, 1), type = "spop")

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

