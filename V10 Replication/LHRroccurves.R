################################################################################
##### LHR ROC curves
# Estimates cumulative and riskset ROC curves for LHR models
################################################################################
library(survivalROC)
library(risksetROC)
library(tidyverse)
library(survival)
library(survminer)



################################################################################
##### Load models and obtain linear predictors
################################################################################

lhr$coxlp  = predict(cox2, type = "lp")
lhr$curelp = as.numeric(cure2$X %*% cure2$beta)

################################################################################
##### Cumulative ROC Curves
################################################################################

### Cox model
cox2_helper <- function(t) {
  survivalROC(entry        = lhr$start,
              Stime        = lhr$stop,
              status       = lhr$event,
              marker       = lhr$coxlp,
              predict.time = t,
              method       = "NNE",
              span = 0.25 * nrow(lhr)^(-0.20))
}
cox2_roc <- data_frame(t = 3650) %>% #* c(1,2,3,4,5, 6)
  mutate(survivalROC = map(t, cox2_helper),
         auc = map_dbl(survivalROC, magrittr::extract2, "AUC"),
         df_survivalROC = map(survivalROC, function(obj) {
           as_data_frame(obj[c("cut.values","TP","FP")])
         })) %>%
  dplyr::select(-survivalROC) %>%
  unnest(cols = c(df_survivalROC)) %>%
  arrange(t, FP, TP)
cox2_rocplot = cox2_roc %>%
  ggplot(mapping = aes(x = FP, y = TP)) +
  geom_point() +
  geom_line() +
  geom_label(data = cox2_roc %>% dplyr::select(t,auc) %>% unique,
             mapping = aes(label = sprintf("%.3f", auc)), x = 0.5, y = 0.5) +
  facet_wrap( ~ t) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank()); cox2_rocplot

cure2_helper <- function(t) {
  survivalROC(entry        = lhr$start,
              Stime        = lhr$stop,
              status       = lhr$event,
              marker       = lhr$curelp,
              predict.time = t,
              method       = "NNE",
              span = 0.25 * nrow(lhr)^(-0.20))
}
cure2_roc <- data_frame(t = 3650 * c(1, 2, 3, 4, 5, 6)) %>%
  mutate(survivalROC = map(t, cure2_helper),
         auc = map_dbl(survivalROC, magrittr::extract2, "AUC"),
         df_survivalROC = map(survivalROC, function(obj) {
           as_data_frame(obj[c("cut.values","TP","FP")])
         })) %>%
  dplyr::select(-survivalROC) %>%
  unnest(cols = c(df_survivalROC)) %>%
  arrange(t, FP, TP)

cure2_rocplot = cure2_roc %>%
  ggplot(mapping = aes(x = FP, y = TP)) +
  geom_point() +
  geom_line() +
  geom_label(data = cure2_roc %>% dplyr::select(t,auc) %>% unique,
             mapping = aes(label = sprintf("%.3f", auc)), x = 0.5, y = 0.5) +
  facet_wrap( ~ t) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank()); cure2_rocplot

################################################################################
##### Riskset ROC Curves
################################################################################

##### Cox model risksetROC curves ----------------------------------------------

cox_riskset_helper <- function(t) {
  risksetROC(
    entry        = lhr$start,
    Stime        = lhr$stop,
    status       = lhr$event,
    marker       = lhr$coxlp,
    predict.time = t,
    method       = "Cox",
    plot = F)
}

cox_riskset_data <- data_frame(t = 3650 * c(1,2,3,4,5,6)) %>%
  mutate(risksetROC = map(t, cox_riskset_helper),
         auc = map_dbl(risksetROC, magrittr::extract2, "AUC"),
         df_risksetROC = map(risksetROC, function(obj) {
           marker <- c(-Inf, obj[["marker"]], Inf)
           bind_cols(data_frame(marker = marker),
                     as_data_frame(obj[c("TP","FP")]))
         })) %>%
  dplyr::select(-risksetROC) %>%
  unnest(cols = c(df_risksetROC)) %>%
  arrange(t, FP, TP)

cox_riskset_plot = cox_riskset_data %>%
  ggplot(mapping = aes(x = FP, y = TP)) +
  geom_point() +
  geom_line() +
  geom_label(data = cox_riskset_data %>% dplyr::select(t,auc) %>% unique,
             mapping = aes(label = sprintf("%.3f", auc)), x = 0.5, y = 0.5) +
  facet_wrap( ~ t) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank()); cox_riskset_plot


##### Cure model risksetROC curves ---------------------------------------------

cure_riskset_helper <- function(t) {
  risksetROC(
    entry        = lhr$start,
    Stime        = lhr$stop,
    status       = lhr$event,
    marker       = lhr$curelp,
    predict.time = t,
    method       = "Cox",
    plot = F)
}

cure_riskset_data <- data_frame(t = 3650 * c(1,2,3,4,5,6)) %>%
  mutate(risksetROC = map(t, cure_riskset_helper),
         auc = map_dbl(risksetROC, magrittr::extract2, "AUC"),
         df_risksetROC = map(risksetROC, function(obj) {
           marker <- c(-Inf, obj[["marker"]], Inf)
           bind_cols(data_frame(marker = marker),
                     as_data_frame(obj[c("TP","FP")]))
         })) %>%
  dplyr::select(-risksetROC) %>%
  unnest(cols = c(df_risksetROC)) %>%
  arrange(t, FP, TP)

cure_riskset_plot = cure_riskset_data %>%
  ggplot(mapping = aes(x = FP, y = TP)) +
  geom_point() +
  geom_line() +
  geom_label(data = cure_riskset_data %>% dplyr::select(t,auc) %>% unique,
             mapping = aes(label = sprintf("%.3f", auc)), x = 0.5, y = 0.5) +
  facet_wrap( ~ t) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank()); cure_riskset_plot
