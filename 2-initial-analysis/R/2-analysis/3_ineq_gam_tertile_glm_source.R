
# title: "Measuring inequalities"
# author: "Pearl Ante-Testard"
# R script version: source code for the R script "3_ineq_gam_tertile_comp"

# packages
library(mgcv)
library(ggeffects)
library(DHARMa)
library(mgcViz)
library(geepack)
library(msm)
library(tidymv)
#remotes::install_github("stefanocoretta/tidygam@devel")
library(tidygam)
library(cowplot)
library(ggpubr)
library(lmtest)
library(sandwich)
library(gridExtra)
library(RColorBrewer)
library(here)

#rm(list=ls(all=TRUE))

# configuration
source(here::here("2-initial-analysis/R", "0-config.R"))

#df_analysis <- readRDS(file = here::here("1-data", "2-final",
#"enrol_diar_tr_wealth_indiv_svy012.rds"))

# Surveys 1 and 2 (8440 children)
df_analysis_svy12 <- readRDS(file = here::here("1-data", "2-final",
                                               "enrol_diar_tr_wealth_indiv_svy012.rds")) %>%
                     filter(svy!=0)

df_analysis_svy12 <- df_analysis_svy12 %>%
  mutate(wealth_rank = rank(wealthscore)/nrow(.),
         block=factor(block),
         wealth_tertile=factor(wealth_tertile),
         diar7df=factor(diar7d),
         Arms=factor(tr_comb, levels = c(0,1), labels = c("Control","Intervention")))

#summary(df_analysis_svy12$wealth_rank)
#str(df_analysis_svy12$diar7d)

## Estimating diarrhea prevalence per wealth tertile in the 2 arms

#df_analysis_svy12$diar7d <- as.numeric(df_analysis_svy12$diar7d)-1
df_analysis_svy12 <- df_analysis_svy12 %>%
  mutate(diar7d = case_when(
    diar7df == "0" ~ 0,
    diar7df == "1" ~ 1,
    TRUE ~ as.numeric(NA)
  ))

str(df_analysis_svy12$diar7d)

prev_diar <- df_analysis_svy12 %>%                         
  group_by(Arms, wealth_tertile) %>%         
  summarise_at(vars(diar7d),                  
               list(length = length, sum = sum, mean = mean, sd = sd)) 

print(prev_diar)

## Estimating PRs (Control/Intervention) and 95% CIs using log binomial

# releving the variable Arms with the intervention arm as a reference
# rationale: there were more diarrhea prevalence in the control arm
df_analysis_svy12$Arms <- relevel(df_analysis_svy12$Arms, ref = "Intervention")

# modeling a log binomial fit with interaction
glm_int_log <- glm(diar7df ~ wealth_tertile + Arms + wealth_tertile*Arms,
                   family = binomial(link="log"),
                   data = df_analysis_svy12)
summary(glm_int_log)

# get the var-cov matrix
glmfitlog_robust_vcov <- vcov(glm_int_log, cluster=df_analysis_svy12$block)

# update regression fit
glmfitlog_lin_robustse <- coeftest(glm_int_log, vcov. =  glmfitlog_robust_vcov)
print(glmfitlog_lin_robustse)

# modeling a log binomial fit without interaction
glm_noint_log <- glm(diar7df ~ wealth_tertile + Arms,
                     family = binomial(link="log"),
                     data = df_analysis_svy12)
summary(glm_noint_log)
print(coeftest(glm_noint_log, vcov. =  glmfitlog_robust_vcov))

# wald test to compare the models with and without interaction
wald_pr <- waldtest(glm_int_log, glm_noint_log, vcov = glmfitlog_robust_vcov) 
print(wald_pr)

# get PR from glm_int_log

# method 1: manual
PR_1 <- exp(glm_int_log$coefficients[4])
PR_2 <- exp((glm_int_log$coefficients[1] + glm_int_log$coefficients[2] + glm_int_log$coefficients[4]+ glm_int_log$coefficients[5]) - (glm_int_log$coefficients[1] + glm_int_log$coefficients[2]))
PR_3 <- exp((glm_int_log$coefficients[1] + glm_int_log$coefficients[3]  + glm_int_log$coefficients[4] + glm_int_log$coefficients[6]) - (glm_int_log$coefficients[1] + glm_int_log$coefficients[3]))

#PR_1; PR_2; PR_3

# method 2
# PR for tertile 1
logPR = glmfitlog_lin_robustse[4,1]
logPR_se = glmfitlog_lin_robustse[4,2]
PR_1 <- exp(logPR)
PR_1_lower <- exp(logPR-1.96*logPR_se)
PR_1_upper <- exp(logPR+1.96*logPR_se)
#PR_1; PR_1_lower; PR_1_upper
dat_1 <- data.frame(PR_1, PR_1_lower, PR_1_upper)
dat_1$tertile <- "T1 \n(lowest socioeconomic position)"
colnames(dat_1) <- c("PR", "lower", "upper", "Tertile")

# PR for tertile 2
lincom_2 <- c(0,0,0,1,1,0)
logPR_2 = t(lincom_2) %*% glmfitlog_lin_robustse[,1]
logPR_se_2 = sqrt( t(lincom_2) %*% glmfitlog_robust_vcov %*% lincom_2 )
PR_2 <- exp(logPR_2)
PR_2_lower <- exp(logPR_2-1.96*logPR_se_2)
PR_2_upper <- exp(logPR_2+1.96*logPR_se_2)
#PR_2; PR_2_lower; PR_2_upper
dat_2 <- data.frame(PR_2, PR_2_lower, PR_2_upper)
dat_2$tertile <- "T2"
colnames(dat_2) <- c("PR", "lower", "upper", "Tertile")

# PR for tertile 3
lincom_3 <- c(0,0,0,1,0,1)
logPR_3 = t(lincom_3) %*% glmfitlog_lin_robustse[,1]
logPR_se_3 = sqrt( t(lincom_3) %*% glmfitlog_robust_vcov %*% lincom_3 )
PR_3 <- exp(logPR_3)
PR_3_lower <- exp(logPR_3-1.96*logPR_se_3)
PR_3_upper <- exp(logPR_3+1.96*logPR_se_3)
#PR_3; PR_3_lower; PR_3_upper
dat_3 <- data.frame(PR_3, PR_3_lower, PR_3_upper)
dat_3$tertile <- "T3 \n(highest socioeconomic position)"
colnames(dat_3) <- c("PR", "lower", "upper", "Tertile")

# merging dataframes
dat <- rbind(dat_1, dat_2, dat_3)

print(dat)

## Estimating PDs (Control - Intervention) and 95% CIs using linear-binomial

# modeling a glm fit with interaction
# using the factor diar7df for glm
glm_int_iden <- glm(diar7df ~ wealth_tertile + Arms + wealth_tertile*Arms,
                    family = binomial(link="identity"),
                    data = df_analysis_svy12
)
#summary(glm_int_iden)

# get robust var-covar matrix, allowing for block-level clustering 
glmfit_robust_iden_vcov <- vcovCL(glm_int_iden,cluster=df_analysis_svy12$block)

# update regression fit
glmfit_lin_robustse <- coeftest(glm_int_iden,vcov. =  glmfit_robust_iden_vcov)
print(glmfit_lin_robustse)

# modeling a glm fit without an interaction
glm_noint_iden <- glm(diar7df ~ wealth_tertile + Arms,
                      family = binomial(link="identity"),
                      data = df_analysis_svy12
)
#summary(glm_noint_iden)
print(coeftest(glm_noint_iden,vcov. =  glmfit_robust_iden_vcov))

# wald test to compare the models with and without interaction
wald_pd <- waldtest(glm_int_iden,glm_noint_iden, vcov = glmfit_robust_iden_vcov) 
print(wald_pd)

# PD for tertile 1
logPD = glmfit_lin_robustse[4,1]
logPD_se = glmfit_lin_robustse[4,2]
PD_1_lower <- logPD-1.96*logPD_se
PD_1_upper <- logPD+1.96*logPD_se
#PR_1; PR_1_lower; PR_1_upper
dat_1_pd <- data.frame(logPD, PD_1_lower, PD_1_upper)
dat_1_pd$tertile <- "T1 \n(lowest socioeconomic position)"
colnames(dat_1_pd) <- c("PD", "lower", "upper", "Tertile")

# PD for tertile 2
lincom_2 <- c(0,0,0,1,1,0)
logPD_2 = t(lincom_2) %*% glmfit_lin_robustse[,1]
logPD_se_2 = sqrt( t(lincom_2) %*% glmfit_robust_iden_vcov %*% lincom_2 )
PD_2_lower <- logPD_2-1.96*logPD_se_2
PD_2_upper <- logPD_2+1.96*logPD_se_2
#PR_2; PR_2_lower; PR_2_upper
dat_2_pd <- data.frame(logPD_2, PD_2_lower, PD_2_upper)
dat_2_pd$tertile <- "T2"
colnames(dat_2_pd) <- c("PD", "lower", "upper", "Tertile")

# PD for tertile 3
lincom_3 <- c(0,0,0,1,0,1)
logPD_3 = t(lincom_3) %*% glmfit_lin_robustse[,1]
logPD_se_3 = sqrt( t(lincom_3) %*% glmfit_robust_iden_vcov %*% lincom_3 )
PD_3_lower <- logPD_3-1.96*logPD_se_3
PD_3_upper <- logPD_3+1.96*logPD_se_3
#PR_3; PR_3_lower; PR_3_upper
dat_3_pd <- data.frame(logPD_3, PD_3_lower, PD_3_upper)
dat_3_pd$tertile <- "T3 \n(highest socioeconomic position)"
colnames(dat_3_pd) <- c("PD", "lower", "upper", "Tertile")

# merging dataframes
dat_pd <- rbind(dat_1_pd, dat_2_pd, dat_3_pd)

print(dat_pd)

#####################
##### Tertile 1 #####
#####################

# control
lincom_prev_t1_control <- c(1,0,0,1,0,0)
log_prev_t1_control = t(lincom_prev_t1_control) %*% glmfit_lin_robustse[,1]
logprev_t1_control_se = sqrt( t(lincom_prev_t1_control) %*% glmfit_robust_iden_vcov %*% lincom_prev_t1_control)
prev_t1_control_lower <- log_prev_t1_control-1.96*logprev_t1_control_se
prev_t1_control_upper <- log_prev_t1_control+1.96*logprev_t1_control_se
prev_t1_control <- data.frame(log_prev_t1_control, prev_t1_control_lower, prev_t1_control_upper)
prev_t1_control$tertile <- "T1 \n(lowest socioeconomic position)"
prev_t1_control$Arms <- "control"
colnames(prev_t1_control) <- c("prevalence", "lower", "upper", "Tertile", "Arms")
# intervention
lincom_prev_t1_interv <- c(1,0,0,0,0,0)
log_prev_t1_interv = t(lincom_prev_t1_interv) %*% glmfit_lin_robustse[,1]
logprev_t1_interv_se = sqrt( t(lincom_prev_t1_interv) %*% glmfit_robust_iden_vcov %*% lincom_prev_t1_interv)
prev_t1_interv_lower <- log_prev_t1_interv-1.96*logprev_t1_interv_se
prev_t1_interv_upper <- log_prev_t1_interv+1.96*logprev_t1_interv_se
prev_t1_interv <- data.frame(log_prev_t1_interv, prev_t1_interv_lower, prev_t1_interv_upper)
prev_t1_interv$tertile <- "T1 \n(lowest socioeconomic position)"
prev_t1_interv$Arms <- "intervention"
colnames(prev_t1_interv) <- c("prevalence", "lower", "upper", "Tertile", "Arms")

#####################
##### Tertile 2 #####
#####################

# control
lincom_prev_t2_control <- c(1,1,0,1,1,0)
log_prev_t2_control = t(lincom_prev_t2_control) %*% glmfit_lin_robustse[,1]
logprev_t2_control_se = sqrt( t(lincom_prev_t2_control) %*% glmfit_robust_iden_vcov %*% lincom_prev_t2_control)
prev_t2_control_lower <- log_prev_t2_control-1.96*logprev_t2_control_se
prev_t2_control_upper <- log_prev_t2_control+1.96*logprev_t2_control_se
prev_t2_control <- data.frame(log_prev_t2_control, prev_t2_control_lower, prev_t2_control_upper)
prev_t2_control$tertile <- "T2"
prev_t2_control$Arms <- "control"
colnames(prev_t2_control) <- c("prevalence", "lower", "upper", "Tertile", "Arms")
# intervention
lincom_prev_t2_interv <- c(1,1,0,0,0,0)
log_prev_t2_interv = t(lincom_prev_t2_interv) %*% glmfit_lin_robustse[,1]
logprev_t2_interv_se = sqrt( t(lincom_prev_t2_interv) %*% glmfit_robust_iden_vcov %*% lincom_prev_t2_interv)
prev_t2_interv_lower <- log_prev_t2_interv-1.96*logprev_t2_interv_se
prev_t2_interv_upper <- log_prev_t2_interv+1.96*logprev_t2_interv_se
prev_t2_interv <- data.frame(log_prev_t2_interv, prev_t2_interv_lower, prev_t2_interv_upper)
prev_t2_interv$tertile <- "T2"
prev_t2_interv$Arms <- "intervention"
colnames(prev_t2_interv) <- c("prevalence", "lower", "upper", "Tertile", "Arms")

#####################
##### Tertile 3 #####
#####################

# control
lincom_prev_t3_control <- c(1,0,1,1,0,1)
log_prev_t3_control = t(lincom_prev_t3_control) %*% glmfit_lin_robustse[,1]
logprev_t3_control_se = sqrt( t(lincom_prev_t3_control) %*% glmfit_robust_iden_vcov %*% lincom_prev_t3_control)
prev_t3_control_lower <- log_prev_t3_control-1.96*logprev_t3_control_se
prev_t3_control_upper <- log_prev_t3_control+1.96*logprev_t3_control_se
prev_t3_control <- data.frame(log_prev_t3_control, prev_t3_control_lower, prev_t3_control_upper)
prev_t3_control$tertile <- "T3 \n(highest socioeconomic position)"
prev_t3_control$Arms <- "control"
colnames(prev_t3_control) <- c("prevalence", "lower", "upper", "Tertile", "Arms")
# intervention
lincom_prev_t3_interv <- c(1,0,1,0,0,0)
log_prev_t3_interv = t(lincom_prev_t3_interv) %*% glmfit_lin_robustse[,1]
logprev_t3_interv_se = sqrt( t(lincom_prev_t3_interv) %*% glmfit_robust_iden_vcov %*% lincom_prev_t3_interv)
prev_t3_interv_lower <- log_prev_t3_interv-1.96*logprev_t3_interv_se
prev_t3_interv_upper <- log_prev_t3_interv+1.96*logprev_t3_interv_se
prev_t3_interv <- data.frame(log_prev_t3_interv, prev_t3_interv_lower, prev_t3_interv_upper)
prev_t3_interv$tertile <- "T3 \n(highest socioeconomic position)"
prev_t3_interv$Arms <- "intervention"
colnames(prev_t3_interv) <- c("prevalence", "lower", "upper", "Tertile", "Arms")

# merging dataframes
dat_prev <- rbind(prev_t1_control, prev_t1_interv, 
                  prev_t2_control, prev_t2_interv,
                  prev_t3_control, prev_t3_interv)

print(dat_prev)

## Plotting Prevalence Ratio (control/intervention) and Prevalence Difference (control-intervention)

# plots

# plotting diarrhea prevalence by arm and per wealth tertile
plot_prev <- ggplot(data = dat_prev, 
                    aes(x = Tertile, y = prevalence, color = Arms
                    )) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), 
                width = 0.1, linewidth = 0.6, position = position_dodge(width = 0.4)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.4)) +
  scale_color_manual(values = c('#E69F00', 'turquoise')) + 
  #theme(legend.position = "top") +
  scale_y_continuous(breaks=c(0,0.02,0.04,0.06,0.08,0.10,0.12), limits = c(0.00,0.12),
                     #scale_y_continuous(breaks=seq(0,0.12,by=0.02), 
                     labels =   sprintf("%1.0f",c(0,0.02,0.04,0.06,0.08,0.10,0.12)*100)) +
  labs(x = "Wealth index tertiles", y = "Diarrhea Prevalence (%)"#, tag = "  \n"
  ) + 
  theme_minimal() +
  theme(legend.position = c(0.9,0.8),
        legend.title = element_blank(),
        legend.key.size = unit(0.05, 'cm'),
        text = element_text(size=7.5))

# plotting PR control/intervention
glm_plot_pr <- ggplot(dat, aes(Tertile, PR)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper#, color = Tertile
  ), 
  width = 0.06, linewidth = 0.6, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linetype="dotted") +
  geom_point(size = 1.5, 
             position = position_dodge(width = 0.4)) +
  scale_y_continuous(breaks=c(1,1.5,2,2.5,3), trans = "log", limits = c(0.8,3)) +
  #scale_color_brewer(palette="Dark2") +
  #theme(legend.position = "top") +
  labs(x = "Wealth index tertiles", y = "Prevalence Ratio (PR)"#, tag = "  \n"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size=7.5)) 


# plotting PD control-intervention
glm_plot_pd <- ggplot(dat_pd, aes(Tertile, PD)) +
  scale_y_continuous(breaks=c(0.00,0.02,0.04,0.06,0.08), limits = c(-0.0101,0.08),
                     labels =   sprintf("%1.0f",c(0.00,0.02,0.04,0.06,0.08)*100)) +
  geom_errorbar(aes(ymin=lower, ymax=upper#, color = Tertile
  ), 
  width = 0.06, linewidth = 0.6, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype="dotted") +
  geom_point(size = 1.5, 
             position = position_dodge(width = 0.4)) +
  #scale_color_brewer(palette="Dark2") +
  #theme(legend.position = "top") +
  labs(x = "Wealth index tertiles", y = "Prevalence Difference (PD, %)"#, tag = "  \n"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size=7.5)) 

#plot_composite <- plot_grid(gee_plot_pr, glm_plot_pd, nrow=1, ncol=2)

plot_prev_composite <- plot_grid(plot_prev, glm_plot_pr,glm_plot_pd, ncol=1, 
                                 nrow=3#, bottom = quote("Wealth index tertiles")
)
#plot_prev_composite <- plot_grid(plot_prev,glm_plot_pd, ncol=1, 
#nrow=3#, bottom = quote("Wealth index tertiles")
#nrow=2
#)
plot_prev_composite

