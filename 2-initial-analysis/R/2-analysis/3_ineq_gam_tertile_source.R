
# title: "Measuring inequalities"
# author: "Pearl Ante-Testard"
# R script version: source code for the R script "2.1_ineq_gam_tertile_comp

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

# clean environment

#rm(list=ls(all=TRUE))


# configuration, message=FALSE}

library(here)
source(here::here("2-initial-analysis/R", "0-config.R"))


# read formatted data

#df_analysis <- readRDS(file = here::here("1-data", "2-final",
#"enrol_diar_tr_wealth_indiv_svy012.rds"))

# Surveys 1 and 2 (8440 children)
df_analysis_svy12_2 <- readRDS(file = here::here("1-data", "2-final",
                                               "enrol_diar_tr_wealth_indiv_svy012.rds")) %>%
                       filter(svy!=0)


# data

df_analysis_svy12_2 <- df_analysis_svy12_2 %>%
  mutate(wealth_rank = rank(wealthscore)/nrow(.),
         block=factor(block),
         wealth_tertile=factor(wealth_tertile),
         diar7df=factor(diar7d),
         Arms=factor(tr_comb, levels = c(0,1), labels = c("Control","Intervention")))

quantile(df_analysis_svy12_2$wealthscore, probs=seq(0, 1, 1/3))
#summary(df_analysis_svy12$wealth_rank)
#str(df_analysis_svy12$diar7d)


## Estimating diarrhea prevalence per wealth tertile in the 2 arms

# diarrhea prevalence by arm

#df_analysis_svy12$diar7d <- as.numeric(df_analysis_svy12$diar7d)-1
df_analysis_svy12_2 <- df_analysis_svy12_2 %>%
  mutate(diar7d = case_when(
    diar7df == "0" ~ 0,
    diar7df == "1" ~ 1,
    TRUE ~ as.numeric(NA)
  ))

str(df_analysis_svy12_2$diar7d)

prev_diar <- df_analysis_svy12_2 %>%                         
  group_by(Arms, wealth_tertile) %>%         
  summarise_at(vars(diar7d),                  
               list(length = length, sum = sum, mean = mean, sd = sd)) 

print(prev_diar)

## Estimating PRs (Control/Intervention) and 95% CIs using modified Poisson

# geeglm models

# releving the variable Arms with the intervention arm as reference
# rationale: there were more diarrhea prevalence in the control arm
df_analysis_svy12_2$Arms <- relevel(df_analysis_svy12_2$Arms, ref = "Intervention")

# modeling a geeglm fit with interaction
# using the numeric diar7d for geeglm
geemod_int <- geeglm(diar7d ~ wealth_tertile + Arms +
                       wealth_tertile*Arms,
                     family = poisson(link="log"), id=block,  corstr='independence', std.err = "san.se",
                     data = df_analysis_svy12_2)
summary(geemod_int)

# get the var-cov matrix
geefit_robust_vcov <- vcov(geemod_int,cluster=df_analysis_svy12_2$block)

# modeling a geeglm fit without interaction
geemod_noint <- geeglm(diar7d ~ wealth_tertile + Arms,
                       family = poisson(link="log"), id=block,  corstr='independence', std.err = "san.se",
                       data = df_analysis_svy12_2)
summary(geemod_noint)

# wald test to compare the models with and without interaction
wald_pr <- waldtest(geemod_int,geemod_noint, vcov = geefit_robust_vcov) 
print(wald_pr)



# PR estimation using mod Poisson}

# get PR from geemod_int

# method 1: manual
PR_1 <- exp(geemod_int$coefficients[4])
PR_2 <- exp((geemod_int$coefficients[1] + geemod_int$coefficients[2] + geemod_int$coefficients[4]+ geemod_int$coefficients[5]) - (geemod_int$coefficients[1] + geemod_int$coefficients[2]))
PR_3 <- exp((geemod_int$coefficients[1] + geemod_int$coefficients[3]  + geemod_int$coefficients[4] + geemod_int$coefficients[6]) - (geemod_int$coefficients[1] + geemod_int$coefficients[3]))

#PR_1; PR_2; PR_3

# method 2
# PR for tertile 1
logPR = geemod_int$coefficients[4]
logPR_se = coef(summary(geemod_int))[4,"Std.err"]
PR_1 <- exp(logPR)
PR_1_lower <- exp(logPR-1.96*logPR_se)
PR_1_upper <- exp(logPR+1.96*logPR_se)
#PR_1; PR_1_lower; PR_1_upper
dat_1 <- data.frame(PR_1, PR_1_lower, PR_1_upper)
dat_1$tertile <- "T1 \n(poorest)"
colnames(dat_1) <- c("PR", "lower", "upper", "Tertile")

# PR for tertile 2
lincom_2 <- c(0,0,0,1,1,0)
logPR_2 = t(lincom_2) %*% geemod_int$coeff
logPR_se_2 = sqrt( t(lincom_2) %*% summary(geemod_int)$cov.unscaled %*% lincom_2 )
PR_2 <- exp(logPR_2)
PR_2_lower <- exp(logPR_2-1.96*logPR_se_2)
PR_2_upper <- exp(logPR_2+1.96*logPR_se_2)
#PR_2; PR_2_lower; PR_2_upper
dat_2 <- data.frame(PR_2, PR_2_lower, PR_2_upper)
dat_2$tertile <- "T2"
colnames(dat_2) <- c("PR", "lower", "upper", "Tertile")

# PR for tertile 3
lincom_3 <- c(0,0,0,1,0,1)
logPR_3 = t(lincom_3) %*% geemod_int$coeff
logPR_se_3 = sqrt( t(lincom_3) %*% summary(geemod_int)$cov.unscaled %*% lincom_3 )
PR_3 <- exp(logPR_3)
PR_3_lower <- exp(logPR_3-1.96*logPR_se_3)
PR_3_upper <- exp(logPR_3+1.96*logPR_se_3)
#PR_3; PR_3_lower; PR_3_upper
dat_3 <- data.frame(PR_3, PR_3_lower, PR_3_upper)
dat_3$tertile <- "T3 \n(wealthiest)"
colnames(dat_3) <- c("PR", "lower", "upper", "Tertile")

# merging dataframes
dat <- rbind(dat_1, dat_2, dat_3)

print(dat)


## Estimating PDs (Control - Intervention) and 95% CIs using linear-binomial

# glm models

# modeling a glm fit with interaction
# using the factor diar7df for glm
glm_int_iden <- glm(diar7df ~ wealth_tertile + Arms + wealth_tertile*Arms,
                    family = binomial(link="identity"),
                    data = df_analysis_svy12_2
)
#summary(glm_int_iden)

# get robust var-covar matrix, allowing for block-level clustering 
glmfit_robust_iden_vcov <- vcovCL(glm_int_iden,cluster=df_analysis_svy12_2$block)

# update regression fit
glmfit_lin_robustse <- coeftest(glm_int_iden,vcov. =  glmfit_robust_iden_vcov)
print(glmfit_lin_robustse)

# modeling a glm fit without an interaction
glm_noint_iden <- glm(diar7df ~ wealth_tertile + Arms,
                      family = binomial(link="identity"),
                      data = df_analysis_svy12_2
)
#summary(glm_noint_iden)
print(coeftest(glm_noint_iden,vcov. =  glmfit_robust_iden_vcov))

# wald test to compare the models with and without interaction
wald_pd <- waldtest(glm_int_iden,glm_noint_iden, vcov = glmfit_robust_iden_vcov) 
print(wald_pd)


# PD estimation using glm }

# PD for tertile 1
logPD = glmfit_lin_robustse[4,1]
logPD_se = glmfit_lin_robustse[4,2]
PD_1_lower <- logPD-1.96*logPD_se
PD_1_upper <- logPD+1.96*logPD_se
#PR_1; PR_1_lower; PR_1_upper
dat_1_pd <- data.frame(logPD, PD_1_lower, PD_1_upper)
dat_1_pd$tertile <- "T1 \n(poorest)"
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
dat_3_pd$tertile <- "T3 \n(wealthiest)"
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
prev_t1_control$tertile <- "T1 \n(poorest)"
prev_t1_control$Arms <- "control"
colnames(prev_t1_control) <- c("prevalence", "lower", "upper", "Tertile", "Arms")
# intervention
lincom_prev_t1_interv <- c(1,0,0,0,0,0)
log_prev_t1_interv = t(lincom_prev_t1_interv) %*% glmfit_lin_robustse[,1]
logprev_t1_interv_se = sqrt( t(lincom_prev_t1_interv) %*% glmfit_robust_iden_vcov %*% lincom_prev_t1_interv)
prev_t1_interv_lower <- log_prev_t1_interv-1.96*logprev_t1_interv_se
prev_t1_interv_upper <- log_prev_t1_interv+1.96*logprev_t1_interv_se
prev_t1_interv <- data.frame(log_prev_t1_interv, prev_t1_interv_lower, prev_t1_interv_upper)
prev_t1_interv$tertile <- "T1 \n(poorest)"
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
prev_t3_control$tertile <- "T3 \n(wealthiest)"
prev_t3_control$Arms <- "control"
colnames(prev_t3_control) <- c("prevalence", "lower", "upper", "Tertile", "Arms")
# intervention
lincom_prev_t3_interv <- c(1,0,1,0,0,0)
log_prev_t3_interv = t(lincom_prev_t3_interv) %*% glmfit_lin_robustse[,1]
logprev_t3_interv_se = sqrt( t(lincom_prev_t3_interv) %*% glmfit_robust_iden_vcov %*% lincom_prev_t3_interv)
prev_t3_interv_lower <- log_prev_t3_interv-1.96*logprev_t3_interv_se
prev_t3_interv_upper <- log_prev_t3_interv+1.96*logprev_t3_interv_se
prev_t3_interv <- data.frame(log_prev_t3_interv, prev_t3_interv_lower, prev_t3_interv_upper)
prev_t3_interv$tertile <- "T3 \n(wealthiest)"
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
gee_plot_pr <- ggplot(dat, aes(Tertile, PR)) + 
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

plot_prev_composite <- plot_grid(plot_prev, gee_plot_pr,glm_plot_pd, ncol=1, 
                                 nrow=3#, bottom = quote("Wealth index tertiles")
)
#plot_prev_composite <- plot_grid(plot_prev,glm_plot_pd, ncol=1, 
                                 #nrow=3#, bottom = quote("Wealth index tertiles")
                                 #nrow=2
#)
plot_prev_composite


## RII and SII estimates from the geeglm model

# Here, used wealth_rank in the model.

# RII > 1 or SII > 0: diarrhea prev is higher among the poor than the rich; 
# RII = 1 or SII = 0: no inequality

# RII and SII

# control
df_control <- df_analysis_svy12_2 %>%
  filter(Arms == "Control")

geemod_c <- geeglm(diar7d ~ wealth_rank,
                   family = poisson(link="log"), id=block,  corstr='independence', std.err = "san.se",
                   data = df_control)
#summary(geemod_c)

# RII
# Here, since the outcome is unfavorable, the formula is intercept-(intercept+slope).
beta_gee_c = coef(geemod_c)[1] - (coef(geemod_c)[1]+coef(geemod_c)[2])
RII_gee_c = exp(beta_gee_c)
#RII_gee_c 
# 95% CI: based on wald interval
se_b1 = coef(summary(geemod_c))[2,"Std.err"]; #se_b1
RII_low<-exp(beta_gee_c-1.96*se_b1)#; RII_low
RII_up<-exp(beta_gee_c+1.96*se_b1)#; RII_up

# SII
SII_gee_c = exp(coef(geemod_c)[1]) - exp(coef(geemod_c)[1]+coef(geemod_c)[2])
#SII_gee_c
# 95% CI: based on the delta method
b <- coef(geemod_c)
v = vcov(geemod_c)
SII_sd = deltamethod(~exp(x1)-exp(x1+x2),b,v) 
SII_low<-SII_gee_c-1.96*SII_sd#; SII_low
SII_up<-SII_gee_c+1.96*SII_sd#; SII_up

#-----------------------------------------------------------------------------

# intervention
df_interv <- df_analysis_svy12_2 %>%
  filter(Arms == "Intervention")

geemod_i <- geeglm(diar7d ~ wealth_rank,
                   family = poisson(link="log"), id=block,  corstr='independence', std.err = "san.se",
                   data = df_interv)

# RII 
beta_gee_i = coef(geemod_i)[1] - (coef(geemod_i)[1]+coef(geemod_i)[2])
RII_gee_i = exp(beta_gee_i)
#RII_gee_i 
se_b1_i = coef(summary(geemod_i))[2,"Std.err"]; #se_b1_i
RII_low_i<-exp(beta_gee_i-1.96*se_b1_i)#; RII_low_i
RII_up_i<-exp(beta_gee_i+1.96*se_b1_i)#; RII_up_i

# SII
SII_gee_i = exp(coef(geemod_i)[1]) - exp(coef(geemod_i)[1]+coef(geemod_i)[2])
#SII_gee_i
b_i <- coef(geemod_i)
v_i = vcov(geemod_i)
SII_sd_i = deltamethod(~exp(x1)-exp(x1+x2),b_i,v_i) 
SII_low_i<-SII_gee_i-1.96*SII_sd_i#; SII_low_i
SII_up_i<-SII_gee_i+1.96*SII_sd_i#; SII_up_i

#-----------------------------------------------------------------------------

# creating a dataframe with the estimates 
RII <- c(RII_gee_c, RII_gee_i)
RII_lower <- c(RII_low, RII_low_i)
RII_upper <- c(RII_up, RII_up_i)
SII <- c(SII_gee_c, SII_gee_i)
SII_lower <- c(SII_low, SII_low_i)
SII_upper <- c(SII_up, SII_up_i)

ineq_tab <- data.frame(RII, RII_lower, RII_upper, SII, SII_lower, SII_upper)
ineq_tab$Arms <- c("Control", "Intervention")
print(ineq_tab)


## Plotting RII and SII 

# RII > 1 or SII > 0: diarrhea prev is higher among the poor than the rich; 
# RII = 1 or SII = 0: no inequality

# plotting RII and SII

plot_rii_sii <- ggplot(ineq_tab, aes(SII, RII, fill=Arms)) +
  scale_fill_manual(values = c('#E69F00', 'turquoise')) +
  geom_linerange(aes(ymin = RII_lower, ymax = RII_upper)) +
  scale_y_continuous(breaks=c(0.5,1,1.5,2,2.5,3), trans = "log") +
  geom_linerange(aes(xmin = SII_lower, xmax = SII_upper)) +
  geom_point(aes(fill=Arms), shape = 21, size = 3, 
             #position = position_dodge(width = 0.4)
  ) +
  geom_hline(yintercept = 1, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  #coord_flip() +
  #theme(legend.position = "top") +
  labs(x = "Slope Index of Inequality (95% Confidence Interval)", 
       y = "Relative Index of Inequality (95% Confidence Interval)", 
       title = "Wealth-related inequality indicators") +
  guides(col = "none")+
  theme_bw() +
  theme(legend.position = c(0.9,0.9),
        legend.title = element_blank(),
        legend.key.size = unit(0.05, 'cm'),
        text = element_text(size=12),
        plot.title = element_text(face = "bold")) 
plot_rii_sii




#ggsave(plot_prev_composite, file = here::here("2-initial-analysis", "output",
                                             # "plot_effectmodsep.pdf"), height=8,width=5)

#ggsave(plot_prev_composite, file = here::here("2-initial-analysis", "output",
                                             # "plot_effectmodsep.jpg"), height=8,width=5)

#ggsave(plot_rii_sii, file = here::here("2-initial-analysis", "output",
                                       # "plot_riisii.pdf"), height=7,width=7)

ggsave(plot_rii_sii, file = here::here("2-initial-analysis", "output",
                                       "plot_riisii_v2_poster.jpg"), height=8,width=8)


