
# title: "Measuring inequalities: using GAMS"
# author: "Pearl Ante-Testard"
# R script version: code to plot both wealth tertiles and continuous wealth (Fig. 3)


# clean environment
rm(list=ls(all=TRUE))

# configuration
library(here)
source(here::here("2-initial-analysis/R", "0-config.R"))

################## monsoon #################
# Surveys 1 and 2 (monsoon)
df_analysis_svy12_monsoon <- readRDS(file = here::here("1-data", "2-final",
                                               "enrol_diar_tr_wealth_indiv_svy012.rds")) %>%
                                                filter(svy!=0) %>%
                                                filter(monsoon==1)

df_analysis_svy12_monsoon <- df_analysis_svy12_monsoon %>%
  mutate(wealth_rank = rank(wealthscore)/nrow(.),
         block=factor(block),
         wealth_tertile=factor(wealth_tertile),
         diar7df=factor(diar7d),
         Arms=factor(tr_comb, levels = c(0,1), labels = c("Control","Intervention")),
         dummy=rep(1, nrow(.)))

summary(df_analysis_svy12_monsoon$wealth_rank)
str(df_analysis_svy12_monsoon$diar7d)


# prev by tertile and arm

df_analysis_svy12_monsoon <- df_analysis_svy12_monsoon %>%
  mutate(diar7d = case_when(
    diar7d == "0" ~ 0,
    diar7d == "1" ~ 1,
    TRUE ~ as.numeric(NA)
  ))
str(df_analysis_svy12_monsoon$diar7d)

df_analysis_svy12_monsoon %>%                         
  group_by(Arms, wealth_tertile) %>%         
  summarise_at(vars(diar7d),                  
               list(length = length, sum = sum, mean = mean, sd = sd)) 


## Modelling diarrhea ~ SEP using different model specifications

# gam

# releving the variable Arms with the intervention arm as reference
# rationale: there were more diarrhea prevalence in the control arm
df_analysis_svy12_monsoon$Arms <- relevel(df_analysis_svy12_monsoon$Arms, ref = "Intervention")

# gam to estimate the prevalence difference
mod_gam_1_monsoon = gam(diar7d ~ Arms + s(wealth_rank, bs = "cr", by = Arms) +
                  s(block, bs = "re", by = dummy), # re is on
                data = df_analysis_svy12_monsoon,
                family = "gaussian", method = "REML")
summary(mod_gam_1_monsoon)

# gam to estimate the prevalence ratio
mod_gam_2_monsoon = gam(diar7d ~ Arms + s(wealth_rank, bs = "cr", by = Arms) +
                  s(block, bs = "re", by = dummy), # re is on
                data = df_analysis_svy12_monsoon,
                family = binomial(link="log"), method = "REML")
summary(mod_gam_2_monsoon)


## Getting the smooth difference and ratio

# smooth difference and ratio

# get the smooth difference
# codes for get_smooths_difference: https://github.com/stefanocoretta/tidymv/blob/main/R/plotting.R 
# that uses the get_difference function (https://github.com/stefanocoretta/tidymv/blob/main/R/imported-fun.R)

# smooth difference
gam_diff_monsoon <- get_smooths_difference(mod_gam_1_monsoon, wealth_rank, difference = list(Arms = c("Control",
                                                                                      "Intervention")))
print(gam_diff_monsoon)

# smooth ratio - the difference would need to be exponentiated
gam_ratio_monsoon <- get_smooths_difference(mod_gam_2_monsoon, wealth_rank, difference = list(Arms = c("Control",
                                                                                       "Intervention")))
print(gam_ratio_monsoon)
exp(gam_ratio_monsoon$difference); exp(gam_ratio_monsoon$CI_lower); exp(gam_ratio_monsoon$CI_upper)


## Plotting prevalence ratio (control/interv) and prevalence difference (control-interv)


#plot gam

# Diarrhea prevalence
gam_plot_prev_monsoon <- plot_smooths(model = mod_gam_1_monsoon, series = wealth_rank, 
                              comparison = Arms) +
  #scale_x_continuous(breaks=c(0.00,0.33,0.67,1)) +
  #scale_x_continuous(breaks=c(0.00,0.17,0.50,0.84,1)) +
  scale_x_continuous(breaks=c(0.00,0.165,0.33,0.50,0.835,0.67,1)) +
  geom_vline(xintercept=c(0.333,0.67), linetype='dashed', color="grey") +
  scale_colour_manual(values = c('turquoise','#E69F00')) + 
  #scale_colour_discrete(labels=c("Control","Intervention")) +
  scale_fill_manual(values = c('turquoise','#E69F00')) + 
  #scale_fill_discrete(labels=c("Control","Intervention")) + 
  scale_linetype_manual(values=c("solid","dashed")) +
  #scale_linetype_discrete(labels=c("Control","Intervention")) +
  scale_y_continuous(breaks=c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14), limits = c(0.00,0.14),
                     #scale_y_continuous(breaks=seq(0,0.12,by=0.02), 
                     labels =   sprintf("%1.0f",c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14)*100)) +
  labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
  labs(x = "Wealth rank based on the wealth index score",
       y = "Diarrhea Prevalence (%)",#, tag = "a. Prevalence"
       title = "A. Monsoon season"
  ) +
  theme_minimal() +
  theme(legend.position = c(0.85,0.75),
        legend.title = element_blank(),
        legend.key.size = unit(0.3, 'cm'),text = element_text(size=7.5),
        plot.title = element_text(face = "bold")
  ) 
#gam_plot_prev <- gam_plot_prev + coord_fixed(ratio=10) #10 divided by 1

#gam_plot_prev

# Prevalence ratio
#ratio.display.pr <- 1.618 # golden ratio
#ratio.values.pr <- (max(gam_ratio$wealth_rank)-min(gam_ratio$wealth_rank))/
#  (max(exp(gam_ratio$difference))-min(exp(gam_ratio$difference)))

plot_ratio_monsoon <- ggplot(gam_ratio_monsoon, aes(wealth_rank, exp(difference)#, group=group
)) +
  #scale_x_continuous(breaks=c(0.00,0.33,0.67,1)) +
  scale_x_continuous(breaks=c(0.00,0.165,0.33,0.50,0.835,0.67,1)) +
  geom_vline(xintercept=c(0.33,0.67), linetype='dashed', color="grey") +
  geom_hline(aes(yintercept = 1), linetype = "dotted") + 
  geom_ribbon(aes(ymin = exp(CI_lower), ymax = exp(CI_upper)#, fill = sig_diff
  ),  alpha = 0.3) +
  geom_line(aes(#olour = sig_diff
  ), linewidth = 0.6) +
  #scale_color_manual(values = c("#e35760", "#6f849c")) +
  #scale_fill_manual(values = c("#e35760", "#6f849c")) +
  #scale_color_discrete(labels=c("False", "True")) +
  #scale_fill_discrete(labels=c("False", "True")) +
  #labs(colour = "Significant", fill = "Significant") +
  scale_y_continuous(breaks=c(0.5,0.8,1,1.5,2,3,4), trans = "log", limits = c(0.5,4)) +
  labs(x = "Wealth rank based on the wealth index score",
       y = "Prevalence Ratio (PR)"
  ) +
  #coord_cartesian(ylim=c(0.8,3)) +
  theme_minimal() +
  theme(#legend.position = c(0.75,0.75),
    legend.title = element_blank(),
    legend.key.size = unit(0.4, 'cm'),text = element_text(size=7.5),
    plot.title = element_text(face = "bold")) 
#plot_ratio <- plot_ratio + coord_fixed(ratio.values.pr / ratio.display.pr)

#plot_ratio

# Prevalence difference
#ratio.display.pd <- 1.618
#ratio.values.pd <- (max(gam_diff$wealth_rank)-min(gam_diff$wealth_rank))/
#(max(gam_diff$difference)-min(gam_diff$difference))

plot_diff_monsoon <- ggplot(gam_diff_monsoon, aes(wealth_rank, difference#, group=group
)) +
  scale_x_continuous(breaks=c(0.00,0.165,0.33,0.50,0.835,0.67,1)) +
  scale_y_continuous(breaks=c(-0.04,-0.02,0.00,0.02,0.04,0.06,0.08,0.10), limits = c(-0.04,0.10),
                     labels =   sprintf("%1.0f",c(-0.04,-0.02,0.00,0.02,0.04,0.06,0.08,0.10)*100))  +
  geom_vline(xintercept=c(0.33,0.67), linetype='dashed', color="grey") +
  geom_hline(aes(yintercept = 0), linetype = "dotted") + 
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper#, fill = sig_diff
  ), alpha = 0.3) +
  geom_line(aes(#colour = sig_diff
  ), linewidth = 0.6) +
  #scale_colour_manual(values = c("#e35760", "#6f849c")) +
  #scale_fill_manual(values = c("#e35760", "#6f849c")) +
  #scale_color_discrete(labels=c("False", "True")) +
  #scale_fill_discrete(labels=c("False", "True")) +
  #labs(colour = "Significant", fill = "Significant") +
  labs(x = "Wealth rank based on the wealth index score",
       y = "Prevalence Difference (PD, %)"
  ) +
  #coord_cartesian(ylim=c(-0.01,0.07)) +
  theme_minimal() +
  theme(#legend.position = c(0.75,0.75),
    #legend.title = element_blank(),
    legend.key.size = unit(0.4, 'cm'),text = element_text(size=7.5),
    plot.title = element_text(face = "bold")) 
#plot_diff <- plot_diff + coord_fixed(ratio.values.pd / ratio.display.pd)

################## dry #################

# Surveys 1 and 2 (dry)
df_analysis_svy12_dry <- readRDS(file = here::here("1-data", "2-final",
                                               "enrol_diar_tr_wealth_indiv_svy012.rds")) %>%
                                                filter(svy!=0) %>%
                                                filter(monsoon==0)


df_analysis_svy12_dry <- df_analysis_svy12_dry %>%
  mutate(wealth_rank = rank(wealthscore)/nrow(.),
         block=factor(block),
         wealth_tertile=factor(wealth_tertile),
         diar7df=factor(diar7d),
         Arms=factor(tr_comb, levels = c(0,1), labels = c("Control","Intervention")),
         dummy=rep(1, nrow(.)))

summary(df_analysis_svy12_dry$wealth_rank)
str(df_analysis_svy12_dry$diar7d)


# prev by tertile and arm

df_analysis_svy12_dry <- df_analysis_svy12_dry %>%
  mutate(diar7d = case_when(
    diar7d == "0" ~ 0,
    diar7d == "1" ~ 1,
    TRUE ~ as.numeric(NA)
  ))
str(df_analysis_svy12_dry$diar7d)

df_analysis_svy12_dry %>%                         
  group_by(Arms, wealth_tertile) %>%         
  summarise_at(vars(diar7d),                  
               list(length = length, sum = sum, mean = mean, sd = sd)) 


## Modelling diarrhea ~ SEP using different model specifications

# gam

# releving the variable Arms with the intervention arm as reference
# rationale: there were more diarrhea prevalence in the control arm
df_analysis_svy12_dry$Arms <- relevel(df_analysis_svy12_dry$Arms, ref = "Intervention")

# gam to estimate the prevalence difference
mod_gam_1_dry = gam(diar7d ~ Arms + s(wealth_rank, bs = "cr", by = Arms) +
                  s(block, bs = "re", by = dummy), # re is on
                data = df_analysis_svy12_dry,
                family = "gaussian", method = "REML")
summary(mod_gam_1_dry)

# gam to estimate the prevalence ratio
mod_gam_2_dry = gam(diar7d ~ Arms + s(wealth_rank, bs = "cr", by = Arms) +
                  s(block, bs = "re", by = dummy), # re is on
                data = df_analysis_svy12_dry,
                family = binomial(link="log"), method = "REML")
summary(mod_gam_2_dry)


## Getting the smooth difference and ratio

# smooth difference and ratio

# get the smooth difference
# codes for get_smooths_difference: https://github.com/stefanocoretta/tidymv/blob/main/R/plotting.R 
# that uses the get_difference function (https://github.com/stefanocoretta/tidymv/blob/main/R/imported-fun.R)

# smooth difference
gam_diff_dry <- get_smooths_difference(mod_gam_1_dry, wealth_rank, difference = list(Arms = c("Control",
                                                                                      "Intervention")))
print(gam_diff_dry)

# smooth ratio - the difference would need to be exponentiated
gam_ratio_dry <- get_smooths_difference(mod_gam_2_dry, wealth_rank, difference = list(Arms = c("Control",
                                                                                       "Intervention")))
print(gam_ratio_dry)
exp(gam_ratio_dry$difference); exp(gam_ratio_dry$CI_lower); exp(gam_ratio_dry$CI_upper)


## Plotting prevalence ratio (control/interv) and prevalence difference (control-interv)


#plot gam

# Diarrhea prevalence
gam_plot_prev_dry <- plot_smooths(model = mod_gam_1_dry, series = wealth_rank, 
                              comparison = Arms) +
  #scale_x_continuous(breaks=c(0.00,0.33,0.67,1)) +
  #scale_x_continuous(breaks=c(0.00,0.17,0.50,0.84,1)) +
  scale_x_continuous(breaks=c(0.00,0.165,0.33,0.50,0.835,0.67,1)) +
  geom_vline(xintercept=c(0.333,0.67), linetype='dashed', color="grey") +
  scale_colour_manual(values = c('turquoise','#E69F00')) + 
  #scale_colour_discrete(labels=c("Control","Intervention")) +
  scale_fill_manual(values = c('turquoise','#E69F00')) + 
  #scale_fill_discrete(labels=c("Control","Intervention")) + 
  scale_linetype_manual(values=c("solid","dashed")) +
  #scale_linetype_discrete(labels=c("Control","Intervention")) +
  scale_y_continuous(breaks=c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14), limits = c(0.00,0.14),
                     #scale_y_continuous(breaks=seq(0,0.12,by=0.02), 
                     labels =   sprintf("%1.0f",c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14)*100)) +
  labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
  labs(x = "Wealth rank based on the wealth index score",
       y = "Diarrhea Prevalence (%)",#, tag = "a. Prevalence"
       title = "B. Dry season"
  ) +
  #coord_cartesian(ylim=c(0.01,0.12)) +
  theme_minimal() +
  theme(legend.position = c(0.85,0.75),
        legend.title = element_blank(),
        legend.key.size = unit(0.3, 'cm'),text = element_text(size=7.5),
        plot.title = element_text(face = "bold")
  ) 
#gam_plot_prev <- gam_plot_prev + coord_fixed(ratio=10) #10 divided by 1

#gam_plot_prev

# Prevalence ratio
#ratio.display.pr <- 1.618 # golden ratio
#ratio.values.pr <- (max(gam_ratio$wealth_rank)-min(gam_ratio$wealth_rank))/
#  (max(exp(gam_ratio$difference))-min(exp(gam_ratio$difference)))

plot_ratio_dry <- ggplot(gam_ratio_dry, aes(wealth_rank, exp(difference)#, group=group
)) +
  #scale_x_continuous(breaks=c(0.00,0.33,0.67,1)) +
  scale_x_continuous(breaks=c(0.00,0.165,0.33,0.50,0.835,0.67,1)) +
  geom_vline(xintercept=c(0.33,0.67), linetype='dashed', color="grey") +
  geom_hline(aes(yintercept = 1), linetype = "dotted") + 
  geom_ribbon(aes(ymin = exp(CI_lower), ymax = exp(CI_upper)#, fill = sig_diff
  ),  alpha = 0.3) +
  geom_line(aes(#olour = sig_diff
  ), linewidth = 0.6) +
  #scale_color_manual(values = c("#e35760", "#6f849c")) +
  #scale_fill_manual(values = c("#e35760", "#6f849c")) +
  #scale_color_discrete(labels=c("False", "True")) +
  #scale_fill_discrete(labels=c("False", "True")) +
  #labs(colour = "Significant", fill = "Significant") +
  scale_y_continuous(breaks=c(0.5,0.8,1,1.5,2,3,4), trans = "log", limits = c(0.5,4)) +
  labs(x = "Wealth rank based on the wealth index score",
       y = "Prevalence Ratio (PR)"
  ) +
  #coord_cartesian(ylim=c(0.8,3)) +
  theme_minimal() +
  theme(#legend.position = c(0.75,0.75),
    legend.title = element_blank(),
    legend.key.size = unit(0.4, 'cm'),text = element_text(size=7.5),
    plot.title = element_text(face = "bold")) 
#plot_ratio <- plot_ratio + coord_fixed(ratio.values.pr / ratio.display.pr)

#plot_ratio

# Prevalence difference
#ratio.display.pd <- 1.618
#ratio.values.pd <- (max(gam_diff$wealth_rank)-min(gam_diff$wealth_rank))/
#(max(gam_diff$difference)-min(gam_diff$difference))

plot_diff_dry <- ggplot(gam_diff_dry, aes(wealth_rank, difference#, group=group
)) +
  scale_x_continuous(breaks=c(0.00,0.165,0.33,0.50,0.835,0.67,1)) +
  scale_y_continuous(breaks=c(-0.04,-0.02,0.00,0.02,0.04,0.06,0.08,0.10), limits = c(-0.04,0.10),
                     labels =   sprintf("%1.0f",c(-0.04,-0.02,0.00,0.02,0.04,0.06,0.08,0.10)*100))  +
  geom_vline(xintercept=c(0.33,0.67), linetype='dashed', color="grey") +
  geom_hline(aes(yintercept = 0), linetype = "dotted") + 
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper#, fill = sig_diff
  ), alpha = 0.3) +
  geom_line(aes(#colour = sig_diff
  ), linewidth = 0.6) +
  #scale_colour_manual(values = c("#e35760", "#6f849c")) +
  #scale_fill_manual(values = c("#e35760", "#6f849c")) +
  #scale_color_discrete(labels=c("False", "True")) +
  #scale_fill_discrete(labels=c("False", "True")) +
  #labs(colour = "Significant", fill = "Significant") +
  labs(x = "Wealth rank based on the wealth index score",
       y = "Prevalence Difference (PD, %)"
  ) +
  #coord_cartesian(ylim=c(-0.01,0.07)) +
  theme_minimal() +
  theme(#legend.position = c(0.75,0.75),
    #legend.title = element_blank(),
    legend.key.size = unit(0.4, 'cm'),text = element_text(size=7.5),
    plot.title = element_text(face = "bold")) 

###################

plot_prev_comp <- plot_grid(gam_plot_prev_monsoon, gam_plot_prev_dry, ncol = 2, align = "h")

plot_ratio_comp <- plot_grid(plot_ratio_monsoon, plot_ratio_dry, ncol = 2, align = "h")

plot_diff_comp <- plot_grid(plot_diff_monsoon, plot_diff_dry, ncol = 2, align = "h")

plot_gam <- plot_grid(plot_prev_comp, plot_ratio_comp, plot_diff_comp,
                              ncol = 1, nrow = 3, align = "h")
plot_gam

###################

ggsave(plot_gam, file = here::here("4-supplementary", "output",
                                           "plot_effectmod_sep_monsoon_gam.pdf"), height=210, width=180, units = "mm")
