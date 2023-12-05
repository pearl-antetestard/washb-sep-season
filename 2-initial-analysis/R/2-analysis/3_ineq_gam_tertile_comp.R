
# title: "Measuring inequalities: using GAMS"
# author: "Pearl Ante-Testard"
# R script version: code to plot both wealth tertiles and continuous wealth (Fig. 3)


# clean environment
#rm(list=ls(all=TRUE))

# configuration
library(here)
source(here::here("2-initial-analysis/R", "0-config.R"))

#source(here::here("2-initial-analysis/R", "2-analysis",
                  #"3_ineq_gam_tertile_source.R"), local = T)
source(here::here("2-initial-analysis/R", "2-analysis",
                  "3_ineq_gam_tertile_glm_source.R"), local = T)
plot_prev_composite


# data
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
         Arms=factor(tr_comb, levels = c(0,1), labels = c("Control","Intervention")),
         dummy=rep(1, nrow(.)))

summary(df_analysis_svy12$wealth_rank)
str(df_analysis_svy12$diar7d)


# prev by tertile and arm

df_analysis_svy12 <- df_analysis_svy12 %>%
  mutate(diar7d = case_when(
    diar7d == "0" ~ 0,
    diar7d == "1" ~ 1,
    TRUE ~ as.numeric(NA)
  ))
str(df_analysis_svy12$diar7d)

df_analysis_svy12 %>%                         
  group_by(Arms, wealth_tertile) %>%         
  summarise_at(vars(diar7d),                  
               list(length = length, sum = sum, mean = mean, sd = sd)) 


## Modelling diarrhea ~ SEP using different model specifications

# gam

# releving the variable Arms with the intervention arm as reference
# rationale: there were more diarrhea prevalence in the control arm
df_analysis_svy12$Arms <- relevel(df_analysis_svy12$Arms, ref = "Intervention")

# gam to estimate the prevalence difference
mod_gam_1 = gam(diar7d ~ Arms + s(wealth_rank, bs = "cr", by = Arms) +
                  s(block, bs = "re", by = dummy), # re is on
                data = df_analysis_svy12,
                family = "gaussian", method = "REML")
summary(mod_gam_1)

# gam to estimate the prevalence ratio
mod_gam_2 = gam(diar7d ~ Arms + s(wealth_rank, bs = "cr", by = Arms) +
                  s(block, bs = "re", by = dummy), # re is on
                data = df_analysis_svy12,
                family = binomial(link="log"), method = "REML")
summary(mod_gam_2)


## Getting the smooth difference and ratio

# smooth difference and ratio

# get the smooth difference
# codes for get_smooths_difference: https://github.com/stefanocoretta/tidymv/blob/main/R/plotting.R 
# that uses the get_difference function (https://github.com/stefanocoretta/tidymv/blob/main/R/imported-fun.R)
# computes simultaneous CIs which follows Gavin Simpson's: https://fromthebottomoftheheap.net/2016/12/15/simultaneous-interval-revisited/

# smooth difference
gam_diff <- get_smooths_difference(mod_gam_1, wealth_rank, difference = list(Arms = c("Control",
                                                                                      "Intervention")))
print(gam_diff)

# smooth ratio - the difference would need to be exponentiated
gam_ratio <- get_smooths_difference(mod_gam_2, wealth_rank, difference = list(Arms = c("Control",
                                                                                       "Intervention")))
print(gam_ratio)
exp(gam_ratio$difference); exp(gam_ratio$CI_lower); exp(gam_ratio$CI_upper)


## Plotting prevalence ratio (control/interv) and prevalence difference (control-interv)


#plot gam

# Diarrhea prevalence
gam_plot_prev <- plot_smooths(model = mod_gam_1, series = wealth_rank, 
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
  scale_y_continuous(breaks=c(0,0.02,0.04,0.06,0.08,0.10,0.12), limits = c(0.00,0.12),
                     #scale_y_continuous(breaks=seq(0,0.12,by=0.02), 
                     labels =   sprintf("%1.0f",c(0,0.02,0.04,0.06,0.08,0.10,0.12)*100)) +
  labs(colour = "Arms", fill = "Arms", linetype = "Arms") +
  labs(x = "Wealth rank based on the wealth index score",
       y = "Diarrhea Prevalence (%)",#, tag = "a. Prevalence"
       title = "A. Prevalence"
       ) +
  annotate(geom="text", x=c(0.333,0.67), y= c(0.10,0.10), 
           label= c("cut-off: \n1.028","cut-off: \n1.778"),
           color="black", size=2.5) +
  annotate(geom="text", x=c(0.17,0.50,0.84), y= c(0.11,0.11,0.11), 
           label= c("Tertile 1","Tertile 2",
                    "Tertile 3"),
           color="black", size=3) +
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

plot_ratio <- ggplot(gam_ratio, aes(wealth_rank, exp(difference)#, group=group
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
  scale_y_continuous(breaks=c(1,1.5,2,2.5,3), trans = "log", limits = c(0.8,3)) +
  labs(x = "Wealth rank based on the wealth index score",
       y = "Prevalence Ratio (PR)", title = "B. Prevalence Ratio"
       ) +
  annotate(geom="text", x=c(0.33,0.67), y= c(2.5,2.5), 
           label= c("cut-off: \n1.028","cut-off: \n1.778"),
           color="black", size=2.5) +
  annotate(geom="text", x=c(0.17,0.50,0.84), y= c(2.8,2.8,2.8), 
           label= c("Tertile 1","Tertile 2",
                    "Tertile 3"),
           color="black", size=3) +
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

plot_diff <- ggplot(gam_diff, aes(wealth_rank, difference#, group=group
)) +
  scale_x_continuous(breaks=c(0.00,0.165,0.33,0.50,0.835,0.67,1)) +
  scale_y_continuous(breaks=c(0.00,0.02,0.04,0.06,0.08), limits = c(-0.0101,0.08),
                     labels =   sprintf("%1.0f",c(0.00,0.02,0.04,0.06,0.08)*100))  +
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
       y = "Prevalence Difference (PD, %)", title = "C. Prevalence Difference"
         #"c. Prevalence Difference"
       ) +
  annotate(geom="text", x=c(0.33,0.67), y= c(0.06,0.06), 
           label= c("cut-off: \n1.028","cut-off: \n1.778"),
           color="black", size=2.5) +
  annotate(geom="text", x=c(0.17,0.50,0.84), y= c(0.07,0.07,0.07), 
           label= c("Tertile 1","Tertile 2",
                    "Tertile 3"),
           color="black", size=3) +
  #coord_cartesian(ylim=c(-0.01,0.07)) +
  theme_minimal() +
  theme(#legend.position = c(0.75,0.75),
    #legend.title = element_blank(),
    legend.key.size = unit(0.4, 'cm'),text = element_text(size=7.5),
    plot.title = element_text(face = "bold")) 
#plot_diff <- plot_diff + coord_fixed(ratio.values.pd / ratio.display.pd)

#plot_diff


#plot_composite <- plot_grid(plot_ratio, plot_diff, nrow=1, ncol=2)

#plot_gam_composite <- plot_grid(gam_plot_prev, plot_ratio, plot_diff, 
                               #  nrow=3) 
#plot_gam_composite


plot_prev_comp <- plot_grid(gam_plot_prev, plot_prev, ncol = 2, align = "h")

plot_ratio_comp <- plot_grid(plot_ratio, gee_plot_pr, ncol = 2, align = "h")

plot_diff_comp <- plot_grid(plot_diff, glm_plot_pd, ncol = 2, align = "h")

#ggsave(plot_prev_composite, file = here::here("2-initial-analysis", "output",
# "plot_effectmodsep_gam.pdf"), height=7,width=7)

#ggsave(plot_prev_composite, file = here::here("2-initial-analysis", "output",
# "plot_effectmodsep_gam.jpg"), height=8,width=5)



###################


plot_gam_tertile <- plot_grid(plot_prev_comp, plot_ratio_comp, plot_diff_comp,
                             ncol = 1, nrow = 3, align = "h")
#plot_gam_tertile <- plot_grid(plot_prev_comp, plot_diff_comp,
                             # ncol = 1)

ggsave(plot_gam_tertile, file = here::here("2-initial-analysis", "output",
                                           "plot_effectmod_sep_gam_tertiles_glm.png"), height=6.5,width=6.5)

ggsave(plot_gam_tertile, file = here::here("2-initial-analysis", "output",
                                               "plot_effectmod_sep_gam_tertiles_glm.pdf"), height=6.5,width=6.5)
