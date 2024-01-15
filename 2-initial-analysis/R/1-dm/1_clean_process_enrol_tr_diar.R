
#-------------------------------------------------------------------------------
# @Organization - Proctor Foundation - UCSF
# @Project - WASHB-SEP-Season
# @Author - Pearl Ante-Testard, pearl.ante@ucsf.edu
# @Description - Data management and join dataframes
#-------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

##############################
# Section 1 ##################
##############################

library(here)
here::here()
source(here::here("2-initial-analysis/R", "0-config.R"))

# Read csv files

df_enrol <- read_csv(file = here::here("1-data", "0-untouched",
                                   "washb-bangladesh-enrol-public.csv"))

df_tr <- read_csv(file = here::here("1-data", "0-untouched",
                                   "washb-bangladesh-tr-public.csv"))

df_diar <- read_csv(file = here::here("1-data", "0-untouched",
                                   "washb-bangladesh-diar-public.csv"))


##############################
# Section 2 ##################
##############################

# Join dataframes

df_enrol_tr <- left_join(df_enrol, df_tr, by=c("clusterid","block"))
df_enrol_tr_diar <- left_join(df_enrol_tr, df_diar, by=c("clusterid","block","dataid"))


##############################
# Section 3 ##################
##############################

# Process joint dataframe

# Exclude:
## siblings who were >36 mos at enrollment
## children with missing outcome data (diar7d)
## W, S, H and N single arms

ad <- (df_enrol_tr_diar %>% 
         filter(!is.na(diar7d)) %>% # remove NAs
         filter(gt36mos==0) %>% # siblings who were >36 mos at enrollment
         #filter(svy!=0) %>%
         filter(tr!="Nutrition") %>% # remove W, S, H and N single arms
         filter(tr!="Water") %>%
         filter(tr!="Sanitation") %>%
         filter(tr!="Handwashing"))


##############################
# Section 4 ##################
##############################

# Recoding variables

## rainy dates are based on the findings of Nguyen et al.
## rainy seasons (elevated precipitation): May 27 – September 27 in 2014 and April 1 – September 26 in 2015
df_recoded <- ad %>%
  mutate(tr_comb = ifelse(tr == "Nutrition + WSH" |
                            tr ==  "WSH", 1,
                          ifelse(tr == "Control", 0, NA)),
         tr_comb = as.factor(tr_comb),
         monsoon = ifelse(svy == 1 &
                            svyweek.x >= 22 & # http://www.week-number.net/calendar-with-week-numbers-2015.html
                            svyweek.x <= 39, 1,
                          ifelse(svy == 2 &
                                   svyweek.y >= 14 &
                                   svyweek.y <= 39, 1, 0)),
          monsoon = as.factor(monsoon))

table(df_recoded$tr_comb, useNA = "always")
table(df_recoded$monsoon, useNA = "always")


# Export dataframe as RDS

saveRDS(df_recoded, here::here("1-data", "2-final",
                      "enrol_diar_tr_surv012_formatted.rds"))
