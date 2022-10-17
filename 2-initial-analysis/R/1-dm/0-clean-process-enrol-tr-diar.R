
#-------------------------------------------------------------------------------
# @Organization - Proctor Foundation - UCSF
# @Project - WASHB-SEP-Season
# @Author - Pearl Ante-Testard, pearl.ante@ucsf.edu
# @Description - Data management
#-------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

##############################
# Section 1 ##################
##############################

library(here)
here::here()
source(here::here("2-initial-analysis/R", "0-config.R"))

# Read csv files

df_1 <- read_csv(file = here::here("1-data", "0-untouched",
                                   "washb-bangladesh-enrol-public.csv"))

df_2 <- read_csv(file = here::here("1-data", "0-untouched",
                                   "washb-bangladesh-tr-public.csv"))

df_3 <- read_csv(file = here::here("1-data", "0-untouched",
                                   "washb-bangladesh-diar-public.csv"))


##############################
# Section 2 ##################
##############################

# Join dataframes

df <- inner_join(df_1, df_2, by=c("clusterid","block"))
df <- inner_join(df, df_3, by=c("clusterid","block","dataid"))


##############################
# Section 3 ##################
##############################

# Process joint dataframe

# Exclude:
## siblings who were >36 mos at enrollment
## children with missing outcome data (diar7d)
## W, S, H and N single arms

ad <- (df %>% 
         filter(!is.na(diar7d)) %>%
         filter(gt36mos==0) %>%
         filter(svy!=0) %>%
         filter(tr!="Nutrition") %>%
         filter(tr!="Water") %>%
         filter(tr!="Sanitation") %>%
         filter(tr!="Handwashing"))

# Export dataframe as RDS

saveRDS(ad, here::here("1-data", "2-final",
                      "enrol_diar_tr_formatted.rds"))
