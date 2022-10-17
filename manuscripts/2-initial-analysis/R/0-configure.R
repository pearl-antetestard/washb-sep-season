
#-------------------------------------------------------------------------------
# @Organization - Proctor Foundation - UCSF
# @Project - WASHB-SEP-Season
# @Author - Pearl Ante-Testard, pearl.ante@ucsf.edu
# @Description - Configuration
#-------------------------------------------------------------------------------


rm(list=ls(all=TRUE))

##############################
# Section 1 ##################
##############################

# Packages and function

library(tidyverse)
library(here)
library(psych)
library(plyr)
library(Rfast)
library(caret)
library(dplyr)
library(survival)
library(furniture)
library(boot)
library(table1)

here::here()

source(file = here::here("manuscripts", "2-initial-analysis", "R",
                         "pca_wealth_scores_tertiles.R"))

##############################
# Section 2 ##################
##############################

# Read csv files

df_1 <- read_csv(file = here::here("manuscripts", "1-data", "0-untouched",
                                   "washb-bangladesh-enrol-public.csv"))

df_2 <- read_csv(file = here::here("manuscripts", "1-data", "0-untouched",
                                   "washb-bangladesh-tr-public.csv"))

df_3 <- read_csv(file = here::here("manuscripts", "1-data", "0-untouched",
                                   "washb-bangladesh-diar-public.csv"))

##############################
# Section 3 ##################
##############################

# Join dataframes

df <- inner_join(df_1, df_2, by=c("clusterid","block"))
df <- inner_join(df, df_3, by=c("clusterid","block","dataid"))

# Export dataframe as RDS



scipen=999 # removes scientific notation

##############################
# Section 4 ##################
##############################

# Variables needed for the function asset_PCA

## With WASH-related variables 
#varlist = c("landacre","tubewell","storewat","latown","latslab","latseal","roof","walls","floor",
# "elec","asset_radio","asset_refrig","asset_bike","asset_moto",
# "asset_sewmach","asset_tv","asset_wardrobe","asset_table","asset_chair","asset_clock",
#"asset_khat","asset_chouki","asset_mobile") #removed "asset_phone" because not enough variation
###removed asset_tvbw and asset_tvcol, because they're redundant with asset_tv and cement which is the same with floor

## Without WASH-related variables 
varlist = c("landacre","roof","walls","floor",
            "elec","asset_radio","asset_refrig","asset_bike","asset_moto",
            "asset_sewmach","asset_tv","asset_wardrobe","asset_table","asset_chair","asset_clock",
            "asset_khat","asset_chouki","asset_mobile")
varlist<-c("dataid","clusterid","hhid","block", varlist)

varlist_fac = c("roof","walls","floor",
                "elec","asset_radio","asset_refrig","asset_bike","asset_moto",
                "asset_sewmach","asset_tv","asset_wardrobe","asset_table","asset_chair","asset_clock",
                "asset_khat","asset_chouki","asset_mobile")

stat_vars = c("landacre","walls","floor",
              "elec","asset_refrig","asset_bike","asset_moto",
              "asset_sewmach","asset_tv","asset_wardrobe","asset_table","asset_chair","asset_khat","asset_chouki","asset_mobile","wealthscore","wealth_tertile")

