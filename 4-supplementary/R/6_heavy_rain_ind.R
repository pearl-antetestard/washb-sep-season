
#### Creating the heavy rain indicator

rm(list=ls())

library(data.table)
library(dplyr)
library(plyr)
library(haven)
library(readr)

# WASH Benefits Bangladesh clusters
df_gps <- read_dta(here::here("1-data","0-untouched","6. WASHB_Baseline_gps.dta"))
df_publicid <- read_csv(here::here("1-data","0-untouched","public-ids.csv"))

# individual datasets included in the analysis
df_analysis_svy12 <- readRDS(here::here("1-data","2-final","enrol_diar_tr_wealth_indiv_svy012.rds")) %>%
  filter(svy!=0)

df_gps_publicid <- left_join(df_publicid, df_gps, by = "dataid")  %>%
  mutate("date" = as.character(as.Date(entrydate,"%d%b%Y"))) 

df_gps_publicid <- df_gps_publicid[,-c(1,2,3,9)]
colnames(df_gps_publicid) <- c("clusterid","dataid","block","qgpslong","qgpslat","date")

df_analysis_svy12_gps <- left_join(df_analysis_svy12, df_gps_publicid, by=c("dataid","clusterid","block"))

options(scipen = 100, digits = 4)
daily_ppts <- readRDS(here::here("1-data","2-final","all-ppt-data.RDS")) 
daily_ppts <- daily_ppts[ grep("2016", daily_ppts$date, invert = TRUE) , ]
daily_ppts <- daily_ppts[ grep("2015", daily_ppts$date, invert = TRUE) , ]
daily_ppts <- daily_ppts[ grep("2014", daily_ppts$date, invert = TRUE) , ]
daily_ppts <- daily_ppts[ grep("2011", daily_ppts$date, invert = TRUE) , ] %>% 
  mutate("date" = as.character(as.Date(date,"%d%b%Y"))) 

average_daily_ppts = daily_ppts %>% 
  #filter(date >= min(df_gps_publicid$date), 
        # ppt > 0) %>% 
  group_by(date) %>% 
  dplyr::summarise(max_ppt = max(ppt))

df_gps_publicid_washb_sepseason <- left_join(df_gps_publicid, average_daily_ppts, by = "date")

date <- data.table(df_gps_publicid_washb_sepseason)

dt1 <- copy(date)
dt1[, laggedDate:=date]
setkey(dt1, "date", "laggedDate")

dt2 <- copy(date)
dt2[, laggedDate_start:=as.Date(date)-7]
setkey(dt2, "date", "laggedDate_start")
dt2[, laggedDate_end:=as.Date(date)-0]
setkey(dt2, "date", "laggedDate_end")

heavy_rain_cutoff = as.numeric(quantile(df_gps_publicid_washb_sepseason$max_ppt, 0.8))


dt3 <- dt2 %>% #filter(!row_number() %in% c(1609,1610,1611,1612)) %>%
  add_row(date = " ", max_ppt=0, laggedDate_start = NA, laggedDate_end = NA) %>%
  mutate(dummy=rep(1:694, each = 8)) 
  
date_heavy_rain = dt3 %>% 
  arrange(date) %>%
  #filter(between(date,laggedDate_start,laggedDate_end)) %>%
  group_by(dummy) %>% 
  dplyr::summarize(sums = sum(max_ppt)) %>% 
  mutate(heavy_rain_ind = sums >= heavy_rain_cutoff) %>%
  as.data.frame()
  
date_heavy_rain_ind <- left_join(dt3,date_heavy_rain,by="dummy")
date_heavy_rain_ind <- date_heavy_rain_ind %>% filter(!row_number() %in% c(5552)) 


df_washb_gps_average_daily_ppts <- left_join(df_analysis_svy12_gps,date_heavy_rain_ind,
                                              by = c("dataid","clusterid","block","qgpslong", "qgpslat","date"))

saveRDS(df_washb_gps_average_daily_ppts, here::here("1-data","2-final", "df-washb-gps-heavy-rain-ind.RDS"))
