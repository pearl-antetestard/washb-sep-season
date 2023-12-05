

rm(list=ls(all=TRUE))

library(here)
library(cowplot)
library(sp)
library(raster)
library(scales)
library(ggpubr)
library(patchwork)

source(here::here("3-secondary-analysis/R", "0_config.R"))

#####################
##### Read data
####################

# world population in 2014 (from worldpop)
pop_worldpop_bgd <- readRDS(file = here::here("1-data",
                                              "1-temp",
                                              "pop-under3-grid-cell-bgd.rds")) 
st_crs(pop_worldpop_bgd) # WGS 84

# wealth index layer in 2011 (from worldpop)
wealth_worldpop_bgd <- readRDS(file = here::here("1-data",
                                                 "2-final",
                                                 "wealthindex-grid-cell-bgd.rds")) 
st_crs(wealth_worldpop_bgd) # WGS 84

# WASH Benefits Bangladesh clusters
df_gps <- read_dta(file = here::here("1-data", "0-untouched",
                                     "6. WASHB_Baseline_gps.dta"))

df_publicid <- read_csv(file = here::here("1-data", "0-untouched",
                                          "public-ids.csv"))

# individual datasets included in the analysis
df_analysis_svy12 <- readRDS(file = here::here("1-data", "2-final",
                             "enrol_diar_tr_wealth_indiv_svy012.rds")) %>%
                              filter(svy!=0)

#####################
##### Formate and join data
####################


## join worldpop population 2014 and worldpop wealth 2011
wealth_popunder3_gps <- st_join(wealth_worldpop_bgd, pop_worldpop_bgd) %>%
  filter(!is.na(bgd2011wipov.tif))
wealth_popunder3_gps <-wealth_popunder3_gps[!duplicated(wealth_popunder3_gps$bgd_ppp_2014_1km_Aggregated.tif), ]
st_crs(wealth_popunder3_gps) # WGS 84

## join WASHB gps data with public ids
df_gps_publicid <- left_join(df_gps, df_publicid, by = "dataid")
df_gps_publicid <- df_gps_publicid[,-c(1,5,6)]
colnames(df_gps_publicid) <- c("qgpslong","qgpslat","entrydate","clusterid",
                               "dataid","block")

df_gps_publicid$dataid <- as.character(df_gps_publicid$dataid)
df_gps_publicid$clusterid <- as.character(df_gps_publicid$clusterid)
df_analysis_svy12$dataid <- as.character(df_analysis_svy12$dataid)
df_analysis_svy12$clusterid <- as.character(df_analysis_svy12$clusterid)

## join de-identified WASHB gps with WASHB dataset
df_gps_publicid_washb_sepseason <- left_join(df_analysis_svy12, df_gps_publicid, by = c("dataid",
                                                                                         "clusterid",
                                                                                         "block"))
## reformat the WASHB gps to polygons and get the cluster-level mean wealth index score
df_gps_publicid_clust <- df_gps_publicid_washb_sepseason %>%
  #mutate(wealth_rank_washb = rank(wealthscore)/nrow(.),
       #  wealth_rank_washb = round(wealth_rank_washb,2)) %>%
  st_as_sf(coords = c("qgpslong", "qgpslat"), crs = 4326) %>%
  group_by(block) %>% 
  #dplyr::summarise() %>%
  dplyr::summarise(mean_wealth = mean(wealthscore, na.rm=TRUE),
                   #mean_wealthrank = mean(wealth_rank_washb, na.rm=TRUE)
                   ) %>%
  mutate(wealth_rank_washb = rank(mean_wealth)/nrow(.),
         mean_wealthrank = round(wealth_rank_washb,2))  %>%
  st_cast("POLYGON")

## join the WASHB cluster-level gps and wealth score with the whole WASHB dataset 
washbgps_df <- left_join(df_gps_publicid_washb_sepseason, 
                         df_gps_publicid_clust %>% as.data.frame(), by="block") %>%
               dplyr::select(clusterid,wealthscore,mean_wealth,mean_wealthrank,wealth_tertile,geometry) #%>%
               #mutate(wealth_rank_washb = rank(wealthscore)/nrow(.),
               #wealth_rank_washb = round(wealth_rank_washb,2))

## transform dataframe to an sf object and make valid to avoid polygons crossing               
washbgps_df_sf <- st_as_sf(washbgps_df)
washbgps_df_sf <- st_make_valid(washbgps_df_sf)

## create wealth rank variable for the WASHB dataset
wealth_popunder3_gps_sf <- wealth_popunder3_gps %>%
  mutate(wealth_rank_worldpop = rank(bgd2011wipov.tif)/nrow(.),
         wealth_rank_worldpop = round(wealth_rank_worldpop,2))
                        
## join the WASHB dataset with gps with the Worldpop wealth index layer 
## also created the wealth rank variable for the Worldpop dataset
sf_use_s2(FALSE)
wealth_join <- st_join(washbgps_df_sf, wealth_popunder3_gps_sf) %>%
  dplyr::select(clusterid,wealthscore,mean_wealth,mean_wealthrank,wealth_tertile,
         bgd2011wipov.tif,wealth_rank_worldpop,geometry)
wealth_join <-wealth_join[!duplicated(wealth_join$geometry), ] # remove duplicated clusters/gps


#####################
##### Correlation scatter plot
####################

# correlation scatter plot wealth rank 
wealthrank_corr <- ggscatter(wealth_join,
                         x = "mean_wealthrank", y = "wealth_rank_worldpop",
                         add = "loess",                         # Add regression line
                         #conf.int = TRUE,                          # Add confidence interval
                         #col="Gender",
                         repel = F, fullrange = F, size=2,
                         legend = "right", 
                         xlab = "Block-level mean wealth rank (WASHB)",
                         ylab = "Wealth rank at a 1-km resolution per grid-cell (WorldPop)",
                         cor.method = "spearman"
) +
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
           p.accuracy = 0.001,r.accuracy = 0.001,digits=3,
           size = 5, 
           label.x.npc = 0.01, label.y.npc = 0.96 
  ) 

wealthrank_corr

# correlation scatter plot wealth score
wealthscore_corr <- ggscatter(wealth_join,
          x = "mean_wealth", y = "bgd2011wipov.tif",
          add = "loess",                         # Add regression line
          #conf.int = TRUE,                          # Add confidence interval
          #col="Gender",
          repel = F, fullrange = F, size=2,
          legend = "right", 
          xlab = "Block-level mean wealth score (WASHB)",
          ylab = "Wealth score at a 1-km resolution per grid-cell (WorldPop)",
          cor.method = "spearman"
) +
  stat_cor(aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
           p.accuracy = 0.001,r.accuracy = 0.001,digits=3,
           size = 5, 
           label.x.npc = 0.01, label.y.npc = 0.96
  ) 

wealthscore_corr


plot_wealthcorr_comp <- plot_grid(wealthrank_corr, wealthscore_corr, ncol = 2)

plot_wealthcorr_comp

#cor.test(wealth_join$mean_wealth, wealth_join$bgd2011wipov.tif, method = "spearman")

#####################
##### Save plot
####################

ggsave(wealthrank_corr, file = here::here("4-supplementary", "output",
                                       "wealth_rank_corr_block_2.png"), height=8,width=9)

ggsave(wealthscore_corr, file = here::here("4-supplementary", "output",
                                      "wealth_score_corr_block.png"), height=8,width=9)

ggsave(plot_wealthcorr_comp, file = here::here("4-supplementary", "output",
                                               "wealth_rankscore_corr_block.png"), height=6,width=10)
