
rm(list=ls(all=TRUE))

library(anytime)
library(here)
library(gghighlight)
library(pacman)
#remove.packages("lubridate")
#pacman::p_load(lubridate)
library(lubridate)
source(here::here("2-initial-analysis/R", "0-config.R"))

aet = readRDS(paste0(here::here(),"/1-data/2-final/washb-bgd-aet.RDS"))
ppt = readRDS(paste0(here::here(),"/1-data/2-final/washb-bgd-ppt.RDS"))
soil = readRDS(paste0(here::here(),"/1-data/2-final/washb-bgd-soil.RDS"))
tmax = readRDS(paste0(here::here(),"/1-data/2-final/washb-bgd-tmax.RDS"))
tmin = readRDS(paste0(here::here(),"/1-data/2-final/washb-bgd-tmin.RDS"))


table(aet$year, aet$month)
table(ppt$year, ppt$month)
table(soil$year, soil$month)
table(tmax$year, tmax$month)
table(tmin$year, tmin$month)


# monsoon: May 27 – September 27, 2014, and April 1 – September 26, 2015
# survey enrolment: 2012-05-31 to 2013-07-07

aet_mean <- aet %>%    
  group_by(year, month) %>%   
  summarise_at(vars(aet),                  
               list(length = length, mean = mean, sd = sd)) 
aet_mean


p <- ggplot(aet_mean, aes(x=month, y=mean)) +
  geom_line() + 
  facet_wrap(~year, scales = "free") +
  xlab("")
p


ppt_mean <- ppt %>%   
  group_by(year, month) %>%   
  summarise_at(vars(ppt),                  
               list(length = length, mean = mean, sd = sd)) 
ppt_mean

ppt_plot <- ggplot(ppt_mean, aes(x=month, y=mean)) +
  geom_line() + 
  facet_wrap(~year, scales = "free") +
  xlab("")
ppt_plot


soil_mean <- soil %>%   
  group_by(year, month) %>%   
  summarise_at(vars(soil),                  
               list(length = length, mean = mean, sd = sd)) 
soil_mean

soil_plot <- ggplot(soil_mean, aes(x=month, y=mean)) +
  geom_line() + 
  facet_wrap(~year, scales = "free") +
  xlab("")
soil_plot

tmax_mean <- tmax %>%  
  group_by(year, month) %>%   
  summarise_at(vars(tmax),                  
               list(length = length, mean = mean, sd = sd)) 
tmax_mean

tmax_plot <- ggplot(tmax_mean, aes(x=month, y=mean)) +
  geom_line() + 
  facet_wrap(~year, scales = "free") +
  xlab("")
tmax_plot

tmin_mean <- tmin %>% 
  group_by(year, month) %>%   
  summarise_at(vars(tmin),                  
               list(length = length, mean = mean, sd = sd)) 
tmin_mean

tmin_plot <- ggplot(tmin_mean, aes(x=month, y=mean)) +
  geom_line() + 
  facet_wrap(~year, scales = "free") +
  xlab("")
tmin_plot

############

aet_mean$terraclim_var <- "aet"
aet_mean <- aet_mean %>%
  dplyr::mutate(date = as.Date(anytime::anydate(paste0(year, "-", month))),
                monsoon = ifelse(year == 2014 &
                                   month >= 5 & month <= 8, 1,
                          ifelse(year == 2015 &
                                   month >= 4 & month <= 8 ,1,0)),
                survey = ifelse(year == 2012 &
                                  month >= 5, 1,
                         ifelse( year == 2013 &
                                  month <= 6, 1, 0)))

ppt_mean$terraclim_var <- "Precipitation (mm)"
ppt_mean <- ppt_mean %>%
  dplyr::mutate(date = as.Date(anytime::anydate(paste0(year, "-", month))),
                monsoon = ifelse(year == 2014 &
                                   month >= 5 & month <= 8, 1,
                                 ifelse(year == 2015 &
                                          month >= 4 & month <= 8 ,1,0)),
                survey = ifelse(year == 2012 &
                                  month >= 5, 1,
                                ifelse( year == 2013 &
                                          month <= 6, 1, 0)))

soil_mean$terraclim_var <- "Soil Moisture (mm)"
soil_mean <- soil_mean %>%
  dplyr::mutate(date = as.Date(anytime::anydate(paste0(year, "-", month))),
                monsoon = ifelse(year == 2014 &
                                   month >= 5 & month <= 8, 1,
                                 ifelse(year == 2015 &
                                          month >= 4 & month <= 8 ,1,0)),
                survey = ifelse(year == 2012 &
                                  month >= 5, 1,
                                ifelse( year == 2013 &
                                          month <= 6, 1, 0)))

tmax_mean$terraclim_var <- "Maximum Temperature (°C)"
tmax_mean <- tmax_mean %>%
  dplyr::mutate(date = as.Date(anytime::anydate(paste0(year, "-", month))),
                monsoon = ifelse(year == 2014 &
                                   month >= 5 & month <= 8, 1,
                                 ifelse(year == 2015 &
                                          month >= 4 & month <= 8 ,1,0)),
                survey = ifelse(year == 2012 &
                                  month >= 5, 1,
                                ifelse( year == 2013 &
                                          month <= 6, 1, 0)))

tmin_mean$terraclim_var <- "tmin"
tmin_mean <- tmin_mean %>%
  dplyr::mutate(date = as.Date(anytime::anydate(paste0(year, "-", month))),
                monsoon = ifelse(year == 2014 &
                                   month >= 5 & month <= 8, 1,
                                 ifelse(year == 2015 &
                                          month >= 4 & month <= 8 ,1,0)),
                survey = ifelse(year == 2012 &
                                  month >= 5, 1,
                                ifelse( year == 2013 &
                                          month <= 6, 1, 0)))

terraclim_mean <- rbind(#aet_mean,
                        ppt_mean,soil_mean,tmax_mean#, tmin_mean
                        )

terraclim_mean_2014_2015 <- terraclim_mean %>%
                  mutate(terraclim_var=factor(terraclim_var, levels=c("Precipitation (mm)",
                                                                      "Maximum Temperature (°C)",
                                                                      "Soil Moisture (mm)"))) %>%
                  filter(year>2013) 

#terraclim_mean_2014_2015<-terraclim_mean_2014_2015[-72,]

terraclim_plot_1 <- ggplot(terraclim_mean_2014_2015, aes(x=date)) +
  geom_point(aes(y = mean#,  color = monsoon
                 )) +
  geom_line(aes(y = mean#, color = monsoon
                )) +
  scale_x_date(date_breaks="2 month", date_labels="%m-%Y", limits = c(as.Date("2014-01-01"), as.Date("2015-11-01")))+
  #geom_ribbon(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd,alpha=0.1))+
  #gghighlight(date >= 2014,
             # unhighlighted_params = list(linewidth = 1, colour = alpha("pink", 0.4))) +
  facet_wrap(~terraclim_var,scales = "free", nrow = 3) +
  geom_rect(aes(xmin=rep(ymd(c('2014-05-01', '2015-04-01')), 36),
                xmax =rep(ymd(c('2014-09-01', '2015-09-01')), 36),
                ymin = -Inf,
                ymax = Inf),  alpha = 0.01, fill="darkgrey") +
  xlab("Date") +
  ylab("Monthly mean") +
  #coord_flip() +
  theme_minimal() +
  theme(legend.position="none",
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
terraclim_plot_1

ggsave(terraclim_plot_1, file = here::here("4-supplementary", 
                                         "output",
                                         "terraclim-plot-5.png"), height=8,width=9)

terraclim_mean_2012_2013 <- terraclim_mean %>%
  filter(year<2014)

terraclim_plot_2 <- ggplot(terraclim_mean_2012_2013, aes(x=date)) + 
  geom_line(aes(y = mean#, color = survey
                )) +
  #scale_color_gradient(low = "black", high = "green", na.value = NA) +
  scale_x_date(date_breaks="2 month", date_labels="%m-%Y")+
  #geom_ribbon(aes(ymin=mean-1.96*sd, ymax=mean+1.96*sd,alpha=0.1))+
  #gghighlight(date >= 2014,
  # unhighlighted_params = list(linewidth = 1, colour = alpha("pink", 0.4))) +
  facet_wrap(~terraclim_var,scales = "free", nrow = 3) +
  geom_rect(aes(xmin=rep(ymd('2012-05-01'), 72),
                xmax =rep(ymd('2013-07-01'), 72),
                ymin = -Inf,
                ymax = Inf),  alpha = 0.01, fill="darkgrey") +
  xlab("Date") +
  ylab("Mean") +
  #coord_flip() +
  theme_minimal() +
  theme(legend.position="none",
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
terraclim_plot_2

ggsave(terraclim_plot_2, file = here::here("4-supplementary", 
                                           "output",
                                           "terraclim-surveydate-plot.png"), height=8,width=9)
