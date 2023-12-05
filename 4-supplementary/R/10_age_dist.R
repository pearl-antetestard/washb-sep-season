
rm(list=ls(all=TRUE))
#renv::install("readstata13")

tab_hh = readstata13::read.dta13(here::here("1-data", "0-dhs",
                      "BDPR72DT", "BDPR72FL.DTA")) 

hist(tab_hh$hv105)
         
tab_hh_14 = tab_hh %>%
          dplyr::filter(!is.na(hv105)) %>%
          dplyr::filter(hv105<15) %>%
          dplyr::select(hhid,hvidx,hv001,hv002,hv105,hc1) 

table(tab_hh_14$hv105)

plot_discrete <- tab_hh_14 %>%
  ggplot( aes(x=hv105)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Children's age distribution between 0 and 14 years") +
  labs(x = "Age of children in years",
       y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=14)
  )
plot_discrete

ggsave(plot_discrete, file = here::here("4-supplementary", "output",
                                        "plot_age_discrete_dist.png"), height=7,width=8)

#################################
#################################

tab_hh_14$age_break <- cut(tab_hh_14$hv105, breaks=c(0, 3, 7, 11, 14), include.lowest=TRUE)
table(tab_hh_14$age_break)

plot_breaks <- tab_hh_14 %>%
  #drop_na(age_break) %>%
  ggplot( aes(x=age_break)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9, stat="count") +
  ggtitle("Children's age distribution per age group") +
  labs(x = "Age group",
       y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=12)
  )

plot_breaks

table(tab_hh_14$age_break, tab_hh_14$hv105)

#           0    1    2    3    4    5    6    7    8    9   10   11   12   13   14
#[0,3]   1698 1664 1670 1688    0    0    0    0    0    0    0    0    0    0    0
#(3,7]      0    0    0    0 1651 1568 1812 2007    0    0    0    0    0    0    0
#(7,11]     0    0    0    0    0    0    0    0 1898 1691 2175 1802    0    0    0
#(11,14]    0    0    0    0    0    0    0    0    0    0    0    0 2031 1808 1855

#(1698+1664+1670)/(1698+1664+1670+1688+1651+ 1568 +1812 +2007+1898+ 1691 +2175+ 1802+2031 +1808 +1855)
# 0.1862462

plot_composite <- plot_grid(plot_cont, plot_breaks, ncol=2, align = "h", labels = c("A", "B")) 

plot_composite

ggsave(plot_composite, file = here::here("4-supplementary", "output",
                                              "plot_age_dist_inc15.png"), height=7,width=9)
