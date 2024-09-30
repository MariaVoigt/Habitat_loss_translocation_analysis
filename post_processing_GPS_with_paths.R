#------------------------------------#
# Script accompanying Sherman and Voigt et al
#
# STEPS
# 1. extract 5 km buffer around locations -> files supplied
# 2. this script
#
#------------------------------------#
rm(list = ls())
# import packages
library(tidyverse)
library(foreign)
library(rgdal)
library(foreach)
library(doParallel)
library("RColorBrewer")

options(scipen = 100, digits = 4)


# function to calculate annual change
# calculates absolute percent, if including direction, remove the - before 100
calc.annual.change <- function(value_start, value_end, year_start, year_end, round_to = 2){
  result <- (((value_end / value_start )^(1/(year_end - year_start))) - 1) * -100
  return(round(result, round_to))
}


res2ha = 30*30 * 0.0001
# get all the yearly csv 
# extract 1-intact forest and 2-degraded forest

forest_stat_path_Sumatra <- "/Users/maria/Desktop/Work/Wildlife_Impact/Rescue_location_forest_analysis/Sumatra"

forest_stat_list_sumatra <- list.files(forest_stat_path_Sumatra, full.names = T) # , pattern = "gps_forest_stats_*")

forest_stat_path_Kalimantan<- "/Users/maria/Desktop/Work/Wildlife_Impact/Rescue_location_forest_analysis/Kalimantan"

forest_stat_list_kalimantan <- list.files(forest_stat_path_Kalimantan, full.names = T) 

forest_stat_list <- c(forest_stat_list_sumatra, forest_stat_list_kalimantan)

#forest_further = forest_stat_list[1]
# in this data there are instances in which at the same location multiple animals are rescued
# we thus create ids to tell them apart instead of working with latlong as id
# as all forest stats are in the same order, it should be ok 

forest_summary_sumatra <- foreach(forest_stat_file = forest_stat_list_sumatra, .combine = rbind)%do%{
  file_year <- substr(forest_stat_file, nchar(forest_stat_file)-7, nchar(forest_stat_file)-4)
  forest_stat_year <-  read.csv(forest_stat_file, stringsAsFactors = F) %>% 
    mutate(id = 1:n(),
           year = file_year,
           rescue_year = Rescue_dat,
           long = GPS_resc_1,
           lat = GPS_rescue,  
           lat_long = paste0(lat, "_", long),
           sex = Sex,
           intact_forest_ha = X1.0 * res2ha, 
           degraded_forest_ha = X2.0 * res2ha,
           deforested_ha= X3.0 * res2ha,
           regrowth_ha = X4.0 * res2ha) %>% 
    # rename 
    filter(year != 2021) %>% 
    dplyr::select( 
      id,
      year, 
      rescue_year,
      long, 
      lat, 
      lat_long,
      sex,
      intact_forest_ha, 
      degraded_forest_ha,
      deforested_ha,
      regrowth_ha, 
    ) 
}

# CONTINUE here adapting the table
summary_kalimantan <- foreach(forest_stat_file = forest_stat_list_kalimantan, .combine = rbind)%do%{
  file_year <- substr(forest_stat_file, nchar(forest_stat_file)-7, nchar(forest_stat_file)-4)
  forest_stat_year <-  read.csv(forest_stat_file, stringsAsFactors = F) %>% 
    mutate(id = 1:n(),
           year = file_year,
           rescue_year = rescue_yea,
           long = rescue_gps,
           lat = rescue_g_1,  
           lat_long = paste0(lat, "_", long),
           sex = sex,
           intact_forest_ha = X1 * res2ha, 
           degraded_forest_ha = X2 * res2ha,
           deforested_ha= X3 * res2ha,
           regrowth_ha = X4 * res2ha) %>% 
    # rename 
    filter(year != 2021) %>% 
    dplyr::select( 
      id,
      year, 
      rescue_year,
      long, 
      lat, 
      lat_long,
      sex,
      intact_forest_ha, 
      degraded_forest_ha,
      deforested_ha,
      regrowth_ha, 
    ) 
}

summary_kalimantan$id <- summary_kalimantan$id+max(forest_summary_sumatra$id)

forest_summary <- rbind(summary_kalimantan, forest_summary_sumatra)
unique(forest_summary$year)
forest_summary <- filter(forest_summary, year >= 2000)

# replacing Male with male and Female with female

forest_summary[forest_summary$sex == "Male", "sex"] <- "male"
forest_summary[forest_summary$sex == "Female", "sex"] <- "female"
forest_summary$id <- as.factor(forest_summary$id)

# calculate the loss of forest overall
# to separate curves

# this is the percentage forest left at rescue from 2000
forest_perc_baseline <- foreach(id = unique(forest_summary$id), .combine = rbind)%do%{
  point_forest <-  forest_summary[forest_summary$id==id, ]
  baseline2_int_forest <-  point_forest[point_forest$year == 2000, "intact_forest_ha"]
  
  point_forest <- point_forest %>% 
    filter(year == rescue_year) %>% 
    mutate(
           perc_int_forest_at_rescue_from_bl2 = round(intact_forest_ha *100/baseline2_int_forest),
    )} %>% 
  dplyr::select(id, perc_int_forest_at_rescue_from_bl2
  ) 



forest_summary <- forest_summary %>% 
  left_join(forest_perc_baseline, by = "id")



#CONTINUE HERE AND USE THIS TO SEPARATE BY QUARTILES 
# AND THEN USE LABEL AND PLOT BASED ON THIS
# 
forest_transl_for_plotting <-forest_summary  %>%
  filter(year == rescue_year)



# make this relative to the initial forest cover
# this is where the plot starts from


# two baselines 1990 and 2000
forest_perc_plotting <- foreach(id = unique(forest_summary$id), .combine = rbind)%do%{
  point_forest <-  forest_summary[forest_summary$id==id, ]
  baseline2_int_forest <-  point_forest[point_forest$year == 2000, "intact_forest_ha"]
  
  point_forest <- point_forest %>% 
    mutate(
           perc_int_forest_from_baseline2 = intact_forest_ha *100/baseline2_int_forest)
}


forest_transl_perc_plotting <- forest_perc_plotting %>% 
  # only retain the rows of the transl
  filter(year == rescue_year)

forest_perc_plotting$year <- as.numeric(forest_perc_plotting$year)
forest_transl_perc_plotting$year <- as.numeric(forest_transl_perc_plotting$year)



length(unique(forest_perc_plotting$id))


average_forest_per_year <- forest_perc_plotting %>% 
  group_by(year) %>% 
  summarise(median_perc_int_forest_from_baseline2 = median(perc_int_forest_from_baseline2),
            lCI_perc_int_forest_from_baseline2 = quantile(perc_int_forest_from_baseline2, 0.025, na.rm =T),
            uCI_perc_int_forest_from_baseline2 = quantile(perc_int_forest_from_baseline2, probs = 0.975, na.rm =T)
           )

p2b <- ggplot()+
  geom_rect(aes(xmin = 2012, xmax = 2020,   ymin = 0, ymax = 100),   fill = "grey") +
  geom_point(data =forest_perc_plotting  ,
             aes(x = year, y = perc_int_forest_from_baseline2, group = id))+
  geom_line(data = forest_perc_plotting,
            aes(x = year, y = perc_int_forest_from_baseline2, group= id))+
  # geom_ribbon(data = average_forest_per_year,
  #             aes(ymin = lCI_perc_int_forest_from_baseline2, ymax = uCI_perc_int_forest_from_baseline2,
  #             x = year), fill= "blue", alpha = 0.2)+
  geom_line(data = average_forest_per_year,
            aes(x = year, y = median_perc_int_forest_from_baseline2, group= id), 
            size = 1.5, color = "blue")+
  geom_point(data = average_forest_per_year,
            aes(x = year, y = median_perc_int_forest_from_baseline2, group= id),
           color = "blue", size = 1.5)+
  geom_point(data =forest_transl_perc_plotting,
             aes(x = year, y =  perc_int_forest_from_baseline2, group = id), color = "red")+

  ylab("intact forest cover (% from 2000)")+
  ylim(0,100)+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2012, 2015, 2020), 
                   labels = c(2000, 2005, 2010, 2012, 2015, 2020), 
                   limits = c(2000, 2020))+
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x=element_text(size = 10, angle = 90, hjust=0.5,vjust=0.2),
    # axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title=element_text(size = 18, face = "bold"),
    #legend.title=element_text(size=18, face = "bold") ,
    legend.title=element_blank(),
    legend.text=element_text(size=14),
    legend.key.height=unit(0.8,"cm"),
    strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
    strip.text = element_text(size = 12, face="bold")) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1),"cm"))
print(p2b)


ggsave("/Users/maria/Dropbox (Personal)/USFW_stuff/translocation_counterfactual/plots/all_rescues_intact_percent_2000baseline.jpg", 
       plot = p2b, device = "jpeg", width = 210, units = "mm", dpi = 1000)


ggsave("/Users/maria/Dropbox (Personal)/USFW_stuff/translocation_counterfactual/plots/all_rescues_intact_percent_2000baseline.pdf", 
       plot = p2b, device = "pdf", width = 185, height = 114, units = "mm", dpi = 1000)

ggsave("/Users/maria/Dropbox (Personal)/USFW_stuff/translocation_counterfactual/plots/all_rescues_intact_percent_2000baseline_slim.pdf", 
       plot = p2b, device = "pdf", width = 185, height = 97, units = "mm", dpi = 1000)

# To do

#include an exemplary circle with landcover in beginning and end
# this is the median location
forest_perc_plotting[forest_perc_plotting$perc_int_forest_from_baseline2 > 22.8 & forest_perc_plotting$perc_int_forest_from_baseline2 < 23 & forest_perc_plotting$year == 2020, ]
median_value <-forest_perc_plotting[forest_perc_plotting$year >= 2000 & forest_perc_plotting$lat_long == "3.72638856_98.1905531", ]

#in first year obv all have 100 %