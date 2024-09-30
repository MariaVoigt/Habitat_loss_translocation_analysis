#------------------------------------#
# Script to work on forest info 
# incoming from rescue locations
# STEPS
# 1. prep_and_extract_zonal_GPDS.py
# 2. this script
#
#   ##############
#   # Sumatra #
#   ##############
#
#------------------------------------#
rm(list = ls())
# import packages
library(tidyverse)
library(foreign)

library(foreach)
library(doParallel)
library("RColorBrewer")
library(corrplot)

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
           regrowth_ha = X4 * res2ha)  %>% 
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
# calculate loss in years prior
# consider following years
intervals <- c(seq(1:5))

for (i in 1:nrow(forest_summary)){
  print(i)
  i_id <- forest_summary[i, "id"]
  i_year <- as.numeric(forest_summary[i, "year"])
  for (interval_i in intervals){
    if (i_year-interval_i > 2000) {
      print(interval_i)
      forest_summary[i, paste0("int_forest_year_minus", interval_i)] <- 
        forest_summary[forest_summary$id ==i_id & 
                         forest_summary$year == (i_year - interval_i), 
                       "intact_forest_ha"]
      forest_summary[i, paste0("perc_int_forest_loss_year_minus", interval_i)] <- 
        100-(round(forest_summary[i, "intact_forest_ha"] *
                     100/ forest_summary[i, paste0("int_forest_year_minus", interval_i)] ))
      }else {"nope"}
  }
  }

forest_summary_complete <- na.omit(forest_summary)

head(forest_summary_complete)

# create boolean for rescue

forest_summary_complete$is_rescue_year<- (forest_summary_complete$year == 
                                            forest_summary_complete$rescue_year)

# do a simple model to see if they are correlated
# is year of translocation rleated to forest cover loss in years preceding the translocation

# test correlation between factors
# Compute the correlation matrix
corr_data <- forest_summary_complete[ , c("is_rescue_year",
                             "perc_int_forest_loss_year_minus1",
                             "perc_int_forest_loss_year_minus2",
                             "perc_int_forest_loss_year_minus3",
                             "perc_int_forest_loss_year_minus4", 
                             "perc_int_forest_loss_year_minus5")]
names(corr_data) <- c("rescue", paste0("perc_yr",seq(1:5)))
correlation_matrix <- cor(corr_data)
# years are highly correlated, only 1 and five less than 7 (0.68)



# Create the correlogram
corrplot(correlation_matrix, method = "color", addCoef.col = "black")

# test for distribution yr 1
ggplot(forest_summary_complete,
       aes(x = factor(is_rescue_year), 
           y = perc_int_forest_loss_year_minus1)) +
  geom_boxplot() +
  geom_point()+
  labs(
    y = "Percentage of Intact Forest Loss in Year Minus 1",
    x = "Rescue Year",
    title = "Boxplot of Rescue Year by Percentage of Intact Forest Loss in Year Minus 1"
  )
# and in yr 5
ggplot(forest_summary_complete,
       aes(x = factor(is_rescue_year), 
           y = perc_int_forest_loss_year_minus5)) +
  geom_boxplot() +
  geom_point()+
  labs(
    y = "Percentage of Intact Forest Loss in Year Minus 5",
    x = "Rescue Year",
    title = "Boxplot of Rescue Year by Percentage of Intact Forest Loss in Year Minus 5"
  )

# Perform logistic regression

model_null <- glm(is_rescue_year ~ 1,data = forest_summary_complete,
                  family = "binomial")
summary(model_null)

model_yr1 <- glm(is_rescue_year ~ perc_int_forest_loss_year_minus1 , 
             data = forest_summary_complete, family = "binomial")

summary(model_yr1)

model_yr5 <- glm(is_rescue_year ~ perc_int_forest_loss_year_minus5 , 
              data = forest_summary_complete, family = "binomial")

summary(model_yr5)

model_yr1_5 <- glm(is_rescue_year ~ perc_int_forest_loss_year_minus1 +
                perc_int_forest_loss_year_minus5 , 
              data = forest_summary_complete, family = "binomial")
summary(model_yr1_5 )

model_yr1n5 <- glm(is_rescue_year ~ perc_int_forest_loss_year_minus1 +
                perc_int_forest_loss_year_minus5 +
                perc_int_forest_loss_year_minus1 *
                perc_int_forest_loss_year_minus5 , 
              data = forest_summary_complete, family = "binomial")

summary(model_yr1n5 )

summary(model_yr1)
summary(model_yr5)
summary(model_yr1_5)
summary(model_yr1n5)

#test for overdispersion
# Fit logistic regression model


# test whether full model better than null model (1 year)
dev_full <- deviance(model_yr1)
dev_null <- deviance(model_null)

# Calculate the difference in deviance
dev_diff <- dev_null - dev_full


# Degrees of freedom for the test
df.full <- nrow(forest_summary_complete) - length(coef(model_yr1))
df.null <- nrow(forest_summary_complete) - length(coef(model_null))

df_diff <- df.null - df.full  # df.null is the degrees of freedom of the null model
# df.full is the degrees of freedom of the full model

# Perform the likelihood ratio test
p_value <- pchisq(dev_diff, df_diff, lower.tail = FALSE)

# Output the results
cat("Likelihood ratio test result:\n")
cat("Chi-square statistic:", dev_diff, "\n")
cat("Degrees of freedom:", df_diff, "\n")
cat("p-value:", p_value, "\n")

# Interpret the result based on the p-value
if (p_value < 0.05) {
  cat("The full model is significantly better than the null model.\n")
} else {
  cat("There is no significant difference between the full and null models.\n")
}

# test whether full model better than null model (5 year)
dev_full <- deviance(model_yr5)
dev_null <- deviance(model_null)

# Calculate the difference in deviance
dev_diff <- dev_null - dev_full


# Degrees of freedom for the test
df.full <- nrow(forest_summary_complete) - length(coef(model_yr5))
df.null <- nrow(forest_summary_complete) - length(coef(model_null))

df_diff <- df.null - df.full  # df.null is the degrees of freedom of the null model
# df.full is the degrees of freedom of the full model

# Perform the likelihood ratio test
p_value <- pchisq(dev_diff, df_diff, lower.tail = FALSE)

# Output the results
cat("Likelihood ratio test result:\n")
cat("Chi-square statistic:", dev_diff, "\n")
cat("Degrees of freedom:", df_diff, "\n")
cat("p-value:", p_value, "\n")

# Interpret the result based on the p-value
if (p_value < 0.05) {
  cat("The full model is significantly better than the null model.\n")
} else {
  cat("There is no significant difference between the full and null models.\n")
}



# test whether full model better than null model (1 and 5 year)
dev_full <- deviance(model_yr1_5)
dev_null <- deviance(model_null)

# Calculate the difference in deviance
dev_diff <- dev_null - dev_full


# Degrees of freedom for the test
df.full <- nrow(forest_summary_complete) - length(coef(model_yr1_5))
df.null <- nrow(forest_summary_complete) - length(coef(model_null))

df_diff <- df.null - df.full  # df.null is the degrees of freedom of the null model
# df.full is the degrees of freedom of the full model

# Perform the likelihood ratio test
p_value <- pchisq(dev_diff, df_diff, lower.tail = FALSE)

# Output the results
cat("Likelihood ratio test result:\n")
cat("Chi-square statistic:", dev_diff, "\n")
cat("Degrees of freedom:", df_diff, "\n")
cat("p-value:", p_value, "\n")

# Interpret the result based on the p-value
if (p_value < 0.05) {
  cat("The full model is significantly better than the null model.\n")
} else {
  cat("There is no significant difference between the full and null models.\n")
}



# test whether full model better than null model (1x5 year)
dev_full <- deviance(model_yr1n5)
dev_null <- deviance(model_null)

# Calculate the difference in deviance
dev_diff <- dev_null - dev_full


# Degrees of freedom for the test
df.full <- nrow(forest_summary_complete) - length(coef(model_yr1n5))
df.null <- nrow(forest_summary_complete) - length(coef(model_null))

df_diff <- df.null - df.full  # df.null is the degrees of freedom of the null model
# df.full is the degrees of freedom of the full model

# Perform the likelihood ratio test
p_value <- pchisq(dev_diff, df_diff, lower.tail = FALSE)

# Output the results
cat("Likelihood ratio test result:\n")
cat("Chi-square statistic:", dev_diff, "\n")
cat("Degrees of freedom:", df_diff, "\n")
cat("p-value:", p_value, "\n")

# Interpret the result based on the p-value
if (p_value < 0.05) {
  cat("The full model is significantly better than the null model.\n")
} else {
  cat("There is no significant difference between the full and null models.\n")
}


forest_summary_rescue <- filter(forest_summary, year == rescue_year)
table(forest_summary_rescue$perc_int_forest_loss_year_previous) 
forest_summary_rescue[forest_summary_rescue$perc_int_forest_loss_year_previous>34, ]
paste0("Only ", round(nrow(forest_summary_rescue[forest_summary_rescue$perc_int_forest_loss_year_previous >= 25, ])/nrow(forest_summary_rescue)*100), "% of translocations above 25% loss in year prior to translocation")

paste0("The average (median) loss in the year prior to translocation is ", median(forest_summary_rescue$perc_int_forest_loss_year_previous), "%." )
