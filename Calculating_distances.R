library(sf)
library(dplyr)
library(ggplot2)

rm(list = ls())

# I have two sets of data, one where we have coordinates and one where we h

#rescue and release data
# note that x and y where swapped in the original
rr_data <- read.csv("/Users/maria/Desktop/Work/Wildlife_Impact/Rescue_location_forest_analysis/data_distance_translocation/rescue_release_coords_edit.csv")
head(rr_data)

missing_coords <- rr_data %>% 
  filter(if_any(c(rescue_y, release_y), ~ is.na(.))) %>% 
  filter(release_name != "Tamiang Protected Forest")
# how much of the data is missing either rescue or release coordinates?

# add in the missing data from the source and the destination centroids

# first fold the desa centroid info in, based on desa name
# import centroids for desas
desa_source_centr <- st_read("~/Desktop/Work/Wildlife_Impact/Rescue_location_forest_analysis/data_distance_translocation/desa_transl_source_centroid.shp",
                             crs = "lat_lon")
# assign info based on lookup

desa_lookup <- read.csv("~/Desktop/Work/Wildlife_Impact/Rescue_location_forest_analysis/data_distance_translocation/desa_lookup.csv")

desa_source_centr <-   desa_source_centr %>% 
  st_set_geometry(NULL) %>%        # Remove the geometry column
  bind_cols(st_coordinates(desa_source_centr)) %>% 
  rename(desa_rescue_x=X, desa_rescue_y=Y) %>% 
  left_join(desa_lookup, by = "desa_id")

# assemble the destination data
# import centroids for PAs
release_PA_centr <- st_read("~/Desktop/Work/Wildlife_Impact/Rescue_location_forest_analysis/data_distance_translocation/PA_transl_dest_centroid.shp",
                            crs = "lat_lon") 

release_PA_centr <- release_PA_centr%>% 
  st_set_geometry(NULL) %>%        # Remove the geometry column
  bind_cols(st_coordinates(release_PA_centr)) %>% 
  rename(PA_release_x=X, 
         PA_release_y=Y,
         release_name= PA) 


rr_data_complete <- rr_data %>% 
  left_join(desa_source_centr, by = c("province", "kabupaten", "kecamatan", "desa") ) %>% 
  mutate(rescue_x= if_else(is.na(rescue_x) & !is.na(desa_rescue_x), desa_rescue_x, rescue_x),
         rescue_y = if_else(is.na(rescue_y) & !is.na(desa_rescue_y), desa_rescue_y, rescue_y),
  # the PA names aren't matching, so I move destination that came with desa info over to 
  # release name
    release_name = if_else(!is.na(destination), destination, release_name)) %>% 
left_join(release_PA_centr, by = "release_name") %>% 
  mutate(release_x= if_else(is.na(release_x) & !is.na(PA_release_x), PA_release_x, release_x),
         release_y = if_else(is.na(release_y) & !is.na(PA_release_y), PA_release_y, release_y)) %>% 
  filter(!is.na(release_y)) %>% 
   dplyr::select( province:kecamatan, desa, 
                rescue_x, rescue_y,
                release_name, 
                release_x, release_y
                ) 


# Step 1: Convert rescue and release coordinates to sf point geometries
df_rescue <- rr_data_complete %>% 
  st_as_sf(coords = c("rescue_x", "rescue_y"), crs = 4326, remove = FALSE) %>% 
  st_transform(crs = "+proj=aea +lat_1=7 +lat_2=25 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") #Reproject to a CRS that uses meters albers equal area

df_release <- rr_data_complete %>% 
  st_as_sf(coords = c("release_x", "release_y"), crs = st_crs(4326) , remove = FALSE) %>% 
  st_transform(crs = "+proj=aea +lat_1=7 +lat_2=25 +lat_0=0 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") #Reproject to a CRS that uses meters albers equal area


# Step 3: Calculate the distance between rescue and release points for each row
df_rescue$distance <- st_distance(df_rescue$geometry, df_release$geometry, by_element = TRUE)
df_final <- df_rescue

convert_to_km <- function(distance_str) {
  round(as.numeric(gsub(" \\[m\\]", "", distance_str)) / 1000,2)
}

distance_summary <- df_final %>% 
  group_by(province) %>% 
  summarize( median_distance = convert_to_km(median(distance)),
             sd_distance = convert_to_km(sd(distance)),
             max_distance = convert_to_km(max(distance)),
            n = n()) %>%
  st_set_geometry(NULL) %>% 
  as.data.frame()

df_final_export <- df_final %>% 
  st_set_geometry(NULL) %>% 
  as.data.frame()
  
#calculate how many missed coordinates
nrow(missing_coords)*100/nrow(df_final)

# write out results
write.csv(df_final_export,"~/Desktop/Work/Wildlife_Impact/Rescue_location_forest_analysis/distance_table.csv", row.names = F)
write.csv(distance_summary,"~/Desktop/Work/Wildlife_Impact/Rescue_location_forest_analysis/distance_summary_provinces.csv", row.names = F )

# get n
numbers <- df_final_export  %>% 
  group_by(province) %>% 
  summarise(n=n())
