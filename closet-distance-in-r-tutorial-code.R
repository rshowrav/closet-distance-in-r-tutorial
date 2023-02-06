# Installing necessary packages

install.packages(c("dplyr","tidygeocoder", "rgeos"))

# Loading packages

library(dplyr)
library(tidygeocoder)
library(rgeos)

# Loading Datasets

rand_address = read.csv("Random Address.csv")
noaa_address = read.csv("NOAA Weather Locations.csv")

# Creating dataframe with lat long data for NOAA Weather Stations

noaa_coordinates = noaa_address %>%
  geocode(Office.Address)

# Creating dataframe with lat long data for Terminals

rand_coordinates = rand_address %>%
  geocode(ï..Random.Address)

# Removing NA records

noaa_coordinates = na.omit(noaa_coordinates)
rand_coordinates = na.omit(rand_coordinates)

# Limiting to only lat long data

noaa_lat_long = cbind(noaa_coordinates$long,noaa_coordinates$lat)
rand_lat_long = cbind(rand_coordinates$long,rand_coordinates$lat)

# Creating distance formula

distp1p2 <- function(p1,p2) {
  dst <- sqrt((p1[1]-p2[1])^2+(p1[2]-p2[2])^2)
  return(dst)
}


# Returning lat long based on closet distance

dist2b <- function(y) which.min(apply(noaa_lat_long, 1, function(x) min(distp1p2(x,y))))
apply(rand_lat_long, 1, dist2b)

# Creating an expected noaa weather station based on results

closet = noaa_lat_long[apply(rand_lat_long, 1, dist2b),]
df_closet = data.frame(closet)
df_closet$long = df_closet$X1
df_closet$lat = df_closet$X2
df_closet$expected_noaa_station = df_closet %>% left_join( noaa_coordinates, 
                                                         by=c('long'='long', 
                                                              'lat'='lat'))


df_close_noaa = cbind(df_closet$expected_noaa_station$Office.call.sign,df_closet$expected_noaa_station$Office.Address,df_closet$expected_noaa_station$lat,df_closet$expected_noaa_station$long) 
df_close_noaa = data.frame(df_close_noaa)
colnames(df_close_noaa)=c("noaa_station_office_call", "noaa_station_address", "noaa_station_lat","noaa_station_long")

# Creating final dataset with closet market terminal combo

df_final = cbind(rand_coordinates,df_close_noaa)

