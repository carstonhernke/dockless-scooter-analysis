### preprocessing.R ###
### Copyright Carston Hernke ###
### The purpose of this script is to enrich scooter trip data with transporation analysis zones ###

library(readr)
library(sf)
library(dplyr)
library(lubridate)

# Import data on scooter rides from CSV
Motorized_Foot_Scooter_Trips_2018 <- read_csv("~/Google Drive/Google Drive Personal/College/Fall 2018/Thesis/Analysis/data/mpls_scooter_data/Motorized_Foot_Scooter_Trips_2018.csv", 
                                              col_types = cols(EndTime = col_datetime(format = ""), 
                                                               StartTime = col_datetime(format = "")))
scooter_data <-  Motorized_Foot_Scooter_Trips_2018

# fix timezones
scooter_data$StartTime = with_tz(scooter_data$StartTime, tzone = "America/Chicago")
scooter_data$EndTime = with_tz(scooter_data$EndTime, tzone = "America/Chicago")

# import street centerline shapefile data from the City of Minneapolis
mpls_centerlines <- st_read("data/centerlines/Street_Centerline/Street_Centerline.shp")
crs = st_crs(mpls_centerlines)

# import shapefile with transportation analysis zones (TAZ) from Minnesota Geospatial Commons
taz_data <- st_read("data/taz_shapefiles/TAZOfficialWCurrentForecasts.shp")

# adjust the coordinate reference system for the TAZ data to match that of the street centerline data
crs = st_crs(mpls_centerlines)
taz_data_transform <- st_transform(taz_data,crs = crs)

# get the centroid of each street centerline
mpls_centerline_points = st_centroid(mpls_centerlines)

# find which TAZ the centroid of each street centerline falls into
centerlines_w_taz <- st_join(mpls_centerline_points, taz_data_transform, join = st_within, left = TRUE)

# use this data to build a lookup table that maps street centerline ID to TAZ ID
lookup_df <- centerlines_w_taz[,c("FID","GBSID","TAZ")]

# use the lookup table to enrich the scooter data with Start_TAZ and End_TAZ information
enriched_scooters <- left_join(scooter_data, lookup_df, by = c("StartCenterlineID" = "GBSID"))
enriched_scooters <- enriched_scooters[,-c(8,9,11)]
names(enriched_scooters)[names(enriched_scooters) == 'TAZ'] <- 'Start_TAZ'
enriched_scooters <- left_join(enriched_scooters, lookup_df, by = c("EndCenterlineID" = "GBSID"))
enriched_scooters <- enriched_scooters[,-c(9,11)]
names(enriched_scooters)[names(enriched_scooters) == 'TAZ'] <- 'End_TAZ'

# save the enriched data to csv
write.csv(enriched_scooters,"enriched_scooter_trips.csv")