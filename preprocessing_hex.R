# Preprocesses the data using a hexagonal grid rather than TAZ

### preprocessing.R ###
### Copyright Carston Hernke ###
### The purpose of this script is to enrich scooter trip data with transporation analysis zones ###

library(readr)
library(sf)
library(dplyr)
library(lubridate)
library(ggplot2)

# Import data on scooter rides from CSV
Motorized_Foot_Scooter_Trips_2018 <- read_csv("data/mpls_scooter_data/Motorized_Foot_Scooter_Trips_2018.csv", 
                                              col_types = cols(EndTime = col_datetime(format = ""), 
                                                               StartTime = col_datetime(format = "")))
scooter_data <-  Motorized_Foot_Scooter_Trips_2018

# fix timezones
scooter_data$StartTime = with_tz(scooter_data$StartTime, tzone = "America/Chicago")
scooter_data$EndTime = with_tz(scooter_data$EndTime, tzone = "America/Chicago")

# import street centerline shapefile data from the City of Minneapolis
mpls_centerlines <- st_read("data/centerlines/Street_Centerline/Street_Centerline.shp")
crs = st_crs(mpls_centerlines)

# import Minneapolis boundary
mpls_boundary <- st_read("data/minneapolis_boundary/msvcGIS_MinneapolisCityLimits.shp")

# build a hexagonal grid within the bounding box of the Minneapolis boundary
# https://r-spatial.github.io/sf/reference/st_make_grid.html
hex_grid <- st_sf(st_make_grid(mpls_boundary, cellsize = .005, square = FALSE)) %>%
  mutate(HEX_ID = seq.int(n())) %>%
  rename(geometry = st_make_grid.mpls_boundary..cellsize...0.005..square...FALSE.)


# adjust the coordinate reference system for the TAZ data to match that of the street centerline data
#crs = st_crs(mpls_centerlines)
#taz_data_transform <- st_transform(taz_data,crs = crs)

# get the centroid of each street centerline
mpls_centerline_points = st_centroid(mpls_centerlines)

# find which TAZ the centroid of each street centerline falls into
# use this data to build a lookup table that maps street centerline ID to TAZ ID
lookup_df <- st_join(mpls_centerline_points, hex_grid, join = st_within, left = TRUE) %>%
  select("GBSID","HEX_ID")

# use the lookup table to enrich the scooter data with Start_TAZ and End_TAZ information
enriched_scooters <- scooter_data %>%
  left_join(lookup_df, by = c("StartCenterlineID" = "GBSID")) %>%
  mutate(Start_Hex_ID = HEX_ID) %>%
  select(-c(ObjectId,geometry,HEX_ID)) %>%
  left_join(lookup_df, by = c("EndCenterlineID" = "GBSID")) %>%
  mutate(End_Hex_ID = HEX_ID) %>%
  select(-c(geometry,HEX_ID))

# a chart showing the number of trips per TAZ
trips_per_hex <- enriched_scooters %>%
  group_by(Start_Hex_ID) %>%
  tally()

hex_freq <- hex_grid %>%
  left_join(trips_per_hex, by = c("HEX_ID" = "Start_Hex_ID")) %>%
  select(HEX_ID, n, geometry)

plot(hex_freq['n'])

ggplot() +
  geom_sf(data = hex_freq, aes(fill = n))

library(ggmap)
b <- st_bbox(mpls_boundary)
names(b) <- c("left","bottom","right","top")
#mpls_map <- get_stamenmap(bbox = b, maptype = 'toner-2011',color = c('bw'), zoom = 12)
#ggmap(mpls_map)

library(ggplot2)
library(RColorBrewer)
library(ggsf)
#ggmap(mpls_map) +
#  theme_bw() +
#  ggplot2::geom_sf(data = hex_freq, aes(fill = n, alpha = n), inherit.aes = FALSE, clip = 'off') +
#  scale_fill_gradient(low = "darkgreen", high = "darkgreen")

library(sf)
#plot(hex_freq[1], bgMap = get_map(location = b), alpha = .1)
# save the enriched data to csv
write.csv(enriched_scooters,"enriched_scooter_trips_hex.csv")
st_write(hex_grid,"hex_grid.shp")
