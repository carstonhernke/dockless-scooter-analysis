# trips per TAZ visualization
library(sf)
library(ggplot2)
library(readr)
library(dplyr)

# scooter data
# import trip data from csv
enriched_scooter_trips <- read_csv("enriched_scooter_trips.csv", col_types = cols(EndTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"), StartTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

# count the number of NAs
sum(is.na(enriched_scooter_trips))

# remove rows that contain an NA
enriched_scooter_trips <- enriched_scooter_trips[complete.cases(enriched_scooter_trips),]

# TAZ data

# Minneapolis boundary data
mpls_boundary <- st_read("data/minneapolis_boundary/msvcGIS_MinneapolisCityLimits.shp")

taz_data <- st_read("data/taz_shapefiles/TAZOfficialWCurrentForecasts.shp")

# standardize crs
crs = st_crs(mpls_boundary)
taz_data <- st_transform(taz_data,crs = crs)

contains <- st_contains(mpls_boundary,taz_data) %>%
  as.matrix() %>%
  t() %>%
  as.list()

taz_data$mpls <- contains
taz_data <- taz_data %>%
  filter(mpls == TRUE)

# a chart showing the number of trips per TAZ
trips_per_taz <- enriched_scooter_trips %>%
  group_by(Start_TAZ) %>%
  tally()

taz_with_freq <- taz_data %>%
  left_join(trips_per_taz, by = c("TAZ" = "Start_TAZ")) %>%
  select(TAZ, n, geometry)

# almost all trips occur on the UMN campus or downtown... this is definitely skewing data
plot(taz_with_freq['n'], main = "Freqency of scooter trip starts by TAZ")