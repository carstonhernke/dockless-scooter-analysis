# Research Question: How many rides (or percentage) start or end in close proximity to the light rail?

library(readr)
library(sf)
library(dplyr)
library(chron)
library(ggplot2)
library(scales)

### PREP ###

# scooter data
# import trip data from csv
enriched_scooter_trips <- read_csv("~/Google Drive/Google Drive Personal/College/Fall 2018/Thesis/Analysis/enriched_scooter_trips.csv", col_types = cols(EndTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"), StartTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

# count the number of NAs
sum(is.na(enriched_scooter_trips))

# remove rows that contain an NA
enriched_scooter_trips <- enriched_scooter_trips[complete.cases(enriched_scooter_trips),]

# transit data
transit_locations <- st_read("data/transit_locations/TransitwayStations.shp")

transit_locations <- transit_locations %>%
  filter(Type == 'Light Rail')

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

# adjust the coordinate reference system 
transit_locations <- st_transform(transit_locations,crs = crs)

# create a flag that indicates whether a TAZ contains a light rail stop
contains_bool = st_contains(taz_data$geometry,transit_locations$geometry) %>%
  as.matrix() %>%
  apply(1,any)

taz_data_enriched <- taz_data %>%
  select(TAZ, geometry) %>%
  mutate(has_light_rail = contains_bool)

# plot(taz_data_enriched) # see which TAZ have light rail stops in them

### ANALYSIS ###
trips_with_transit <- enriched_scooter_trips %>%
  left_join(taz_data_enriched, by = c("Start_TAZ" = "TAZ")) %>%
  mutate(start_light_rail = has_light_rail) %>%
  select(TripID,TripDuration,TripDistance,StartTime,EndTime,Start_TAZ,End_TAZ,start_light_rail) %>%
  left_join(taz_data_enriched, by = c("End_TAZ" = "TAZ")) %>%
  mutate(end_light_rail = has_light_rail) %>%
  select(TripID,TripDuration,TripDistance,StartTime,EndTime,Start_TAZ,End_TAZ,start_light_rail,end_light_rail)

# how many trips started or ended in a TAZ with light rail?
table(trips_with_transit$start_light_rail | trips_with_transit$end_light_rail)

# look at proportion of trips that start/ end with light rail throughout the day

# start light rail
trips_with_transit$time_of_day <-  times(format(trips_with_transit$StartTime, "%H:%M:%S"))

hourly_transit_start_count <- trips_with_transit %>%
  mutate(time = times(format(trips_with_transit$StartTime, "%H:%M:%S"))) %>%
  group_by(time,start_light_rail) %>%
  tally() %>%
  na.omit()

date = as.character("2018-07-10")
hourly_transit_start_count$time <- as.POSIXct(paste(date, as.character(hourly_transit_start_count$time)), format="%Y-%m-%d %H:%M:%S")

ggplot(data = hourly_transit_count, aes(x = time, y = n, fill = start_light_rail)) +
  geom_col(position = 'fill') +
  labs(title = "Proportion of trips that begin in a TAZ with a light rail station",
       y = "Proportion",
       x = "Time of Day")

# end light rail
hourly_transit_end_count <- trips_with_transit %>%
  mutate(time = times(format(trips_with_transit$StartTime, "%H:%M:%S"))) %>%
  group_by(time,end_light_rail) %>%
  tally() %>%
  na.omit()

date = as.character("2018-07-10")
hourly_transit_end_count$time <- as.POSIXct(paste(date, as.character(hourly_transit_end_count$time)), format="%Y-%m-%d %H:%M:%S")

ggplot(data = hourly_transit_count, aes(x = time, y = n, fill = end_light_rail)) +
  geom_col(position = 'fill')

# combined
hourly_transit_combined_count <- trips_with_transit %>%
  mutate(time = times(format(trips_with_transit$StartTime, "%H:%M:%S"))) %>%
  mutate(trip_type = case_when(start_light_rail == TRUE ~ 'Start near Light Rail', end_light_rail == TRUE ~ 'End Near Light Rail', start_light_rail == TRUE & end_light_rail == TRUE ~ 'both', start_light_rail != TRUE & end_light_rail != TRUE ~ 'a_none')) %>%
  group_by(time,trip_type) %>%
  tally() %>%
  na.omit()

date = as.character("2018-07-10")
hourly_transit_combined_count$time <- as.POSIXct(paste(date, as.character(hourly_transit_combined_count$time)), format="%Y-%m-%d %H:%M:%S")

lims <- as.POSIXct(strptime(c("2018-07-10 06:00:00","2018-07-10 23:30:00"), format = "%Y-%m-%d %H:%M:%S"))    
ggplot(data = hourly_transit_combined_count, aes(x = time, y = n, fill = trip_type)) +
  scale_x_datetime(limits = lims) +
  geom_col(position = 'fill') +
  labs(title = "Proportion of trips that begin or end in a TAZ with a light rail station throughout the day",
       y = "Proportion",
       x = "Time of Day")
