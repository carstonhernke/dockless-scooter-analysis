# Research question: What is the correlation between different weather conditions (temperature, precipitation) and scooter usage?
library(dplyr)

### PREP ###
library(sf)
library(ggplot2)
library(readr)
library(dplyr)

# import trip data from csv
enriched_scooter_trips <- read_csv("enriched_scooter_trips.csv", col_types = cols(EndTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"), StartTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

# count the number of NAs
sum(is.na(enriched_scooter_trips))

# remove rows that contain an NA
enriched_scooter_trips <- enriched_scooter_trips[complete.cases(enriched_scooter_trips),]

# get daily trip counts
daily_ride_count <- enriched_scooter_trips %>%
  mutate(day = as.Date(StartTime, format = "%Y-%m-%d")) %>%
  group_by(day) %>%
  tally()

# get weather data
library(readr)
mpls_historical <- read_csv("data/historical_weather/mpls_historical.csv", 
                            col_types = cols(AWND = col_double(), 
                                             DATE = col_date(format = "%Y-%m-%d"), 
                                             TMAX = col_integer(), TMIN = col_integer(), 
                                             WT01 = col_logical(), WT02 = col_logical(), 
                                             WT03 = col_logical(), WT05 = col_logical(), 
                                             WT06 = col_logical(), WT08 = col_logical(), 
                                             WT09 = col_logical()))

mpls_weather <- mpls_historical %>%
  filter(STATION == 'USW00014922')

# add weather data to daily counts
trips_and_weather <- left_join(daily_ride_count,mpls_weather, by = c("day" = "DATE"))

### ANALYSIS ###

# plot number of trips per day vs high temperature
# this doesn't adjust for the number of scooters added in the middle of the pilot! Need to reach out for this info
ggplot(data = trips_and_weather, aes(x = TMAX, y = n)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = "Number of trips per day vs. high temperature",
       subtitle = "Does NOT account for the change in number of available scooters halfway through the pilot period",
       y = "Number of trips",
       x = "Daily high temperature at MSP airport")

# plot number of trips per day vs inches of precipitation
# this doesn't adjust for the number of scooters added in the middle of the pilot! Need to reach out for this info
ggplot(data = trips_and_weather, aes(x = PRCP, y = n)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(title = "Number of trips per day vs. inches of precipitation",
       subtitle = "Does NOT account for the change in number of available scooters halfway through the pilot period",
       y = "Number of trips",
       x = "Inches of precipitation recorded at MSP airport")

# plot number of trips per day vs the presence of fog
# this doesn't adjust for the number of scooters added in the middle of the pilot! Need to reach out for this info
ggplot(data = trips_and_weather, aes(x = WT01, y = n)) +
  geom_boxplot() +
  labs(title = "Number of trips per day based on the presence of fog",
       subtitle = "Does NOT account for the change in number of available scooters halfway through the pilot period",
       y = "Number of trips",
       x = "Fog recorded at any point in the day at MSP airport")
