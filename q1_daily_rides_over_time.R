# Research question: where are scooters distributed at the beginning of the day?
library(dplyr)
library(ggplot2)
library(readr)

### PREP ###

# import trip data from csv
enriched_scooter_trips <- read_csv("enriched_scooter_trips.csv", col_types = cols(EndTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"), StartTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

# count the number of NAs
sum(is.na(enriched_scooter_trips))

# remove rows that contain an NA
enriched_scooter_trips <- enriched_scooter_trips[complete.cases(enriched_scooter_trips),]

### ANALYSIS ###

daily_ride_count <- enriched_scooter_trips %>%
  mutate(day = as.Date(StartTime, format = "%Y-%m-%d")) %>%
  group_by(day) %>%
  tally()

### VISUALIZATION ###

ggplot(data = daily_ride_count, aes(x = day, y = n)) +
  geom_line() +
  ggtitle("Number of scooter rides per day") +
  ylab("Number of scooter rides") +
  xlab("Date")