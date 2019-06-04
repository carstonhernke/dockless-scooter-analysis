# Research Question: Does the average number of trips differ significantly on weekends vs. weekdays?

library(dplyr)
library(lubridate)

### PREP ###

# import trip data from csv
enriched_scooter_trips <- read_csv("enriched_scooter_trips.csv", col_types = cols(EndTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"), StartTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

# count the number of NAs
sum(is.na(enriched_scooter_trips))

# remove rows that contain an NA
enriched_scooter_trips <- enriched_scooter_trips[complete.cases(enriched_scooter_trips),]

### ANALYSIS ###

# add a day identifier
enriched_scooter_trips$day_of_week <- wday(enriched_scooter_trips$StartTime, label = FALSE)

# create a column that indicates whether the day is a weekend
enriched_scooter_trips <- enriched_scooter_trips %>%
  mutate(day_of_week = wday(StartTime, label = FALSE)) %>%
  mutate(weekend = case_when(day_of_week == 6 | day_of_week == 7 ~ TRUE, day_of_week != 6 & day_of_week != 7 ~ FALSE)) %>%
  mutate(weekend = as.factor(weekend))

daily_ride_count <- enriched_scooter_trips %>%
  mutate(day = as.Date(StartTime, format = "%Y-%m-%d")) %>%
  group_by(day) %>%
  tally() %>% 
  mutate(day_of_week = wday(day, label = FALSE)) %>%
  mutate(weekend = case_when(day_of_week == 6 | day_of_week == 7 ~ TRUE, day_of_week != 6 & day_of_week != 7 ~ FALSE)) %>%
  mutate(weekend = as.factor(weekend))

# boxplot of daily number of rides depending on whether it is a weekend or weekday
ggplot(data = daily_ride_count, aes(x = weekend, y = n)) +
  geom_boxplot() +
  ggtitle("Number of trips on Weekdays vs. Weekends") +
  xlab("Weekend") +
  ylab("Number of rides")  

# boxplot of duration of ride depending on whether it is a weekend or weekday
ggplot(data = enriched_scooter_trips, aes(x = weekend, y = TripDuration)) +
  geom_boxplot() +
  ylim(0, 5000) +
  ggtitle("Duration of rides on Weekdays vs. Weekends") +
  xlab("Weekend") +
  ylab("Trip Duration")
