library(tidyverse)

# The Usual Suspects
library(ggplot2)
library(ggthemes)
library(magrittr)
library(lubridate)

# Combine plots
library(patchwork)

# Maps
library(ggmap)

# Read CSV ----------------------------------------------------------------
scooter_trips_with_coordinates <- read_csv("~/Google Drive/Google Drive Personal/College/Fall 2018/Thesis/dockless-scooter-analysis/scooter_trips_with_coordinates.csv",col_types = cols(EndTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),StartTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),TripDistance = col_integer(), TripDuration = col_integer(),TripID = col_character()))

# Drop scooter_trips_with_coordinates with missing coordinates
scooter_trips_with_coordinates %<>% filter(!is.na(start_lon) & !is.na(start_lat) &
                  !is.na(end_lon) & !is.na(end_lat))

# Get the hour and day of the ride
scooter_trips_with_coordinates$hour <- scooter_trips_with_coordinates$StartTime %>% hour()
scooter_trips_with_coordinates$day <- scooter_trips_with_coordinates$StartTime %>% wday(week_start = 7)
scooter_trips_with_coordinates$weekend <- ifelse(scooter_trips_with_coordinates$day %in% c(1,7),TRUE,FALSE)

# Long format
scooter_trips_with_coordinates_start <- scooter_trips_with_coordinates %>% 
  select(lat = start_lat,
         long = start_lon,
         weekend = weekend) %>%
  add_column(position = 1)
scooter_trips_with_coordinates_end <- scooter_trips_with_coordinates %>% 
  select(lat = end_lat,
         long = end_lon,
         weekend = weekend) %>%
  add_column(position = -1)
scooter_trips_with_coordinates_long <- rbind(scooter_trips_with_coordinates_start, scooter_trips_with_coordinates_end)

# Clean up
rm(scooter_trips_with_coordinates_start, scooter_trips_with_coordinates_end)

# Make map ------------------------------------------------------
# Don't commit key
# register_google(key = "INSERT_KEY_HERE")
mpls <- get_map(c(left = min(scooter_trips_with_coordinates_long$long), 
                  bottom = min(scooter_trips_with_coordinates_long$lat), 
                  right = max(scooter_trips_with_coordinates_long$long), 
                  top = max(scooter_trips_with_coordinates_long$lat)),
                maptype='terrain', source='stamen', zoom=13)

# Weekend -----------------------------------------------------------------

plots <- list()
plots$p1 <- ggmap(mpls,darken = c(.5,"#000000")) + 
  stat_summary_hex(data = scooter_trips_with_coordinates_long %>% filter(weekend),
             aes(x = long, 
                 y = lat,
                 z = position),
             size = .2,
             alpha=.7) + 
  geom_segment(data = scooter_trips_with_coordinates %>% filter(weekend),
               aes(x = start_lon, 
                   y = start_lat,
                   xend = end_lon,
                   yend = end_lat),
               color = "#0000FF",
               alpha = .002) + coord_cartesian() +
  scale_fill_gradient2(name = "Average Ride\nState at Position",
                        breaks = c(1, 0, -1),
                        labels = c("Started",
                                   "Neutral",
                                   "Ended"),
                        low = "#e000e3", mid = "white",
                        high = "#000000", midpoint = 0) +
  ggtitle("Rides Taken During the Weekend") 


# Weekday -----------------------------------------------------------------

plots$p2 <- ggmap(mpls,darken = c(.5,"#000000")) + 
  stat_summary_hex(data = scooter_trips_with_coordinates_long %>% filter(!weekend),
                   aes(x = long, 
                       y = lat,
                       z = position),
                   size = .2,
                   alpha=.7) + 
  geom_segment(data = scooter_trips_with_coordinates %>% filter(!weekend),
               aes(x = start_lon, 
                   y = start_lat,
                   xend = end_lon,
                   yend = end_lat),
               color = "#0000FF",
               alpha = .002) + coord_cartesian() +
  scale_fill_gradient2(name = "Average Ride\nState at Position",
                       breaks = c(1, 0, -1),
                       labels = c("Started",
                                  "Neutral",
                                  "Ended"),
                       low = "#e000e3", mid = "white",
                       high = "#000000", midpoint = 0) +
  ggtitle("Rides Taken During the Week")

# Save plots using plot title as filename
plots %>% lapply(function(p){
  filename = paste0("../visualizations/hex_", p$labels$title) %>% 
    tolower %>% 
    str_replace_all('[^a-z_\\.\\/]','_') %>%
    paste0(".png")
  print(filename)
  ggsave(filename, p, width = 8.5, height = 11, units = "in")
})