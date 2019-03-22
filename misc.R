# miscellaneous analyses

# a chart showing the duration of scooter rides throughout the pilot period
ggplot(data = enriched_scooter_trips, aes(x = StartTime, y = TripDuration)) +
  geom_point()