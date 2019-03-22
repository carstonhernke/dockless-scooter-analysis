# Research question: where are scooters distributed at the beginning of the day?

# import trip data from csv
trip_data <- read_csv("enriched_scooter_trips.csv")

# remove rows that contain an NA
trip_data <- trip_data[complete.cases(trip_data),]

