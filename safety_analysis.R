# goal: to identify if there is a correlation between the presence of enhanced bike infrastructure and scooter usage
# approach- use bike lane data to assign an 'infrastructure score' to each hex area, and then use linear regression to test if this is a significant factor for start/end rides
library(dplyr)
library(sf)
library(readr)
library(ggplot2)

### import hex data
hex_data <- st_read("hex_grid.shp")
crs = st_crs(hex_data)

### import scooter ride data
scooter_trips <- read_csv("enriched_scooter_trips_hex.csv", col_types = cols(EndTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"), StartTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
mpls_centerlines <- st_read("data/centerlines/Street_Centerline/Street_Centerline.shp")

### import bike infrastructure data
bike_infrastructure <- st_read("data/bike_paths/ped_bike/Pedestrian_and_Bicycle_Trails.shp")
bike_infrastructure <- st_transform(bike_infrastructure,crs = crs)

# find the total number of kilometers of bike infrastructure
total_dist <- sum(st_length(bike_infrastructure)) / 1000 # Minneapolis has 133km of bike infrastructure

# calculate an 'infrastructure score' for each hex zone
hex_infra <- hex_data %>%
  mutate(infrastructure_length = -1)

get_infra_length <- function(hex_geo){
  hex_geo <- st_sf(hex_geo)
  intersections <- st_intersection(hex_geo, bike_infrastructure)
  infra_length <- st_length(intersections)
  infra_length <- ifelse(is.null(infra_length), -1, infra_length)
  return(infra_length)
}

hex_infra2 <- hex_infra$geometry %>%
  map(get_infra_length)

distance = vector(length = nrow(hex_infra))
for(i in hex_infra[[1]]){
  distance[[i]] <- get_infra_length(hex_infra[i,])
}
hex_infra$infrastructure_length <- distance
hex_infra$infrastructure_length[is.na(hex_infra$infrastructure_length)] <- 0

hex_rides <-  scooter_trips %>%
  group_by(Start_Hex_ID) %>%
  tally()

hex_infra_joined <- hex_infra %>%
  left_join(hex_rides, by = c("HEX_ID" = "Start_Hex_ID")) %>%
  mutate(rides_started = n) %>%
  select(rides_started, HEX_ID, infrastructure_length)

ggplot(data = hex_infra_joined, aes(x = infrastructure_length, y = rides_started)) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)

# running regression
plot(hex_infra_joined)
m1 <- lm(rides_started ~ infrastructure_length, data = hex_infra_joined)
summary(m1)
