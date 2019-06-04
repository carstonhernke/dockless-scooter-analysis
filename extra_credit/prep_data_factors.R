# prep full data for ML

# Usage Analysis

# goal: to identify what types of trips scooters are used for
# approach: use Minneapolis zoning data to assign a 'location type' to the start and end points of each scooter trip. Then see how these change over time
library(sf)
library(readr)
library(ggplot2)
library(plyr)
library(dplyr)
library(ggalluvial)
library(chron)
library(scales)
library(lubridate)
library(RColorBrewer)
library(xtable)

refine_type <- function(x){
  if(substr(x, 1, 1) == 'C') return("Commercial")
  if(substr(x, 1, 1) == 'B') return("Downtown")
  if(substr(x, 1, 1) == 'I') return("Industrial")
  if((x == 'R1') | (x == 'R1A') | (x == 'R2') | (x == 'R2B')) return("Residential (LD)")
  if((x == 'R3') | (x == 'R4') | (x == 'R5') | (x == 'R6')) return("Residential (HD)")
  if(x == 'OR3') return("Institutional Office")
  if(substr(x, 1, 1) == 'O') return("Non-Institutional Office")
  if(TRUE) return("Other")
}

near_transit <- function(x){
  result <- any(st_contains(transit_locations_buffer, x))
  return(result)
}
crs <- st_crs(4267) #3488

# import transit location data
transit_locations <- st_read("data/transit_locations/TransitwayStations.shp")

transit_locations <- transit_locations %>%
  filter(Type == 'Light Rail')

transit_locations <- st_transform(transit_locations, crs)

transit_locations_buffer <- st_buffer(transit_locations,0.00179969743) # within 200m of a light rail station



# import and enrich zoning data
### import zoning data
zoning_data <- st_read("data/minneapolis_zoning/Planning_Primary_Zoning.shp")
zoning_data <- st_transform(zoning_data, crs)


### enrich with the type of zoning (by taking first character from zone)
zoning_data <- na.omit(zoning_data)
zoning_data_enriched <- zoning_data %>%
  mutate(zone_type = lapply(zoning_data$ZONE_CODE, refine_type))

zoning_data_enriched$zone_type <- as.character(zoning_data_enriched$zone_type)
plot(zoning_data_enriched['zone_type']) # plot zoning
legend(xpd = TRUE)

ggplot() + geom_sf(data = zoning_data_enriched, aes(fill = zone_type)) +
  scale_fill_Publication + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.background=element_blank())
# import scooter data with lat/lon coordinates
scooter_trips_with_coordinates <- read_csv("~/Google Drive/Google Drive Personal/College/Fall 2018/Thesis/dockless-scooter-analysis/scooter_trips_with_coordinates.csv",col_types = cols(EndTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),StartTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),TripDistance = col_integer(), TripDuration = col_integer(),TripID = col_character()))
complete_trips <- scooter_trips_with_coordinates[complete.cases(scooter_trips_with_coordinates),]
scooter_start_geo = st_as_sf(complete_trips, coords = c("start_lon","start_lat"), crs = crs) %>%
  mutate(start_geo = geometry)

scooter_end_geo = st_as_sf(complete_trips, coords = c("end_lon","end_lat"), crs = crs) %>%
  mutate(end_geo = geometry)

scooter_geo <- scooter_start_geo %>%
  mutate(end_geo = scooter_end_geo$end_geo)

st_geometry(joined) <- "start_geo"
scooter_geo <- st_transform(scooter_geo, crs)
scooter_geo$start_transit <- st_within(scooter_geo, transit_locations_buffer, prepared = FALSE) %>% lengths > 0

scooter_geo$start_transit <- ifelse(scooter_geo$start_transit == TRUE, 1, 0)

# for each scooter trip, identify which type of zoning category the start and end points fall into.
st_geometry(scooter_geo) <- "start_geo"
joined_start <- st_join(scooter_geo, zoning_data_enriched['zone_type']) %>%
  dplyr::rename(start_zone_type = zone_type)

st_geometry(joined_start) <- "end_geo"
joined <- st_join(joined_start, zoning_data_enriched['zone_type']) %>%
  dplyr::rename(end_zone_type = zone_type)

scooter_trips <- read_csv("enriched_scooter_trips_hex.csv", col_types = cols(EndTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"), StartTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
scooter_trips$TripID <- as.character(scooter_trips$TripID)

merged <- left_join(joined,scooter_trips,by = c("X1","TripID","TripDuration","TripDistance","StartTime","EndTime")) %>%
  select(-contains("geo"))
merged$end_geo <- NULL

hex_attributes <- st_read("hex_with_attributes.shp")
hex_attributes$geometry <- NULL

all_attr <- left_join(merged,hex_attributes,by = c("Start_Hex_ID" = "HEX_ID"))

joined_wday <- all_attr %>%
  mutate(day_of_week = wday(StartTime, label = FALSE)) %>%
  mutate(weekend = case_when(day_of_week == 6 | day_of_week == 7 ~ TRUE, day_of_week != 6 & day_of_week != 7 ~ FALSE)) %>%
  mutate(weekend = as.factor(weekend))

joined_wday$weekend <- ifelse(joined_wday$weekend == TRUE, 1, 0)

joined_wday$hour <- as.numeric(format(joined_wday$StartTime, "%H"))

output <- joined_wday[,c(2,9,10,14,15,16,17,18,19,20,21)]
output$end_hex <- output$End_Hex_ID
output$End_Hex_ID <- NULL
output <- output[complete.cases(output),]

write.csv(output, "extra_credit/scooter_trips_with_attributes.csv")


### Basic data only (start and end zone and time)
basic_output <- output %>% select(Start_Hex_ID, hour, end_hex)
write.csv(basic_output, "extra_credit/basic_trips.csv")


### get hex data 
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

hex_centers <- st_centroid(hex_grid)
dist_matrix <- st_distance(hex_centers)
write.csv(dist_matrix, "extra_credit/distance_matrix.csv")

plot(scooter_data$TripDistance, scooter_data$TripDuration)
m1 <- lm(TripDuration ~ TripDistance, data = scooter_data)
summary(m1)
