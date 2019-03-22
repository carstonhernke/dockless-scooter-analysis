# goal: to find the latitude and longitude of the centerline that the scooter trip is matched to
# this data will be more accurate than just a TAZ match
library(dplyr)
library(sf)
library(readr)

# Import data on scooter rides from CSV
Motorized_Foot_Scooter_Trips_2018 <- read_csv("~/Google Drive/Google Drive Personal/College/Fall 2018/Thesis/Analysis/data/mpls_scooter_data/Motorized_Foot_Scooter_Trips_2018.csv", 
                                              col_types = cols(EndTime = col_datetime(format = ""), 
                                                               StartTime = col_datetime(format = "")))
scooter_data <-  Motorized_Foot_Scooter_Trips_2018

mpls_centerlines <- st_read("data/centerlines/Street_Centerline/Street_Centerline.shp")
mpls_centerline_points = st_centroid(mpls_centerlines)

scooter_data_start_points <- scooter_data %>%
  left_join(mpls_centerline_points, by = c('StartCenterlineID' = 'GBSID')) %>%
  select(TripID,TripDuration,TripDistance,StartTime,EndTime,geometry) %>%
  st_sf()

sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- do.call(rbind,sf::st_geometry(x))
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

output <- scooter_data_start_points
output <-  sfc_as_cols(output)
output$geometry <- NULL

write.csv(output,"scooter_trips_with_start_points.csv")
