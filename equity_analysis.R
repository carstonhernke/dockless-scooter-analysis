# 5.2: Equity Analysis
# Hypothesis: There is a relationship between the household income of a zone and the net availability of scooters within that zone throughout the day
library(chron)
library(sf)
library(dplyr)
library(readr)

### DATA SETUP ###

# import household income data
hh_income <- read_csv("data/census_household_income/census_block_income_data/nhgis0001_ds233_20175_2017_blck_grp.csv")

hh_income <- hh_income %>%
  mutate(join_id = paste(STATEA,COUNTYA,TRACTA,BLKGRPA,sep = '')) %>% # remove the preceding 'G' from the variable
  select(join_id,AH1PE001)

# import block shapefile data
block_groups <- st_read("data/census_household_income/census_block_shapefiles/tl_2018_27_bg.shp")

block_groups <- block_groups %>%
  select(GEOID, geometry)

# join them
income_by_block_group <- left_join(block_groups,hh_income,by = c("GEOID" = "join_id"))

# Minneapolis boundary data
mpls_boundary <- st_read("data/minneapolis_boundary/msvcGIS_MinneapolisCityLimits.shp")

hex_data <- st_read("hex_grid.shp")

# adjust the coordinate reference system for census data to match that of the hex
crs = st_crs(hex_data)
income_by_block_group <- st_transform(income_by_block_group,crs = crs)

hex_with_income = hex_data %>%
  st_join(income_by_block_group) %>%
  group_by(HEX_ID) %>%
  summarize(average_income = mean(AH1PE001, na.rm = TRUE))

# import scooter data
scooter_trips <- read_csv("enriched_scooter_trips_hex.csv", col_types = cols(EndTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"), StartTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))


### CALUCULATE NET RIDES BY DAY AND HOUR WITHIN A ZONE ###

# calculate daily nets
daily_ride_count_start <- scooter_trips %>%
  mutate(day = as.Date(StartTime, format = "%Y-%m-%d")) %>%
  group_by(day,Start_Hex_ID) %>%
  tally() %>%
  rename(start_count = n)

daily_ride_count_end <- scooter_trips %>%
  mutate(day = as.Date(StartTime, format = "%Y-%m-%d")) %>%
  group_by(day,End_Hex_ID) %>%
  tally() %>%
  rename(end_count = n)

in_and_out <- left_join(daily_ride_count_start, daily_ride_count_end, by = c("day" = "day", "Start_Hex_ID" = "End_Hex_ID")) %>%
  rename(HEX_ID = Start_Hex_ID)

in_and_out$net <- in_and_out$end_count - in_and_out$start_count

# # see which hexes have the most dramatic daily nets
# mean_hex <- in_and_out %>%
#   group_by(HEX_ID) %>%
#   summarize(Mean = mean(net, na.rm=TRUE))
# 
# hex_data_net_means <- hex_data %>%
#   left_join(mean_hex, by = c("HEX_ID" = "HEX_ID"))
# 
# # plot daily means
# plot(hex_data_net_means['Mean'])
# 
# calculate hourly nets

hourly_ride_count_start <- scooter_trips %>%
  mutate(time = times(format(StartTime, "%H:%M:%S"))) %>%
  group_by(time,Start_Hex_ID) %>%
  tally() %>%
  rename(start_count = n)

hourly_ride_count_end <- scooter_trips %>%
  mutate(time = times(format(StartTime, "%H:%M:%S"))) %>%
  group_by(time,End_Hex_ID) %>%
  tally() %>%
  rename(end_count = n)

in_and_out_hour <- left_join(hourly_ride_count_start, hourly_ride_count_end, by = c("time" = "time", "Start_Hex_ID" = "End_Hex_ID")) %>%
  rename(HEX_ID = Start_Hex_ID)

in_and_out_hour$net <- in_and_out_hour$end_count - in_and_out_hour$start_count
# 
# # see which hexes have the most dramatic daily nets
# 
# times <-  as.character(unique(mean_hex_hour$time))
# fixed_colors <- scale_fill_gradient2(low="red", mid="grey", high="green", #colors in the scale
#                      midpoint= 0,
#                      limits = c(-200,200))
# 
# b <- st_bbox(mpls_boundary)
# x_limits <- c(b[1],b[3])
# y_limits <- c(b[2],b[4])
# 
# count = 0
# for (t in times)
# {
#   count <-count + 1
#   print(t)
#   hex_data_net_hour_means <- hex_data %>%
#     left_join(mean_hex_hour, by = c("HEX_ID" = "HEX_ID")) %>%
#     filter(time == t)
# 
#   # plot hourly means
#   fname = paste("hourly_net_plots/",count,".png",sep = '')
#   ggplot() +
#     geom_sf(data = hex_data_net_hour_means, aes(fill = Mean)) +
#     xlim(x_limits) +
#     ylim(y_limits) +
#     fixed_colors +
#     ggtitle(paste("net change in scooters at time", t))
#   ggsave(filename = fname)
# }
# 
# hex_data_net_hour_means <- hex_data %>%
#   left_join(mean_hex_hour, by = c("HEX_ID" = "HEX_ID"))
# 
# t <- "18:00:00"
# hex_data_net_hour_means <- mean_hex_hour %>%
#   filter(time == t) %>%
#   left_join(hex_with_income, by = c("HEX_ID" = "HEX_ID")) %>%
#   select(HEX_ID,Mean,average_income,time)
# 
# library(stats)
# norm <- data.frame(scale(hex_data_net_hour_means))
# m1 <- lm(Mean ~ average_income, data = norm)
# summary(m1)
# 
# # plot hourly means
# plot(hex_data_net_hour_means['Mean'])

### Calculate the average net for each hex and time
mean_hex_hour <- in_and_out_hour %>%
  group_by(time,HEX_ID) %>%
  summarize(mean_net = mean(net, na.rm=TRUE))

### Now we have data on net movements in zones by the hour - lets look at whether there is any correlation with income:

### Testing for spatial autocorrelation using strategy here:
### https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
library(ape)
hex_centers <- hex_data %>%
  st_centroid() %>%
  st_sf()
  
sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- do.call(rbind,sf::st_geometry(x))
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

hex_centers <- sfc_as_cols(hex_centers)


### TIME = 8:00
t <- "8:00:00"
hex_data_net_hour_means <- mean_hex_hour %>%
  filter(time == t) %>%
  left_join(hex_with_income, by = c("HEX_ID" = "HEX_ID")) %>%
  select(HEX_ID,mean_net,average_income,time)

library(stats)
combined_data_scaled <- hex_data_net_hour_means
combined_data_scaled$mean_net <- (scale(hex_data_net_hour_means$mean_net))
combined_data_scaled$average_income <- (scale(hex_data_net_hour_means$average_income))
combined_data_scaled <- combined_data_scaled[complete.cases(combined_data_scaled),]

hex_list <- combined_data_scaled[complete.cases(combined_data_scaled),]$HEX_ID
# subset the center data only for the hexes we have data for
hex_centers_sub <- hex_centers[hex_centers$HEX_ID %in% hex_list,]
hex_dists <- as.matrix(dist(cbind(hex_centers_sub$lon, hex_centers_sub$lat)))
diag(hex_dists) <- 0
hex_dists_inv <- 1/hex_dists

hdnhm <-  hex_data_net_hour_means[complete.cases(hex_data_net_hour_means),]

# Run Moran's I to test for Spatial Autocorrelation
Moran.I(hdnhm$mean_net, hex_dists)

# there is no spatial autocorrelation in the response data, so it is ok to use a normal regression!

m1 <- lm(mean_net ~ average_income, data = combined_data_scaled)
summary(m1)

### TIME = 12:00
t <- "12:00:00"
hex_data_net_hour_means <- mean_hex_hour %>%
  filter(time == t) %>%
  left_join(hex_with_income, by = c("HEX_ID" = "HEX_ID")) %>%
  select(HEX_ID,mean_net,average_income,time)

library(stats)
combined_data_scaled <- hex_data_net_hour_means
combined_data_scaled$mean_net <- (scale(hex_data_net_hour_means$mean_net))
combined_data_scaled$average_income <- (scale(hex_data_net_hour_means$average_income))
combined_data_scaled <- combined_data_scaled[complete.cases(combined_data_scaled),]

hex_list <- combined_data_scaled[complete.cases(combined_data_scaled),]$HEX_ID
# subset the center data only for the hexes we have data for
hex_centers_sub <- hex_centers[hex_centers$HEX_ID %in% hex_list,]
hex_dists <- as.matrix(dist(cbind(hex_centers_sub$lon, hex_centers_sub$lat)))
diag(hex_dists) <- 0
hex_dists_inv <- 1/hex_dists

hdnhm <-  hex_data_net_hour_means[complete.cases(hex_data_net_hour_means),]

# Run Moran's I to test for Spatial Autocorrelation
Moran.I(hdnhm$mean_net, hex_dists)

# there is no spatial autocorrelation in the response data, so it is ok to use a normal regression!

m1 <- lm(mean_net ~ average_income, data = combined_data_scaled)
summary(m1)

### TIME = 16:00
t <- "16:00:00"
hex_data_net_hour_means <- mean_hex_hour %>%
  filter(time == t) %>%
  left_join(hex_with_income, by = c("HEX_ID" = "HEX_ID")) %>%
  select(HEX_ID,mean_net,average_income,time)

library(stats)
combined_data_scaled <- hex_data_net_hour_means
combined_data_scaled$mean_net <- (scale(hex_data_net_hour_means$mean_net))
combined_data_scaled$average_income <- (scale(hex_data_net_hour_means$average_income))
combined_data_scaled <- combined_data_scaled[complete.cases(combined_data_scaled),]

hex_list <- combined_data_scaled[complete.cases(combined_data_scaled),]$HEX_ID
# subset the center data only for the hexes we have data for
hex_centers_sub <- hex_centers[hex_centers$HEX_ID %in% hex_list,]
hex_dists <- as.matrix(dist(cbind(hex_centers_sub$lon, hex_centers_sub$lat)))
diag(hex_dists) <- 0
hex_dists_inv <- 1/hex_dists

hdnhm <-  hex_data_net_hour_means[complete.cases(hex_data_net_hour_means),]

# Run Moran's I to test for Spatial Autocorrelation
Moran.I(hdnhm$mean_net, hex_dists)

# there IS spatial autocorrelation in the response data, so now we have to look at the variogram
combined <- hex_data_net_hour_means %>%
  left_join(hex_data, by = c("HEX_ID" = "HEX_ID")) %>%
  st_sf %>%
  st_centroid() %>%
  sfc_as_cols %>%
  select(HEX_ID, mean_net, average_income, lat, lon)

combined <- combined[-nrow(combined),]
combined <- combined[complete.cases(combined$mean_net),]

library(geoR)
variogram <- variog(coords = cbind(combined$lon, combined$lat), data = combined$mean_net)
plot(variogram, type = 'b', main = "variogram")

m1 <- lm(mean_net ~ average_income, data = combined_data_scaled)
summary(m1)