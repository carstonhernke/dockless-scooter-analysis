# goal: to identify if there is a correlation between the presence of enhanced bike infrastructure and scooter usage
# approach- use bike lane data to assign an 'infrastructure score' to each hex area, and then use linear regression to test if this is a significant factor for start/end rides
library(dplyr)
library(sf)
library(readr)
library(ggplot2)

### import hex data
hex_data <- st_read("hex_grid.shp")
crs <-  st_crs(hex_data)

### import population and student data
attributes <- st_read("hex_with_attributes.shp")
attributes$geometry <- NULL

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

# summary stats on infra length per zone:
summary(hex_infra$infrastructure_length)

hex_rides <-  scooter_trips %>%
  group_by(Start_Hex_ID) %>%
  tally()

hex_infra_joined <- hex_infra %>%
  left_join(hex_rides, by = c("HEX_ID" = "Start_Hex_ID")) %>%
  mutate(rides_started = n) %>%
  select(rides_started, HEX_ID, infrastructure_length)

hex_infra_attributes <- hex_infra_joined %>%
  left_join(attributes, by = c("HEX_ID" = "HEX_ID"))

hex_infra_attributes$rides_started_per_capita <- hex_infra_attributes$rides_started / hex_infra_attributes$popultn
  
# change data to useful units
# kilometers of infrastructure per square kilometer
hex_infra_attributes$infra_per_km2 <- (hex_infra_attributes$infrastructure_length / 1000) / .1900683

# rides per day
hex_infra_attributes$rides_per_day <- hex_infra_attributes$rides_started/144


# rides_started_per_capita vs. college students  
ggplot(data = hex_infra_attributes, aes(x = cllg_st, y = rides_started_per_capita)) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)

# population vs infrastructure
ggplot(data = hex_infra_attributes, aes(x = popultn, y = infrastructure_length)) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)
# running regression 
# rides_started_per_capita vs. infrastructure_length 

ggplot(data = hex_infra_attributes, aes(x = infrastructure_length, y = rides_started_per_capita)) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE)
plot(hex_infra_joined)
m1 <- lm(rides_started_per_capita ~ infrastructure_length + cllg_st, data = hex_infra_attributes)
summary(m1)

# running regression - controlling for students
plot(hex_infra_joined)
m2 <- lm(rides_started_per_capita ~ infrastructure_length + cllg_st, data = hex_infra_attributes)
summary(m2)

# regression - daily rides, no controls
m3 <- lm(rides_per_day ~ infra_per_km2, data = hex_infra_attributes)
summary(m3)

# regression - total rides, controlling for pop and students
m3 <- lm(rides_per_day ~ infra_per_km2 + popultn + cllg_st, data = hex_infra_attributes)
summary(m3)

# set ggplot themes for publishing

library(extrafont)
font_import()

theme_Publication <- function(base_size=14, base_family="CMU Bright") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

# plots
par(family = "CMU Bright")
plot(bike_infrastructure['ID'], main = "")
plot(hex_infra['infrastructure_length'], main = "")

ggplot(data = hex_infra_attributes, aes(x = infra_per_km2, y = rides_per_day)) +
  geom_point() +
  ggtitle("Bike Infrastructure and Scooter Rides") +
  xlab("Kilometers of Enhanced Bike Infrastructure per Square Kilometer") +
  ylab("Average Daily Scooter Ride Originations") +
  geom_smooth(method='lm', se = FALSE) +
  scale_colour_Publication() +
  theme_Publication()
  

grid.arrange(p,(p +scale_colour_Publication()+ theme_Publication()),nrow=1)
