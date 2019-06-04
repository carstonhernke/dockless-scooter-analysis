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

# for each scooter trip, identify which type of zoning category the start and end points fall into.
st_geometry(scooter_geo) <- "start_geo"
joined_start <- st_join(scooter_geo, zoning_data_enriched['zone_type']) %>%
  dplyr::rename(start_zone_type = zone_type)

st_geometry(joined_start) <- "end_geo"
joined <- st_join(joined_start, zoning_data_enriched['zone_type']) %>%
  dplyr::rename(end_zone_type = zone_type)


# import transit location data
transit_locations <- st_read("data/transit_locations/TransitwayStations.shp")

transit_locations <- transit_locations %>%
  filter(Type == 'Light Rail')

transit_locations <- st_transform(transit_locations, crs)

transit_locations_buffer <- st_buffer(transit_locations,0.00179969743) # within 200m of a light rail station
plot(transit_locations_buffer['Station'])

ggmap(mpls_map) +
  theme_bw() +
  ggplot2::geom_sf(data = transit_locations_buffer, aes(fill = 'blue'), inherit.aes = FALSE) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

st_geometry(joined) <- "start_geo"
joined <- st_transform(joined, crs)
joined$start_transit <- st_within(joined, transit_locations_buffer, prepared = FALSE) %>% lengths > 0

st_geometry(joined) <- "end_geo"
joined <- st_transform(joined, crs)
joined$end_transit <- st_within(joined, transit_locations_buffer, prepared = FALSE) %>% lengths > 0

### add weekend data

joined_wday <- joined %>%
  mutate(day_of_week = wday(StartTime, label = FALSE)) %>%
  mutate(weekend = case_when(day_of_week == 6 | day_of_week == 7 ~ TRUE, day_of_week != 6 & day_of_week != 7 ~ FALSE)) %>%
  mutate(weekend = as.factor(weekend))
# plot & analyze (taking into account time)
### aggregate based on unique start/end combinations
zoning_sub <- joined[,c('start_zone_type','end_zone_type')]
zoning_sub$end_geo <- NULL
complete_joined <- zoning_sub[((zoning_sub$start_zone_type != 'NULL') & (zoning_sub$end_zone_type != 'NULL')),]
complete_joined$start_zone_type <- as.character(complete_joined$start_zone_type)
complete_joined$end_zone_type <- as.character(complete_joined$end_zone_type)

combination_counts <- data.frame(count(complete_joined, c('start_zone_type','end_zone_type')))
combination_counts <- na.omit(combination_counts)
combination_counts$pct <- combination_counts$freq / sum(combination_counts$freq)
print(xtable(combination_counts[order(combination_counts$pct, decreasing = TRUE),], caption = "Frequency of Start and End Zone Combinations, ordered by decreasing frequency"), include.rownames = FALSE)
is_alluvia_form(combination_counts, silent = TRUE)

ggplot(as.data.frame(combination_counts), aes(y = freq, axis1 = start_zone_type, axis2 = end_zone_type)) +
      geom_alluvium(aes(fill = start_zone_type), width = 1/12) +
      geom_stratum(width = 1/12, fill = "black", color = "grey") +
      geom_label(stat = "stratum", label.strata = TRUE) +
      scale_x_discrete(limits = c("Start Zoning", "End_Zoning"), expand = c(.05, .05)) +
      ggtitle("RQ-A1: What types of trips are scooters used for?") +
      theme(legend.position = "none")

### get percentages
start_freq <- data.frame(count(complete_joined, c('start_zone_type')))
start_freq <- na.omit(start_freq)
start_freq$pct <- start_freq$freq / sum(start_freq$freq)
colnames(start_freq) <- c("Start Zone Type", "Frequency", "Proportion")
start_freq <- start_freq[order(start_freq$Proportion, decreasing = TRUE),]
print(xtable(start_freq, caption = "Frequency of Start Zone Type"), include.rownames = FALSE)

hist(time_values)

time_values <- times(format(joined$StartTime, "%H:%M:%S"))
times <-  sort(as.character(unique(time_values)))
count = 0
times_sub <- times[times > times[13] & times < times[46]]
for (t in times_sub)
{
  count <-count + 1
  print(t)
  j <- joined %>%
    filter(format(StartTime, "%H:%M:%S") == t)
  
  zoning_sub <- j[,c('start_zone_type','end_zone_type')]
  zoning_sub$end_geo <- NULL
  complete_joined <- na.omit(zoning_sub)
  
  library(plyr)
  combination_counts <- data.frame(count(complete_joined, c('start_zone_type','end_zone_type')))

  # plot hourly means
  fname = paste("alluvia2/",count,".png",sep = '')
  ggplot(as.data.frame(combination_counts), aes(y = freq, axis1 = start_zone_type, axis2 = end_zone_type)) +
    geom_alluvium(aes(fill = start_zone_type), width = 1/12) +
    geom_stratum(width = 1/12, fill = "black", color = "grey") +
    geom_label(stat = "stratum", label.strata = TRUE) +
    scale_x_discrete(limits = c("Start Zoning", "End_Zoning"), expand = c(.05, .05)) +
    ggtitle(paste("Scooter Movements: ",as.character(t)))
            
  ggsave(filename = fname)
}

### Transit analysis

# group by hour, tally transit usage
weekdays <- joined_wday[joined_wday$weekend == FALSE,]
j <- weekdays
j[c('start_geo','end_geo','geometry')] <- NULL
j <- data.frame(j)

### overall usage stats
tu <- j %>%
  group_by(start_transit,end_transit) %>%
  tally()
tu$transit_usage <- mapply(tt, tu$start_transit, tu$end_transit)
tu$proportion <- tu$n / sum(tu$n)
tud <- data.frame(tu$transit_usage,tu$n,tu$proportion)
colnames(tud) <- c("Transit Usage Category", "Frequency", "Proportion")
print(xtable(tud[order(tud$Proportion, decreasing = TRUE),], caption = "Frequency of transit usage category, ordered by decreasing frequency"), include.rownames = FALSE)
print(xtable(data.frame(sort(unique(tud$`Transit Usage Category`))), caption = "Categories of relationship to transit"), include.rownames = FALSE)

# by hour stats
transit_by_hour <- j %>%
  group_by(format(StartTime, "%H:%M:%S"),start_transit,end_transit) %>%
  tally()
colnames(transit_by_hour)[1] <- 'time_of_day'
tt <- function(start, end){
  if((start == FALSE) & (end == FALSE))return("1. No Transit")
  if((start == TRUE) & (end == TRUE))return("4. Start and End Near Transit")
  if((start == TRUE) & (end == FALSE))return("2. Start Near Transit")
  if((start == FALSE) & (end == TRUE))return("3. End Near Transit")
  if(TRUE)return("ERROR")
}
transit_by_hour$transit_usage <- mapply(tt, transit_by_hour$start_transit, transit_by_hour$end_transit)
date = as.character("2018-07-10")
transit_by_hour$time_of_day <- as.POSIXct(paste(date, as.character(transit_by_hour$time_of_day)), format="%Y-%m-%d %H:%M:%S")

lims <- as.POSIXct(strptime(c("2018-07-10 06:00:00","2018-07-10 22:00:00"), format = "%Y-%m-%d %H:%M:%S"))    

ggplot(data = transit_by_hour, aes(x = time_of_day, y = n, fill = transit_usage)) +
  geom_col(position = 'fill') +
  scale_x_datetime(limits = lims, labels = date_format("%H:%M"), breaks = date_breaks(width = "2 hours")) +
  labs(title = "Proportion of trips that begin or end within 200m of a light rail station by time of day",
       subtitle = "Weekends Only",
       y = "Proportion",
       x = "Time of Day")+
  scale_fill_discrete(name = "Ride Relationship with Transit")

### Commuter analysis
weekdays <- joined_wday[joined_wday$weekend == TRUE,]
j <- weekdays
j[c('start_geo','end_geo','geometry')] <- NULL
j <- data.frame(j)

find_commuters <- function(start, end){
  if(((start == 'Residential (LD)' | start == 'Residential (HD)')) & ((end == 'Downtown') | (end == 'Industrial') | (end == 'Institutional Office') | (end == 'Non-Institutional Office')))return('Journey to Work')
  if(((end == 'Residential (LD)' | end == 'Residential (HD)')) & ((start == 'Downtown') | (start == 'Industrial') | (start == 'Institutional Office') | (start == 'Non-Institutional Office')))return('Journey from Work')
  
  if(TRUE) return(FALSE)
}
commuter_data <- na.omit(j)
commuter_data$commute <- mapply(find_commuters, commuter_data$start_zone_type, commuter_data$end_zone_type)
df <- data.frame(table(commuter_data$commute))
df$pct <- df$Freq / sum(df$Freq)
colnames(df) <- c("Commute","Freq","Weekend Proportion")
commute_by_hour <- commuter_data %>%
  group_by(format(StartTime, "%H:%M:%S"),commute) %>%
  tally()

date = as.character("2018-07-10")
colnames(commute_by_hour)[1] <- 'time_of_day'
commute_by_hour$time_of_day <- as.POSIXct(paste(date, as.character(commute_by_hour$time_of_day)), format="%Y-%m-%d %H:%M:%S")

lims <- as.POSIXct(strptime(c("2018-07-10 06:00:00","2018-07-10 22:00:00"), format = "%Y-%m-%d %H:%M:%S"))    

wkend <- ggplot(data = commute_by_hour, aes(x = time_of_day, y = n, fill = commute)) +
  geom_col(position = 'fill') +
  scale_x_datetime(limits = lims, labels = date_format("%H:%M"), breaks = date_breaks(width = "2 hours")) +
  labs(title = "Weekend",
       y = "Proportion",
       x = "Time of Day")+
  scale_fill_discrete(name = "Commute")

weekend <- c(df_weekend$`Weekend Proportion`[1],df_weekend$`Weekend Proportion`[2] + df_weekend$`Weekend Proportion`[3] )
weekday <- c(df_weekday$`Weekday Proportion`[1],df_weekday$`Weekday Proportion`[2] + df_weekday$`Weekday Proportion`[3] )
df <- data.frame(Day_Type = c("Weekend","Weekday"), Non_Commute_Pct = c(weekend[1], weekday[1]), Commute_Pct = c(weekend[2], weekday[2]))
print(xtable(df, caption = "Proportion of commuting trips, by type of day"), include.rownames = FALSE)


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

leg <- g_legend(wkend)
require(gridExtra)
grid.arrange(wkday,wkend,ncol=2)

p3 <- grid.arrange(wkday + theme(legend.position="none"),
                  wkend + theme(legend.position="none"),
                   leg, ncol=3)
p3
