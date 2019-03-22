library(sf)
library(readr)
library(ggplot2)

# import household income data
hh_income <- read_csv("~/Google Drive/Google Drive Personal/College/Fall 2018/Thesis/Analysis/data/census_household_income/census_block_income_data/nhgis0001_ds233_20175_2017_blck_grp.csv")

hh_income <- hh_income %>%
  mutate(join_id = paste(STATEA,COUNTYA,TRACTA,BLKGRPA,sep = '')) %>% # remove the preceding 'G' from the variable
  select(join_id,AH1PE001)

# import block shapefile data
block_groups <- st_read("data/census_household_income/census_block_shapefiles/tl_2018_27_bg.shp")

block_groups <- block_groups %>%
  select(GEOID, geometry)

# join them
income_by_block_group <- left_join(block_groups,hh_income,by = c("GEOID" = "join_id"))

# TAZ data

# Minneapolis boundary data
mpls_boundary <- st_read("data/minneapolis_boundary/msvcGIS_MinneapolisCityLimits.shp")

taz_data <- st_read("data/taz_shapefiles/TAZOfficialWCurrentForecasts.shp")

# standardize crs
crs = st_crs(mpls_boundary)
taz_data <- st_transform(taz_data,crs = crs)

contains <- st_contains(mpls_boundary,taz_data) %>%
  as.matrix() %>%
  t() %>%
  as.list()

taz_data$mpls <- contains
taz_data <- taz_data %>%
  filter(mpls == TRUE)

# aggregate the data using the approach here: https://geocompr.robinlovelace.net/spatial-operations.html#spatial-aggr

# adjust the coordinate reference system for census data to match that of the TAZ
income_by_block_group <- st_transform(income_by_block_group,crs = crs)

taz_with_income = taz_data %>%
  st_join(income_by_block_group) %>%
  group_by(TAZ) %>%
  summarize(average_income = mean(AH1PE001, na.rm = TRUE))

summary(taz_with_income$average_income)

taz_with_income$income_group <- cut(x = taz_with_income$average_income, breaks = c(0, 43398, 56846, 79641, 164650))
levels(taz_with_income$income_group) <- c('1st quartile','2nd quartile','3rd quartile','4th quartile')

plot(taz_with_income['income_group'], main = "TAZ by Income Quartile")
# get scooter ride data
# import trip data from csv
enriched_scooter_trips <- read_csv("~/Google Drive/Google Drive Personal/College/Fall 2018/Thesis/Analysis/enriched_scooter_trips.csv", col_types = cols(EndTime = col_datetime(format = "%Y-%m-%d %H:%M:%S"), StartTime = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

# count the number of NAs
sum(is.na(enriched_scooter_trips))

# remove rows that contain an NA
enriched_scooter_trips <- enriched_scooter_trips[complete.cases(enriched_scooter_trips),]

# enrich the trip data with start and end income categories
trips_with_income <- enriched_scooter_trips %>%
  left_join(taz_with_income, by = c("Start_TAZ" = "TAZ")) %>%
  mutate(start_income_group = income_group) %>%
  select(TripID,TripDuration,TripDistance,StartTime,EndTime,Start_TAZ,End_TAZ,start_income_group) %>%
  left_join(taz_with_income, by = c("End_TAZ" = "TAZ")) %>%
  mutate(end_income_group = income_group) %>%
  select(TripID,TripDuration,TripDistance,StartTime,EndTime,Start_TAZ,End_TAZ,start_income_group,end_income_group)

# number of trips starting in each income group area
table(trips_with_income$start_income_group)

# I have a feeling that this is heavily skewed by the trips that occured near the UMN campus
ggplot(data = trips_with_income, aes(x = start_income_group)) +
  geom_bar()


