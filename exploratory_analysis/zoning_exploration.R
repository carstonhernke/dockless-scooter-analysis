# zoning analysis/ exploration

library(dplyr)
library(sf)
library(readr)
library(ggplot2)

### import zoning data
zoning_data <- st_read("data/minneapolis_zoning/Planning_Primary_Zoning.shp")
plot(zoning_data['ZONE_CODE'])

# enrich with the type of zoning (by taking first character from zone)
zoning_data_enriched <- zoning_data %>%
  mutate(zone_type = substr(ZONE_CODE, 1, 1))

plot(zoning_data_enriched['zone_type'])
