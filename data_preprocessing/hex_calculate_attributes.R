# 5.2: Equity Analysis
# Hypothesis: There is a relationship between the household income of a zone and the net availability of scooters within that zone throughout the day
library(chron)
library(sf)
library(dplyr)
library(readr)
library(areal)

### DATA SETUP ###

### import existing hex data
hex_data <- st_read("hex_grid.shp")
crs = st_crs(hex_data)

### import block shapefile data
block_groups <- st_read("data/census_household_income/census_block_shapefiles/tl_2018_27_bg.shp")

block_groups <- block_groups %>%
  select(GEOID, geometry)

## INCOME DATA
# import household income data
hh_income <- read_csv("data/census_household_income/census_block_income_data/nhgis0001_ds233_20175_2017_blck_grp.csv")

# create join_id for income data
hh_income <- hh_income %>%
  mutate(join_id = paste(STATEA,COUNTYA,TRACTA,BLKGRPA,sep = '')) %>% # remove the preceding 'G' from the variable
  select(join_id,AH1PE001)

# join the income data with the block group spatial data
income_by_block_group <- left_join(block_groups,hh_income,by = c("GEOID" = "join_id"))

# adjust the coordinate reference system for census data to match that of the hex
income_by_block_group <- st_transform(income_by_block_group,crs = crs)

# perform the spatial join, averaging the block groups within each hex
hex_with_income <-  hex_data %>%
  st_join(income_by_block_group) %>%
  group_by(HEX_ID) %>%
  summarize(average_income = mean(AH1PE001, na.rm = TRUE))

st_write(hex_with_income,"hex_with_income.shp", delete_dsn = TRUE)

## POPULATION DATA
# import population data
pop_data <- read_csv("data/census-population-students/nhgis0003_ds233_20175_2017_blck_grp.csv")

# create join_id for income data
pop_data <- pop_data %>%
  mutate(join_id = paste(STATEA,COUNTYA,TRACTA,BLKGRPA,sep = '')) %>% # remove the preceding 'G' from the variable
  select(join_id,AHY1E001)

# join the income data with the block group spatial data
population_by_block_group <- left_join(block_groups,pop_data,by = c("GEOID" = "join_id"))

# adjust the coordinate reference system for census data to match that of the hex
population_by_block_group <- st_transform(population_by_block_group,crs = crs)
population_by_block_group$AHY1E001 <- as.numeric(population_by_block_group$AHY1E001)

# perform the spatial join using Areal Interpolation
hex_data <- st_transform(hex_data, crs = 26915)
population_by_block_group <- st_transform(population_by_block_group, crs = 26915)

hex_with_population <-  hex_data %>%
  aw_interpolate('HEX_ID', population_by_block_group, 'GEOID', weight = "total", output = 'sf', extensive = "AHY1E001") %>%
  mutate(population = AHY1E001) %>%
  select(HEX_ID, population)

st_write(hex_with_population,"hex_with_population.shp", delete_dsn = TRUE)

## STUDENT ENROLLMENT DATA
# import student enrollment data
enrollment_data <- read_csv("~/Google Drive/Google Drive Personal/College/Fall 2018/Thesis/dockless-scooter-analysis/data/census-population-students/nhgis0003_ds233_20175_2017_blck_grp.csv", 
                            col_types = cols(AH0TE017 = col_number(), 
                                             AH0TE018 = col_number()))

# add the two counts of students (undergrad and post-undergrad) together
enrollment_data$college_students <- enrollment_data$AH0TE017 + enrollment_data$AH0TE018

# create join_id for income data
enrollment_data <- enrollment_data %>%
  mutate(join_id = paste(STATEA,COUNTYA,TRACTA,BLKGRPA,sep = '')) %>% # remove the preceding 'G' from the variable
  select(join_id,college_students)

# join the income data with the block group spatial data
students_by_block_group <- left_join(block_groups,enrollment_data,by = c("GEOID" = "join_id"))

# adjust both the coordinate reference systems to be planar
hex_data <- st_transform(hex_data, crs = 26915)
students_by_block_group <- st_transform(students_by_block_group, crs = 26915)

# perform the spatial join using Areal Interpolation
hex_with_students <-  hex_data %>%
  aw_interpolate('HEX_ID', students_by_block_group, 'GEOID', weight = "total", output = 'sf', extensive = "college_students")

st_write(hex_with_students,"hex_with_students.shp", delete_dsn = TRUE)

hex_with_population$geometry <- NULL
hex_with_students$geometry <- NULL

hex_with_attributes <- hex_with_income %>%
  left_join(hex_with_population, by = c("HEX_ID" = "HEX_ID")) %>%
  left_join(hex_with_students, by = c("HEX_ID" = "HEX_ID"))

st_write(hex_with_attributes,"hex_with_attributes.shp", delete_dsn = TRUE)

plot(hex_with_attributes['average_income'], main = "")
plot(hex_with_attributes['population'], main = "")
plot(hex_with_attributes['college_students'], main = "")
