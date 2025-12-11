# This script prepares 2019 IHME county-level, all-cause mortality by sex, race and age group data for analysis
# Dataset used may be downloaded from https://ghdx.healthdata.org/record/ihme-data/united-states-causes-death-life-expectancy-by-county-race-ethnicity-2000-2019
# County cartographic boundary shapefiles (2024) may be downloaded from https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html 
#   
library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(stringr)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)

# load county shapefiles
counties <- st_read("data/raw/county_census_cb_2024.shp")
counties <- st_as_sf(counties, crs = 4326)
counties <- counties[, 2]
colnames(counties) <- c("fips", "geometry")


load("data/raw/sfba.RData")

# load mortality data, limit to needed columns and rows
acm <- read_csv("data/raw/IHME_USA_COD_COUNTY_RACE_ETHN_2000_2019_MX_2019_ALL_BOTH_Y2023M06D12.CSV")
acm <- acm[, c(4,5,11,9,7,13,14,16:19)]

# recode variables for consistency across health outcome datasets
acm$location_name <- gsub("\\s*County \\(California\\)\\s*", "", acm$location_name)

# limit to counties in study area (SFBA)
acm <- acm[acm$location_name %in% sfba, ]

# recode race category names for consistency across datasets and for visualization and shinyapp labels
acm[acm$race_name == "AIAN", 'race_name'] <- "American Indian / Alaskan Native"
acm[acm$race_name == "API", 'race_name'] <- "Asian / Pacific Islander"
acm[acm$race_name == "Latino", 'race_name'] <- "Hispanic"

acm$race_name <- factor(acm$race_name, levels = c("American Indian / Alaskan Native", "Black", "White", "Hispanic", "Asian / Pacific Islander", "Total"))  

acm[acm$age_name == "All Ages", "age_name"] <- "All ages"

acm$age_name <- factor(acm$age_name, levels = c("<1 year", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",        
                                                  "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",        
                                                  "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",        
                                                  "85 plus", "All ages", "Age-standardized"))  
acm$cause_name <- "All-cause mortality"

# change column names for consistency across health data sets
colnames(acm) <- c("location_name",
                   "fips",
                   "age_name",
                   "sex_name",
                   "race_name",
                   "outcome_name", 
                   "year",          
                   "metric_name",
                   "mx",
                   "uCI",          
                   "lCI"
)

# multiply mx by 100000 to calculate deaths per 100000 pop
acm$mx_100k <- round(acm$mx*100000, 0)
acm$lCI_100k <- round(acm$lCI*100000, 0)
acm$uCI_100k <- round(acm$uCI*100000, 0)

# add a zero to left side of fips for consistency with other datasets
acm$fips <- str_pad(acm$fips, width = 5, side = "left", pad = "0")

#write csv files to clean data folder
write_csv(acm, "data/processed/ac_mortality_county_2019_IHME.csv", append = FALSE)


# make shapefiles
acm_shp <- merge(acm, counties, by = "fips")

# write shapefiles to processed data folder
st_write(acm_shp, "data/processed/ac_mortality_county_2019_IHME.shp", append = FALSE)
