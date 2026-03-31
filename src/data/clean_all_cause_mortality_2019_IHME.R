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



load("data/raw/sfba.RData")

# load mortality data, limit to needed columns and rows
acm <- read_csv("data/raw/baseline_health_outcomes/IHME_USA_COD_COUNTY_RACE_ETHN_2000_2019_MX_2019_ALL_BOTH_Y2023M06D12.CSV")

# remove age-standardized estimates
acm <- acm[!acm$age_name == "Age-standardized", c(4,5,11,9,7,13,14,16:19)]

# recode variables for consistency across health outcome datasets
acm$location_name <- gsub("\\s*County \\(California\\)\\s*", "", acm$location_name)

# limit to counties in study area (SFBA)
acm <- acm[acm$location_name %in% sfba, ]

# recode race category names for consistency across datasets and for visualization and shinyapp labels
acm[acm$race_name == "AIAN", 'race_name'] <- "American Indian / Alaskan Native"
acm[acm$race_name == "API", 'race_name'] <- "Asian / Pacific Islander"
acm[acm$race_name == "Latino", 'race_name'] <- "Hispanic"

acm[acm$age_name == "All Ages", "age_name"] <- "All ages"


acm$geolevl <- "county"
acm$cause_name <- "All-cause mortality"
acm$source <- "IHME"
acm$mx_name <- "person-year at risk"
acm$q_flag <- 0
acm$year <- as.character(acm$year)


acm <- acm[, c(2, 12,1,3:7, 13, 14, 9, 11,10, 15)]
# change column names for consistency across health data sets
colnames(acm) <- c("geoid",
                   "geolevl",
                   "lctn_nm",
                   "age_grp",
                   "sex_grp",
                   "race_grp",
                   "otcm_nm", 
                   "year",          
                   "source",
                   "mx_name",
                   "mx",
                   "mx_lower",          
                   "mx_upper",
                   "q_flag"
)


# add state code to geoid
acm$geoid <- str_pad(acm$geoid, width = 5, side = "left", pad = "0")


#write csv files to clean data folder
write_csv(acm, "data/processed/ac_mortality_county_2019_IHME.csv", append = FALSE)

rm(acm)
gc()


