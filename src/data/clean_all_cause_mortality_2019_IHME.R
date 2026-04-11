# This script prepares 2019 IHME county-level, all-cause mortality by sex, race and age group data for analysis
# Dataset: https://ghdx.healthdata.org/record/ihme-data/united-states-causes-death-life-expectancy-by-county-race-ethnicity-2000-2019
# County cartographic boundary shapefiles (2024): https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html

library(tidyverse)
library(sf)
library(stringr)

source("config.R")

acm <- read_csv("data/raw/baseline_health_outcomes/IHME_USA_COD_COUNTY_RACE_ETHN_2000_2019_MX_2019_ALL_BOTH_Y2023M06D12.CSV") |>
  filter(age_name != "Age-standardized") |>
  select(fips, location_name, age_name, sex_name, race_name, cause_name, year, val, lower, upper) |>
  mutate(
    location_name = str_remove(location_name, "\\s*County \\(California\\)\\s*"),
    race_name = recode(race_name,
                       "AIAN"   = "American Indian / Alaskan Native",
                       "API"    = "Asian / Pacific Islander",
                       "Latino" = "Hispanic"
    ),
    age_name   = if_else(age_name == "All Ages", "All ages", age_name),
    geolevl    = "county",
    cause_name = "All-cause mortality",
    source     = "IHME",
    mx_name    = "person-year at risk",
    q_flag     = 0,
    year       = as.character(year),
    fips       = str_pad(fips, width = 5, side = "left", pad = "0")
  ) |>
  filter(location_name %in% sfba_names) |>
  select(
    geoid    = fips,
    geolevl,
    lctn_nm  = location_name,
    age_grp  = age_name,
    sex_grp  = sex_name,
    race_grp = race_name,
    otcm_nm  = cause_name,
    year,
    source,
    mx_name,
    mx       = val,
    mx_lower = lower,
    mx_upper = upper,
    q_flag
  )

#write csv files to clean data folder
write_csv(acm, "data/processed/ac_mortality_county_2019_IHME.csv", append = FALSE)

rm(acm)
gc()


