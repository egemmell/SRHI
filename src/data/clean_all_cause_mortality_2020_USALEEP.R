# USA Mortality Incidence Data (BenMap-ready)
# Row = state-county FIPS (5 digits), Column = tract FIPS (6 digits)
# Concatenated to generate full 11-digit tract GEOID
# 2020 mortality estimates (rates from 2010-2015, updated to 2020 population) with 2010 census tract geography

library(tidyverse)
library(stringr)

source("config.R")

ac <- read_csv("data/raw/baseline_health_outcomes/BenMAP_Ready_USALEEP_AllCauseRates_2020.csv") |>
  mutate(
    # pad and combine FIPS codes into full tract GEOID
    Row    = str_pad(Row,    width = 5, side = "left", pad = "0"),
    Column = str_pad(Column, width = 6, side = "left", pad = "0"),
    geoid  = paste0(Row, Column),
    # create age groups using case_when instead of repeated row assignment
    age_grp = case_when(
      `Start Age` ==  0 & `End Age` ==  0 ~ "<1 year",
      `Start Age` ==  1 & `End Age` ==  4 ~ "1 to 4",
      `Start Age` ==  5 & `End Age` == 14 ~ "5 to 14",
      `Start Age` == 15 & `End Age` == 24 ~ "15 to 24",
      `Start Age` == 25 & `End Age` == 34 ~ "25 to 34",
      `Start Age` == 35 & `End Age` == 44 ~ "35 to 44",
      `Start Age` == 45 & `End Age` == 54 ~ "45 to 54",
      `Start Age` == 55 & `End Age` == 64 ~ "55 to 64",
      `Start Age` == 65 & `End Age` == 74 ~ "65 to 74",
      `Start Age` == 75 & `End Age` == 84 ~ "75 to 84",
      `Start Age` == 85 & `End Age` == 99 ~ "85 to 99",
      .default = NA_character_
    ),
    sex_grp      = "Both",
    race_grp     = "Total",
    geolevl      = "tract",
    lctn_nm      = paste(Row, Column),
    year         = "2020",
    otcm_nm      = "All-cause mortality",
    source       = "USALEEP",
    mx_name      = "person-years at risk",
    mx_lower     = NA_real_,
    mx_upper     = NA_real_,
    q_flag       = 0
  ) |>
  # limit to SFBA tracts
  filter(
    str_sub(geoid, 1, 2) == ca_fips,
    str_sub(geoid, 3, 5) %in% sfba_fips_3
  ) |>
  select(
    geoid,
    geolevl,
    lctn_nm,      # full 11-digit geoid
    age_grp,
    sex_grp,
    race_grp,
    otcm_nm,
    year,
    source,
    mx_name,
    mx       = Value,      # correct column name
    mx_lower,
    mx_upper,
    q_flag
  )

write_csv(ac, "data/processed/ac_mortality_ct_tract_2020_USALEEP.csv", append = FALSE)
rm(ac)
gc()
