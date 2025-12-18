# This script cleans IHD county-level mortality data for 10-year age groups, sex and race_ethnicity.
# Data was downloaded from CDC Wonder: 1999-2020 underlying cause of death by bridged race categories https://wonder.cdc.gov/ucd-icd10.html
# The CDC Wonder tool was used to select crude mortality rates from 2019, with ICD-10 codes for Ischemic Heart Disease 
# as the underlying cause of death (I20-I25). The saved data request may be accessed here: http://wonder.cdc.gov/controller/saved/D76/D459F919

library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(stringr)

# load county shapefiles
counties <- st_read("data/raw/cb_2024_us_county_500k.shp")
counties <- st_as_sf(counties, crs = 4326)
counties <- counties[,2]
colnames(counties) <- c("fips", "geometry")

# process ihd mortality rates by county
ihd <- read_csv("data/raw/ihd_mortality_county_no_strata_2019_CDC.csv")

# limit to relevant relevant and exclude metadata rows
ihd <- ihd[-c(10:61), c(3,2,6:9)]

# update column names
colnames(ihd) <- c("fips", "location_name", "mx", "lCI", "uCI", "se")

ihd$year <- "2019"
ihd$outcome_name <- "Ischemic heart disease mortality"
ihd$metric_name <- "Rate"

write_csv(ihd, "ihd_county_2019_CDC.csv")
# process crude ihd mortality rates stratified by age, sex and race/ethnicity
#ihd <- read_csv("data/raw/ihd_mortality_county_2019_CDC.csv")
ihd <- read_csv("data/raw/ihd_mortality_county_10y_2019_CDC.csv")
ihd <- ihd[-c(2593:2660), c(2,3,4,6,8,10,12,13:16)]

# suppressed death counts (deaths between 1-9) were recoded as "5", mid-point between 1-9.
ihd[ihd$Deaths == "Suppressed", "Deaths"] <- "5"
ihd$Deaths <- as.numeric(ihd$Deaths)

# remove rows where age and/or hispanic origin is not stated, or where population totals for these groups are suppressed or not available
# as we are unable to calculate a crude rate for these groups. 
ihd <- ihd[!ihd$Population == "Not Applicable", ]
ihd <- ihd[!ihd$Population == "Suppressed", ]

ihd$Population <- as.numeric(ihd$Population)

#recode Race category - if Hispanic origin is "Hispanic or Latino" change race to "Hispanic" for consistency
ihd[ihd$`Hispanic Origin`== "Hispanic or Latino", "Race"] <- "Hispanic"
ihd <- ihd[, -5]

# recode county variable
ihd$County <- str_remove(ihd$County, " County, CA")

# after removing hispanic origin column, sum deaths and population by other variables
ihd <- ihd %>%
  group_by(County, `County Code`, `Ten-Year Age Groups`, Sex, Race) %>%
  summarize(Deaths = sum(Deaths, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE)) 

# Find totals for both sexes by county, age and race/ethnicity
both <- ihd %>%
  group_by(County, `County Code`, `Ten-Year Age Groups`, Race) %>%
  summarize(Deaths = sum(Deaths, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE)) 
both$Sex <- "Both"
# rbind rows with both sex totals
ihd <- rbind(ihd, both)

# find totals for all ages by county, sex and race. 
ages <- ihd %>%
  group_by(County, `County Code`, Sex, Race) %>%
  summarize(Deaths = sum(Deaths, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE)) 

ages$'Ten-Year Age Groups' <- "All ages"
# rbind rows with all age groups
ihd <- rbind(ihd, ages)

# find totals for all races by county, age and sex
race <- ihd %>%
  group_by(County, `County Code`, Sex, `Ten-Year Age Groups`) %>%
  summarize(Deaths = sum(Deaths, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE)) 

race$Race <- "Total"

ihd <- rbind(ihd, race)

# Calculate crude mortality rate
ihd$mx <- (ihd$Deaths/ihd$Population)
ihd$lCI <- NA
ihd$uCI <- NA

# calculate crude deaths per 100,000 pop
ihd$mx_100k <- (ihd$Deaths/ihd$Population)*100000
ihd$lCI_100k <- NA
ihd$uCI_100k <- NA

# Calculate death and population totals by county and age group
county <- ihd %>%
  group_by(County, `Ten-Year Age Groups`) %>%
  summarize(Deaths = sum(Deaths),
            Population = sum(Population))

# calculate county age group-specific crude mortality rates (both sexes, all races)
county$mx <- county$Deaths/county$Population
county$mx_100k <- county$mx*100000
county$Sex <- "Both"
county$Race <- "Total"

ihd$outcome_name <- "Ischemic heart disease mortality"

ihd$metric_name <- "Rate"

ihd$year <- "2019"

ihd <- ihd[, -c(6,7)]

ihd <- ihd[, c(1:5,12,14,13, 6:11)]

# under 25 year olds had no lc deaths
children <- c("< 1 year", "1-4 years", "5-14 years","15-24 years")

ihd <- ihd[!ihd$`Ten-Year Age Groups` %in% children, ]

colnames(ihd) <- c("location_name", 
                   "fips", 
                   "age_name", 
                   "sex_name", 
                   "race_name", 
                   "outcome_name", 
                   "year", 
                   "metric_name", 
                   "mx",
                   "lCI",
                   "uCI",
                   "mx_100k",
                   "lCI_100k",
                   "uCI_100k")

# save processed data
write_csv(ihd, "data/processed/ihd_mortality_county_2019_CDC.csv", append = FALSE)

# make shapefiles
ihd <- merge(ihd, counties)
st_write(ihd, "data/processed/ihd_mortality_county_2019_CDC.shp", append = FALSE)
