############################################################################################
# This script cleans data on number of ER visits and hospitalizations in 2019, for children 0-17 (inclusive) residing in the 
# San Francisco Bay Area, for children, ER visits or hospitalizations were included if the primary diagnosis was one of the 
# ICD-10 codes for acute lower respiratory infections,
# including those for influenza, pneumonia and other acute respiratory infections (J09-J18, J20-J22).
# The custom dataset was requested from the HCAi Department of Health Care Access and Information Patient Discharge (PDD)
# and Emergency Department and Ambulatory Surgery (EDAS) Datasets.
#############################################################################################
# clean ALRI data

library(readr)
library(sf)
library(dplyr)

# load county and census tract shapefiles
counties <- st_read("data/raw/cb_2024_06_tract_500k.shp")
counties <- st_as_sf(counties, crs = 4326)
counties <- counties[, 2]
colnames(counties) <- c("fips", "geometry")

# read in csv data file
alri <- read.csv("data/raw/ALRI_2019_ CS3044.csv", encoding = "UTF-8")
alri <- as.data.frame(alri)

# We will not differentiate between ER visits and hospital admissions for this analysis,
# so remove Patient Type variable. We will also omit the cost variables for this dataset.
alri <- alri[, -c(2,6,7)]

# Change column names
colnames(alri) <- c("location_name",  "race_name",            "sex_name",             "mx")

# In the HCAi dataset, actual number of visits for a subgroup was recoded to "<11" if 10 or below for patient privacy
# Recode to numeric 10 for analysis
alri[alri$mx == "<11", "mx"] <- "10"
alri$mx <- as.numeric(alri$mx)

# sum the ER visit and Hospital admission counts by county, race and sex
alri <- alri %>%
  group_by(location_name, race_name, sex_name) %>%
  summarize(mx = sum(mx, na.rm = TRUE),
            .groups = "keep")

# calculate total visits for males and females combined
both <- alri %>%
  group_by(location_name, race_name) %>%
  summarize(mx = sum(mx, na.rm = TRUE),
                .groups = "keep")

both$sex_name <- "Both"

# add the rows with combined male and female totals to the alri dataset
alri <- rbind(alri, both)

# calculate the total visits for all races combined - HCAI data comes in pre-defined race groups
# White, Black, Asian, American Indian / Alaskan Native, Native Hawaiian or other Pacific Islander, 
#Multi-racial and Other/Unknown. Because of small cell sizes, categories were recoded to: White, Black, 
# Asian/Pacific Islander, Hispanic, Other (where 'Other' are all other categories) before data was released.
# To calculate county level incidence, we merge US Census 2020 unbridged single-year population estimates were used to estimate prevalence of alri in 2019 
# for children 0-17 years by sex, and race/ethinicity. Only race/ethnicity categories White, Black, Asian/Pacific
# Islander and Hispanic were present in both the ALRI and US Census 2020 single-year population datasets, so we
# were unable to estimate prevalence for other race groups (e.g. American Indian / Alaskan Native). However, we
# calculated an overall prevalence for all race/ethnicities which includes these groups.

# add a "Total" category in race_name variable and sum visits for all races by county and sex
allraces <- alri %>%
  group_by(location_name, sex_name) %>%
  summarize(mx = sum(mx, na.rm = TRUE),
            .groups = "keep")
allraces$race_name <- "Total"

# add the rows with combined race data to the alri dataset
alri <- rbind(alri, allraces)

# recode to match population data variables
alri[alri$race_name == "Asian/Pacific Islander", "race_name"] <- "Asian / Pacific Islander"


# load county and census tract shapefiles
counties <- st_read("data/raw/counties_and_centroids.shp")
counties <- st_as_sf(counties, crs = 4326)

# make a dataframe for merge with alri data (to add county fips code column)
fips <- counties[, c(2,3)] %>%
  st_drop_geometry(.)
colnames(fips) <- c("fips", "location_name")

# add fips code column
alri <- merge(fips, alri, all.y = TRUE)

# add age_name, metric_name, lCI and uCI columns to standardize with other health datasets
alri$age_name <- "0-17"
alri$outcome_name <- "acute lower respiratory infection"
alri$year <- "2019"
alri$metric_name <- "count"
alri$lCI <- NA
alri$uCI <- NA

alri <- alri[, c(1,2,6,4,3,7,8,9,5,10,11)]

# load demographic data (e.g. population counts) for children ages 0-17
demo <- read_csv("data/raw/pop_0_17_sex_race_county_2019.csv")


# merge alri data with population data to calculate and estimated incidence rate - this will likely be an underestimate since 
# alri diagnosed and treated in non-hospital clinical settings are not captured. 

# remove non-corresponding race categories from alri ("Other") and demo ("American Indian / Alaskan Native) datasets
alri <- alri[!alri$race_name == "Other", ]
demo <- demo[!demo$race_name == "American Indian / Alaskan Native", ]
alri <- merge(alri, demo)
colnames(alri) <- c("location_name",
                    "fips",
                    "age_name",
                    "sex_name",
                    "race_name",
                    "year",
                    "outcome_name",
                    "metric_name",   
                    "count",            
                    "lCI",
                    "uCI",
                    "population")
# calculate alri EDvisits/hospitalizations per 1,000 children 0-17 by county and race/ethnicity  
alri$mx <- (alri$count/alri$population)*1000 
alri$metric_name <- "Rate per 1,000 children"

alri <- alri[, c(1:7,8,13,10,11,9,12)]

write_csv(alri, "data/processed/alri_county_2019_HCAI.csv")

# make shapefiles
alri <- merge(alri, counties, by = "fips")

st_write(alri, "data/processed/alri_county_2019_HCAI.shp")

rm(alri, counties)
gc()
