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


# read in csv data file
alri <- read.csv("data/raw/baseline_health_outcomes/ALRI_2019_ CS3044.csv", encoding = "UTF-8")
alri <- as.data.frame(alri)

# We will not differentiate between ER visits and hospital admissions for this analysis,
# so remove Patient Type variable. We will also omit the cost variables for this dataset.
alri <- alri[, -c(2,6,7)]

# Change column names
colnames(alri) <- c("lctn_nm",  "race_grp",            "sex_grp",             "mx")

# In the HCAi dataset, actual number of visits for a subgroup was recoded to "<11" if 10 or below for patient privacy
# Recode to numeric 10 for analysis
alri[alri$mx == "<11", "mx"] <- "10"
alri$mx <- as.numeric(alri$mx)

# sum the ER visit and Hospital admission counts by county, race and sex
alri <- alri %>%
  group_by(lctn_nm, race_grp, sex_grp) %>%
  summarize(mx = sum(mx, na.rm = TRUE),
            .groups = "keep")

# calculate total visits for males and females combined
both <- alri %>%
  group_by(lctn_nm, race_grp) %>%
  summarize(mx = sum(mx, na.rm = TRUE),
                .groups = "keep")

both$sex_grp <- "Both"

# add the rows with combined male and female totals to the alri dataset
alri <- rbind(alri, both)

# calculate the total visits for all races combined - HCAI data comes in pre-defined race groups
# White, Black, Asian, American Indian / Alaskan Native, Native Hawaiian or other Pacific Islander, 
#Multi-racial and Other/Unknown. Because of small cell sizes, categories were recoded to: White, Black, 
# Asian/Pacific Islander, Hispanic, Other (where 'Other' are all other categories) before data was released.
# To calculate county level incidence, we merge US Census 2019 unbridged single-year population estimates were used to estimate prevalence of alri in 2019 
# for children 0-17 years by sex, and race/ethinicity. Only race/ethnicity categories White, Black, Asian/Pacific
# Islander and Hispanic were present in both the ALRI and US Census 2020 single-year population datasets, so we
# were unable to estimate prevalence for other race groups (e.g. American Indian / Alaskan Native). However, we
# calculated an overall prevalence for all race/ethnicities which includes these groups.

# add a "Total" category in race_name variable and sum visits for all races by county and sex
allraces <- alri %>%
  group_by(lctn_nm, sex_grp) %>%
  summarize(mx = sum(mx, na.rm = TRUE),
            .groups = "keep")
allraces$race_grp <- "Total"

# add the rows with combined race data to the alri dataset
alri <- rbind(alri, allraces)

# recode to match population data variables
alri[alri$race_grp == "Asian/Pacific Islander", "race_grp"] <- "Asian / Pacific Islander"


# load county and census tract shapefiles
counties <- st_read("data/raw/census_boundaries/tiger_line_shapefiles/2019/tl_2019_06_sfbacounty_cleaned.shp")

# make a dataframe for merge with alri data (to add county fips code column)
fips <- counties[, c(1,2)] %>%
  st_drop_geometry(.)
colnames(fips) <- c("geoid", "lctn_nm")

# add fips code column
alri <- merge(fips, alri, all.y = TRUE)

# add age_name, metric_name, lCI and uCI columns to standardize with other health datasets
alri$geolevl <- "county"
alri$age_grp <- "0 to 17"
alri$otcm_nm <- "acute lower respiratory infection"
alri$year <- "2019"
alri$source <- "HCAi"
alri$mx_lower <- NA
alri$mx_upper <- NA
alri$q_flag <- 0

alri <- alri[, c(2,6,1,7,4,3,8:10,5,11:13)]

# load demographic data (e.g. population counts) for children ages 0-17
demo <- read_csv("data/raw/population_data/pop_0_17_sex_race_county_2019.csv")
demo <- demo[, c(1,4,5,7)]

# merge alri data with population data to calculate and estimated incidence rate - this will likely be an underestimate since 
# alri diagnosed and treated in non-hospital clinical settings are not captured. 

# remove non-corresponding race categories from alri ("Other") and demo ("American Indian / Alaskan Native) datasets
alri <- alri[!alri$race_grp == "Other", ]
demo <- demo[!demo$race_grp == "American Indian / Alaskan Native", ]
alri$geoid <- paste0("06", alri$geoid)

alri <- merge(alri, demo)

# calculate alri EDvisits/hospitalizations per child 0-17 by county and race/ethnicity  
alri$mx <- (alri$mx/alri$population)

alri<- alri[, c(1,4,5,6,2,3,7:13)]


write_csv(alri, "data/processed/alri_county_2019_HCAI.csv")



# make shapefiles
#alri <- merge(alri, counties, by = "fips")
#alri <- alri[, -c(12:17)]

#alri$mx_name <- "person-year at risk"
#alri <- alri[, c(1:9, 14, 10:13)]

write_csv(alri, "data/processed/alri_county_2019_HCAI.csv", append = FALSE)

#st_write(alri, "data/processed/alri_county_2019_HCAI.shp", append = FALSE)

rm(alri, counties)
gc()
