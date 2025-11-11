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

# load county and census tract shapefiles
counties <- st_read("data/raw/counties_and_centroids.shp")
counties <- st_as_sf(counties, crs = 4326)
counties <- counties[, 2]

ct_tract <- st_read("data/raw/ct_tract.shp")

# load mortality data, limit to needed columns and rows

acm <- read_csv("data/raw/IHME_USA_COD_COUNTY_RACE_ETHN_2000_2019_MX_2019_ALL_BOTH_Y2023M06D12.CSV")

acm <- acm[grep("California", acm$location_name), c(3:5,7,9,11,13,14,16:19)]

#standardize column names and limit to counties in the SFBA 

acm$location_name <- sub(" County (California)", "", acm$location_name, fixed = TRUE)

sfba <- c("Alameda", 
          "Contra Costa", 
          "Marin", 
          "Napa", 
          "San Francisco", 
          "San Mateo", 
          "Santa Clara", 
          "Solano", 
          "Sonoma")


acm <- acm[acm$location_name %in% sfba, ] 

acm[acm$race_name == "AIAN", 'race_name'] <- "American Indian / Alaskan Native"
acm[acm$race_name == "API", 'race_name'] <- "Asian / Pacific Islander"
acm[acm$race_name == "Latino", 'race_name'] <- "Hispanic"

acm$race_name <- factor(acm$race_name, levels = c("American Indian / Alaskan Native", "Black", "White", "Hispanic", "Asian / Pacific Islander", "Total"))  

acm$age_name <- factor(acm$age_name, levels = c("<1 year", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",        
                                                            "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",        
                                                            "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",        
                                                            "85 plus", "All Ages", "Age-standardized"))  



acm$cause_name <- "All-cause mortality"

#acm$mx <- acm$val * 100000
#acm$uCI <- acm$upper * 100000
#acm$lCI <- acm$lower * 100000

acm <- acm[, c(2:12)]
colnames(acm) <- c("location_name",
                         "fips",
                         "race_name",
                         "sex_name",
                         "age_name",
                         "outcome_name", 
                         "year",          
                         "metric_name",
                         "mx",
                         "uCI",          
                         "lCI"
                         )

acm$fips <- str_pad(acm$fips, width = 5, side = "left", pad = "0")

#write csv files to clean data folder
write_csv(acm, "data/processed/ac_mortality_county_2019.csv", append = FALSE)


#############################################################################
# Cause-specific mortality: CVD 2014

ihd <- read_csv("data/raw/IHME_USA_COUNTY_CVD_MORTALITY_RATES_1980_2014_CALIFORNIA_Y2017M05D16.CSV")

ihd <- ihd[ihd$year_id == 2014, ]
ihd <- ihd[ihd$cause_name == "Ischemic heart disease", ]

#standardize column names and limit to census tracts in the SFBA of interest

ihd$location_name <- sub(" County", "", ihd$location_name, fixed = TRUE)

sfba <- c("Alameda", 
          "Contra Costa", 
          "Marin", 
          "Napa", 
          "San Francisco", 
          "San Mateo", 
          "Santa Clara", 
          "Solano", 
          "Sonoma")

ihd$race_name <- "Total"

ihd <- ihd[ihd$location_name %in% sfba, c(4,5, 17, 9, 11, 7, 12:16)] 

ihd[ihd$age_name == "Age-standardized", "age_name"] <- "All ages"

ihd[ihd$metric == "Rate", "metric"] <- "Age-standardized rate"


# Calculate number of                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#ihd$mx <- ihd$mx * 100000
#ihd$upper <- ihd$upper * 100000
#ihd$upper <- ihd$lower * 100000

colnames(ihd) <- c("location_name",
                         "fips",
                         "race_name",
                         "sex_name",
                         "age_name",
                         "outcome_name", 
                         "year",          
                         "metric_name",
                         "mx",
                         "uCI",          
                         "lCI"
)



ihd$fips <- str_pad(ihd$fips, width = 5, side = "left", pad = "0")

#write csv files to clean data folder
write_csv(ihd, "data/processed/ihd_mortality_county_2014.csv", append = FALSE)


#############################################################################
# Cause-specific mortality Lung cancer 2014
lcan <- read_csv("data/raw/IHME_USA_COUNTY_CANCER_MORTALITY_RATES_1980_2014_CALIFORNIA_Y2017M01D24.CSV")

lcan <- lcan[lcan$year_id == 2014, ]

#standardize column names and limit to census tracts in the SFBA of interest
lcan$location_name <- sub(" County", "", lcan$location_name, fixed = TRUE)

lcan <- lcan[lcan$location_name %in% sfba, ] 

lcan$race_name <- "Total"
lcan$metric_name <- "Rate"
lcan$age_name <- "All ages"

lcan <- lcan[lcan$cause_name == "Tracheal, bronchus, and lung cancer", c(2,3,12,7,14,5,8,13,9,10,11)] 
lcan$metric_name <- "Rate"
lcan$cause_name <- "Tracheal, bronchus and lung cancer mortality"
colnames(lcan) <- c("location_name",
                   "fips",
                   "race_name",
                   "sex_name",
                   "age_name",
                   "outcome_name", 
                   "year",          
                   "metric_name",
                   "mx",
                   "uCI",          
                   "lCI"
)

lcan$fips <- str_pad(lcan$fips, width = 5, side = "left", pad = "0")
#write csv files to clean data folder

write_csv(lcan, "data/processed/lc_mortality_county_2014.csv")

####################################################################################################
# Lung cancer mortality 2019

lcan_b <- read_csv("data/raw/IHME_USA_LUNG_CANCER_COUNTY_RACE_ETHNICITY_2000_2019_MX_2019_BOTH_Y2025M06D15.csv")
lcan_b <- lcan_b[grep("California", lcan_b$location_name), c(3:5,7,9,11,13,14,16:19)]
lcan_b <- as.data.frame(lcan_b)

lcan_f <- read_csv("data/raw/IHME_USA_LUNG_CANCER_COUNTY_RACE_ETHNICITY_2000_2019_MX_2019_FEMALE_Y2025M06D15.csv")
lcan_f <- lcan_f[grep("California", lcan_f$location_name), c(3:5,7,9,11,13,14,16:19)]
lcan_f <- as.data.frame(lcan_f)

lcan_m <- read_csv("data/raw/IHME_USA_LUNG_CANCER_COUNTY_RACE_ETHNICITY_2000_2019_MX_2019_MALE_Y2025M06D15.csv")
lcan_m <- lcan_m[grep("California", lcan_m$location_name), c(3:5,7,9,11,13,14,16:19)]
lcan_m <- as.data.frame(lcan_m)


lcan <- bind_rows(lcan_b, lcan_f, lcan_m)

rm(lcan_b, lcan_f, lcan_m)

#standardize column names and limit to census tracts in the SFBA of interest

lcan$location_name <- sub(" County (California)", "", lcan$location_name, fixed = TRUE)

sfba <- c("Alameda", 
          "Contra Costa", 
          "Marin", 
          "Napa", 
          "San Francisco", 
          "San Mateo", 
          "Santa Clara", 
          "Solano", 
          "Sonoma")


lcan <- lcan[lcan$location_name %in% sfba, ] 

lcan[lcan$race_name == "AIAN", 'race_name'] <- "American Indian / Alaskan Native"
lcan[lcan$race_name == "Asian", 'race_name'] <- "Asian / Pacific Islander"
lcan[lcan$race_name == "Latino", 'race_name'] <- "Hispanic"

lcan$race_name <- factor(lcan$race_name, levels = c("American Indian / Alaskan Native", "Black", "White", "Hispanic", "Asian / Pacific Islander", "Total"))  

lcan$age_name <- factor(lcan$age_name, levels = c("<1 year", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",        
                                                "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",        
                                                "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",        
                                                "85 plus", "All Ages", "Age-standardized"))  
lcan$cause_name <- "Lung cancer mortality"

#lcan$mx <- lcan$val * 100000
#lcan$uCI <- lcan$upper * 100000
#lcan$lCI <- lcan$lower * 100000

lcan <- lcan[, c(2:12)]
colnames(lcan) <- c("location_name",
                   "fips",
                   "race_name",
                   "sex_name",
                   "age_name",
                   "outcome_name", 
                   "year",          
                   "metric_name",
                   "mx",
                   "uCI",          
                   "lCI"
)

lcan$fips <- str_pad(lcan$fips, width = 5, side = "left", pad = "0")

#write csv files to clean data folder
write_csv(lcan, "data/processed/lcan_mortality_county_2019.csv", append = FALSE)

###################################################################
# calculate the age-adjusted lung cancer mortality rates for census tracts, given 
# census tract race/ethnicity demographics

lc_cty <- read_csv("data/raw/californiahealthmaps_county_all.csv")
lc_cty <- lc_cty[lc_cty$Cancer == "Lung", -c(3,4)]
lc_cty <- lc_cty[lc_cty$Counties %in% sfba, ]
lc_cty <- lc_cty[lc_cty$Years  == "05yr", ]
lc_cty <- lc_cty[lc_cty$Sex  == "Both", ]




