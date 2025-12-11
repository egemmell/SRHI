
# This script downloads and cleans data from the California Health Interview Survey for adult and child asthma prevalence (2021-2022)
# by county.
# Adult asthma prevalence is additionally estimated at the census tract level, using CDC PLACES data, which uses the Behavioral Risk Factor Surveillance System (2019 or 2020 data),
# 2010 Census and the American Community Survey (2015-2019) to generate model-based population estimates

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(RColorBrewer)
library(httr)
library(jsonlite)
library(tibble)


fetch_all_ckan <- function(resource_id, query, limit = 500) {
  
  base_url <- "https://data.chhs.ca.gov/api/3/action/datastore_search"
  
  offset <- 0
  all_records <- list()
  
  repeat {
    # Build URL with pagination
    url <- paste0(
      base_url,
      "?resource_id=", resource_id,
      "&q=", URLencode(query),
      "&limit=", limit,
      "&offset=", offset
    )
    
    res <- GET(url)
    parsed <- content(res, as = "parsed", type = "application/json")
    
    # Extract records
    records <- parsed$result$records
    
    # Break if no more records
    if (length(records) == 0) break
    
    all_records[[length(all_records) + 1]] <- records
    
    # Update offset
    offset <- offset + limit
  }
  
  # Combine into one tibble
  bind_rows(all_records)
}

resource_id <- "a440b99b-ccc6-473c-bea1-2baf36b05dbe"

# fetch all data for years 2021-2022 (dataset currently contains 2015-2022)
chis <- fetch_all_ckan(resource_id, "2021-2022")


# Clean asthma prevalence data (2021-2022)

load("data/raw/sfba.RData")


# make a dataframe to add fips code column
counties <- st_read("data/raw/counties_and_centroids.shp")
counties <- st_as_sf(counties, crs = 4326)

fips <- counties[, c(2,3)] %>%
  st_drop_geometry(.)
colnames(fips) <- c("fips", "location_name")
 
#limit to the SFBA counties
asth_prev <- chis[chis$COUNTY %in% sfba, ] 

asth_prev$lCI <- sub("-.*", "", asth_prev$'95% CONFIDENCE INTERVAL')
asth_prev$lCI <- sub("(", "", asth_prev$lCI, fixed = TRUE)

asth_prev$uCI <- sub(").*", "", asth_prev$'95% CONFIDENCE INTERVAL')
asth_prev$uCI <- sub(".*-", "", asth_prev$uCI)

asth_prev$lCI <- as.numeric(asth_prev$lCI)   
asth_prev$uCI <- as.numeric(asth_prev$uCI)

asth_prev$'CURRENT PREVALENCE' <- as.numeric(asth_prev$`CURRENT PREVALENCE`)

# limit age groups to 18+, 18-64 and 65+. Note that estimates for child age groups are
# statistically unstable or unavailable for most counties
age_groups <- c("18+ years", "18–64 years", "65+ years")

asth_prev <- asth_prev[asth_prev$'AGE GROUP' %in% age_groups, ]

asth_prev$age_name <- dplyr::recode(asth_prev$'AGE GROUP', 
                                    "18+ years" = "18+", 
                           "18-64 years" = "18-64",
                           "65+ years" = "65+")

asth_prev$outcome_name <- "current asthma"
asth_prev$metric_name <- "prevalence"
asth_prev$race_name <- "Total"
asth_prev$sex_name <- "Both"


asth_prev <- merge(asth_prev, fips, by.x = "COUNTY", by.y = "location_name")
asth_prev$fips <- str_pad(asth_prev$fips, width = 5, side = "left", pad = "0")
asth_prev <- asth_prev[, c(1,18,13,17,16,14,3,15,6,11,12)]

colnames(asth_prev) <- c("location_name",
                         "fips",
                         "age_name",
                         "sex_name",
                         "race_name",
                         "outcome_name", 
                         "year",          
                         "metric_name",
                         "mx",
                         "uCI",          
                         "lCI")

write_csv(asth_prev, "data/processed/adult_currentasthma_county_2021_2022.csv", append = FALSE)

################################################################################
# extract child asthma data

child_asth <- chis[chis$COUNTY %in% sfba, ] 

child_asth$lCI <- sub("-.*", "", child_asth$'95% CONFIDENCE INTERVAL')
child_asth$lCI <- sub("(", "", child_asth$lCI, fixed = TRUE)

child_asth$uCI <- sub(").*", "", child_asth$'95% CONFIDENCE INTERVAL')
child_asth$uCI <- sub(".*-", "", child_asth$uCI)

child_asth$lCI <- as.numeric(child_asth$lCI)   
child_asth$uCI <- as.numeric(child_asth$uCI)

child_asth$'CURRENT PREVALENCE' <- as.numeric(child_asth$`CURRENT PREVALENCE`)

# limit age groups to 0–4 years, 5–17 years and 0–17 years. Note that estimates for child age groups are
# statistically unstable or unavailable for most counties
age_groups <- c( "0–4 years",  "5–17 years", "0–17 years")

child_asth <- child_asth[child_asth$'AGE GROUP' %in% age_groups, ]

child_asth$age_name <- dplyr::recode(child_asth$'AGE GROUP', "0-4 years" = "0-4", 
                             "5-17 years" = "5-17",
                             "0-17 years" = "0-17")

child_asth$outcome_name <- "current asthma"
child_asth$metric_name <- "prevalence"
child_asth$race_name <- "Total"
child_asth$sex_name <- "Both"

child_asth <- merge(child_asth, fips, by.x = "COUNTY", by.y = "location_name")
child_asth <- child_asth[, c(1,18,13,17,16,14,3,15,6,11,12,10)]

colnames(child_asth) <- c("location_name",
                         "fips",
                         "age_name",
                         "sex_name",
                         "race_name",
                         "outcome_name", 
                         "year",          
                         "metric_name",
                         "mx",
                         "uCI",          
                         "lCI",
                         "comment")

write_csv(child_asth, "data/processed/child_currentasthma_county_2021_2022.csv", append = FALSE)



# reference 
# annotation "Percentage ever having been diagnosed with asthma by a health care provider AND report they still have asthma and/or had an asthma episode or attack within the past 12 months
# missing bars for ages 0-17 in some counties are due to unreliable estimates 

########################################################################
# CDC PLACES adult asthma by census tract 

ct_asth <- read_csv("data/raw/PLACES__Census_Tract_Data__GIS_Friendly_Format___2024_release_20250313.csv")

ct_asth <- ct_asth[ct_asth$StateAbbr == "CA", c(3:7, 20, 21) ]

ct_asth <- ct_asth[ct_asth$CountyName %in% sfba, ]

ct_asth$lCI <- sub(",.*", "", ct_asth$CASTHMA_Crude95CI)
ct_asth$lCI <- sub("(", "", ct_asth$lCI, fixed = TRUE)

ct_asth$uCI <- sub(").*", "", ct_asth$CASTHMA_Crude95CI)
ct_asth$uCI <- sub(".*,", "", ct_asth$uCI)

ct_asth$lCI <- as.numeric(ct_asth$lCI)   
ct_asth$uCI <- as.numeric(ct_asth$uCI)

ct_asth$CASTHMA_CrudePrev <- as.numeric(ct_asth$CASTHMA_CrudePrev)

ct_asth <- ct_asth %>%
  mutate(outcome_name = "current asthma",
         metric_name = "prevalence",
         race_name = "Total",
         sex_name = "Both",
         age_name = "18+",
         year = "2024")

ct_asth <- ct_asth[, c(1:3, 14,13,12,10, 15,11,6,8,9)]

colnames(ct_asth) <- c("location_name",
                          "fips",
                          "ct_fips",
                          "age_name",
                          "sex_name",
                          "race_name",
                          "outcome_name", 
                          "year",          
                          "metric_name",
                          "mx",
                          "lCI",
                          "uCI")



write_csv(ct_asth, "data/processed/adult_currentasthma_ctract_2024_CDCPlaces.csv", append = FALSE)

#make shapefiles
ct_tract <- st_read("data/raw/ct_tract.shp")
ct_asth <- merge(ct_asth, ct_tract, by.x = "ct_fips", by.y = "GEOID", all = TRUE)
ct_asth$ct_fips <- substr(ct_asth$ct_fips, 6, 11)
ct_asth <- ct_asth[, c(2,3,1,4:13)]
st_write(ct_asth, "data/processed/adult_currentasthma_ctract_2024_CDCPlaces.shp", append = FALSE)



