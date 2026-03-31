
# This script cleans data from the California Health Interview Survey for adult and child asthma prevalence (2021-2022)
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


# load county and census tract shapefiles
ct <- st_read("data/raw/census_boundaries/tiger_line_shapefiles/2019/tl_2019_06_sfbatract_cleaned.shp")
ct <- st_as_sf(ct, crs = 4326)

cty <- st_read("data/raw/census_boundaries/tiger_line_shapefiles/2019/tl_2019_06_sfbacounty_cleaned.shp")
cty <- st_as_sf(cty, crs = 4326)

fips <- cty[, c(2,1)] %>%
  st_drop_geometry(.)
colnames(fips) <- c("lctn_nm", "geoid")




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
chis <- fetch_all_ckan(resource_id, "2017–2018")


# Clean asthma prevalence data 

sfba <- c("Alameda", 
          "Contra Costa", 
          "Marin", 
          "Napa", 
          "San Francisco", 
          "San Mateo", 
          "Santa Clara", 
          "Solano", 
          "Sonoma",
          "California")

asth_prev <- chis[chis$COUNTY %in% sfba, ] 

asth_prev$mx_lower <- sub("-.*", "", asth_prev$'95% CONFIDENCE INTERVAL')
asth_prev$mx_lower<- sub("(", "", asth_prev$mx_lower, fixed = TRUE)

asth_prev$mx_upper <- sub(").*", "", asth_prev$'95% CONFIDENCE INTERVAL')
asth_prev$mx_upper <- sub(".*-", "", asth_prev$mx_upper)

asth_prev$mx_lower <- as.numeric(asth_prev$mx_lower)   
asth_prev$mx_upper <- as.numeric(asth_prev$mx_upper)

asth_prev$'CURRENT PREVALENCE' <- as.numeric(asth_prev$`CURRENT PREVALENCE`)

asth_prev$q_flag <- 0
asth_prev[is.na(asth_prev$COMMENT), "COMMENT"] <- "None"
asth_prev[asth_prev$COMMENT == "Prevalence not available due to unreliable estimate", "q_flag"] <- 1  
asth_prev[asth_prev$COMMENT == "Estimate is statistically unstable. Caution is recommended when reporting or relying on statistically unstable estimates.", "q_flag"] <- 2       


# limit age groups to 18+, 18-64 and 65+. Note that estimates for child age groups are
# statistically unstable or unavailable for most counties
age_groups <- c("18+ years", "18–64 years", "65+ years")

asth_prev <- asth_prev[asth_prev$'AGE GROUP' %in% age_groups, ]

asth_prev$age_grp <- dplyr::recode(asth_prev$'AGE GROUP', 
                                    "18+ years" = "18 plus", 
                                    "18–64 years" = "18 to 64",
                                    "65+ years" = "65 plus")

asth_prev$otcm_nm <- "Current asthma prevalence"

asth_prev$race_grp <- "Total"
asth_prev$sex_grp <- "Both"
asth_prev$source <- "CHIS"
asth_prev$geolevl <- "county"
# if county is "California" change geolevl to "state"

asth_prev[asth_prev$COUNTY == "California", "geolevl"] <- "state"
asth_prev <- merge(asth_prev, fips, by.x = "COUNTY", by.y = "lctn_nm", all.x = TRUE)

asth_prev <- asth_prev[, c(20,19,1,14,17,16,15,3,18,6,11:13)]

colnames(asth_prev) <- c("geoid",
                   "geolevl",
                   "lctn_nm",
                   "age_grp",
                   "sex_grp",
                   "race_grp",
                   "otcm_nm", 
                   "year",          
                   "source",
                   "mx",
                   "mx_lower",          
                   "mx_upper",
                   "q_flag"
)

# add state code to geoid
asth_prev[asth_prev$lctn_nm == "California", "geoid"] <- ""
asth_prev$geoid <- paste0("06", asth_prev$geoid)

# only keep the rows where data quality is good (0)
asth_prev <- asth_prev[asth_prev$q_flag == 0, ]

# add mx_name
asth_prev$mx_name <- "prevalence"
asth_prev <- asth_prev[, c(1:9, 14, 10:13)]

write_csv(asth_prev, "data/processed/adult_currentasthma_state_county_2017_2018_CHIS.csv", append = FALSE)



################################################################################
# extract child asthma data

child_asth <- chis[chis$COUNTY %in% sfba, ] 

child_asth$mx_lower <- sub("-.*", "", child_asth$'95% CONFIDENCE INTERVAL')
child_asth$mx_lower <- sub("(", "", child_asth$mx_lower, fixed = TRUE)

child_asth$mx_upper <- sub(").*", "", child_asth$'95% CONFIDENCE INTERVAL')
child_asth$mx_upper <- sub(".*-", "", child_asth$mx_upper)

child_asth$mx_lower <- as.numeric(child_asth$mx_lower)   
child_asth$mx_upper <- as.numeric(child_asth$mx_upper)

child_asth$'CURRENT PREVALENCE' <- as.numeric(child_asth$`CURRENT PREVALENCE`)

# limit age groups to 0–4 years, 5–17 years and 0–17 years. Note that estimates for child age groups are
# statistically unstable or unavailable for most counties
age_groups <- c( "0–4 years",  "5–17 years", "0–17 years")

child_asth <- child_asth[child_asth$'AGE GROUP' %in% age_groups, ]

#replace en-dashes with hyphens in age groups
child_asth$`AGE GROUP` <- str_replace_all(child_asth$`AGE GROUP`, "\u2013", "-")

child_asth <- child_asth %>%
  mutate(`AGE GROUP` = case_when(
    `AGE GROUP` == "0-4 years"  ~ "0 to 4",
    `AGE GROUP` == "5-17 years" ~ "5 to 17",
    `AGE GROUP` == "0-17 years" ~ "0 to 17",
    TRUE ~ `AGE GROUP`  # keep everything else as-is
  ))
                             
child_asth$otcm_nm <- "Current asthma prevalence"
child_asth$race_name <- "Total"
child_asth$sex_name <- "Both"
child_asth$source <- "CHIS"
child_asth$geolevl <- "county"
child_asth[child_asth$COUNTY == "California", "geolevl"] <- "state"
child_asth$q_flag <- 0
child_asth[is.na(child_asth$COMMENT), "COMMENT"] <- "None"
child_asth[child_asth$COMMENT == "Prevalence not available due to unreliable estimate", "q_flag"] <- 1  
child_asth[child_asth$COMMENT == "Estimate is statistically unstable. Caution is recommended when reporting or relying on statistically unstable estimates.", "q_flag"] <- 2       


child_asth <- merge(child_asth, fips, by.x = "COUNTY", by.y = "lctn_nm", all.x = TRUE)

child_asth <- child_asth[, c(19,17,1,5,15,14,13,3,16, 6, 11, 12, 18)]

colnames(child_asth) <- c("geoid",
                   "geolevl",
                   "lctn_nm",
                   "age_grp",
                   "sex_grp",
                   "race_grp",
                   "otcm_nm", 
                   "year",          
                   "source",
                   "mx",
                   "mx_lower",          
                   "mx_upper",
                   "q_flag"
)

# add state code to geoid

child_asth[child_asth$lctn_nm == "California", "geoid"] <- ""
child_asth$geoid <- paste0("06", child_asth$geoid)

# keep only those rows with 0 q_flag value. 1 indicates missing and 2 indicates unstable estimate
child_asth <- child_asth[child_asth$q_flag != 1, ]

child_asth$mx_name <- "prevalence"
child_asth <- child_asth[, c(1:9, 14, 10:13)]

write_csv(child_asth, "data/processed/child_currentasthma_state_county_2017_2018_CHIS.csv", append = FALSE)


# reference 
# annotation "Percentage ever having been diagnosed with asthma by a health care provider AND report they still have asthma and/or had an asthma episode or attack within the past 12 months
# missing bars for ages 0-17 in some counties are due to unreliable estimates 

