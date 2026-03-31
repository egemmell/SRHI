# =============================================================================
# Download CDC PLACES 2021 tract-level asthma prevalence data for California
#
# Source: CDC PLACES 2021 release (model year 2019)
#         Census tract level
# Target: Current asthma prevalence + 95% CI for California (state FIPS = "06")
#
# Instructions:
#   1. Replace YOUR_API_ENDPOINT with your CDC PLACES API endpoint URL
#   2. Replace field names in `fields` if they differ in your endpoint
#      (check available fields at your API endpoint + "/meta")
# =============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# =============================================================================
# A. Configuration
#    Replace endpoint URL and verify field names before running 
# =============================================================================

api_endpoint <- "https://data.cdc.gov/resource/373s-ayzu.json"   # e.g. "https://data.cdc.gov/resource/..."


# check column names
httr::GET(paste0(api_endpoint, "?$limit=1")) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  names()

# Field names to request — adjust to match exact column names in your endpoint
# Check available fields at: paste0(api_endpoint, "?$limit=1")
fields <- list(
  tract_fips  = "locationname",     # census tract FIPS code — verify field name
  state_fips  = "stateabbr",        # state identifier — verify field name
  measure     = "measureid",        # measure identifier for filtering
  prevalence  = "data_value",       # asthma prevalence estimate
  mx_name     = "data_value_type",
  ci_low      = "low_confidence_limit",   # lower 95% CI
  ci_high     = "high_confidence_limit"  # upper 95% CI
  
)

ca_state_fips <- "CA"
asthma_measure_id <- "CASTHMA"      # CDC PLACES measure ID for current asthma
mx_type <- "Crude prevalence"   # exclude age-adjusted prevalence
batch_size <- 50000                 # rows per API request (PLACES has ~85k tracts)

# =============================================================================
# B. Download function with pagination
#    CDC Socrata APIs default to 1000 rows; use $limit + $offset to paginate
# =============================================================================

download_places_paginated <- function(endpoint, state_fips, measure_id, mx_type,
                                      fields, batch_size = 50000) {
  
  select_fields <- paste(unique(unlist(fields)), collapse = ",")
  
  # SoQL filter: California tracts for asthma measure only
  where_clause <- sprintf(
    "%s='%s' AND %s='%s' AND %s='%s'",   # 3 pairs = 6 placeholders
    fields$state_fips, state_fips,
    fields$measure,    measure_id,
    fields$mx_name,    mx_type
  )


  
  all_data <- list()
  offset   <- 0
  page     <- 1
  
  repeat {
    message(sprintf("Downloading batch %d (offset %d)...", page, offset))
    
    response <- GET(
      url   = endpoint,
      query = list(
        `$select` = select_fields,
        `$where`  = where_clause,
        `$limit`  = batch_size,
        `$offset` = offset
      )
    )
    
    # Handle HTTP errors
    if (http_error(response)) {
      stop(sprintf("API request failed: HTTP %d\n%s",
                   status_code(response),
                   content(response, as = "text", encoding = "UTF-8")))
    }
    
    batch <- content(response, as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE) %>%
      as_tibble()
    
    if (nrow(batch) == 0) {
      message("No more rows — download complete.")
      break
    }
    
    all_data[[page]] <- batch
    message(sprintf("  Retrieved %d rows.", nrow(batch)))
    
    if (nrow(batch) < batch_size) break   # last page
    
    offset <- offset + batch_size
    page   <- page + 1
  }
  
  bind_rows(all_data)
}

# =============================================================================
# C. Download
# =============================================================================

message("Downloading CDC PLACES 2021 asthma data for California census tracts...")

raw <- download_places_paginated(
  endpoint   = api_endpoint,
  state_fips = ca_state_fips,
  measure_id = asthma_measure_id,
  mx_type = mx_type,
  fields     = fields,
  batch_size = batch_size
)

message(sprintf("Downloaded %d rows.", nrow(raw)))

# =============================================================================
# D. Clean and standardise
#    Rename to match all_outcomes schema and convert prevalence to proportion
# =============================================================================

places_asthma <- raw %>%
  rename(
    geoid    = !!fields$tract_fips,
    mx_name = !!fields$mx_name,
    mx       = !!fields$prevalence,
    mx_lower = !!fields$ci_low,
    mx_upper = !!fields$ci_high,
  ) %>%
  mutate(
    across(c(mx, mx_lower, mx_upper), as.numeric),
  )


places_asthma <- places_asthma %>%
  mutate(
    mx       = mx       / 100,   # percent → proportion
    mx_lower = mx_lower / 100,
    mx_upper = mx_upper / 100
  ) %>%
  # Add columns to match all_outcomes schema
  mutate(
    otcm_nm  = "Current asthma prevalence",
    lctn_nm = substr(geoid, 6, 11),
    source   = "CDC Places",
    geolevl  = "tract",
    age_grp  = "18 plus",
    sex_grp  = "Both",
    race_grp = "Total",
    year     = "2019",           # PLACES 2021 release = 2019 model year
    mx_name  = "prevalence",
    q_flag   = 0L                # PLACES suppresses unreliable estimates
  ) %>%
  select(
    geoid, geolevl, lctn_nm, age_grp, sex_grp, race_grp,
    otcm_nm, year, source, mx_name, mx, mx_lower, mx_upper, q_flag
  )

# =============================================================================
# E. QA checks
# =============================================================================

message(sprintf("Tracts retrieved: %d", n_distinct(places_asthma$geoid)))
message(sprintf("CA tracts expected: ~9,000"))

# Check for missing values
n_missing_mx <- sum(is.na(places_asthma$mx))
if (n_missing_mx > 0)
  warning(sprintf("%d tracts have missing asthma prevalence.", n_missing_mx))

# Spot check plausible range (expect ~5-15% adult asthma prevalence)
summary(places_asthma[, c("mx", "mx_lower", "mx_upper")])

# filter to sfba census tracts
#limit to sfba counties
ct <- st_read("data/raw/census_boundaries/tiger_line_shapefiles/2019/tl_2019_06_sfbatract_cleaned.shp")
ct <- ct[, 1] |>
  st_drop_geometry()

places_asthma <- places_asthma |> 
  filter(geoid %in% ct$GEOID)
###################################################################################################
# 
# =============================================================================
# Now download CDC PLACES 2021 county-level asthma prevalence data for California
#
# Source: CDC PLACES 2021 release (model year 2019)
#         County level
# Target: Current asthma prevalence + 95% CI for California (state abbr = "CA")
#
#
#  Replace COUNTY_API_ENDPOINT with correct CDC PLACES county endpoint for release 2021
#  
# =============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# =============================================================================
# A. Configuration
#    !! Replace endpoint URL and verify field names before running !!
# =============================================================================
# county endpoint for 2021 release (2019 data)
api_endpoint <- [ENDPOINT URL]

# check column names
httr::GET(paste0(api_endpoint, "?$limit=1")) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  names()

fields <- list(
  county_fips = "locationid",
    county =   "locationname", # county name -  verify field name
  state_fips  = "stateabbr",        # state identifier — verify field name 
  measure     = "measureid",         # measure identifier for filtering
  prevalence  = "data_value",      # asthma prevalence estimate
  mx_name     = "data_value_type",
  ci_low      = "low_confidence_limit",    # lower 95% CI
  ci_high     = "high_confidence_limit"  # upper 95% CI
)

ca_state_fips <- "CA"
asthma_measure_id <- "CASTHMA"      # CDC PLACES measure ID for current asthma
mx_type <- "Crude prevalence"   # exclude age-adjusted prevalence
batch_size <- 50000                 # rows per API request (PLACES has ~85k tracts)

# =============================================================================
# B. Download function with pagination
#    CDC Socrata APIs default to 1000 rows; use $limit + $offset to paginate
# =============================================================================

download_places_paginated <- function(endpoint, state_fips, measure_id, mx_type,
                                      fields, batch_size = 50000) {
  
  select_fields <- paste(unique(unlist(fields)), collapse = ",")
  
  where_clause <- sprintf(
    "%s='%s' AND %s='%s'AND %s='%s'",
    fields$state_fips, state_fips,
    fields$measure,    measure_id,
    fields$mx_name, mx_type
  )
  
  all_data <- list()
  offset   <- 0
  page     <- 1
  
  repeat {
    message(sprintf("Downloading batch %d (offset %d)...", page, offset))
    
    response <- GET(
      url   = endpoint,
      query = list(
        `$select` = select_fields,
        `$where`  = where_clause,
        `$limit`  = batch_size,
        `$offset` = offset
      )
    )
    
    if (http_error(response)) {
      stop(sprintf("API request failed: HTTP %d\n%s",
                   status_code(response),
                   content(response, as = "text", encoding = "UTF-8")))
    }
    
    batch <- content(response, as = "text", encoding = "UTF-8") %>%
      fromJSON(flatten = TRUE) %>%
      as_tibble()
    
    if (nrow(batch) == 0) {
      message("No more rows — download complete.")
      break
    }
    
    all_data[[page]] <- batch
    message(sprintf("  Retrieved %d rows.", nrow(batch)))
    
    if (nrow(batch) < batch_size) break
    offset <- offset + batch_size
    page   <- page + 1
  }
  
  bind_rows(all_data)
}

# =============================================================================
# C. Download
# =============================================================================

message("Downloading CDC PLACES 2021 asthma data for California counties...")

raw <- download_places_paginated(
  endpoint   = api_endpoint,
  state_fips = ca_state_fips,
  measure_id = asthma_measure_id,
  mx_type = mx_type,
  fields     = fields,
  batch_size = batch_size
)

message(sprintf("Downloaded %d rows.", nrow(raw)))

# =============================================================================
# D. Clean and standardise to match all_outcomes schema
# =============================================================================

places_asthma_county <- raw %>%
  rename(
    geoid    = !!fields$county_fips,   # county FIPS
    lctn_nm   = !!fields$county,
    mx_name = !!fields$mx_name,
    mx       = !!fields$prevalence,
    mx_lower = !!fields$ci_low,
    mx_upper = !!fields$ci_high,
  ) %>%
  mutate(across(c(mx, mx_lower, mx_upper), as.numeric))

places_asthma_county <- places_asthma_county %>%
  mutate(
    mx       = mx       / 100,   # percent → proportion
    mx_lower = mx_lower / 100,
    mx_upper = mx_upper / 100
  ) %>%
  # Add columns to match all_outcomes schema
  mutate(
    otcm_nm  = "Current asthma prevalence",
    source   = "CDC Places",
    geolevl  = "county",
    age_grp  = "18 plus",
    sex_grp  = "Both",
    race_grp = "Total",
    year     = "2019",           # PLACES 2021 release = 2019 model year
    mx_name  = "prevalence",
    q_flag   = 0L                # PLACES suppresses unreliable estimates
  ) %>%
  select(
    geoid, geolevl, lctn_nm, age_grp, sex_grp, race_grp,
    otcm_nm, year, source, mx_name, mx, mx_lower, mx_upper, q_flag
  )

# =============================================================================
# E. QA checks
# =============================================================================

message(sprintf("Counties retrieved: %d", n_distinct(places_asthma_county$geoid)))
message(sprintf("CA counties expected: 58"))

n_missing_mx <- sum(is.na(places_asthma_county$mx))
if (n_missing_mx > 0)
  warning(sprintf("%d counties have missing asthma prevalence.", n_missing_mx))

summary(places_asthma_county[, c("mx", "mx_lower", "mx_upper")])



#limit to sfba counties
load("data/raw/sfba.RData")
places_asthma_county <- places_asthma_county |> 
  filter(lctn_nm %in% sfba)

# =============================================================================
# F. Limit to study census tracts and write out
# =============================================================================
data <- list(places_asthma, places_asthma_county)

# combine cdc county and census tract level data
asth <- do.call(rbind, data)


write_csv(asth,
          "data/processed/adult_currentasthma_ctract_county_2019_CDCPlaces.csv")


##################################################################################
##################################################################################
# alternative script for data downloaded from cdc places as csv files (2024)

library(dplyr)
library(readr)
# CDC PLACES adult asthma by county

load("W:/BREATHE/BREATHE_PROJECT/data/raw/sfba.RData")

asth <- read_csv("data/raw/baseline_health_outcomes/PLACES__Census_Tract_Data__GIS_Friendly_Format___2024_release_20250313.csv")

asth <- asth[asth$StateAbbr == "CA", c(3, 4,20,21) ]

asth <- asth[asth$CountyName %in% sfba, ]

asth$lCI <- sub(",.*", "", asth$CASTHMA_Crude95CI)
asth$lCI <- sub("(", "", asth$lCI, fixed = TRUE)

asth$uCI <- sub(").*", "", asth$CASTHMA_Crude95CI)
asth$uCI <- sub(".*,", "", asth$uCI)

asth$lCI <- as.numeric(asth$lCI)   
asth$uCI <- as.numeric(asth$uCI)

asth$CASTHMA_CrudePrev <- as.numeric(asth$CASTHMA_CrudePrev)

asth <- asth %>%
  mutate(outcome_name = "Current asthma prevalence",
         source = "CDC Places",
         mx_name = "prevalence",
         race_grp = "Total",
         sex_grp = "Both",
         age_grp = "18 plus",
         year = "2024",
         geolevl = "county",
         q_flag = 0)

asth <- asth[, c(2,14,1,12,11,10,7,8,13,9,3,5,6,15)]

colnames(asth) <- c("geoid",
                       "geolevl",
                       "lctn_nm",
                       "age_grp",
                       "sex_grp",
                       "race_grp",
                       "otcm_nm", 
                       "source",
                       "year",  
                       "mx_name",
                       "mx",
                       "mx_lower",
                       "mx_upper",
                       "q_flag")


########################################################################
# CDC PLACES adult asthma by census tract 
load("W:/BREATHE/BREATHE_PROJECT/data/raw/sfba.RData")

ct_asth <- read_csv("data/raw/baseline_health_outcomes/PLACES__Census_Tract_Data__GIS_Friendly_Format___2024_release_20250313.csv")

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
  mutate(outcome_name = "Current asthma prevalence",
         source = "CDC Places",
         mx_name = "prevalence",
         race_grp = "Total",
         sex_grp = "Both",
         age_grp = "18 plus",
         year = "2024",
         geolevl = "tract",
         q_flag = 0)

ct_asth <- ct_asth[, c(3,17,1,15,14,13,10,11, 16,12,6,8,9,18)]

colnames(ct_asth) <- c("geoid",
                       "geolevl",
                       "lctn_nm",
                       "age_grp",
                       "sex_grp",
                       "race_grp",
                       "otcm_nm", 
                       "source",
                       "year",
                       "mx_name",
                       "mx",
                       "mx_lower",
                       "mx_upper",
                       "q_flag")

data <- list(asth, ct_asth)

asth <- do.call(rbind, data)

write_csv(asth, "data/processed/adult_currentasthma_ctract_county_2024_CDCPlaces.csv", append = FALSE)



