
# =============================================================================
# Clean 2019 HCAi ALRI data for children 0-17 in the San Francisco Bay Area
#
# Source: HCAi Patient Discharge (PDD) and Emergency Department and
#         Ambulatory Surgery (EDAS) Datasets (custom request CS3044)
# Includes ER visits and hospitalizations with primary ICD-10 diagnosis
# of acute lower respiratory infection (J09-J18, J20-J22)
# =============================================================================

library(tidyverse)
library(sf)

source("config.R")

# =============================================================================
# A. Load and initial clean
# =============================================================================

alri <- read_csv("data/raw/baseline_health_outcomes/ALRI_2019_CS3044.csv",
                 locale = locale(encoding = "UTF-8")) |>
  select(
    lctn_nm  = County,
    race_grp = Race,
    sex_grp  = Sex,
    mx       = `Number of Cases`
  ) |>
  mutate(
    mx = if_else(mx == "<11", "10", mx),
    mx = as.numeric(mx)
  )
# =============================================================================
# B. Aggregate counts by group
# =============================================================================

# sum ER visits and hospital admissions by county, race, sex
alri <- alri |>
  group_by(lctn_nm, race_grp, sex_grp) |>
  summarize(mx = sum(mx, na.rm = TRUE), .groups = "drop")

# add Both sex totals
alri <- alri |>
  bind_rows(
    alri |>
      group_by(lctn_nm, race_grp) |>
      summarize(mx = sum(mx, na.rm = TRUE), .groups = "drop") |>
      mutate(sex_grp = "Both")
  )

# add Total race totals
# Note: race categories in ALRI are White, Black, Asian/Pacific Islander,
# Hispanic, Other. "Other" is excluded from rate calculations due to no
# corresponding census population data, but included in "Total"
alri <- alri |>
  bind_rows(
    alri |>
      group_by(lctn_nm, sex_grp) |>
      summarize(mx = sum(mx, na.rm = TRUE), .groups = "drop") |>
      mutate(race_grp = "Total")
  )

# =============================================================================
# C. Recode and add constant columns
# =============================================================================

alri <- alri |>
  mutate(
    race_grp = recode(race_grp,
                      "Asian/Pacific Islander" = "Asian / Pacific Islander"
    ),
    geolevl  = "county",
    age_grp  = "0 to 17",
    otcm_nm  = "Acute lower respiratory infection (children)",
    year     = "2019",
    source   = "HCAi",
    mx_name  = "prevalence",   # although mx is currently a count, we will estimate alri prevalence in the block below, so changing the name here
    mx_lower = NA_real_,
    mx_upper = NA_real_,
    q_flag   = 0L
  )

# =============================================================================
# D. Add county FIPS codes
# =============================================================================

fips <- st_read("data/raw/census_boundaries/tiger_line_shapefiles/2019/tl_2019_06_sfbacounty_cleaned.shp") |>
  st_drop_geometry() |>
  select(geoid = 1, lctn_nm = 2)

alri <- alri |>
  left_join(fips, by = "lctn_nm") |>
  mutate(geoid = paste0("06", geoid))

# =============================================================================
# E. Merge with population data to calculate incidence rate
# =============================================================================

# Note: "Other" race excluded from rate calculation (no corresponding census
# population data). "American Indian / Alaskan Native" excluded from demo
# data as it is not present in ALRI data.

demo <- read_csv("data/raw/population_data/pop_0_17_sex_race_county_2019.csv") |>
  select(geoid, sex_grp, race_grp, population)

alri <- alri |>
  filter(race_grp != "Other") |>
  left_join(
    demo |> filter(race_grp != "American Indian / Alaskan Native"),
    by = c("geoid", "sex_grp", "race_grp")
  ) |>
  mutate(mx = mx / population) |>
  select(lctn_nm, geolevl, geoid, age_grp, race_grp, sex_grp,
         otcm_nm, year, source, mx_name, mx, mx_lower, mx_upper, q_flag)

# =============================================================================
# F. QA checks
# =============================================================================

message(sprintf("Counties retained: %d (expected 9)", n_distinct(alri$geoid)))
message(sprintf("Missing incidence rates: %d", sum(is.na(alri$mx))))
summary(alri$mx)

# =============================================================================
# G. Write output
# =============================================================================

write_csv(alri, "data/processed/alri_county_2019_HCAi.csv", append = FALSE)
rm(alri, demo, fips)
gc()
