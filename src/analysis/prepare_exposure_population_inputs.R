# 
# =============================================================================
# Merge exposure grid data with simulated population residential point locations
# and health outcome baseline data at census tract, county and state-level geographies
#
#   1. Load & clean simulated population
#   2. Load & reproject census tract boundaries
#   3. Spatially assign persons to census tracts (tigerline shapefiles 2019)
#   4. Load & reproject ISRM exposure grid
#   5. Spatially assign exposures to persons (residential point location)
#   6. Add in NO2, BC sample data
# =============================================================================

library(sf)
library(readr)
library(dplyr)

# -----------------------------------------------------------------------------
# 1. Load and clean simulated population
# -----------------------------------------------------------------------------

pop <- read_csv("data/raw/simulated_population/sfbay-tr_capacity_1_5-20230608_activitysim_data_persons.csv")

pop <- pop %>%
  select(person_id, age, hispanic, race_id, sex, household_id, home_x, home_y)

# Recode race to IHME categories
# RAC1P codes: 1=White, 2=Black, 3=AIAN, 4=AIAN, 5=AIAN,
#              6=Asian, 7=NHPI, 8=Other, 9=Two or more
pop <- pop %>%
  mutate(
    race_name = recode(as.character(race_id),
                       "1" = "White",
                       "2" = "Black",
                       "3" = "American Indian / Alaskan Native",
                       "4" = "American Indian / Alaskan Native",
                       "5" = "American Indian / Alaskan Native",
                       "6" = "Asian / Pacific Islander",
                       "7" = "Asian / Pacific Islander",
                       "8" = "Other",
                       "9" = "Other"
    ),
    race_name = if_else(hispanic == 1, "Hispanic", race_name),
    sex_name  = recode(as.character(sex), "1" = "Male", "2" = "Female")
  ) %>%
  rename(age_name = age) %>%
  select(person_id, household_id, age_name, sex_name, race_name, home_x, home_y)

# transform to crs 4236 prior to merging with 2019 ct
pop <- st_as_sf(pop, coords = c("home_x", "home_y"), crs = 4269)

# -----------------------------------------------------------------------------
# 2. Load census tract boundaries (2018 TIGER/Line, pre-cleaned)
# -----------------------------------------------------------------------------

ct <- st_read("data/raw/census_boundaries/tiger_line_shapefiles/2019/tl_2019_06_sfbatract_cleaned.shp")
ct <- ct[, 1]

# -----------------------------------------------------------------------------
# 3. Spatially assign persons to census tracts
# -----------------------------------------------------------------------------

sf_use_s2(FALSE)
popct <- st_intersection(ct, pop)
sf_use_s2(TRUE)

rm(ct, pop)
gc()

popct <- st_transform(popct, 3310)   # reproject to CA Albers (EPSG:3310)


colnames(popct) <- c("geoid",    
                     "ind_id",
                     "hsld_id",
                     "age",
                     "sex" ,
                     "race",
                     "geometry")

#st_write(popct, "data/processed/popsim_indiv_censustract_2019.shp", append = FALSE)

#popct <- st_read("data/processed/popsim_indiv_censustract_2019.shp")

# -----------------------------------------------------------------------------
# 4. Load and reproject ISRM simulated exposure grid
# -----------------------------------------------------------------------------

sim <- read_csv("data/raw/exposure_data/sfbay-tr-discount-100-20230703_sfbay-tr_capacity_1_5-20230608_All_resultsISRM.csv")

# ISRM has a custom projection
custom_wkt <- 'PROJCRS["Lambert_Conformal_Conic",
  BASEGEOGCRS["GCS_unnamed ellipse",
    DATUM["unknown",
      ELLIPSOID["Unknown",6370997,0,
        LENGTHUNIT["metre",1,ID["EPSG",9001]]]],
    PRIMEM["Greenwich",0,
      ANGLEUNIT["Degree",0.0174532925199433]]],
  CONVERSION["unnamed",
    METHOD["Lambert Conic Conformal (2SP)",ID["EPSG",9802]],
    PARAMETER["Latitude of false origin",40,
      ANGLEUNIT["Degree",0.0174532925199433],ID["EPSG",8821]],
    PARAMETER["Longitude of false origin",-97,
      ANGLEUNIT["Degree",0.0174532925199433],ID["EPSG",8822]],
    PARAMETER["Latitude of 1st standard parallel",33,
      ANGLEUNIT["Degree",0.0174532925199433],ID["EPSG",8823]],
    PARAMETER["Latitude of 2nd standard parallel",45,
      ANGLEUNIT["Degree",0.0174532925199433],ID["EPSG",8824]],
    PARAMETER["Easting at false origin",0,
      LENGTHUNIT["metre",1],ID["EPSG",8826]],
    PARAMETER["Northing at false origin",0,
      LENGTHUNIT["metre",1],ID["EPSG",8827]]],
  CS[Cartesian,2],
  AXIS["(E)",east,ORDER[1],LENGTHUNIT["metre",1,ID["EPSG",9001]]],
  AXIS["(N)",north,ORDER[2],LENGTHUNIT["metre",1,ID["EPSG",9001]]]]'

sim <- st_as_sf(sim, wkt = "geometry")
sim <- st_set_crs(sim, custom_wkt)
sim <- st_transform(sim, 3310)

rm(custom_wkt)
gc()

# -----------------------------------------------------------------------------
# 5. Spatially assign exposures to persons by residential location
# -----------------------------------------------------------------------------

sf_use_s2(FALSE)
exp <- st_intersection(sim, popct)
sf_use_s2(TRUE)
exp <- st_as_sf(exp, sf_column_name = "geometry", 3310)


exp <- exp[, c(10,1,11:15, 7)]

colnames(exp) <- c("geoid",
                   "isrm",
                   "ind_id",
                   "hsld_id",
                   "age",
                   "sex",      
                   "race",
                   "TotalPM25",
                   "geometry" )

# =============================================================================
# The below code chunk will be removed when actual delta x exposure estimates are available
# Generate sample NO2 and BC exposure change data
# for HIA testing using existing merged dataset spatial structure
#
# Ranges for SF Bay Area:
#   NO2: 6.3 to 84.6 ug/m3 (3.37 to 45 ppb)
#   BC:  0.2 to 2.5 ug/m3
#
# Approach: spatially smoothed random values using ISRM grid cell (isrm)
#   as the unit of variation — persons in the same grid cell get the same
#   exposure change, consistent with how TotalPM25 is assigned
# =============================================================================

library(dplyr)
library(readr)

set.seed(42)   # for reproducibility

# -----------------------------------------------------------------------------
# Generate one exposure value per ISRM grid cell
#    Persons within the same cell share the same exposure change
# -----------------------------------------------------------------------------

isrm_cells <- exp %>%
  st_drop_geometry() %>%
  distinct(isrm) %>%
  mutate(
    NO2 = runif(n(), min = 0.1,  max = 28.3),  # ug/m3 existing SFBA range 6.3 - 84.6ug/m3
    BC  = runif(n(), min = 0.01,  max = 2.3)    # ug/m3 existing SFBA range 0.2 - 2.5 ug/m3
  )

# -----------------------------------------------------------------------------
# Join back to exp on isrm grid cell id
# -----------------------------------------------------------------------------

exp <- exp %>%
  left_join(isrm_cells, by = "isrm")

# change sign on delta x for NO2 and BC
exp$NO2 <- (-1)*exp$NO2
exp$BC <- (-1)*exp$BC

# -----------------------------------------------------------------------------
# QA checks
# -----------------------------------------------------------------------------

message("NO2 range:")
print(summary(exp$NO2))

message("BC range:")
print(summary(exp$BC))

message(sprintf("Persons with NO2 assigned: %d of %d (%.1f%%)",
                sum(!is.na(exp$NO2)), nrow(exp),
                100 * mean(!is.na(exp$NO2))))

message(sprintf("Persons with BC assigned: %d of %d (%.1f%%)",
                sum(!is.na(exp$BC)), nrow(exp),
                100 * mean(!is.na(exp$BC))))

# ================================================================================

exp %>%
  st_drop_geometry() %>%
  saveRDS("data/processed/exposure_population.rds")

rm(popct, sim, isrm_cells)
gc()




