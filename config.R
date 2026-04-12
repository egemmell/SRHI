# config.R
# Central configuration file for BREATHE HIA pipeline
# Source this file at the top of each script: source("config.R")

# San Francisco Bay Area county FIPS codes (9 counties)
sfba_fips <- c("06001", "06013", "06041", "06055", "06075", 
               "06081", "06085", "06095", "06097")

# County-only FIPS (3-digit, for tract-level filtering)
sfba_fips_3 <- c("001", "013", "041", "055", "075", 
                 "081", "085", "095", "097")

# County names (for datasets that use names instead of FIPS)
sfba_names <- c("Alameda", "Contra Costa", "Marin", "Napa",
                "San Francisco", "San Mateo", "Santa Clara",
                "Solano", "Sonoma")

# SFBA census tract GEOIDs (generate once from shapefile)
ct <- sf::st_read("data/raw/census_boundaries/tiger_line_shapefiles/2019/tl_2019_06_sfbatract_cleaned.shp") |>
  sf::st_drop_geometry()
sfba_geoids <- ct$GEOID


# Named vector linking FIPS to names (useful for joins and labels)
sfba_lookup <- setNames(sfba_names, sfba_fips)

# State FIPS
ca_fips <- "06"

