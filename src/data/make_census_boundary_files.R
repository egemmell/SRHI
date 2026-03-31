# Make geographic boundary shapefiles

library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(stringr)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(tigris)

# create function to clean and save census tigerline shapefiles for analysis

library(sf)
library(ggplot2)

# SFBA county FIPS codes
sfba <- c("001", "013", "041", "055", "075", "081", "085", "095", "097")

clean_census_shp <- function(year,
                             geography  = c("tract", "county"),
                             file_type  = c("tigerline", "cartographic"),
                             state_fips = "06",
                             county_filter = NULL,
                             base_dir   = "data/raw/census_boundaries",
                             plot       = TRUE) {
  
  geography <- match.arg(geography)
  file_type <- match.arg(file_type)
  
  # --- Build input path and column selection by file type ---
  if (file_type == "tigerline") {
    in_dir   <- file.path(base_dir, "tiger_line_shapefiles", year)
    col_keep <- c("GEOID", "INTPTLAT", "INTPTLON")
    
    if (geography == "tract") {
      in_file <- file.path(in_dir, paste0("tl_", year, "_", state_fips, "_tract.shp"))
    } else {
      in_file <- file.path(in_dir, paste0("tl_", year, "_us_county.shp"))
    }
    
  } else {  # cartographic
    in_dir   <- file.path(base_dir, "cartographic_shapefiles", year)
    col_keep <- c("GEOID")  # no INTPTLAT/INTPTLON in cartographic files
    
    if (geography == "tract") {
      in_file <- file.path(in_dir, paste0("cb_", year, "_", state_fips, "_tract_500k.shp"))
    } else {
      in_file <- file.path(in_dir, paste0("cb_", year, "_us_county_500k.shp"))
    }
  }
  
  # --- Read shapefile ---
  if (!file.exists(in_file)) stop("Input shapefile not found: ", in_file)
  shp <- st_read(in_file)
  
  # --- Filter to state (national county files only) ---
  if (geography == "county") {
    shp <- shp[shp$STATEFP == state_fips, ]
  }
  
  # --- Select columns (sf retains geometry automatically) ---
  available_cols <- col_keep[col_keep %in% names(shp)]
  missing_cols   <- setdiff(col_keep, names(shp))
  if (length(missing_cols) > 0) {
    warning("The following columns were not found and will be skipped: ",
            paste(missing_cols, collapse = ", "))
  }
  shp <- shp[ , available_cols]
  
  # --- Build output prefix by file type ---
  out_label  <- if (geography == "tract") "tract" else "county"
  out_prefix <- if (file_type == "tigerline") {
    paste0("tl_", year, "_", state_fips)
  } else {
    paste0("cb_", year, "_", state_fips)
  }
  
  out_state <- file.path(in_dir, paste0(out_prefix, "_", out_label, "_cleaned.shp"))
  
  if (plot) {
    print(ggplot(data = shp) + geom_sf() +
            ggtitle(paste(year, file_type, "-", geography, "- State level")))
  }
  
  st_write(shp, out_state, append = FALSE)
  message("Saved: ", out_state)
  
  # --- Optionally filter to county subset ---
  # County FIPS extracted from GEOID positions 3-5 for both geographies and file types:
  #   tract  GEOID = 11 chars: SS + CCC + TTTTTT
  #   county GEOID =  5 chars: SS + CCC
  if (!is.null(county_filter)) {
    shp_sub <- shp[substr(shp$GEOID, 3, 5) %in% county_filter, ]
    out_sub <- file.path(in_dir, paste0(out_prefix, "_sfba", out_label, "_cleaned.shp"))
    
    if (plot) {
      print(ggplot(data = shp_sub) + geom_sf() +
              ggtitle(paste(year, file_type, "-", geography, "- SFBA subset")))
    }
    
    st_write(shp_sub, out_sub, append = FALSE)
    message("Saved: ", out_sub)
    
    return(invisible(list(state = shp, county_subset = shp_sub)))
  }
  
  return(invisible(list(state = shp)))
}

# Usage

# TigerLine tract and county
#clean_census_shp(year = 2020, geography = "tract",  file_type = "tigerline",      county_filter = sfba)
#clean_census_shp(year = 2019, geography = "county", file_type = "tigerline",      county_filter = sfba)

# Cartographic tract and county
#clean_census_shp(year = 2019, geography = "tract",  file_type = "cartographic",   county_filter = sfba)
#clean_census_shp(year = 2019, geography = "county", file_type = "cartographic",   county_filter = sfba)

# Loop over years
# Limit to study counties and save cartographic and tigerline shapefiles
purrr::walk(c(2018, 2019, 2020), clean_census_shp, geography = "tract", file_type = "tigerline", county_filter = sfba)
purrr::walk(c(2018, 2019, 2020), clean_census_shp, geography = "tract", file_type = "cartographic", county_filter = sfba)

# Save complete state data, tigerline and cartographic
purrr::walk(c(2018, 2019, 2020), clean_census_shp, geography = "tract", file_type = "tigerline", county_filter = NULL) 
purrr::walk(c(2018, 2019, 2020), clean_census_shp, geography = "tract", file_type = "cartographic", county_filter = NULL)  

rm(sfba, clean_census_shp)
gc()
