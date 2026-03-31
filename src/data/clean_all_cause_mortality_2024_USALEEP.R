# USA Mortality Incidence Data (BenMap-ready) was downloaded from 
# Row is the state-county FIPS code
# Column is the tract FIPS code. 
# Add leading 0s to the row (to make it 5 digits) and to the column (to make it 6 digits)
# Concatenate the two to generate a full 11-digit tract GEOID. 
# Although the estimates are updated to 2020-centered population, the geography is census 2010


# for the main HI analysis, we will use 2020 mortality estimates (5-year estimates centered around 2020) with 2018 geographies (based on census tract geography for 2010) since
# simulated population data (from activity sim) is centered around 2018

library(sf)
library(dplyr)
library(readr)
library(stringr)

sfba <- c("013", "001", "081", "095", "085", "075", "055", "097","041")
# load ct-tract level all-cause mortality data
ac <- read_csv("data/raw/baseline_health_outcomes/BenMAP_Ready_USALEEP_AllCauseRates_2020.csv")

# add padding zeros to 2010 census geography geoids
ac$Row <- str_pad(ac$Row, width = 5, side = "left", pad = "0")
ac$Column <- str_pad(ac$Column, width = 6, side = "left", pad = "0")

# combine to ct geoid
ac$geoid <- paste0(ac$Row, ac$Column)

# make age groups
ac$age_grp <- "NA"
ac$sex_grp <- "Both"
ac$race_grp <- "Total"

ac[ac$`Start Age`== 0 & ac$`End Age` == 0, "age_grp"] <- "<1 year"
ac[ac$`Start Age` == 1 & ac$`End Age` == 4, "age_grp" ] <- "1 to 4"
ac[ac$`Start Age` == 5 & ac$`End Age` == 14, "age_grp" ] <- "5 to 14"
ac[ac$`Start Age` == 15 & ac$`End Age` == 24, "age_grp" ] <- "15 to 24"
ac[ac$`Start Age` == 25 & ac$`End Age` == 34, "age_grp" ] <- "25 to 34"
ac[ac$`Start Age` == 35 & ac$`End Age` == 44, "age_grp" ] <- "35 to 44"
ac[ac$`Start Age` == 45 & ac$`End Age` == 54, "age_grp" ] <- "45 to 54"
ac[ac$`Start Age` == 55 & ac$`End Age` == 64, "age_grp" ] <- "55 to 64"
ac[ac$`Start Age` == 65 & ac$`End Age` == 74, "age_grp" ] <- "65 to 74"
ac[ac$`Start Age` == 75 & ac$`End Age` == 84, "age_grp" ] <- "75 to 84"
ac[ac$`Start Age` == 85 & ac$`End Age` == 99, "age_grp" ] <- "85 to 99"


ac$geolevel <- "tract"
ac$location_name <- as.character(str_sub(ac$geoid, 6, 11))
ac$year <- "2020"  # mortality estimates are based on rates from 2010-2015, updated to 2020 population
ac$outcome_name <- "All-cause mortality"
ac$source <- "USALEEP"
ac$mx_lower <- NA
ac$mx_upper <- NA
ac$qflag <- 0
ac <- ac[, c(12, 16,17, 13:15, 19, 18, 20, 11, 21:23)]
colnames(ac) <- c("geoid",
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
                  "q_flag")

# limit to tracts within study area
ac <- ac %>%
  filter(substr(geoid, 1, 2) == "06",
         substr(geoid, 3, 5) %in% sfba)

# link to cartographic 2019 census tract geography
ct <- st_read("data/raw/census_boundaries/tiger_line_shapefiles/2019/tl_2019_06_sfbatract_cleaned.shp")
ct <- ct[, 1]
colnames(ct) <- c("geoid", "geometry")


ac$mx_name <- "person-years at risk"
ac <- ac[, c(1:9, 14, 10:13)]

write_csv(ac, "data/processed/ac_mortality_ct_tract_2019_USALEEP.csv", append = FALSE)

ct_ac <- ct %>%
  select(geoid) %>%
  left_join(ac, by ="geoid") 



# visualization

library(leaflet)
AGE_GRP <- "45 to 54"
data <- ct_ac[ct_ac$age_grp == AGE_GRP ,]

pal <- colorNumeric(
  palette = "Spectral", 
  domain = data$mx, 
  reverse = TRUE,
  na.color = "grey"
)


leaflet(data = data) %>% 
  addTiles(group = "OpenStreetMap") %>%
  
  addPolygons(data = data,
              stroke = TRUE, 
              weight = 0.3, 
              color = "darkgrey",
              smoothFactor = 0.5,
              opacity = 0.7, 
              fillOpacity = 0.6,
              fillColor = ~pal(mx),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  
  addLegend(data = data,
            position = "topright",
            pal = pal,
            values = ~mx,
            opacity = 1,
            title = paste0("All-cause mortality<br>among people ages ", AGE_GRP, ":<br>deaths/person-year at risk")
  )

#####################################################################################################
# Use the commented out code to update the geography of the estimates to 2020 census geographies, if needed, using the census tract 2010 to 2020 relationship file for 
# California available at https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2020.html
# load 2010 to 2020 crosswalk with population weights
#cw <- read_csv("data/raw/census_boundaries/nhgis_tr2010_tr2020_06.csv")

# limit to needed crosswalk columns and census tracts in California
#cw <- cw[, c(2,4,5,6)]
#colnames(cw) <- c("ct2010", "ct2020", "parea", "wt_pop")
#cw$state <- str_sub(cw$ct2020, 1, 2)
#cw <- cw[cw$state == "06", ]
#cw <- cw[, -5]

# save 2010 to 2020 crosswalk
#write_csv(cw, "data/raw/census_boundaries/cw_2010_to_2020_pweighted_nhgis.csv", append = FALSE)

#ac <- read_csv("data/processed/ac_mortality_ct_tract_2020_USALEEP_allUSA.csv")


# use crosswalk to replace 2010 with 2020 California census tracts 
#ac <- merge(ac, cw, by = "ct2010", all.y = TRUE)

#ac$deaths <- ac$mx * ac$wt_pop   # total deaths rescaled to 2020 geography (wt_pop variable is the proportion of total 2010 population living in 2020 census tract)
#ac$pyrisk <- ac$wt_pop * 1       # total person-years at risk using 2020 geography


#ac <- ac %>%
 # group_by(ct2020, age_name, sex_name, race_name, year, outcome_name, metric_name) %>%
#   summarize(deaths = sum(deaths),
    #        pyrisk = sum(pyrisk))

# ac$mx_2020 <- ac$deaths/ac$pyrisk  # calculate mortality rate within 2020 census tract

# ac <- ac[, -c(8,9)]

#vintage 2020 TigerLine shapefiles geography decennial for analysis
#ct <- st_read("data/raw/census_boundaries/tiger_line_shapefiles/2020/tl_2020_sfbatract_06_cleaned.shp")
#cb <- st_read("data/raw/census_boundaries/cartographic_shapefiles/2020/cb_2020_06_sfbatract_cleaned.shp")
#ct <- ct[ ,c(4,11,12)]
#colnames(ct) <- c("ct2020", "lat", "lon", "geometry")

#cb <- st_read("data/raw/census_boundaries/cartographic_shapefiles/2020/cb_2020_06_sfbatract_cleaned.shp")
#ct2 <- st_intersection(ct, cb)

#ac <- merge(ac, ct, by = "ct2020", all.y = TRUE)
#ac <- st_as_sf(ac, sf_column_name = "geometry")
#ac <- st_transform(ac, 4326)




