

library(sf)
library(dplyr)
library(readr)
library(ggplot2)
library(RColorBrewer)
library(stringr)

counties <- st_read("counties_and_centroids.shp")
counties <- st_as_sf(counties, crs = 4326)
counties <- counties[, 2]

lcan <- read_csv("lcan_mortality_2019.csv")
lcan$fips <- str_pad(lcan$fips, width = 5, side = "left", pad = "0")
lcan[is.na(lcan$race_name), "race_name"] <- "API"
lcan <- merge(lcan, counties, by = "fips") 
st_write(lcan, "lcan_mortality_2019.shp", append = FALSE)


cvd <- read_csv("cvd_mortality_2019.csv")
cvd$fips <- str_pad(cvd$fips, width = 5, side = "left", pad = "0")
cvd <- merge(cvd, counties, by = "fips") 
st_write(cvd, "cvd_mortality_2019.shp")


acm <- read_csv("all_cause_mortality_2019.csv")
acm$fips <- str_pad(acm$fips, width = 5, side = "left", pad = "0")
acm <- merge(acm, counties, by = "fips")
st_write(acm, "all_cause_mortality_2019.shp")


alri <- read_csv("alri_2019.csv")
alri$fips <- str_pad(alri$fips, width = 5, side = "left", pad = "0")
alri <- merge(alri, counties, by = "fips")
st_write(alri, "alri_2019.shp")


demo <- read_csv("demographic_data.csv")


asth_prev <- read_csv("asthma_prev_2015.csv")



ct_tract <- st_read("ct_tract.shp")




# rows 10 and 164 are duplicates, remove
demo <- demo[-c(10,164),]
demo <- demo[demo$location_name %in% sfba,]
demo <- demo %>%
  mutate(pop_pctl = ntile(population, 100),
         fem_pctl = ntile(female, 100),
         male_pctl = ntile(male, 100),
         u18_pctl = ntile(u18, 100),              
         adult_pctl = ntile(`18_65`, 100),
         elder_pctl = ntile(`65+`, 100),
         Hisp_pctl = ntile(Hispanic, 100),
         White_pctl = ntile(White, 100),
         AfricanA_pctl = ntile(African_American, 100),
         Native_pctl= ntile(Native_American, 100),
         Asian_pctl = ntile(Asian_American, 100),
         PacIs_pctl = ntile(Pacific_Islander, 100),
         Other_pctl = ntile(Other_Multiple, 100),
         Ozone_pctl = ntile(Ozone, 100),
         PM2.5_pctl = ntile(PM2.5, 100),
         Diesel_pctl = ntile(DieselPM, 100),
         Poverty_pctl = ntile(Poverty, 100),
         Unemp_pctl = ntile(Unemployment, 100),
         HousBurd_pctl = ntile(HousingBurden, 100))

pal <- colorNumeric(
  palette = "BuGn", domain = ac$,
  na.color = "transparent"
)

demo <- na.omit(demo)

leaflet(data = ac) %>% 
  addTiles(group = "OpenStreetMap") %>%
  
  addPolygons(data = ac,
              stroke = TRUE, 
              weight = 0.2, 
              smoothFactor = 0.5,
              opacity = 1,
              fillOpacity = 1,
              fillColor = ~pal(pop_pctl),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(data = demo,
            position = "topright",
            pal = pal,
            values = ~pop_pctl,
            opacity = 1,
            title = "Total population by census tract"
  )

acm <- merge(acm, counties, by = )
