library(vroom)
library(tidyr)
library(sf)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(bslib)
library(RColorBrewer)


ac <- read_csv("all_cause_mortality_2019.csv")
ac$outcome_name <- "All-cause mortality"
ac$metric_name <- "age-standardized rate"

ac <- ac[, c(2,3,6,4,5,7:12)]
ac$comment <- NA
ac[ac$race_name == "AIAN", "race_name"] <- "American Indian/Alaskan Native"
ac[ac$race_name == "API", "race_name"] <- "Asian/Pacific Islander"
ac$year <- as.character(ac$year)
ac$fips <- str_pad(ac$fips, width = 5, side = "left", pad = "0")
# Cause-specific mortality: CVD


ihd <- read_csv("IHME_USA_COUNTY_CVD_MORTALITY_RATES_1980_2014_CALIFORNIA_Y2017M05D16.CSV")

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


ihd <- ihd[ihd$location_name %in% sfba, c(4,5,7,11,9,7,12:16)] 
ihd$race_name <- "Total"
ihd$comment <- NA
ihd <- ihd[, c(2,1,4,12,5, 3,7:11,13)]

colnames(ihd) <- c("fips",
                   "location_name",
                   "age_name",
                   "race_name",
                   "sex_name",
                   "outcome_name", 
                   "year",          
                   "metric_name",
                   "mx",
                   "uCI",          
                   "lCI",
                   "comment")

ihd$fips <- str_pad(ihd$fips, width = 5, side = "left", pad = "0")
ihd$year <- as.character(ihd$year)


#write csv files to clean data folder
write_csv(ihd, "ihd_mortality_2014.csv", append = FALSE)

county <- st_read("counties_and_centroids.shp")
county$fips <- paste0("6", county$COUNTYF)




lcan <- read_csv("lcan_mortality_2019.csv")
lcan[is.na(lcan$race_name), "race_name" ] <- "Asian/Pacific Islander"

lcan$year <- as.character(lcan$year)
lcan$comment <- NA
lcan[is.na(lcan$mx), "comment"] <- "Estimate unavailable"
lcan$outcome_name <- "Lung cancer mortality"
lcan[lcan$age_name != "Age_standardized", "metric_name"] <- "rate"
lcan[lcan$age_name == "Age-standardized", "metric_name"] <- "age-standardized rate"
#cvd <- read_csv("cvd_mortality_2019.csv")
#cvd[is.na(cvd$race_name), "race_name" ] <- "API"
#cvd$fips <- str_pad(cvd$fips, width = 5, side = "left", pad = "0")
#cvd$year <- as.character(cvd$year)
#cvd$comment <- NA
#cvd[is.na(cvd$mx), "comment"] <- "Estimate unavailable"

asthma <- read_csv("asthma_prev_2021_2022.csv")
asthma <- merge(asthma, fips, by = "location_name")
asthma$race_name <- "Total"
asthma$sex_name <- "Both"
asthma$metric_name <- "prevalence"
asthma$outcome_name <- "Asthma"
asthma[is.na(asthma$comment), "comment"] <- "NA"
asthma[asthma$comment == "Estimate is statistically unstable. Caution is recommended when reporting or relying on statistically unstable estimates.", "comment"] <- "Unstable estimate"
asthma[asthma$comment == "Prevalence not available due to unreliable estimate", "comment"] <- "Estimate unavailable"
asthma[asthma$comment == "NA", "comment"] <- NA

alri <- read_csv("alri_2019.csv")
alri$year <- as.character(alri$year)
alri$comment <- NA
alri$outcome_name <- "Acute lower respiratory infection"
alri$metric_name <- "count"
alri <- as.data.frame(alri)

all_outcomes <- rbind(ac, lcan, ihd, asthma, alri)


all_outcomes <- merge(all_outcomes, counties, by = "fips")
all_outcomes <- st_as_sf(all_outcomes, crs = 4326)

all_outcomes[all_outcomes$outcome_name == "all_cause_mortality", "outcome_name"] <- "All-cause mortality"



st_write(all_outcomes, "all_outcomes.csv", layer_options = "GEOMETRY=AS_WKT", append = FALSE)
st_write(all_outcomes, "all_outcome_data.shp", append = FALSE)


ao <- read_csv("all_outcomes.csv")
ao <- st_as_sf(ao, geometry = "WKT", crs = 4326)

age <- "Age-standardized"
race <- "Black"
sex <- "Both"
outcome <- "All-cause mortality"

dat <- filter(ao, outcome_name == (switch(outcome, 
                                                  "All-cause mortality" = "all_cause_mortality",
                                                  "Ischemic heart disease mortality" = "Ischemic heart disease mortality", 
                                                  "Lung cancer mortality" = "lcan_mortality",
                                                  "Asthma prevalence" = "asthma_prevalence",
                                                  "Acute lower respiratory infection" = "alri")),
                       age_name == age,
                       race_name == race,
                       sex_name == sex)  %>%
  st_as_sf(., wkt = "WKT", crs = 4326)
  


# Classic palette BuPu, with 4 colors
pal <- brewer.pal(4, "BuPu") 

# Add more colors to this palette :
n <- nrow(dat)
color_scale <- colorNumeric(pal, domain = dat$mx)


leaflet(data = dat) %>%
  addTiles(group = "OpenStreetMap") %>%
  
  # Add boundary polygons
  addPolygons(data = dat,
              stroke = TRUE,
              fill = TRUE,
              weight = 3, 
              color = "darkgrey",
              fillColor = ~color_scale(mx),
              fillOpacity = 0.8,
              popup = paste0(dat$outcome_name, 
                             dat$metric_name, 
                             ":",
                             "<BR>", 
                             dat$mx, 
                             ", ", 
                             dat$uCI, 
                             "-", 
                             dat$lCI)) %>%
  
  # Add legend for initial load
  addLegend(position = "topright", 
            pal = color_scale, 
            values = dat$mx,
            title = paste0(dat$outcome_name[1], "<BR>", dat$metric_name[1]),
            opacity = 1)






output$map2 <- renderLeaflet({
  data <- demo_dat()
  if (is.null(data) || nrow(data) == 0) return()
  
  clr <- switch(rv$demo,
                "Population" = "Reds",
                "Population density" = "BuPu", 
                "Low income proportion" = "YlGn", 
                "Proportion children under 15" = "GnBu", 
                "Income inequality" = "Oranges")
  
  color_scale_ct <- colorBin(palette = clr, bins = 9, domain = data$voi, na.color = "lightgrey")
  
  leaflet(data = data) %>%
    addTiles(group = "OpenStreetMap") %>%
    
    #Add boundary polygons
    addPolygons(data = cma_bounds[cma_bounds$cma_nam == rv$cma, ],
                stroke = TRUE,
                fill = FALSE,
                weight = 3, 
                color = "darkgrey") %>%
    
    #Add polygons directly on initial load
    addPolygons(data = data,
                stroke = TRUE,
                color = "black",
                fill = TRUE,
                fillOpacity = 1,
                weight = 0.5,
                fillColor = ~ color_scale_ct(voi),
                popup = paste("Census tract: ", data$DGUID, "<BR>",
                              rv$demo, ":", switch(rv$demo,
                                                   "Population" = data$pop_2021,
                                                   "Population density" = data$pd_km2,
                                                   "Low income proportion" = data$LIMAT,
                                                   "Proportion children under 15" = data$a0_14,
                                                   "Income inequality" = data$GINI),
                              switch(rv$demo,
                                     "Population" = " persons",
                                     "Population density" = " persons/km2",
                                     "Low income proportion" = " %",
                                     "Proportion children under 15" = " %",
                                     "Income inequality" = ""))) %>%
    
    #Show legend on initial load
    addLegend(position = "topright", 
              pal = color_scale_ct, 
              values = data$voi,
              title = paste0(rv$demo, "<BR>", "decile"),
              opacity = 1)
})
#Update dynamically without clustering on input changes
# observe({
#    data <- demo_dat()
#    if (is.null(data) || nrow(data) == 0) return()

#    clr <- switch(rv$demo,
#                 "Population" = "Reds",
#                  "Population density" = "BuPu", 
#                  "Low income proportion" = "YlGn", 
#                  "Proportion children under 15" = "GnBu", 
#                  "Income inequality" = "Oranges")

#    color_scale_ct <- colorBin(palette = clr, bins = 9, domain = data$voi, na.color = "lightgrey")

#    leafletProxy("map2") %>%
#        clearShapes() %>%      
#        clearControls() %>%  

#        addPolygons(data = data,
#                   stroke = TRUE,
#                    color = "black",
#                    fill = TRUE,
#                    fillOpacity = 1,
#                    weight = 0.5,
#                    fillColor = ~ color_scale_ct(voi),
#                    popup = paste("Census tract: ", data$DGUID, "<BR>",
#                                   rv$demo, ":", switch(rv$demo,
#                                                        "Population" = data$pop_2021,
#                                                        "Population density" = data$pd_km2,
##                                                        "Low income proportion" = data$LIMAT,
#                                                       "Proportion children under 15" = data$a0_14,
#                                                       "Income inequality" = data$GINI),
#                                  switch(rv$demo,
#                                         "Population" = " persons",
#                                         "Population density" = " persons/km2",
#                                         "Low income proportion" = " %",
#                                         "Proportion children under 15" = " %",
#                                         "Income inequality" = ""))) %>%

#         #Update legend dynamically
#        addLegend(position = "topright", 
#                  pal = color_scale_ct, 
#                   values = data$voi,
#                  title = paste0(rv$demo),
#                   opacity = 1)
# })
}          

