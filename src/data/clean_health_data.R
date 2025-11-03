library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(stringr)
library(leaflet)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)

# load county and census tract shapefiles
counties <- st_read("BREATHE HEALTH/data/raw/counties_and_centroids.shp")
counties <- st_as_sf(counties, crs = 4326)
counties <- counties[, 2]

ct_tract <- st_read("BREATHE HEALTH/data/raw/ct_tract.shp")

# load mortality data, limit to needed columns and rows

acm <- read_csv("BREATHE HEALTH/data/raw/IHME_USA_COD_COUNTY_RACE_ETHN_2000_2019_MX_2019_ALL_BOTH_Y2023M06D12.CSV")

acm <- acm[grep("California", acm$location_name), c(3:5,7,9,11,13,14,16:19)]

#standardize column names and limit to census tracts in the SFBA of interest

acm$location_name <- sub(" County (California)", "", acm$location_name, fixed = TRUE)

sfba <- c("Alameda", 
          "Contra Costa", 
          "Marin", 
          "Napa", 
          "San Francisco", 
          "San Mateo", 
          "Santa Clara", 
          "Solano", 
          "Sonoma")


acm <- acm[acm$location_name %in% sfba, ] 

acm[acm$race_name == "AIAN", 'race_name'] <- "American Indian / Alaskan Native"
acm[acm$race_name == "API", 'race_name'] <- "Asian / Pacific Islander"
acm[acm$race_name == "Latino", 'race_name'] <- "Hispanic"

acm$race_name <- factor(acm$race_name, levels = c("American Indian / Alaskan Native", "Black", "White", "Hispanic", "Asian / Pacific Islander", "Total"))  

acm$age_name <- factor(acm$age_name, levels = c("<1 year", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",        
                                                            "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",        
                                                            "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",        
                                                            "85 plus", "All Ages", "Age-standardized"))  



acm$cause_name <- "All-cause mortality"

#acm$mx <- acm$val * 100000
#acm$uCI <- acm$upper * 100000
#acm$lCI <- acm$lower * 100000

acm <- acm[, c(2:12)]
colnames(acm) <- c("location_name",
                         "fips",
                         "race_name",
                         "sex_name",
                         "age_name",
                         "outcome_name", 
                         "year",          
                         "metric_name",
                         "mx",
                         "uCI",          
                         "lCI"
                         )

acm$fips <- str_pad(acm$fips, width = 5, side = "left", pad = "0")

#write csv files to clean data folder
write_csv(acm, "BREATHE HEALTH/data/processed/all_cause_mortality_2019.csv", append = FALSE)


#############################################################################
# Cause-specific mortality: CVD 2014

ihd <- read_csv("BREATHE HEALTH/data/raw/IHME_USA_COUNTY_CVD_MORTALITY_RATES_1980_2014_CALIFORNIA_Y2017M05D16.CSV")

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

ihd$race_name <- "Total"

ihd <- ihd[ihd$location_name %in% sfba, c(4,5, 17, 9, 11, 7, 12:16)] 

ihd[ihd$age_name == "Age-standardized", "age_name"] <- "All ages"

ihd[ihd$metric == "Rate", "metric"] <- "Age-standardized rate"


# Calculate number of                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
#ihd$mx <- ihd$mx * 100000
#ihd$upper <- ihd$upper * 100000
#ihd$upper <- ihd$lower * 100000

colnames(ihd) <- c("location_name",
                         "fips",
                         "race_name",
                         "sex_name",
                         "age_name",
                         "outcome_name", 
                         "year",          
                         "metric_name",
                         "mx",
                         "uCI",          
                         "lCI"
)



ihd$fips <- str_pad(ihd$fips, width = 5, side = "left", pad = "0")

#write csv files to clean data folder
write_csv(ihd, "BREATHE HEALTH/data/processed/cvd_mortality_2014.csv", append = FALSE)


#############################################################################
# Cause-specific mortality Lung cancer 2014
lcan <- read_csv("BREATHE HEALTH/data/raw/IHME_USA_COUNTY_CANCER_MORTALITY_RATES_1980_2014_CALIFORNIA_Y2017M01D24.CSV")

lcan <- lcan[lcan$year_id == 2014, ]

#standardize column names and limit to census tracts in the SFBA of interest
lcan$location_name <- sub(" County", "", lcan$location_name, fixed = TRUE)

lcan <- lcan[lcan$location_name %in% sfba, ] 

lcan$race_name <- "Total"
lcan$metric_name <- "Rate"
lcan$age_name <- "All ages"

lcan <- lcan[lcan$cause_name == "Tracheal, bronchus, and lung cancer", c(2,3,12,7,14,5,8,13,9,10,11)] 
lcan$metric_name <- "Rate"
lcan$cause_name <- "Tracheal, bronchus and lung cancer mortality"
colnames(lcan) <- c("location_name",
                   "fips",
                   "race_name",
                   "sex_name",
                   "age_name",
                   "outcome_name", 
                   "year",          
                   "metric_name",
                   "mx",
                   "uCI",          
                   "lCI"
)


#write csv files to clean data folder

write_csv(lcan, "BREATHE HEALTH/data/processed/lc_mortality_2014.csv")

####################################################################################################
# Lung cancer mortality 2019

lcan_b <- read_csv("BREATHE HEALTH/data/raw/IHME_USA_LUNG_CANCER_COUNTY_RACE_ETHNICITY_2000_2019_MX_2019_BOTH_Y2025M06D15.csv")
lcan_b <- lcan_b[grep("California", lcan_b$location_name), c(3:5,7,9,11,13,14,16:19)]
lcan_b <- as.data.frame(lcan_b)

lcan_f <- read_csv("BREATHE HEALTH/data/raw/IHME_USA_LUNG_CANCER_COUNTY_RACE_ETHNICITY_2000_2019_MX_2019_FEMALE_Y2025M06D15.csv")
lcan_f <- lcan_f[grep("California", lcan_f$location_name), c(3:5,7,9,11,13,14,16:19)]
lcan_f <- as.data.frame(lcan_f)

lcan_m <- read_csv("BREATHE HEALTH/data/raw/IHME_USA_LUNG_CANCER_COUNTY_RACE_ETHNICITY_2000_2019_MX_2019_MALE_Y2025M06D15.csv")
lcan_m <- lcan_m[grep("California", lcan_m$location_name), c(3:5,7,9,11,13,14,16:19)]
lcan_m <- as.data.frame(lcan_m)


lcan <- bind_rows(lcan_b, lcan_f, lcan_m)

rm(lcan_b, lcan_f, lcan_m)

#standardize column names and limit to census tracts in the SFBA of interest

lcan$location_name <- sub(" County (California)", "", lcan$location_name, fixed = TRUE)

sfba <- c("Alameda", 
          "Contra Costa", 
          "Marin", 
          "Napa", 
          "San Francisco", 
          "San Mateo", 
          "Santa Clara", 
          "Solano", 
          "Sonoma")


lcan <- lcan[lcan$location_name %in% sfba, ] 

lcan[lcan$race_name == "AIAN", 'race_name'] <- "American Indian / Alaskan Native"
lcan[lcan$race_name == "API", 'race_name'] <- "Asian / Pacific Islander"
lcan[lcan$race_name == "Latino", 'race_name'] <- "Hispanic"

lcan$race_name <- factor(lcan$race_name, levels = c("American Indian / Alaskan Native", "Black", "White", "Hispanic", "Asian / Pacific Islander", "Total"))  

lcan$age_name <- factor(lcan$age_name, levels = c("<1 year", "1 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24",        
                                                "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54",        
                                                "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84",        
                                                "85 plus", "All Ages", "Age-standardized"))  
lcan$cause_name <- "Lung cancer mortality"

#lcan$mx <- lcan$val * 100000
#lcan$uCI <- lcan$upper * 100000
#lcan$lCI <- lcan$lower * 100000

lcan <- lcan[, c(2:12)]
colnames(lcan) <- c("location_name",
                   "fips",
                   "race_name",
                   "sex_name",
                   "age_name",
                   "outcome_name", 
                   "year",          
                   "metric_name",
                   "mx",
                   "uCI",          
                   "lCI"
)

lcan$fips <- str_pad(lcan$fips, width = 5, side = "left", pad = "0")

#write csv files to clean data folder
write_csv(lcan, "BREATHE HEALTH/data/processed/lcan_mortality_2019.csv", append = FALSE)

lcan_b <- lcan[lcan$sex_name == "Both", ]
lcan_c <- lcan_b[lcan_b$race_name == "Total", ]
lcan_d <- lcan_c[lcan_c$age_name == "Age-standardized", ]
lcan_e <- drop_na(lcan_d)

# lung cancer, age standardized
my_palette <- brewer.pal(8, "Set2") 

# Add more colors to this palette :
my_palette <- colorRampPalette(my_palette)(9)

# Plot it
pie(rep(1, length(my_palette)), col = my_palette , main="") 

ac_mort1 <- lcan_e %>%
  ggplot(aes(x = location_name, y = mx)) +
  geom_bar(aes(fill = location_name), stat = "identity", position = "dodge") +
  guides(fill = "none") +
  geom_errorbar(aes(ymin=lCI, ymax=uCI, group = location_name), 
                position = position_dodge(.9), 
                stat = "identity",
                width = 0.25, 
                colour="orange", 
                alpha=0.9, 
                linewidth=0.5) +
  xlab("County") + # for the x axis label
  ylab("Age-adjusted, lung cancer mortality (deaths per 100,000 pop)") + # for the y axis label
  labs(title = "Age-adjusted, lung cancer mortality in 2019 by county") +
  scale_fill_brewer(palette = "Set3") +
  theme_classic()

ac_mort1


#lung cancer, age standardized, by race

ac_mort1 <- lcan_b[lcan_b$age_name == "Age-standardized", ] %>%
  ggplot(aes(x = location_name, y = mx)) +
  geom_bar(aes(fill = race_name), stat = "identity", position = "dodge") +

  geom_errorbar(aes(ymin=lCI, ymax=uCI, group = race_name), 
                position = position_dodge(.9), 
                stat = "identity",
                width = 0.25, 
                colour="orange", 
                alpha=0.9, 
                linewidth=0.5) +
  xlab("County") + # for the x axis label
  ylab("Age-adjusted, lung cancer mortality (deaths per 100,000 pop)") + # for the y axis label
  labs(title = "Age-adjusted, lung cancer mortality in 2019, by race", fill = "Race") +
  scale_fill_brewer(palette = "Set2") +

  theme_classic()

ac_mort1

# LC deaths by age (all races, sexes)

lcan_b <- lcan[lcan$sex_name == "Both", ]
lcan_b <- lcan_b[lcan_b$race_name == "Total", ]

#ages for which 2019 data has >0 lung cancer deaths
lc_ages <- c("15 to 19",         
             "20 to 24",        
             "25 to 29",         
             "30 to 34",         
             "35 to 39",         
             "40 to 44",
             "45 to 49",         
             "50 to 54",        
             "55 to 59",         
             "60 to 64",         
             "65 to 69",         
             "70 to 74",         
             "75 to 79",         
             "80 to 84",        
             "85 plus")

lcan_b <- lcan_b[lcan_b$age_name %in% lc_ages, ]
# Classic palette BuPu, with 4 colors

my_palette <- brewer.pal(8, "Set2") 

# Add more colors to this palette :
my_palette <- colorRampPalette(my_palette)(15)

# Plot it
pie(rep(1, length(my_palette)), col = my_palette , main="") 

ac_mort1 <- lcan_b %>%
  ggplot(aes(x = location_name, y = mx)) +
  geom_bar(aes(fill = age_name), stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=lCI, ymax=uCI, group = age_name), 
                position = position_dodge(.9), 
                stat = "identity",
                width = 0.25, 
                colour="orange", 
                alpha=0.9, 
                linewidth=0.5) +
  xlab("County") + # for the x axis label
  ylab("Lung cancer mortality (deaths per 100,000 pop)") + # for the y axis label
  labs(title = "Lung cancer mortality by age group, 2019", fill = "Age Group") +
  #scale_fill_brewer(palette = my_palette) +
  theme_classic()

ac_mort1

# LC deaths by sex (all races, age adjusted)
lcan_b <- lcan[lcan$race_name == "Total", ]
lcan_b <- drop_na(lcan_b)


ac_mort1 <- lcan_b[lcan_b$age_name == "Age-standardized", ] %>%
  ggplot(aes(x = location_name, y = mx)) +
  geom_bar(aes(fill = sex_name), stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=lCI, ymax=uCI, group = sex_name), 
                position = position_dodge(.9), 
                stat = "identity",
                width = 0.25, 
                colour="orange", 
                alpha=0.9, 
                linewidth=0.5) +
  xlab("County") + # for the x axis label
  ylab("Age-standardized lung cancer mortality (deaths per 100,000 pop)") + # for the y axis label
  labs(title = "Age-standardized lung cancer mortality by sex, 2019", fill = "Sex") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic()

ac_mort1
# merge shapefiles with data

all_cause <- all_cause[all_cause$race_name == "Total", ]

ac_shp <- merge(counties, all_cause, by.x = "NAME", by.y = "location_name")

ac_shp <- st_as_sf(ac_shp)
#ac_shp <- st_transform(ac_shp)


pal <- colorNumeric(
  palette = "OrRd", domain = ac_shp$deaths,
  na.color = "transparent"
)

leaflet(data = baseline) %>% 
  addTiles(group = "OpenStreetMap") %>%
  
  addPolygons(stroke = TRUE, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = ~pal(deaths),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  
  
#      opacity = 1,
#      values = ~ (switch(rv$dom, 
#                  "Playability Score" = dc_ps,
#                  "Space for play" = dc_sfp,
#                  "Social environment" = dc_sdi,
#                  "Traffic environment" = dc_ted,
#                  "Natural environment" = dc_ned,
#                  "Child-relevant destinations" = dc_crd)), 
#      na.label = "missing data",
#      title = paste0(as.character(rv$dom), "<br>(metro area decile)"))



baseline <- all_cause[all_cause$race_name == "Total",]
baseline <- baseline[baseline$age_name == "Age-standardized", ]

# merge with all_cause mortality data

baseline <- merge(baseline, counties, by.x = "location_name", by.y = "NAME", all.x = TRUE)
baseline <- st_as_sf(baseline)
baseline <- st_transform(baseline, 4326)

pal <- colorNumeric(
  palette = "Blues", domain = baseline$deaths,
  na.color = "transparent"
)

leaflet(data = baseline) %>% 
  addTiles(group = "OpenStreetMap") %>%
  
  addPolygons(stroke = TRUE, color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.8,
              fillColor = pal),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend()

%>%
  
  addLabelOnlyMarkers(data = centroids,
                      lng = ~Lon, lat = ~Lat, label = ~NAME,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) 

  #addLegend(data = ps_dat,
            #      position = "topright",
            #      pal = ~color_ps(),
            #      opacity = 1,
            #      values = ~ (switch(rv$dom, 
            #                  "Playability Score" = dc_ps,
            #                  "Space for play" = dc_sfp,
            #                  "Social environment" = dc_sdi,
            #                  "Traffic environment" = dc_ted,
            #                  "Natural environment" = dc_ned,
            #                  "Child-relevant destinations" = dc_crd)), 
            #      na.label = "missing data",
            #      title = paste0(as.character(rv$dom), "<br>(metro area decile)"))
  
  
                  
# Make cvd map
baseline <- cvd[cvd$sex == "Both",]
baseline <- baseline[baseline$cause_name == "Ischemic heart disease", ]
baseline <- baseline[baseline$age_name == "Age-standardized", ]

# merge with all_cause mortality data

baseline <- merge(baseline, counties, by.x = "location_name", by.y = "NAME", all.x = TRUE)
baseline <- st_as_sf(baseline)
baseline <- st_transform(baseline, 4326)

pal <- colorNumeric(
  palette = "BuGn", domain = acm$mx,
  na.color = "transparent"
)

leaflet(data = acm) %>% 
  addTiles(group = "OpenStreetMap") %>%
  
  addPolygons(data = acm,
              stroke = TRUE, 
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.8,
              fillColor = pal,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend(data = baseline,
            position = "topright",
            pal = pal,
            values = ~mx,
            opacity = 1,
            title = "Age-adjusted mortality <br>from IHD<br>(per 100,000 pop)"
            )

%>%