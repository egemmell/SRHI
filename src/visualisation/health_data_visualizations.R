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


# All cause mortality visualizations

acm <- read_csv("BREATHE HEALTH/data/processed/all_cause_mortality_2019.csv")

# make grouped bar plots (subset as needed)
acm <- acm[acm$age_name == "Age-standardized",]

# all races (not stratified by race)
#acm <- acm[acm$race_name == "Total", ]

# by race
ac_mort1 <- acm[acm$age_name == "Age-standardized", ] %>%
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
  ylab("Age-adjusted, all-cause mortality (deaths per 100,000 pop)") + # for the y axis label
  labs(title = "Age-adjusted, all-cause mortality in 2019, by race", fill = "Race") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic()

ac_mort1

#########################################
# barplot Ischemic heart disease mortality by county and sex
ihd_mort <- ihd %>%
  ggplot(aes(x = location_name, y = mx)) +
  geom_bar(aes(fill = sex_name), 
           stat = "identity", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin=lCI, ymax=uCI, group = sex_name), 
                position = position_dodge(.9), 
                stat = "identity",
                width = 0.25, 
                colour="orange", 
                alpha=0.9, 
                linewidth=0.5) +
  xlab("County") + # for the x axis label
  ylab("Age-adjusted mortality (deaths/100,000 pop)") + # for the y axis label
  labs(title = "Age-adjusted mortality from ischemic heart disease in 2014, by sex", fill = "Sex") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic()

ihd_mort


# barplot 2014 age-adjusted lung cancer mortality by sex
lc_mort <- lcan %>%
  ggplot(aes(x = location_name, y = mx)) +
  geom_bar(aes(fill = sex_name), 
           stat = "identity", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin=lCI, ymax=uCI, group = sex_name), 
                position = position_dodge(.9), 
                stat = "identity",
                width = 0.25, 
                colour="orange", 
                alpha=0.9, 
                linewidth=0.5) +
  xlab("County") + # for the x axis label
  ylab("Age-adjusted mortality (deaths per 100,000 pop)") + # for the y axis label
  labs(title = "Age-adjusted mortality from tracheal, bronchus and lung cancer in 2014, by sex", fill = "Sex") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic()

lc_mort

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
  addLegend() %>%

  
  addLabelOnlyMarkers(data = centroids,
                      lng = ~Lon, lat = ~Lat, label = ~NAME,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) 


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