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
# Ischemic heart disease mortality by county and sex
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

