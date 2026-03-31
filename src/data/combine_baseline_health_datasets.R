#############################################################################
# make merged health outcome datasets for input into the health impact assessments
#

 library(readr)
 library(dplyr)

# read in all cleaned health outcome dataframes
# all cause mortality IHME
ac1 <- read_csv("data/processed/ac_mortality_county_2019_IHME.csv", 
               col_types = cols(
                 geoid = col_character(),
                 geolevl = col_character(),
                 lctn_nm = col_character(),
                 age_grp = col_character(),
                 sex_grp = col_character(),
                 race_grp = col_character(),
                 otcm_nm = col_character(),
                 year = col_character(),
                 source = col_character(),
                 mx_name = col_character(),
                 mx = col_double(),
                 mx_lower = col_double(),
                 mx_upper = col_double(),
                 q_flag = col_double(),
                 .default = col_double() # sets default for unspecified columns
               ))

summary(ac1)

# all cause mortality CDC
ac2 <- read_csv("data/processed/ac_mortality_ct_tract_2020_USALEEP.csv", 
                col_types = cols(
                  geoid = col_character(),
                  geolevl = col_character(),
                  lctn_nm = col_character(),
                  age_grp = col_character(),
                  sex_grp = col_character(),
                  race_grp = col_character(),
                  otcm_nm = col_character(),
                  year = col_character(),
                  source = col_character(),
                  mx_name = col_character(),
                  mx = col_double(),
                  mx_lower = col_double(),
                  mx_upper = col_double(),
                  q_flag = col_double(),
                  .default = col_double() # sets default for unspecified columns
                ))
summary(ac2)
# ihd mortality CDC
ihd <- read_csv("data/processed/ihd_mortality_county_state_2019_CDC.csv",
                                col_types = cols(
                                  geoid = col_character(),
                                  geolevl = col_character(),
                                  lctn_nm = col_character(),
                                  age_grp = col_character(),
                                  sex_grp = col_character(),
                                  race_grp = col_character(),
                                  otcm_nm = col_character(),
                                  year = col_character(),
                                  source = col_character(),
                                  mx_name = col_character(),
                                  mx = col_double(),
                                  mx_lower = col_double(),
                                  mx_upper = col_double(),
                                  q_flag = col_double(),
                                  .default = col_double() # sets default for unspecified columns
                                ))
summary(ihd)
# lcan
lcan <- read_csv("data/processed/lcan_mortality_county_2019_IHME.csv",
                                 col_types = cols(
                                   geoid = col_character(),
                                   geolevl = col_character(),
                                   lctn_nm = col_character(),
                                   age_grp = col_character(),
                                   sex_grp = col_character(),
                                   race_grp = col_character(),
                                   otcm_nm = col_character(),
                                   year = col_character(),
                                   source = col_character(),
                                   mx_name = col_character(),
                                   mx = col_double(),
                                   mx_lower = col_double(),
                                   mx_upper = col_double(),
                                   q_flag = col_double(),
                                   .default = col_double() # sets default for unspecified columns
                                 ))
summary(lcan)

asth0 <- read_csv("data/processed/adult_currentasthma_ctract_county_2019_CDCPlaces.csv",
                                  col_types = cols(
                                    geoid = col_character(),
                                    geolevl = col_character(),
                                    lctn_nm = col_character(),
                                    age_grp = col_character(),
                                    sex_grp = col_character(),
                                    race_grp = col_character(),
                                    otcm_nm = col_character(),
                                    year = col_character(),
                                    source = col_character(),
                                    mx_name = col_character(),
                                    mx = col_double(),
                                    mx_lower = col_double(),
                                    mx_upper = col_double(),
                                    q_flag = col_double(),
                                    .default = col_double() # sets default for unspecified columns
                                  ))
summary(asth0)
# asthma (adult)
asth1 <- read_csv("data/processed/adult_currentasthma_state_county_2021_2022_CHIS.csv",
                                  col_types = cols(
                                    geoid = col_character(),
                                    geolevl = col_character(),
                                    lctn_nm = col_character(),
                                    age_grp = col_character(),
                                    sex_grp = col_character(),
                                    race_grp = col_character(),
                                    otcm_nm = col_character(),
                                    year = col_character(),
                                    source = col_character(),
                                    mx_name = col_character(),
                                    mx = col_double(),
                                    mx_lower = col_double(),
                                    mx_upper = col_double(),
                                    q_flag = col_double(),
                                    .default = col_double() # sets default for unspecified columns
                                  ))
summary(asth1)
# asthma (child)
asth2 <- read_csv("data/processed/child_currentasthma_state_county_2021_2022_CHIS.csv",
                                  col_types = cols(
                                    geoid = col_character(),
                                    geolevl = col_character(),
                                    lctn_nm = col_character(),
                                    age_grp = col_character(),
                                    sex_grp = col_character(),
                                    race_grp = col_character(),
                                    otcm_nm = col_character(),
                                    year = col_character(),
                                    source = col_character(),
                                    mx_name = col_character(),
                                    mx = col_double(),
                                    mx_lower = col_double(),
                                    mx_upper = col_double(),
                                    q_flag = col_double(),
                                    .default = col_double() # sets default for unspecified columns
                                  ))
summary(asth2)
# alri (child)
alri <- read_csv("data/processed/alri_county_2019_HCAI.csv",
                                 col_types = cols(
                                   geoid = col_character(),
                                   geolevl = col_character(),
                                   lctn_nm = col_character(),
                                   age_grp = col_character(),
                                   sex_grp = col_character(),
                                   race_grp = col_character(),
                                   otcm_nm = col_character(),
                                   year = col_character(),
                                   source = col_character(),
                                   mx_name = col_character(),
                                   mx = col_double(),
                                   mx_lower = col_double(),
                                   mx_upper = col_double(),
                                   q_flag = col_double(),
                                   .default = col_double() # sets default for unspecified columns
                                 ))
alri$otcm_nm <- "Acute lower respiratory infection (children)"
summary(alri)

data <- list(ac1, ac2, ihd, lcan, asth0, asth1, asth2, alri)

all_outcomes <- do.call(rbind, data)


all_outcomes <- all_outcomes[!is.na(all_outcomes$mx), ]


all_outcomes <- all_outcomes %>%
  mutate(mx_name = case_when(
    mx_name == "person-year at risk" ~ "deaths/person-year",
    mx_name == "person-years at risk" ~ "deaths/person-year",
    mx_name == "prevalence" ~ "prevalence"))

all_outcomes %>%
  count(otcm_nm, source, geolevl, year, age_grp) %>%
  print(n = Inf)


# if mx_lower and mx_upper are NA, use mx for both   
# change prevalence from a percent to a proportion
all_outcomes <- all_outcomes %>%
  mutate(
    mx       = case_when(
      mx_name == "deaths/person-year" ~ mx,
      mx_name == "prevalence"         ~ mx / 100
    ),
    mx_lower = case_when(
      is.na(mx_lower) ~ mx,
      mx_name == "deaths/person-year" ~ mx_lower,
      mx_name == "prevalence"         ~ mx_lower / 100,

    ),
    mx_upper = case_when(
      is.na(mx_upper) ~ mx,
      mx_name == "deaths/person-year" ~ mx_upper,
      mx_name == "prevalence"         ~ mx_upper / 100,
      
    )
  )

# Exclude "All ages" from all_outcomes 
all_outcomes <- all_outcomes %>%
  filter(age_grp != "All ages")

# recode outcome name to distinguish between adult and child asthma
all_outcomes <- all_outcomes %>%
  mutate(otcm_nm = case_when(
    otcm_nm == "Current asthma prevalence" & age_grp %in% 
      c("18 plus", "18 to 64", "65 plus")  ~ "Current asthma prevalence (adults)",
    otcm_nm == "Current asthma prevalence" & age_grp %in% 
      c("0 to 4", "0 to 17", "5 to 17")   ~ "Current asthma prevalence (children)",
    TRUE                                   ~ otcm_nm
  ))


write_csv(all_outcomes, "data/processed/all_baseline_outcomes.csv", append = FALSE)

rm(ac1, ac2, asth0, asth1, asth2, ihd, lcan, alri, data)
gc()
