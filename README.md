# BREATHE: Scenario-Related Health Impacts

## About
This repository contains the R code for cleaning, preparing, and analyzing 
health data for the BREATHE project — a Health Impact Assessment (HIA) 
pipeline for the San Francisco Bay Area. The pipeline estimates how changes 
in air pollutant concentrations (PM2.5, NO2, and black carbon) affect 
population health outcomes across Bay Area census tracts.

Data will be shared securely with team members as not all data is publicly 
available.

---

## Repository Structure
```
├── README.md                  <- You are here
├── CHANGES.md                 <- Version and change log
├── DATAINFO.md                <- Data sources and access instructions
├── TASKS.md                   <- Outstanding tasks and TODOs
├── BREATHE_PROJECT.Rproj      <- RStudio project file
│
├── data
│   ├── raw                    <- Original, unmodified source data
│   └── processed              <- Cleaned and analysis-ready datasets
│
├── output
│   └── figures                <- Generated plots and visualizations
│
├── docs                       <- Project documentation
│
└── src                        <- All R source code
    ├── data                   <- Data cleaning and preparation scripts
    │   ├── clean_all_cause_mortality_2019_IHME.R
    │   ├── clean_all_cause_mortality_2024_USALEEP.R
    │   ├── clean_ischemic_heart_disease_mortality_data_2019_CDC.R
    │   ├── clean_lung_cancer_mortality_data_2019_IHME.R
    │   ├── clean_alri_2019_HCAi.R
    │   ├── clean_asthma_data_2021_2022_CHIS.R
    │   ├── clean_adult_asthma_data_2019_CDCPlaces.R
    │   ├── combine_baseline_health_datasets.R
    │   ├── make_demographic_data_0_17_sex_race_2019_SEER.R
    │   └── make_census_boundary_files.R
    │
    ├── analysis               <- HIA analysis scripts
    │   ├── prepare_exposure_population_inputs.R
    │   └── impact_analysis_03232026.R
    │
    ├── visualisation          <- Visualization scripts
    │
    └── scratch                <- Exploratory and development scripts (not for production)
```

---

## Data Sources

| Dataset | Source | Years |
|---|---|---|
| All-cause mortality | IHME | 2019 |
| All-cause mortality | USALEEP | 2024 |
| Ischemic heart disease mortality | CDC | 2019 |
| Lung cancer mortality | IHME | 2019 |
| Acute lower respiratory infection | HCAi | 2019 |
| Asthma (pediatric) | SEER | 2019 |
| Asthma (adult) | CHIS | 2021–2022 |
| Asthma (adult prevalence) | CDC PLACES | 2019 |
| Census tract boundaries | U.S. Census Bureau TigerLine | 2024 |

*See `DATAINFO.md` for access instructions and data sharing agreements.*

---

## Requirements

- R (>= 4.0)
- Key packages: `jsonlite`, `sf`, `dplyr`, `httr`, `dplyr`, `readr`, `sf`,
`ggplot2`, `RColorBrewer`, `tibble`, `stringr`, `tidyr`, `purrr`, `vroom`

---

## Usage

1. Clone the repository
2. Obtain required datasets per `DATAINFO.md` and place in `data/raw/`
3. Run data cleaning scripts in `src/data/` 
4. Run `src/data/combine_baseline_health_datasets.R`
5. Run `src/analysis/prepare_exposure_population_inputs.R`
6. Run `src/analysis/impact_analysis_03232026.R`

---

## Contributors


## License
