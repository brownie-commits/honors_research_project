# Honors Research Project

Analysis of political trust using ANES (American National Election Studies) and ESS (European Social Survey) data. Examines relationships between economic perceptions, demographics, and political trust indicators through linear regression models.

## Research Focus

- How do economic conditions affect political trust?
- What role do demographic factors play in trust attitudes?
- Comparative analysis between US (ANES) and European (ESS) contexts

## Requirements

- R (>= 4.0)
- Required packages:
  - tidyverse
  - haven
  - here
  - janitor
  - skimr
  - stargazer
  - corrplot

Install packages:
```r
install.packages(c("tidyverse", "haven", "here", "janitor", "skimr", "stargazer", "corrplot"))
```

## Usage

### Run the Analysis Pipeline

```r
# 1. Clean and prepare data
source("code/01_clean.R")

# 2. Exploratory analysis
source("code/02_data_analysis.R")

# 3. Run regression models
source("code/04_models.R")
```

## Project Structure

```
honors_research_project/
├── code/
│   ├── 00_utils_cleaning.R  # Utility functions
│   ├── 01_clean.R           # Data cleaning
│   ├── 02_data_analysis.R   # Exploratory analysis
│   ├── 04_models.R          # Regression models
│   └── archive/             # Old code versions
├── data/
│   ├── Research_DataANES.dta
│   ├── Research_DataESS_Clean.dta
│   ├── anes_cleaned.rds
│   └── ess_cleaned.rds
└── results/
    └── model*.html          # Regression tables
```

## Data Sources

- **ANES**: American National Election Studies
- **ESS**: European Social Survey
