# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Honors research project analyzing political trust using survey data from ANES (American National Election Studies) and ESS (European Social Survey). Uses linear regression models to examine relationships between economic conditions, demographics, and political trust indicators.

## Project Structure

```
honors_research_project/
├── code/
│   ├── 00_utils_cleaning.R  # Utility functions and lookup maps
│   ├── 01_clean.R           # Data cleaning pipeline
│   ├── 02_data_analysis.R   # Exploratory analysis
│   ├── 04_models.R          # Regression models
│   └── archive/             # Old/experimental code
├── data/
│   ├── Research_DataANES.dta    # Raw ANES data (Stata format)
│   ├── Research_DataESS_Clean.dta  # Raw ESS data (Stata format)
│   ├── anes_cleaned.rds     # Cleaned ANES data
│   └── ess_cleaned.rds      # Cleaned ESS data
└── results/
    └── model*.html          # Stargazer regression output tables
```

## Key Commands

### Run Full Pipeline
```r
# In R, from project root:
source("code/01_clean.R")      # Clean data
source("code/02_data_analysis.R")  # Analysis
source("code/04_models.R")     # Run models
```

### Run Individual Scripts
```bash
Rscript code/01_clean.R
```

## R Dependencies

- tidyverse, haven (data import/manipulation)
- here (path management)
- janitor, skimr (data quality)
- stargazer, corrplot (output/visualization)

## Data Schema

### ANES Variables (after cleaning)
- `poltrst` - Political trust (reversed scale)
- `strglead` - Strong leader preference
- `econcond` - Economic conditions assessment
- `finsit` - Financial situation
- `votechoice` - Vote choice (1=Republican, 0=Democrat)
- `race` - Race (1=White, 0=Non-white)
- `gender` - Gender (1=Male, 0=Female)

### ESS Variables
- `trstplt` - Trust in politicians
- `ipstrgv` - Strong government preference
- `psppsgva` - Political system allows say
- `stfeco` - Satisfaction with economy
- `hincfel` - Feeling about household income

## Key Patterns

### Script Numbering
Scripts are numbered to indicate execution order:
- `00_` - Utilities/helpers (sourced by other scripts)
- `01_` - Data cleaning
- `02_` - Analysis
- `04_` - Models

### here() Package
All paths use `here()` for portability:
```r
source(here("code", "00_utils_cleaning.R"))
anes_path <- here("data", "Research_DataANES.dta")
```

### Variable Recoding
ANES uses custom NA codes (-1, -5, -6, -7, -8, -9) and reversed scales. See `anes_na_codes` and `anes_reverse_rules` in `00_utils_cleaning.R`.

### Model Output
Models output HTML tables via stargazer to `results/` directory.
