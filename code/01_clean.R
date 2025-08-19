# ======================================================================
# Clean & prepare ANES and ESS datasets
# Author: Austin J. Brown
# ======================================================================

# ---------- Load Packages ----------
library(tidyverse)
library(haven)
library(rlang)
library(here)
library(janitor)
library(skimr)
library(stargazer)

source(here("code", "utils_cleaning.R"))

# ---- File Paths ----
anes_path <- here("data", "Research_DataANES.dta")
ess_path  <- here("data", "Research_DataESS_Clean.dta")

# ---- Execute Cleaning ----
anes_cleaned <- read_and_clean_anes(anes_path)
ess_cleaned  <- read_and_clean_ess(ess_path)

# ---- Quick QA summaries ----
skim(anes_cleaned)
skim(ess_cleaned)

# ---- Save outputs ----
saveRDS(anes_cleaned, here("data", "anes_cleaned.rds"))
saveRDS(ess_cleaned, here("data", "ess_cleaned.rds"))