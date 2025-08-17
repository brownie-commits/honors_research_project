######################################################################

# Packages and Functions 

# Date: 8-16-2025

# Author: Austin J. Brown

######################################################################


# Load Packages -----------------------------------------------------------

library(tidyverse) # cleaning and plotting
library(here) # finding files
library(haven)
library(car)
library(stargazer) # nice tables
library(rlang)


# ANES Functions ----------------------------------------------------------

# Rename map

anes_rename_map <- c(
  "cgender" = "gender",
  "rpoltrst" = "poltrst",
  "rstrglead" = "strglead",
  "reconcond" = "econcond",
  "rfinsit" = "finsit",
  "cvotechoice" = "votechoice",
  "crace" = "race"
)

# List of codes to make NA

anes_na_codes <- list(
  votechoice = c(-1, 3, 4, 5, 7, 9, -6, -7, -8, -9),
  default = c(-1, -5, -6, -7, -8, -9, 90, 95)
)

# List of reverse rules

anes_reverse_rules <- list(
  votechoice = function(x) case_when(x == 2 ~ 1, x == 1 ~ 0, TRUE ~ x),
  race = function(x) case_when(x %in% c(2, 3, 4, 5, 6) ~ 0, TRUE ~ x),
  gender = function(x) case_when(x == 2 ~ 0, TRUE ~ x),
  poltrst = function(x) case_when(x == 5 ~ 1, x == 2 ~ 4, x == 1 ~ 5, x == 4 ~ 2, TRUE ~ x),
  strglead = function(x) case_when(x == 5 ~ 1, x == 2 ~ 4, x == 1 ~ 5, x == 4 ~ 2, TRUE ~ x),
  econcond = function(x) case_when(x == 5 ~ 1, x == 2 ~ 4, x == 1 ~ 5, x == 4 ~ 2, TRUE ~ x),
  finsit = function(x) case_when(x == 5 ~ 1, x == 2 ~ 4, x == 1 ~ 5, x == 4 ~ 2, TRUE ~ x)
)

# Clean to NA function

anes_clean_var <- function(x, colname, na_codes) {
  x <- haven::zap_labels(x)
  na_vals <- na_codes[[colname]] %||% na_codes$default
  case_when(
    x %in% na_vals ~ NA_real_,
    TRUE ~ as.numeric(x)
  )
}

# Reverse Likert scale function

anes_reverse_var <- function(x, colname, reverse_rules) {
  rule <- reverse_rules[[colname]]
  if (!is.null(rule)) rule(x) else x
}

# ESS Functions -----------------------------------------------------------

# Turn value to zero

ess_code_to_zero <- function(x, codes) case_when(x %in% codes ~ 0, TRUE ~ x)

# Turn value to one

ess_code_to_one  <- function(x, codes) case_when(x %in% codes ~ 1, TRUE ~ x)

# Turn value to NA

ess_code_to_na   <- function(x, codes) case_when(x %in% codes ~ NA_real_, TRUE ~ x)