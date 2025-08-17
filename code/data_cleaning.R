######################################################################

# Data Cleaning

# Date: 8-16-2025

# Author: Austin J. Brown

######################################################################


# Data import -------------------------------------------------------------

#### STATA Research - ANES 2016 #####

# Load ANES 2016 data into R # 

anes <- read_dta("data/Research_DataANES.dta")

ess <- read_dta("data/Research_DataESS_Clean.dta")

# ANES Data Cleaning ------------------------------------------------------

# Rename Variables

anes <- anes %>%
  rename(
    poltrst = V161215,
    strglead = V162263,
    pplrule = V162264,
    econcond = V161140x,
    finsit = V162165,
    imharm = V162269,
    imecon = V162268,
    imempl = V162158,
    votechoice = V162034a,
    income = V161361x,
    race = V161310x,
    gender = V161342,
    edulevel = V161270
  ) %>%
  select(poltrst, strglead, pplrule, econcond, finsit, imharm, imecon, imempl,
         votechoice, income, race, gender, edulevel)

# Clean up NA codes

anes <- anes %>%
  mutate(across(everything(), ~ anes_clean_var(.x, cur_column(), anes_na_codes)))

# Reverse the Likert scale for select variables

anes <- anes %>%
  mutate(across(everything(), ~ anes_reverse_var(.x, cur_column(), anes_reverse_rules)))

# Rename variables

cleaned_anes <- anes %>% rename(!!!anes_rename_map)

# ESS 2017 Data Cleaning --------------------------------------------------

# Re-coding variables

ess_clean <- ess %>%
  mutate(
    uemp3m   = ess_code_to_zero(uemp3m, 2),
    prtvtbit  = ess_code_to_zero(prtvtbit, c(1:8, 10:13)) %>%  ess_code_to_one(9) %>%  ess_code_to_na(14),
    prtvtfnl  = ess_code_to_zero(prtvtfnl, c(1:2, 4:11)) %>%  ess_code_to_one(3) %>%  ess_code_to_na(16),
    edlvdit   = ess_code_to_na(edlvdit, 5555),
    edlvenl   = ess_code_to_na(edlvenl, 5555),
    gndr      = ess_code_to_zero(gndr, 2)
  )