###############################################################################

# Data Cleaning Script

###############################################################################

anes <- read_dta("data/Research_DataANES.dta")

clean_anes <- function(df) {
  # Step 1: Rename raw variables
  df <- df %>%
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
  
  # Step 2: Define NA codes
  na_codes <- list(
    votechoice = c(-1, 3, 4, 5, 7, 9, -6, -7, -8, -9),
    default = c(-1, -5, -6, -7, -8, -9, 90, 95)
  )
  
  # Step 3: Define reverse coding rules
  reverse_rules <- list(
    votechoice = function(x) case_when(x == 2 ~ 1, x == 1 ~ 0, TRUE ~ x),
    race = function(x) case_when(x %in% c(2, 3, 4, 5, 6) ~ 0, TRUE ~ x),
    gender = function(x) case_when(x == 2 ~ 0, TRUE ~ x),
    poltrst = function(x) case_when(x == 5 ~ 1, x == 2 ~ 4, x == 1 ~ 5, x == 4 ~ 2, TRUE ~ x),
    strglead = function(x) case_when(x == 5 ~ 1, x == 2 ~ 4, x == 1 ~ 5, x == 4 ~ 2, TRUE ~ x),
    econcond = function(x) case_when(x == 5 ~ 1, x == 2 ~ 4, x == 1 ~ 5, x == 4 ~ 2, TRUE ~ x),
    finsit = function(x) case_when(x == 5 ~ 1, x == 2 ~ 4, x == 1 ~ 5, x == 4 ~ 2, TRUE ~ x)
  )
  
  # Step 4: Clean NA codes
  clean_var <- function(x, colname, na_codes) {
    x <- haven::zap_labels(x)
    na_vals <- na_codes[[colname]] %||% na_codes$default
    case_when(
      x %in% na_vals ~ NA_real_,
      TRUE ~ as.numeric(x)
    )
  }
  
  df <- df %>%
    mutate(across(everything(), ~ clean_var(.x, cur_column(), na_codes)))
  
  # Step 5: Reverse code
  reverse_var <- function(x, colname, reverse_rules) {
    rule <- reverse_rules[[colname]]
    if (!is.null(rule)) rule(x) else x
  }
  
  df <- df %>%
    mutate(across(everything(), ~ reverse_var(.x, cur_column(), reverse_rules)))
  
  # Step 6: Rename with prefixes
  rename_map <- c(
    "cgender" = "gender",
    "rpoltrst" = "poltrst",
    "rstrglead" = "strglead",
    "reconcond" = "econcond",
    "rfinsit" = "finsit",
    "cvotechoice" = "votechoice",
    "crace" = "race"
  )
  
  df <- df %>% rename(!!!rename_map)
  
  return(df)
}

cleaned_anes <- clean_anes(anes)



