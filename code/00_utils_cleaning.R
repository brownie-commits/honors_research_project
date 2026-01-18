# ======================================================================
# Utility functions + lookup maps for ANES & ESS cleaning
# Author: Austin J. Brown
# ======================================================================

# ---------- Lookup Maps ----------
anes_rename_map <- c(
  "cgender"    = "gender",
  "rpoltrst"   = "poltrst",
  "rstrglead"  = "strglead",
  "reconcond"  = "econcond",
  "rfinsit"    = "finsit",
  "cvotechoice"= "votechoice",
  "crace"      = "race"
)

anes_na_codes <- list(
  votechoice = c(-1, 3, 4, 5, 7, 9, -6, -7, -8, -9),
  default    = c(-1, -5, -6, -7, -8, -9, 90, 95)
)

anes_reverse_rules <- list(
  votechoice = function(x) case_when(x == 2 ~ 1, x == 1 ~ 0, TRUE ~ x),
  race       = function(x) case_when(x %in% c(2,3,4,5,6) ~ 0, TRUE ~ x),
  gender     = function(x) case_when(x == 2 ~ 0, TRUE ~ x),
  poltrst    = function(x) case_when(x == 5 ~ 1, x == 2 ~ 4, x == 1 ~ 5, x == 4 ~ 2, TRUE ~ x),
  strglead   = function(x) case_when(x == 5 ~ 1, x == 2 ~ 4, x == 1 ~ 5, x == 4 ~ 2, TRUE ~ x),
  econcond   = function(x) case_when(x == 5 ~ 1, x == 2 ~ 4, x == 1 ~ 5, x == 4 ~ 2, TRUE ~ x),
  finsit     = function(x) case_when(x == 5 ~ 1, x == 2 ~ 4, x == 1 ~ 5, x == 4 ~ 2, TRUE ~ x)
)

# ---------- ANES Functions ----------
anes_clean_var <- function(x, colname, na_codes) {
  x <- haven::zap_labels(x)
  na_vals <- na_codes[[colname]] %||% na_codes$default
  case_when(
    x %in% na_vals ~ NA_real_,
    TRUE           ~ as.numeric(x)
  )
}

anes_reverse_var <- function(x, colname, reverse_rules) {
  rule <- reverse_rules[[colname]]
  if (!is.null(rule)) rule(x) else x
}

cross_tab_diagnostic <- function(df, var1, var2) {
  df %>%
    count({{ var1 }}, {{ var2 }}) %>%
    mutate(
      cell_pct = n / sum(n)
    ) %>%
    group_by({{ var1 }}) %>%
    mutate(
      row_pct = n / sum(n)
    ) %>%
    ungroup() %>%
    group_by({{ var2 }}) %>%
    mutate(
      col_pct = n / sum(n)
    ) %>%
    ungroup()
}

# ---------- ESS Functions ----------
ess_code_to_zero <- function(x, codes) case_when(x %in% codes ~ 0, TRUE ~ x)
ess_code_to_one  <- function(x, codes) case_when(x %in% codes ~ 1, TRUE ~ x)
ess_code_to_na   <- function(x, codes) case_when(x %in% codes ~ NA_real_, TRUE ~ x)

# ---------- High-Level Cleaning Wrappers ----------
read_and_clean_anes <- function(path) {
  read_dta(path) %>%
    rename(
      poltrst    = V161215,
      strglead   = V162263,
      pplrule    = V162264,
      econcond   = V161140x,
      finsit     = V162165,
      imharm     = V162269,
      imecon     = V162268,
      imempl     = V162158,
      votechoice = V162034a,
      income     = V161361x,
      race       = V161310x,
      gender     = V161342,
      edulevel   = V161270
    ) %>%
    select(poltrst, 
           strglead, 
           pplrule, 
           econcond, 
           finsit, 
           imharm,
           imecon, 
           imempl, 
           votechoice, 
           income, 
           race, 
           gender, 
           edulevel) %>%
    mutate(across(everything(),
                  ~ anes_clean_var(.x, cur_column(), anes_na_codes)
                )
    ) %>%
    mutate(across(everything(),
                  ~ anes_reverse_var(.x, cur_column(), anes_reverse_rules))) %>%
    rename(!!!anes_rename_map)
}

read_and_clean_ess <- function(path) {
  read_dta(path) %>%
    select(
      edlvenl, 
      edlvdit,
      gndr,
      hincfel,
      hinctnta,
      imbgeco,
      imueclt,
      imwbcnt,
      ipstrgv,
      psppsgva,
      prtvtbit,
      prtvtfnl,
      stfeco,
      trstplt,
      uemp3m
    ) %>% 
    haven::zap_labels() %>%  # ADD THIS LINE
    mutate(
      uemp3m = ess_code_to_zero(uemp3m, 2),
      prtvtbit = ess_code_to_zero(prtvtbit, c(1:8, 10:13)) %>%
        ess_code_to_one(9) %>%
        ess_code_to_na(14),
      prtvtfnl = ess_code_to_zero(prtvtfnl, c(1:2, 4:11)) %>%
        ess_code_to_one(3) %>%
        ess_code_to_na(16),
      edlvdit  = ess_code_to_na(edlvdit, 5555),
      edlvenl  = ess_code_to_na(edlvenl, 5555),
      gndr     = ess_code_to_zero(gndr, 2)
    )
}
