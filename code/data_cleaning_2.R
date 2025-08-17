######################################################################

# Data Cleaning

# Date: 8-16-2025

# Author: Austin J. Brown

######################################################################


# Data import -------------------------------------------------------------

#### STATA Research - ANES 2016 #####

# Load ANES 2016 data into R # 

anes <- read_dta("data/Research_DataANES.dta")

Research_DataANES <- read_dta("data/Research_DataANES.dta")

ess <- read_dta("data/Research_DataESS_Clean.dta")

# Data Cleaning -----------------------------------------------------------

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
  select(poltrst, strglead, pplrule, econcond, finsit, imharm, imecon, imempl, votechoice, income, race, gender, edulevel)

anes_na_clean <- function(df) {
  df %>%
    mutate(across(everything(), ~ {
      colname <- cur_column()
      x <- haven::zap_labels(.x)
      
      if (colname == "votechoice") {
        case_when(
          x %in% c(-1, 3, 4, 5, 7, 9, -6, -7, -8, -9) ~ NA_real_, 
          TRUE ~ as.numeric(x)
          )
      } else {
        case_when(
          x %in% c(-1, -5, -6, -7, -8, -9, 90, 95) ~ NA_real_,
          TRUE ~ as.numeric(x)
        )
      }
    }))
}

anes <- anes_na_clean(anes)

anes_reverse_all <- function(df) {
  df %>%
    mutate(across(everything(), ~ {
      colname <- cur_column()
      
      if (colname == "votechoice") {
        case_when(
          .x == 0 ~ 1,
          .x == 2 ~ 1,
          TRUE ~ as.numeric(.x)
        )
      } else if (colname == "race") {
        case_when(
          .x %in% c(2, 3, 4, 5, 6) ~ 0,
          TRUE ~ as.numeric(.x)
        )
      } else if (colname == "gender") {
        case_when(
          .x == 2 ~ 0,
          TRUE ~ as.numeric(.x)
        )
      } else if (colname %in% c("poltrst", "strglead", "econcond", "finsit")) {
        case_when(
          .x == 5 ~ 1,
          .x == 2 ~ 4,
          .x == 1 ~ 5,
          .x == 4 ~ 2,
          TRUE ~ as.numeric(.x)
          )
      }
      
      else {
        .x  # untouched
      }
    }))
}

anes <- anes_reverse_all(anes)

anes <- anes %>% 
  rename(cgender = gender,
         rpoltrst = poltrst, 
         rstrglead = strglead,
         reconcond = econcond,
         rfinsit = finsit, 
         cvotechoice = votechoice, 
         cgender = gender, 
         crace = race)

anes %>% map(~ table(.x, useNA = "ifany"))

Research_DataANES <- Research_DataANES %>% 
  select(rpoltrst, rstrglead, pplrule, reconcond, rfinsit, imharm, imecon, imempl, cvotechoice, income, crace, cgender, edulevel)
  

Research_DataANES %>% map(~ table(.x, useNA = "ifany"))

compare_dfs <- function(df1, df2) {
  stopifnot(identical(names(df1), names(df2)), 
            nrow(df1) == nrow(df2), 
            ncol(df1) == ncol(df2))
  
  map2_dfr(df1, df2, ~ .x != .y) %>%
    mutate(row = row_number()) %>%
    pivot_longer(-row, names_to = "column") %>%
    filter(value)  # only rows where values differ
}


df <- compare_dfs(anes, Research_DataANES)


# poltrst -> rpoltrst # 

Research_DataANES$poltrst <- ifelse(Research_DataANES$V161215 %in% c(-8, -9), NA, Research_DataANES$V161215)

Research_DataANES$votechoice <- ifelse(Research_DataANES$V162034a %in% c(-1, 3, 4, 5, 7, 9, -6, -7, -8, -9), NA, Research_DataANES$V162034a)

Research_DataANES$cvotechoice <- recode(Research_DataANES$votechoice, '1 = 0; 2 = 1')# Reversing Likert Scale # 

Research_DataANES$rpoltrst <- recode(Research_DataANES$poltrst, '1 = 5; 2 = 4; 5 = 1; 4 = 2')

table(Research_DataANES$rpoltrst)

# strglead # 

table(Research_DataANES$V162263)

Research_DataANES$strglead <- ifelse(Research_DataANES$V162263 %in% c(-6, -7, -8, -9), NA, Research_DataANES$V162263)

table(Research_DataANES$strglead)
  
Research_DataANES$rstrglead <- recode(Research_DataANES$strglead, '1 = 5; 2 = 4; 5 = 1; 4 = 2')

table(Research_DataANES$rstrglead)

# pprule # 

table(Research_DataANES$V162264)

Research_DataANES$pplrule <- ifelse(Research_DataANES$V162264 %in% c(-6, -7, -8, -9), NA, Research_DataANES$V162264)

table(Research_DataANES$pplrule)

# econcond -> reconcond# 

table(Research_DataANES$V161140x)

Research_DataANES$econcond <- ifelse(Research_DataANES$V161140x %in% c(-1), NA, Research_DataANES$V161140x)

table(Research_DataANES$econcond)

Research_DataANES$reconcond <- recode(Research_DataANES$econcond, '1 = 5; 2 = 4; 5 = 1; 4 = 2')

table(Research_DataANES$reconcond)

# finsit -> rfinsit # 

table(Research_DataANES$V162165)

Research_DataANES$finsit <- ifelse(Research_DataANES$V162165 %in% c(-6, -7, -8, -9), NA, Research_DataANES$V162165)

table(Research_DataANES$finsit)

Research_DataANES$rfinsit <- recode(Research_DataANES$finsit, '1 = 5; 2 = 4; 5 = 1; 4 = 2')

table(Research_DataANES$rfinsit)

# imharm # 

table(Research_DataANES$V162269)

Research_DataANES$imharm <- ifelse(Research_DataANES$V162269 %in% c(-6, -7, -8, -9), NA, Research_DataANES$V162269)

table(Research_DataANES$imharm)

# imecon # 

table(Research_DataANES$V162268)

Research_DataANES$imecon <- ifelse(Research_DataANES$V162268 %in% c(-6, -7, -8, -9), NA, Research_DataANES$V162268)

table(Research_DataANES$imecon)

# imempl # 

table(Research_DataANES$V162158)

Research_DataANES$imempl <- ifelse(Research_DataANES$V162158 %in% c(-6, -7, -8, -9), NA, Research_DataANES$V162158)

table(Research_DataANES$imempl)

# votechoice -> cvotechoice# 

table(Research_DataANES$V162034a)



table(Research_DataANES$cvotechoice)

# income #

table(Research_DataANES$V161361x)

Research_DataANES$income <- ifelse(Research_DataANES$V161361x %in% c(-5, -9), NA, Research_DataANES$V161361x)

table(Research_DataANES$income)

# gender -> cgender # 

table(Research_DataANES$V161342)

Research_DataANES$gender <- ifelse(Research_DataANES$V161342 %in% c(3, -9), NA, Research_DataANES$V161342)

Research_DataANES$cgender <- recode(Research_DataANES$gender, '2 = 0')

table(Research_DataANES$cgender)

# race -> crace # 

table(Research_DataANES$V161310x)

Research_DataANES$race <- ifelse(Research_DataANES$V161310x %in% c(-9), NA, Research_DataANES$V161310x)

Research_DataANES$crace <- recode(Research_DataANES$race, '2 = 0; 3 = 0; 4 = 0; 5 = 0; 6 = 0')

table(Research_DataANES$crace)

# edulevel # 

table(Research_DataANES$V161270)

Research_DataANES$edulevel <- ifelse(Research_DataANES$V161270 %in% c(-9, 90, 95), NA, Research_DataANES$V161270)

table(anes$edulevel)

glimpse(Research_DataANES)

Research_DataANES <- Research_DataANES %>%  
  select(poltrst, rpoltrst, rstrglead, pplrule, econcond, reconcond, finsit, rfinsit, imharm, imecon, imempl,
    cvotechoice, income, gender, cgender, race, crace, edulevel)

?table



# Data Analysis #

# Cross-tabs #

# Contingency table with column percentages, convert to percentages #

table(Research_DataANES$poltrst, Research_DataANES$econcond)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$poltrst, Research_DataANES$finsit)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$poltrst, Research_DataANES$imharm)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$poltrst, Research_DataANES$imecon)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$poltrst, Research_DataANES$imempl)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$poltrst, Research_DataANES$votechoice)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$strglead, Research_DataANES$econcond)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$strglead, Research_DataANES$finsit)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$strglead, Research_DataANES$imharm)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$strglead, Research_DataANES$imecon)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$strglead, Research_DataANES$imempl)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$strglead, Research_DataANES$votechoice)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$pplrule, Research_DataANES$econcond)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$pplrule, Research_DataANES$finsit)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$pplrule, Research_DataANES$imharm)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$pplrule, Research_DataANES$imecon)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$pplrule, Research_DataANES$imempl)

prop.table(table_row, margin = 1) * 100  

table(Research_DataANES$pplrule, Research_DataANES$votechoice)

prop.table(table_row, margin = 1) * 100  

# Simple Linear Regression # 

install.packages("stargazer")
library(stargazer)

model1 <- lm(rpoltrst ~ reconcond + rfinsit + imharm + cvotechoice + income + cgender + crace + edulevel, data = Research_DataANES)

summary(model1)

stargazer(model1, type = "html")

model2 <- lm(strglead ~ reconcond + rfinsit + imharm + cvotechoice + income + cgender + crace + edulevel, data = Research_DataANES)

summary(model2)

model3 <- lm(pplrule ~ reconcond + rfinsit + imharm + cvotechoice + income + cgender + crace + edulevel, data = Research_DataANES)

summary(model3)

plot(rpoltrst ~ imharm, data = Research_DataANES)

model4 <- lm(rpoltrst ~ income, Research_DataANES)


ggplot(Research_DataANES, aes(x = income, y = rpoltrst)) +
  geom_point() +
  labs(x = "Income", y = "Poltrst", title = "Scatter Plot: Poltrst vs. Income")


ggplot(Research_DataANES, aes(x = gender, y = rpoltrst)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Poltrst", title = "Box Plot: Poltrst by Gender")

ggplot(Research_DataANES, aes(x = income, y = rpoltrst, color = cgender)) +
  geom_point() +
  geom_smooth() +
  facet_grid(~crace) +
  labs(x = "Income", y = "Poltrst", title = "Scatter Plot Matrix")



#### STATA Research - ESS 2017 ####

library(tidyverse)
library(haven)
library(car)
library(dplyr)

# Load data into R

Research_DataESS_Clean <- read_dta("C:/R/Research_DataESS_Clean.dta")

View(Research_DataESS_Clean)

table(Research_DataESS_Clean$trstplt)
table(Research_DataESS_Clean$ipstrgv)
table(Research_DataESS_Clean$psppsgva)
table(Research_DataESS_Clean$stfeco)
table(Research_DataESS_Clean$hincfel)
table(Research_DataESS_Clean$imbgeco)
table(Research_DataESS_Clean$imueclt)
table(Research_DataESS_Clean$imwbcnt)
table(Research_DataESS_Clean$uemp3m)
table(Research_DataESS_Clean$hinctnta)

# Recoding variables

# uemp3m 

Research_DataESS_Clean$uemp3m[Research_DataESS_Clean$uemp3m == 2] <- 0

table(Research_DataESS_Clean$uemp3m)

# prtvtbit

Research_DataESS_Clean$prtvtbit <- ifelse(Research_DataESS_Clean$prtvtbit %in% c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13), 0,
                                        ifelse(Research_DataESS_Clean$prtvtbit == 9, 1,
                                               ifelse(Research_DataESS_Clean$prtvtbit == 14, NA, Research_DataESS_Clean$prtvtbit)))
table(Research_DataESS_Clean$prtvtbit)

# prtvtfnl

Research_DataESS_Clean$prtvtfnl <- ifelse(Research_DataESS_Clean$prtvtfnl %in% c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11), 0,
                                          ifelse(Research_DataESS_Clean$prtvtfnl == 3, 1,
                                                 ifelse(Research_DataESS_Clean$prtvtfnl == 16, NA, Research_DataESS_Clean$prtvtfnl)))
table(Research_DataESS_Clean$prtvtfnl)

# edlvdit 

Research_DataESS_Clean$edlvdit <- ifelse(Research_DataESS_Clean$edlvdit == 5555, NA, Research_DataESS_Clean$edlvdit)

table(Research_DataESS_Clean$edlvdit)

# edlvenl

Research_DataESS_Clean$edlvenl <- ifelse(Research_DataESS_Clean$edlvenl == 5555, NA, Research_DataESS_Clean$edlvenl)

table(Research_DataESS_Clean$edlvenl)

# gndr 

Research_DataESS_Clean$gndr[Research_DataESS_Clean$gndr == 2] <- 0

table(Research_DataESS_Clean$gndr)


# Data Analysis # 

model5 <- lm(trstplt ~ stfeco + hincfel + imueclt + prtvtbit + edlvdit + gndr + hinctnta, data = Research_DataESS_Clean)

summary(model5)

model6 <- lm(trstplt ~ stfeco + hincfel + imueclt + prtvtfnl + edlvenl + gndr + hinctnta, data = Research_DataESS_Clean)

summary(model6)

model7 <- lm(ipstrgv ~ stfeco + hincfel + imueclt + prtvtbit + edlvdit + gndr + hinctnta, data = Research_DataESS_Clean)

summary(model7)

model8 <- lm(ipstrgv ~ stfeco + hincfel + imueclt + prtvtfnl + edlvenl + gndr + hinctnta, data = Research_DataESS_Clean)

summary(model8)

model9 <- lm(psppsgva ~ stfeco + hincfel + imueclt + prtvtbit + edlvdit + gndr + hinctnta, data = Research_DataESS_Clean)

summary(model9)

model10 <- lm(psppsgva ~ stfeco + hincfel + imueclt + prtvtfnl + edlvenl + gndr + hinctnta, data = Research_DataESS_Clean)

summary(model10)




























