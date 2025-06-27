#### STATA Research - ANES 2016 #####

# Load Packages # 

library(tidyverse)
library(haven)
library(car)

# Load ANES 2016 data into R # 
Research_DataANES <- read_dta("C:/Users/brown/Downloads/Research_DataANES.dta")

View(Research_DataANES)

# Recoding Variables # 

# poltrst -> rpoltrst# 

Research_DataANES$poltrst <- ifelse(Research_DataANES$V161215 %in% c(-8, -9), NA, Research_DataANES$V161215)

# Reversing Likert Scale # 

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

Research_DataANES$votechoice <- ifelse(Research_DataANES$V162034a %in% c(-1, 3, 4, 5, 7, 9, -6, -7, -8, -9), NA, Research_DataANES$V162034a)

Research_DataANES$cvotechoice <- recode(Research_DataANES$votechoice, '1 = 0; 2 = 1')

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

table(Research_DataANES$edulevel)

glimpse(Research_DataANES)

Research_DataANES <- Research_DataANES %>%  
  select(poltrst, rpoltrst, strglead, pplrule, econcond, reconcond, finsit, rfinsit, imharm, imecon, imempl,
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




























