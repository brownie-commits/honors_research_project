################################################################################

# Models

################################################################################


model1 <- lm(rpoltrst ~ reconcond + rfinsit + imharm + cvotechoice + income + cgender + crace + edulevel, data = Research_DataANES)

summary(model1)

stargazer(model1, type = "html")

model2 <- lm(strglead ~ reconcond + rfinsit + imharm + cvotechoice + income + cgender + crace + edulevel, data = Research_DataANES)

summary(model2)

model3 <- lm(pplrule ~ reconcond + rfinsit + imharm + cvotechoice + income + cgender + crace + edulevel, data = Research_DataANES)

summary(model3)

plot(rpoltrst ~ imharm, data = Research_DataANES)

model4 <- lm(rpoltrst ~ income, Research_DataANES)


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
