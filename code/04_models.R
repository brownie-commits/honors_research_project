# ======================================================================
# Models
# Author: Austin J. Brown
# ======================================================================


# ANES Models -------------------------------------------------------------

model1 <- lm(rpoltrst ~ reconcond + rfinsit + imharm + cvotechoice + income + cgender + crace + edulevel, data = anes_cleaned)
summary(model1)
stargazer(model1, type = "html", out = "results/model1.html")

model2 <- lm(rstrglead ~ reconcond + rfinsit + imharm + cvotechoice + income + cgender + crace + edulevel, data = anes_cleaned)
summary(model2)
stargazer(model2, type = "html", out = "results/model2.html")

model3 <- lm(pplrule ~ reconcond + rfinsit + imharm + cvotechoice + income + cgender + crace + edulevel, data = anes_cleaned)
summary(model3)
stargazer(model3, type = "html", out = "results/model3.html")

model4 <- lm(rpoltrst ~ income, anes_cleaned)
summary(model4)
stargazer(model4, type = "html", out = "results/model4.html")


# ESS Models --------------------------------------------------------------

model5 <- lm(trstplt ~ stfeco + hincfel + imueclt + prtvtbit + edlvdit + gndr + hinctnta, data = ess_cleaned)
summary(model5)
stargazer(model5, type = "html", out = "results/model5.html")

model6 <- lm(trstplt ~ stfeco + hincfel + imueclt + prtvtfnl + edlvenl + gndr + hinctnta, data = ess_cleaned)
summary(model6)
stargazer(model6, type = "html", out = "results/model6.html")

model7 <- lm(ipstrgv ~ stfeco + hincfel + imueclt + prtvtbit + edlvdit + gndr + hinctnta, data = ess_cleaned)
summary(model7)
stargazer(model7, type = "html", out = "results/model7.html")

model8 <- lm(ipstrgv ~ stfeco + hincfel + imueclt + prtvtfnl + edlvenl + gndr + hinctnta, data = ess_cleaned)
summary(model8)
stargazer(model8, type = "html", out = "results/model8.html")

model9 <- lm(psppsgva ~ stfeco + hincfel + imueclt + prtvtbit + edlvdit + gndr + hinctnta, data = ess_cleaned)
summary(model9)
stargazer(model9, type = "html", out = "results/model9.html")

model10 <- lm(psppsgva ~ stfeco + hincfel + imueclt + prtvtfnl + edlvenl + gndr + hinctnta, data = ess_cleaned)
summary(model10)
stargazer(model10, type = "html", out = "results/model10.html")


