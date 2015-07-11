## ----Stat_WTC_IEM_ChMean_Ammonium
bxplts(value= "nh",  data= IEM_ChMean)

xyplot(nh ~ Time|temp, groups = Chamber, type = "o", 
       panel = panel.superpose, data = IEM_ChMean)

xyplot(nh ~ Time|Chamber, groups = id, type = "o", 
       panel = panel.superpose, data = subset(iem, nh < max(nh)))

# raw data
Iml_nh <- lmer(nh ~ temp*Time  + (1|Chamber), data = IEM_ChMean)
Anova(Iml_nh)
# model diagnosis
plot(Iml_nh)
qqnorm(resid(Iml_nh))
qqline(resid(Iml_nh))
# one obvious outlier. what if it's removed

ol <- which(qqnorm(resid(Iml_nh))$y == min(qqnorm(resid(Iml_nh))$y))
m2 <- update(Iml_nh, subset = -ol)
plot(m2)
qqnorm(resid(m2))
qqline(resid(m2))
Anova(m2, test.statistic = "F")
Anova(Iml_nh, test.statistic = "F")
# not huge difference so just use the 1st one

Fml_nh <- Iml_nh
AnvF_nh <- Anova(Fml_nh, test.statistic = "F")

# contrast
lmemod <- lme(nh ~ temp*Time, random = ~1|Chamber, data = IEM_ChMean)

cntrst<- contrast(lmemod, 
                  a = list(Time = levels(IEM_ChMean$Time), temp = "amb"),
                  b = list(Time = levels(IEM_ChMean$Time), temp = "elev"))
WTC_IEM_Ammonium_CntrstDf <- cntrstTbl(cntrst, IEM_ChMean, variable  = "nh", digit = 2)
WTC_IEM_Ammonium_CntrstDf

############################
# ANCOVA fit soil variable #
############################

#######################
# plot soil variables #
#######################
# each chamber
xyplot(nh ~ moist|temp, groups = Chamber, type = c("r", "p"),data = IEM_DF)
# each time
xyplot(nh ~ moist|temp, groups = Time, type = c("r", "p"), data = IEM_DF)

scatterplotMatrix(~ nh + moist + Temp5_Mean, data = IEM_DF, diag = "boxplot", 
                  groups = IEM_DF$temp, by.group = TRUE)

Iml_ancv_nh <- lmer(nh ~ temp * moist + (1|Time) + (1|Chamber), data = IEM_DF)
Anova(Iml_ancv_nh)
m2 <- update(Iml_ancv_nh, ~. - (1|Time))
m3 <- update(Iml_ancv_nh, ~. - (1|Chamber))
anova(Iml_ancv_nh, m2, m3)
Anova(Iml_ancv_nh, test.statistic = "F")

Fml_ancv_nh <- Iml_ancv_nh

AnvF_ancv_nh <- Anova(Fml_ancv_nh, test.statistic = "F")
AnvF_ancv_nh

# visualise
visreg(Fml_ancv_nh, xvar = "moist", by = "temp", overlay = TRUE)

## ----Stat_WTC_IEM_Ammonium_Smmry
# The initial model is:
Iml_nh@call

# The final model is:
Fml_nh@call
AnvF_nh

# Ancova
Iml_ancv_nh@call
Anova(Iml_ancv_nh)
AnvF_ancv_nh

Fml_ancv_nh@call

visreg(Fml_ancv_nh, xvar = "moist", by = "temp", overlay = TRUE)