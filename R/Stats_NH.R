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
scatterplotMatrix(~ log(nh) + moist + log(Temp5_Mean), data = IEM_DF, diag = "boxplot", 
                  groups = IEM_DF$temp, by.group = TRUE)

m1 <- lmer(log(nh) ~ temp * (moist + Temp5_Mean) + (1|Chamber), data = IEM_DF)
m2 <- lmer(log(nh) ~ temp * (moist + log(Temp5_Mean)) + (1|Chamber), data = IEM_DF)
m3 <- lmer(nh ~ temp * (moist + log(Temp5_Mean)) + (1|Chamber), data = IEM_DF)
m4 <- lmer(nh ~ temp * (moist + Temp5_Mean) + (1|Chamber), data = IEM_DF)
ldply(list(m1, m2, m3, m4), r.squared)

Iml_ancv_nh <- lmer(log(nh) ~ temp * (moist + Temp5_Mean) + (1|Chamber), data = IEM_DF)
Fml_ancv_nh <- stepLmer(Iml_ancv_nh, alpha.fixed = .1)
Anova(Fml_ancv_nh)
AnvF_ancv_nh <- Anova(Fml_ancv_nh, test.statistic = "F")
AnvF_ancv_nh

# model diagnosis
plot(Fml_ancv_nh)
qqnorm(resid(Fml_ancv_nh))
qqline(resid(Fml_ancv_nh))

# visualise
TransVirsreg(visreg(Fml_ancv_nh, xvar = "Temp5_Mean", by = "temp", plot = FALSE),
             overlay = TRUE, trans = exp,
             point = list(col = c(1, 2), cex = 1), 
             line =  list(col = c(1, 2)))


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

TransVirsreg(visreg(Fml_ancv_nh, xvar = "Temp5_Mean", by = "temp", plot = FALSE),
             overlay = TRUE, trans = exp,
             point = list(col = c(1, 2), cex = 1), 
             line =  list(col = c(1, 2)))

