## ----Stat_WTC_IEM_ChMean_Ammonium
bxplts(value= "nh",  data= IEM_ChMean)

# remove one obvious outlier
nhDat <- subset(IEM_ChMean, nh < max(nh))

bxplts(value= "nh",  data= nhDat)

xyplot(nh ~ Time|temp, groups = Chamber, type = "o", 
       panel = panel.superpose, data = nhDat)

xyplot(nh ~ Time|Chamber, groups = id, type = "o", 
       panel = panel.superpose, data = subset(iem, nh < max(nh)))

# raw data
Iml_nh <- lmer(nh ~ temp*Time  + (1|Chamber), data = nhDat)
Anova(Iml_nh)
# model diagnosis
plot(Iml_nh)
qqnorm(resid(Iml_nh))
qqline(resid(Iml_nh))

Fml_nh <- Iml_nh
AnvF_nh <- Anova(Fml_nh, test.statistic = "F")

# contrast
lmemod <- lme(nh ~ temp*Time, random = ~1|Chamber, data = nhDat)

cntrst<- contrast(lmemod, 
                  a = list(Time = levels(nhDat$Time), temp = "amb"),
                  b = list(Time = levels(nhDat$Time), temp = "elev"))
WTC_IEM_Ammonium_CntrstDf <- cntrstTbl(cntrst, nhDat)
WTC_IEM_Ammonium_CntrstDf

## ----Stat_WTC_IEM_ChMean_Ammonium_Smmry
# The initial model is:
Iml_nh@call

# The final model is:
Fml_nh@call
AnvF_nh
