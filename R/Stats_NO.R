## ----Stat_WTC_IEM_Nitrate

bxplts(value= "no",  data= IEM_ChMean)
bxcxplts(value= "no",  data= IEM_ChMean, sval = 0.01, fval =.1)
xyplot(no ~ Time|temp, groups = Chamber, type = "o", 
       panel = panel.superpose, data = IEM_ChMean)

# sqrt looks better
Iml_no <- lmer(sqrt(no) ~ temp*Time  + (1|Chamber), data = IEM_ChMean)
Anova(Iml_no)

# model diagnosis
plot(Iml_no)
qqnorm(resid(Iml_no))
qqline(resid(Iml_no))
# one outlier, what if it's removed
ol <- which(qqnorm(resid(Iml_no))$y == min(qqnorm(resid(Iml_no))$y))
m2 <- update(Iml_no, subset = -ol)
Anova(m2, test.statistic = "F")
plot(m2)
qqnorm(resid(m2))
qqline(resid(m2))
# slightly better but no huge difference, so stay with the first one.

Fml_no <- Iml_no
AnvF_no <- Anova(Fml_no, test.statistic = "F")

# contrast
lmemod <- lme(sqrt(no) ~ temp*Time, random = ~1|Chamber, data = IEM_ChMean)

cntrst<- contrast(lmemod, 
                  a = list(Time = levels(IEM_ChMean$Time), temp = "amb"),
                  b = list(Time = levels(IEM_ChMean$Time), temp = "elev"))
WTC_IEM_Nitrate_CntrstDf <- cntrstTbl(cntrst, IEM_ChMean)
WTC_IEM_Nitrate_CntrstDf

## ----Stat_WTC_IEM_Nitrate_Smmry
# The initial model is:
Iml_no@call

# The final model is:
Fml_no@call

# Chi
Anova(Fml_no)

# F test
AnvF_no

# contrast
WTC_IEM_Nitrate_CntrstDf