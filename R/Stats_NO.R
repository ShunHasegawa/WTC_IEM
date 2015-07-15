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
WTC_IEM_Nitrate_CntrstDf <- cntrstTbl(cntrst, IEM_ChMean, variable  = "no", digit = 2)
WTC_IEM_Nitrate_CntrstDf

############################
# ANCOVA fit soil variable #
############################

#######################
# plot soil variables #
#######################
# inspect interaction
tt <- tree(sqrt(no) ~ ScMoist + ScTemp, data = IEM_DF)
plot(tt)
text(tt)
given.temp <- co.intervals(IEM_DF$ScTemp, number = 2, overlap = .1)
given.moist <- co.intervals(IEM_DF$ScMoist, number = 2, overlap = .1)
coplot(sqrt(no) ~ ScMoist|ScTemp, data = IEM_DF, panel = panel.smooth, given.values = given.temp)
coplot(sqrt(no) ~ ScTemp|ScMoist, data = IEM_DF, panel = panel.smooth, given.values = given.moist)
# no temperature effect when dry but negative effects when wet

# check multicollinearity
vif(lm(no ~ ScMoist * ScTemp, data = IEM_DF))

scatterplotMatrix(~ sqrt(no) + ScMoist + ScTemp + I(ScMoist*ScTemp), data = IEM_DF, diag = "boxplot", 
                  groups = IEM_DF$temp, by.group = TRUE)

# fit interaction
m1 <- lmer(no ~ ScMoist * ScTemp  + (1|Time) +  (1|Chamber), data = IEM_DF)
m2 <- lmer(sqrt(no) ~ ScMoist * ScTemp  + (1|Time) +  (1|Chamber), data = IEM_DF)
ldply(list(m1, m2), r.squared)
Anova(m2)
# significant interaction

Iml_ancv_no <- m2
Anova(Iml_ancv_no)
Fml_ancv_no <- Iml_ancv_no
AnvF_ancv_no <- Anova(Fml_ancv_no, test.statistic = "F")
AnvF_ancv_no

# model diagnosis
plot(Fml_ancv_no)
qqnorm(resid(Fml_ancv_no))
qqline(resid(Fml_ancv_no))

# visualise
a <- visreg(Fml_ancv_no, xvar = "Temp5_Mean", points = list(col = IEM_DF$temp))
Fit_no <- a$fit
plot(log(no) ~ Temp5_Mean, data = IEM_DF, col = temp, pch = 19)
lines(visregFit ~ Temp5_Mean, data = Fit_no)

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

# ANCOVA
Iml_ancv_no@call
Anova(Iml_ancv_no)

Fml_ancv_no@call
Anova(Fml_ancv_no)
AnvF_ancv_no
visreg(Fml_ancv_no, xvar = "Temp5_Mean", points = list(col = IEM_DF$temp))
