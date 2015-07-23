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
xyplot(sqrt(no) ~ moist|temp, groups = Chamber, type = c("r", "p"), data = IEM_DF)
xyplot(sqrt(no) ~ moist|temp, groups = Time, type = c("r", "p"), data = IEM_DF)
scatterplotMatrix(~ sqrt(no) + moist + Temp5_Mean, data = IEM_DF, diag = "boxplot", 
                  groups = IEM_DF$temp, by.group = TRUE)
plot(moist ~ Temp10_Mean, data= IEM_DF, pch = 19, col = temp)

Iml_ancv_no <- lmer(sqrt(no) ~ temp * (moist + Temp5_Mean) + (1|Chamber), data = IEM_DF)
Fml_ancv_no <- stepLmer(m1, alpha.fixed = .1)

AnvF_ancv_no <- Anova(Fml_ancv_no, test.statistic = "F")
AnvF_ancv_no

# model diagnosis
plot(Fml_ancv_no)
qqnorm(resid(Fml_ancv_no))
qqline(resid(Fml_ancv_no))

# visualise
par(mfrow = c(1, 2))
TransVirsreg(visreg(Fml_ancv_no, xvar = "moist", by = "temp", plot = FALSE), 
             trans = function(x) x^2, overlay = TRUE, 
             point = list(col = c(1, 2), cex = 1), 
             line =  list(col = c(1, 2)))
TransVirsreg(visreg(Fml_ancv_no, xvar = "Temp5_Mean", by = "temp", plot = FALSE), 
             trans = function(x) x^2, overlay = TRUE, 
             point = list(col = c(1, 2), cex = 1), 
             line =  list(col = c(1, 2)))

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

par(mfrow = c(1, 2))
par(mfrow = c(1, 2))
TransVirsreg(visreg(Fml_ancv_no, xvar = "moist", by = "temp", plot = FALSE), 
             trans = function(x) x^2, overlay = TRUE, 
             point = list(col = c(1, 2), cex = 1), 
             line =  list(col = c(1, 2)))
TransVirsreg(visreg(Fml_ancv_no, xvar = "Temp5_Mean", by = "temp", plot = FALSE), 
             trans = function(x) x^2, overlay = TRUE, 
             point = list(col = c(1, 2), cex = 1), 
             line =  list(col = c(1, 2)))
