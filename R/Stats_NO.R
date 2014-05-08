## ----Stat_WTC_IEM_Nitrate

bxplts(value= "no",  data= iem)
bxcxplts(value= "no",  data= iem, sval = 0.01, fval =.1)

# sqrt looks better
m1 <- lme(sqrt(no) ~ temp * Time, random = ~1|Chamber/Location, data = iem)
m2 <- lme(sqrt(no) ~ temp * Time, random = ~1|id, data = iem)
m3 <- lme(sqrt(no) ~ temp * Time, random = ~1|Chamber, data = iem)
anova(m1, m2, m3)

# m2 looks better

# autocorrelation
atcr.cmpr(m2, rndmFac= "id")$models
# model3 looks the best
Iml <- atcr.cmpr(m2, rndmFac= "id")[[3]]

# The initial model is:
Iml$call
Anova(Iml)

# model simplification
MdlSmpl(Iml)
  # unable to remove any factors

# The final model is:
Fml <- MdlSmpl(Iml)$model.reml
Anova(Fml)

# contrast
cntrst<- contrast(Fml, 
                  a = list(Time = levels(iem$Time), temp = "amb"),
                  b = list(Time = levels(iem$Time), temp = "elev"))
WTC_IEM_Nitrate_CntrstDf <- cntrstTbl(cntrst, iem)
WTC_IEM_Nitrate_CntrstDf

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|Chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))

## ----Stat_WTC_IEM_Nitrate_Smmry
# The initial model is:
Iml$call
Anova(Iml)

# The final model is:
Fml <- MdlSmpl(Iml)$model.reml
Anova(Fml)

# contrast
WTC_IEM_Nitrate_CntrstDf