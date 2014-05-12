## ----Stat_WTC_IEM_Phosphate

bxplts(value= "po", ofst= .0001, data= iem)
bxcxplts(value= "po", data= iem, sval = 0.0001, fval = .001)
# use box-cox lambda

m1 <- lme((po + .0001)^(-0.2222) ~ temp * Time, random = ~1|Chamber/Location, data = iem)
m2 <- lme((po + .0001)^(-0.2222) ~ temp * Time, random = ~1|Chamber, data = iem)
m3 <- lme((po + .0001)^(-0.2222) ~ temp * Time, random = ~1|id, data = iem)
anova(m1, m2, m3)
# m3 is slightly better, between-chamber variation is 
# less important within-chanmber (=between location within chamber)

# autocorelation
atcr.cmpr(m3, rndmFac="id")$models
# no need for auto-correlation

Iml <- atcr.cmpr(m3, rndmFac="id")[[1]]

# The initial model is:
Iml$call
Anova(Iml)

# model simplification
MdlSmpl(Iml)
  # no factor was remoced

Fml <- MdlSmpl(Iml)$model.reml

# The final model is:
Fml$call

Anova(Fml)

summary(Fml)

# contrast
cntrst<- contrast(Fml, 
                  a = list(Time = levels(iem$Time), temp = "amb"),
                  b = list(Time = levels(iem$Time), temp = "elev"))
WTC_IEM_Phosphate_CntrstDf <- cntrstTbl(cntrst, iem)

WTC_IEM_Phosphate_CntrstDf

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|Chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))

## ----Stat_WTC_IEM_Phosphate_Smmry
# The initial model is:
Iml$call
Anova(Iml)

# The final model is:
Fml$call
Anova(Fml)

# contrast
WTC_IEM_Phosphate_CntrstDf

