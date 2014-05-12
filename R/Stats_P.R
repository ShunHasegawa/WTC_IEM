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

#####################
# tmep and moisture #
#####################
IEM_DF$yv  <- (IEM_DF$po + .0001)^(-0.2222)

scatterplotMatrix(~po + yv + SoilVW_5_25_Mean  
                  + SoilTemp10_Mean + SoilTemp10_Max + SoilTemp10_Min, data = IEM_DF, diag = "boxplot")

m1 <- lme((po + .0001)^(-0.2222) ~ SoilTemp10_Min + Time, random = ~1|Chamber, data = IEM_DF)
summary(m1)
Anova(m1)

plot(allEffects(m1))

b <- update(a, ~. - Time:SoilTemp10_Min)
anova(a, b)
Anova(b)
summary(b)



m1 <- lm((po + .0001)^(-0.2222) ~ SoilTemp10_Min + Time, data = IEM_DF)
Anova(m1)
cf <- coef(m1)
plot((po + .0001)^(-0.2222) ~ SoilTemp10_Min, data = IEM_DF, col = Time, 
     pch = as.numeric(temp), cex = 2)

abline(cf[1], cf[2], col = palette()[1], lwd = 3)
sapply(c(3:5), function(x) abline(cf[1] + cf[x], cf[2], col = palette()[x-1], lwd = 3))


