bxplts(value= "no",  data= iem)
# sqrt looks better

m1 <- lme(sqrt(no) ~ temp * Time, random = ~1|Chamber/Location, data = iem)
m2 <- lme(sqrt(no) ~ temp * Time, random = ~1|id, data = iem)
m3 <- lme(sqrt(no) ~ temp * Time, random = ~1|Chamber, data = iem)
anova(m1, m2, m3)

# m2 looks better

# autocorrelation
atcr.cmpr(m2, rndmFac= "id")
# model3 looks the best
mAt <- atcr.cmpr(m2, rndmFac= "id")[[3]]

# model simplification
MdlSmpl(mAt)

# unable to remove any factors
Fml <- MdlSmpl(mAt)$model.reml
Anova(Fml)
plot(allEffects(Fml))

# contrast
levels(iem$Time)
cntrst<- contrast(Fml, 
                  a = list(Time = levels(iem$Time), temp = "amb"),
                  b = list(Time = levels(iem$Time), temp = "elev"))
WTC_IEM_Nitrate_CntrstDf <- cntrstTbl(cntrst, iem)

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|Chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
