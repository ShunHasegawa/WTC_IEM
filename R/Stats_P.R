bxplts(value= "po", ofst= .0001, data= iem)

# log transformation seems best
m1 <- lme(log(po + .0001) ~ temp * Time, random = ~1|Chamber/Location, data = iem)
m2 <- lme(log(po + .0001) ~ temp * Time, random = ~1|id, data = iem)
m3 <- lme(log(po + .0001) ~ temp * Time, random = ~1|Chamber, data = iem)
anova(m1, m2, m3)

# autocorelation
atcr.cmpr(m3, rndmFac="Chamber")
# no need for auto-correlation

# m3 is slightly better, between-chamber variation is less important within-chanmber (=between location within chamber)
MdlSmpl(m3)
m4 <- update(m3, ~ . -temp:Time)
MdlSmpl(m4)
Fml <- MdlSmpl(m4)$model.reml
Anova(Fml)
summary(Fml)
plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|Chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))
