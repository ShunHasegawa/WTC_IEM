bxplts(value= "nh",  data= iem)

# remove one obvious outlier
nhDat <- subset(iem, nh < max(nh))

bxplts(value= "nh",  data= nhDat)

# log seemes slightly better

m1 <- lme(log(nh) ~ temp * Time, random = ~1|Chamber/Location, data = nhDat)
m2 <- lme(log(nh) ~ temp * Time, random = ~1|id, data = nhDat)
m3 <- lme(log(nh) ~ temp * Time, random = ~1|Chamber, data = nhDat)
anova(m1, m2, m3)
# m2 (or m3) is slightly better than m1

# auutocorrelation
atcr.cmpr(m2, rndmFac= "id")
# model2 looks better
mAt <- atcr.cmpr(m2, rndmFac= "id")[[2]]

# model simplification
MdlSmpl(mAt)

Fml <- MdlSmpl(mAt)$model.reml
Anova(Fml)
plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|Chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))