#############
# Phosphate #
#############
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

###########
# Nitrate #
###########
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


############
# Ammonium #
############
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