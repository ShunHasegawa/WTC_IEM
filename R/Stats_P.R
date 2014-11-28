## ----Stat_WTC_IEM_Phosphate
bxplts(value= "po", ofst= .0001, data= iem)
bxcxplts(value= "po", data= iem, sval = 0.0001, fval = .001)
# use log

# The initial model is
Iml <- lmer(log(po + .0001) ~ temp * Time + (1|Chamber) + (1|id), data = iem)
Anova(Iml)

# The final model is
Fml <- stepLmer(Iml)
Anova(Fml)
AnvF_P <- Anova(Fml, test.statistic = "F")
AnvF_P

summary(Fml)

plot(allEffects(Fml))

# model diagnosis
plot(Fml)
qqnorm(resid(Fml))
qqline(resid(Fml))

## ----Stat_WTC_IEM_Phosphate_Smmry
# The initial model is:
Iml@call
Anova(Iml)

# The final model is:
Fml@call
Anova(Fml)
AnvF_P
