## ----Stat_WTC_IEM_ChMean_Phosphate
bxplts(value= "po", data= IEM_ChMean)
xyplot(log(po) ~ Time|temp, groups = Chamber, type = "o", 
       panel = panel.superpose, data = IEM_ChMean)


# use log

# The initial model is
Iml_po <- lmer(log(po) ~ temp * Time + (1|Chamber), data = IEM_ChMean)
Anova(Iml_po)

# The final model is
Fml_po <- stepLmer(Iml_po)
Anova(Fml_po)
AnvF_po <- Anova(Fml_po, test.statistic = "F")
AnvF_po

summary(Fml_po )

# model diagnosis
plot(Fml_po)
qqnorm(resid(Fml_po))
qqline(resid(Fml_po))

## ----Stat_WTC_IEM_ChMean_Phosphate_Smmry
# The initial model is:
Iml_po@call
Anova(Iml_po)

# The final model is:
Fml_po@call

# Chi test
Anova(Fml_po)

# F test
AnvF_po
