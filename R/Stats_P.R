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

############################
# ANCOVA fit soil variable #
############################

#######################
# plot soil variables #
#######################
# each chamber
xyplot(log(po) ~ moist|temp, groups = Chamber, type = c("r", "p"), data = IEM_DF)
# each time
xyplot(log(po) ~ moist|temp, groups = Time, type = c("r", "p"), data = IEM_DF)

Iml_ancv_po <- lmer(log(po) ~ temp * moist + (1|Time) + (1|Chamber), data = IEM_DF)
m2 <- update(Iml_ancv_po, ~. - (1|Time))
m3 <- update(Iml_ancv_po, ~. - (1|Chamber))
# m3 is best but it's repeated measure, so keep chamber anyway..
Anova(Iml_ancv_po)
Fml_ancv_po <- stepLmer(Iml_ancv_po)
AnvF_ancv_po <- Anova(Fml_ancv_po, test.statistic = "F")

# model diagnosis
plot(Fml_ancv_po)
qqnorm(resid(Fml_ancv_po))
qqline(resid(Fml_ancv_po))

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

## ----Stat_WTC_IEM_ChMean_Phosphate_Smmry_ANCOVA
Iml_ancv_po@call
Anova(Iml_ancv_po)

Fml_ancv_po@call
Anova(Fml_ancv_po)
AnvF_ancv_po
