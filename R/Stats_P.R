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
# inspect interaction
tt <- tree(log(po) ~ ScMoist + ScTemp, data = IEM_DF)
plot(tt)
text(tt)
given.temp <- co.intervals(IEM_DF$ScTemp, number = 2, overlap = .1)
coplot(log(po) ~ ScMoist|ScTemp, data = IEM_DF, panel = panel.smooth, given.values = given.temp)# each chamber

# fit interaction
m1 <- lmer(log(po) ~ ScMoist * ScTemp  + (1|Time) +  (1|Chamber), data = IEM_DF)
Anova(m1)
# no interaction

Iml_ancv_po <- lmer(log(po) ~ Temp5_Mean + moist + (1|Time) + (1|Chamber), data = IEM_DF)
Anova(Iml_ancv_po)
Fml_ancv_po <- stepLmer(Iml_ancv_po)
AnvF_ancv_po <- Anova(Fml_ancv_po, test.statistic = "F")
AnvF_ancv_po

# visualise
a <- visreg(Fml_ancv_po, xvar = "Temp5_Mean", points = list(col = IEM_DF$temp))
Fit_po <- a$fit
plot(log(po) ~ Temp5_Mean, data = IEM_DF, col = temp, pch = 19)
lines(visregFit ~ Temp5_Mean, data = Fit_po)

# model diagnosis
plot(Fml_ancv_po)
qqnorm(resid(Fml_ancv_po))
qqline(resid(Fml_ancv_po))

## ----Stat_WTC_IEM_Phosphate_Smmry
# The initial model is:
Iml_po@call
Anova(Iml_po)

# The final model is:
Fml_po@call

# Chi test
Anova(Fml_po)
# F test
AnvF_po

# ANCOVA
Iml_ancv_po@call
Anova(Iml_ancv_po)

Fml_ancv_po@call
# Chi test
Anova(Fml_ancv_po)
# F test
AnvF_ancv_po

visreg(Fml_ancv_po, xvar = "Temp5_Mean", points = list(col = IEM_DF$temp))