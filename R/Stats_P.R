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
scatterplotMatrix(~ log(po) + moist + Temp5_Mean, data = IEM_DF, diag = "boxplot", 
                  groups = IEM_DF$temp, by.group = TRUE)

Iml_ancv_po <- lmer(log(po) ~ temp * (moist + Temp5_Mean) + (1|Chamber), data = IEM_DF)
Fml_ancv_po <- stepLmer(Iml_ancv_po, alpha.fixed = .1)
AnvF_ancv_po <- Anova(Fml_ancv_po, test.statistic = "F")
AnvF_ancv_po

# model diagnosis
plot(Fml_ancv_po)
qqnorm(resid(Fml_ancv_po))
qqline(resid(Fml_ancv_po))

TransVirsreg(visreg(Fml_ancv_po, xvar = "Temp5_Mean", plot = FALSE),
             trans = exp, point = list(col = c(1, 2), cex = 1))

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

TransVirsreg(visreg(Fml_ancv_po, xvar = "Temp5_Mean", plot = FALSE),
             trans = exp, point = list(col = c(1, 2), cex = 1))
