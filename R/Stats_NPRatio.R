## --------Stat_WTC_IEM_ChMean_NPRatio

#############
# N:P ratio #
#############

## ---- Stat_FACE_IEM_Analyse_NP
############
# NP ratio #
############
bxplts(value= "gmNP", data= IEM_DF)
bxplts(value= "NP", data= IEM_DF)
# use log (geometric mean)

Iml_NP <- lmer(sqrt(gmNP) ~ temp * Time + (1|Chamber), data = IEM_DF)
Anova(Iml_NP, test.statistic = "F")

# The final model is
Fml_NP <- stepLmer(Iml_NP)
Anova(Fml_NP)
AnvF_NP <- Anova(Fml_NP, test.statistic = "F")
AnvF_NP

# model diagnosis
plot(Fml_NP)
qqnorm(resid(Fml_NP))
qqline(resid(Fml_NP))

############################
# ANCOVA fit soil variable #
############################

#######################
# plot soil variables #
#######################
# each chamber
xyplot(gmNP ~ moist|temp, groups = Chamber, type = c("r", "p"), data = IEM_DF)
xyplot(gmNP ~ moist|Chamber, type = c("r", "p"), data = IEM_DF)
# each time
xyplot(gmNP ~ moist|temp, groups = Time, type = c("r", "p"), data = IEM_DF)
xyplot(gmNP ~ moist|Time, type = c("r", "p"), data = IEM_DF)

scatterplotMatrix(~gmNP + moist + Temp5_Mean|temp, data = IEM_DF, diag = "boxplot")
scatterplotMatrix(~log(gmNP) + log(moist) + Temp5_Mean|temp, data = IEM_DF, diag = "boxplot")

Iml_ancv_NP <- lmer(log(gmNP) ~ temp * (moist + Temp5_Mean) + (1|Chamber), data = IEM_DF)
Anova(Iml_ancv_NP)
Fml_ancv_NP <- stepLmer(Iml_ancv_NP, alpha.fixed = .1)
AnvF_ancv_NP <- Anova(Fml_ancv_NP, test.statistic = "F")
AnvF_ancv_NP

# model diagnosis
plot(Fml_ancv_NP)
qqnorm(resid(Fml_ancv_NP))
qqline(resid(Fml_ancv_NP))

par(mfrow = c(1, 2))
TransVirsreg(visreg(Fml_ancv_NP, xvar = "moist", by = "temp", plot = FALSE), 
             overlay = TRUE,
             trans = exp, 
             point = list(col = c(1, 2), cex = 1),
             line = list(col = c(1, 2)))
TransVirsreg(visreg(Fml_ancv_NP, xvar = "Temp5_Mean", by = "temp", plot = FALSE), 
             overlay = TRUE,
             trans = exp, 
             point = list(col = c(1, 2), cex = 1),
             line = list(col = c(1, 2)))

## ----Stat_WTC_IEM_NPRatio_Smmry
Iml_NP@call
Anova(Iml_NP)

Fml_NP@call
Anova(Fml_NP)
AnvF_NP

# ANCOVA
Iml_ancv_NP@call
Anova(Iml_ancv_NP)

Fml_ancv_NP

# Chi test
Anova(Iml_ancv_NP)

# F test
AnvF_ancv_NP

par(mfrow = c(1, 2))
TransVirsreg(visreg(Fml_ancv_NP, xvar = "moist", by = "temp", plot = FALSE), 
             overlay = TRUE,
             trans = exp, 
             point = list(col = c(1, 2), cex = 1),
             line = list(col = c(1, 2)))
TransVirsreg(visreg(Fml_ancv_NP, xvar = "Temp5_Mean", by = "temp", plot = FALSE), 
             overlay = TRUE,
             trans = exp, 
             point = list(col = c(1, 2), cex = 1),
             line = list(col = c(1, 2)))
