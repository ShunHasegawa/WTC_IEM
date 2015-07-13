## --------Stat_WTC_IEM_ChMean_NPRatio

#############
# N:P ratio #
#############

## ---- Stat_FACE_IEM_Analyse_NP
############
# NP ratio #
############
bxplts(value= "gmNP", data= IEM_DF)
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
xyplot(NP ~ moist|temp, groups = Chamber, type = c("r", "p"), data = IEM_DF)
xyplot(NP ~ moist|Chamber, type = c("r", "p"), data = IEM_DF)
# each time
xyplot(NP ~ moist|temp, groups = Time, type = c("r", "p"), data = IEM_DF)
xyplot(NP ~ moist|Time, type = c("r", "p"), data = IEM_DF)

scatterplotMatrix(~NP + moist + Temp5_Mean|temp, data = IEM_DF, diag = "boxplot")
scatterplotMatrix(~log(NP) + log(moist) + Temp5_Mean|temp, data = IEM_DF, diag = "boxplot")

m1 <- lmer(log(NP) ~ temp * log(moist) + (1|Time) + (1|Chamber), data = IEM_DF)
Anova(m1)
visreg(m1, xvar = "moist", by = "temp", overlay = TRUE)
# Interaction is indicated, but moisture range is quite different. what if I use
# the samge range of moisture for both treatment
ddply(IEM_DF, .(temp), summarise, range(moist))
m2 <- update(m1, subset = moist < 0.14)
Anova(m2)
visreg(m2, xvar = "moist", by = "temp", overlay = TRUE)
# interaction is marginally indicated, so include

Iml_ancv_NP <- lmer(log(NP) ~ temp * log(moist) + (1|Time) + (1|Chamber), data = IEM_DF)
m2 <- update(Iml_ancv_NP, ~. - (1|Time))
m3 <- update(Iml_ancv_NP, ~. - (1|Chamber))
anova(Iml_ancv_NP, m2, m3)

Fml_ancv_NP <- Iml_ancv_NP
summary(Fml_ancv_NP)
AnvF_ancv_NP <- Anova(Fml_ancv_NP, test.statistic = "F")
AnvF_ancv_NP
r.squared(Fml_ancv_NP)
visreg(Fml_ancv_NP, xvar = "moist", by = "temp", overlay = TRUE)

# Estimate 95% confidence intervals for this model
range(IEM_DF$moist)
expDF <- expand.grid(temp = c("amb", "elev"), moist = seq(.06, .2, length.out = 50))
bb <- bootMer(Fml_ancv_NP, FUN=function(x) predict(x, expDF, re.form = NA), nsim=500)
lci <- apply(bb$t, 2, quantile, 0.025)
uci <- apply(bb$t, 2, quantile, 0.975)
PredVal <- bb$t0
PredDF <- cbind(lci, uci, PredVal, expDF)

theme_set(theme_bw())
p <- ggplot(PredDF, aes(x = log(moist * 100), y = PredVal, col = temp, fill = temp, group = temp))
p2 <- p + 
  geom_line() +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .4, col = NA) +
  geom_point(data = IEM_DF, aes(x = log(moist * 100), y = log(NP)), size = 3, alpha = .6) +
  scale_colour_manual(values = c("blue", "red"), labels = c("Ambient", "eTemp")) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Ambient", "eTemp")) +
  theme(panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.length = unit(-.2, "lines"),
        axis.ticks.margin = unit(.5, "lines"),
        legend.position = c(.19, .85), 
        legend.title = element_blank(),
        legend.key.width = unit(2.5, "lines"),
        legend.key = element_blank()) + 
  labs(x = "log(Moisture (%))", y = "log(N:P ratio)")
p2
ggsavePP(plot = p2, filename = "Output/Figs/Manuscript/WTC_NPRatioMoist", width = 4, height = 3)  

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