rm(list=ls(all=TRUE))

library(car)
library(gmodels)
library(lme4)
library(lubridate)
library(MASS)
library(nlme)
library(packrat)
library(plyr)
library(reshape)
library(xlsx)
library(contrast)
library(effects)
library(ggplot2)

source("R/functions/functions.R")

iem <- read.csv("Data/WTC_IEM.csv", colClasses = c("Chamber" = "numeric", "Location" = "factor", "insertion" = "character", 
                                                   "sampling" = "character", "Time" = "factor"))

iem$Chamber <- factor(ifelse(iem$Chamber < 10, paste("0", iem$Chamber, sep = ""), iem$Chamber)) #"01" is easier to organise than "1"
iem$insertion <- as.Date(dmy(iem$insertion))
iem$sampling <- as.Date(dmy(iem$sampling))
iem <- iem[complete.cases(iem), ]
iem <- droplevels(iem)
iem$date <- as.Date(ave(apply(cbind(iem$insertion, iem$sampling), 1, mean), iem$Time), origin = origin) # same date for same date

#################
# Summary table #
#################
source("R//SummaryExlTable.R")

#########
# Stats #
#########

#############
# Phosphate #
#############
bxplts(value= "po", ofst= .0001, data= iem)

# log transformation seems best
m1 <- lme(log(po + .0001) ~ temp * Time, random = ~1|Chamber/Location, data = iem)
iem$id <- iem$Chamber:iem$Location
m2 <- lme(log(po + .0001) ~ temp * Time, random = ~1|id, data = iem)
m3 <- lme(log(po + .0001) ~ temp * Time, random = ~1|Chamber, data = iem)
anova(m1, m2, m3)

# autocorelation
atcr.cmpr(m3)
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
qqnorm(Fml, ~ resid(.)|Chamber, abline = c(0,1))
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
cntrst


# model diagnosis
plot(Fml)
qqnorm(Fml, ~ resid(.)|Chamber)
qqnorm(residuals.lm(Fml))
qqline(residuals.lm(Fml))

########
# Figs #
########
source("R//Figs.R")
