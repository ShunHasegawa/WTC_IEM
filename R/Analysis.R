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

# autocorelation
m1 <- lme(log(po + .0001) ~ temp * Time, random = ~1|Chamber/Location, data = iem, method = "ML")
atcr.cmpr(m1)
# no need for auto-correlation

# random factor structure
iem$id <- iem$Chamber:iem$Location
m2 <- lme(log(po + .0001) ~ temp * Time, random = ~1|id, data = iem, method = "ML")
m3 <- lme(log(po + .0001) ~ temp * Time, random = ~1|Chamber, data = iem, method = "ML")
m4 <- lme(log(po + .0001) ~ temp * date, random = ~1|id, data = iem, method = "ML")


anova(m1, m2, m3, m4)
summary(m3)

# m3 is slightly better, between-chamber variation is less important within-chanmber (=between location within chamber)
MdlSmpl(m3)
m4 <- update(m3, ~ . -temp:Time)
MdlSmpl(m4)
Fml <- MdlSmpl(m4)$model.reml
Anova(Fml)
plot(allEffects(Fml))
qqnorm(Fml, ~ranef(.))

qqline(Fml)

########
# Figs #
########
source("R//Figs.R")
