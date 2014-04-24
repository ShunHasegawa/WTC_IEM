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

res <- read.csv("Data/WTC_IEM.csv", colClasses = c("Chamber" = "factor", "Location" = "factor", "insertion" = "character", 
                                                   "sampling" = "character", "Time" = "factor"))
res$insertion <- dmy(res$insertion)
res$sampling <- dmy(res$sampling)
res <- res[complete.cases(res), ]
res <- droplevels(res)

#############
# Phosphate #
#############

## Stats ##
names(res)
summary(subset(res, po < max(po)))
boxplot(log(po+0.0001) ~ temp:Time, data = subset(res, po < max(po)))
boxplot(log(po+0.0001) ~ temp:Time, data = res)

boxplot(sqrt(no) ~ temp:Time, data = subset(res, no < max(no)))
boxplot(no ~ temp:Time, data = res)
boxplot(nh ~ temp:Time, data = subset(res, nh < max(nh)))

res$id <- res$Chamber:res$Location

m1 <- lme(log(po+0.0001) ~ temp * Time, random = ~ 1|Chamber/Location, data = res, method = "ML", subset = po < max(po))
m1 <- lme(log(po+0.0001) ~ temp * Time, random = ~ 1|Chamber/Location, data = res, method = "ML")
m1 <- lme((po+0.0001)^(1/3) ~ temp * Time, random = ~ 1|Chamber/Location, data = res, method = "ML", subset = po < max(po))
Anova(m1)

qqnorm(residuals(m1))
qqline(residuals(m1))


m2 <- lme(log(po+0.0001) ~ temp * Time, random = ~ 1|id, data = res, method = "ML")
m3 <- lme(log(po+0.0001) ~ temp * Time, random = ~ 1|Chamber/id, data = res, method = "ML")
anova(m1, m2, m3)
summary(m2)

m2 <- update(m1, ~.- temp:Time)
drop1(m1, test = "Chi")
Anova(m2)
plot(allEffects(m1), x.var= "temp")

## Table ##
## Figs ##