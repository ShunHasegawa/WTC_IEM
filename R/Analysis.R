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
save(iem, file = "Output//Data/WTC_IEM.Rdata")

#################
# Summary table #
#################
source("R//SummaryExlTable.R")

#########
# Stats #
#########
source("R//Stats.R")

########
# Figs #
########
source("R//Figs.R")
