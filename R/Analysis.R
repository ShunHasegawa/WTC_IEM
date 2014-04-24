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
iem.mlt <- melt(iem, id = c("Time", "insertion", "sampling", "Chamber", "Location", "temp", "date"))

# chamber mean
ChSmmryTbl <- dlply(iem.mlt, .(variable), function(x) CreateTable(x, fac = "Chamber"))

# treat mean
ChMean <- ddply(iem.mlt, .(Time, date, temp, Chamber, variable), summarise, value = mean(value, na.rm = TRUE)) 
TrtSmmryTbl <- dlply(ChMean, .(variable), function(x) CreateTable(x, fac = "temp"))

# export as excel file

# create xcel workbook
wb <- createWorkbook()

# worksheet for rowdata
sheet <- createSheet(wb,sheetName="row_data")
addDataFrame(iem, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for chamber summary
shnames <- paste("Chamber_mean.",c("Nitrate","Ammonium","Phosphate", sep=""))
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = ChSmmryTbl[[x]]))

# worksheets for temp trt summary
shnames <- paste("Temp_mean.", c("Nitrate", "Ammonium", "Phosphate"), sep = "")
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = TrtSmmryTbl[[x]]))

#save file
saveWorkbook(wb,"Output/Table/WTC_IEM.xlsx")

#############
# Phosphate #
#############

## Stats ##
names(iem)
summary(subset(iem, po < max(po)))
boxplot(log(po+0.0001) ~ temp:Time, data = subset(iem, po < max(po)))
boxplot(log(po+0.0001) ~ temp:Time, data = iem)

boxplot(sqrt(no) ~ temp:Time, data = subset(iem, no < max(no)))
boxplot(no ~ temp:Time, data = iem)
boxplot(nh ~ temp:Time, data = subset(iem, nh < max(nh)))

iem$id <- iem$Chamber:iem$Location

m1 <- lme(log(po+0.0001) ~ temp * Time, random = ~ 1|Chamber/Location, data = iem, method = "ML", subset = po < max(po))
m1 <- lme(log(po+0.0001) ~ temp * Time, random = ~ 1|Chamber/Location, data = iem, method = "ML")
m1 <- lme((po+0.0001)^(1/3) ~ temp * Time, random = ~ 1|Chamber/Location, data = iem, method = "ML", subset = po < max(po))
Anova(m1)

qqnorm(residuals(m1))
qqline(residuals(m1))


m2 <- lme(log(po+0.0001) ~ temp * Time, random = ~ 1|id, data = iem, method = "ML")
m3 <- lme(log(po+0.0001) ~ temp * Time, random = ~ 1|Chamber/id, data = iem, method = "ML")
anova(m1, m2, m3)
summary(m2)

m2 <- update(m1, ~.- temp:Time)
drop1(m1, test = "Chi")
Anova(m2)
plot(allEffects(m1), x.var= "temp")

## Table ##
## Figs ##