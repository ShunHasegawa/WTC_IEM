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
m1 <- lme(log(po + .0001) ~ temp * Time, random = ~1|Chamber/Location, data = iem)


atcr.cmpr(m1)
# no need for auto-correlation

# random factor structure
m2 <- lme(log(po + .0001) ~ temp * Time, random = ~1|id, data = iem)
anova(m1, m2)

# m2 is slightly better, between-chamber variation is less important within-chanmber (=between location within chamber)
MdlSmpl(m2)
m3 <- update(m2, ~ . -temp:Time)
MdlSmpl(m3)
Fml <- MdlSmpl(m3)$model.reml
Anova(Fml)
plot(allEffects(Fml))

########
# Figs #
########
# summary data frame
ChMean <- ddply(iem.mlt, .(Time, date, temp, Chamber, variable), Crt_SmryDF) 
TrtMean <- ddply(ChMean, .(Time, date, temp, variable), function(x) Crt_SmryDF(x, val = "Mean"))

palette(c("blue2", "goldenrod1", "firebrick2", "chartreuse4", "deepskyblue1", "darkorange1", 
          "darkorchid3", "darkgrey", "mediumpurple1", "orangered2", "chocolate", "burlywood3"))

theme_set(theme_bw()) # graphic backgroud is white

# plot each nutrient separately
ChFg <- dlply(ChMean, .(variable), PltChmMean)
fls <- paste("Output/Figs/WTC_IEM_Chamber_", c("Nitrate", "Ammonium", "phosphate"), ".pdf",sep = "")
lapply(1:3, function(x) ggsave(filename = fls[x], plot = ChFg[[x]], width = 6, height = 3))

TrtFg <- dlply(TrtMean, .(variable), PltTmpMean)
fls <- paste("Output/Figs/WTC_IEM_Temp_", c("Nitrate", "Ammonium", "phosphate"), ".pdf",sep = "")
lapply(1:3, function(x) ggsave(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

## plot all nutrients ##
# labels for facet_grid
ylabs <- list(
  'no' = expression(NO[3]^"-"-N),
  'nh' = expression(NH[4]^"3+"-N),
  'po' = expression(PO[4]^"3-"-P))

ylab_label <- function(variable, value){
  return(ylabs[value])
}

p <- ggplot(TrtMean, aes(x = date, y = Mean, col = temp))
pl <- p + geom_line(size = 1) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE, col = temp), width = 5) + 
  scale_color_manual(values = c("blue", "red"), "Temp trt", labels = c("Ambient", "eTemp")) +
  facet_grid(variable~., scales= "free_y", labeller= ylab_label) +
  labs(x = "Time", y = expression((mu*g~cm^-2~day^-1)))
ggsave(filename = "Output//Figs/WTC_IEM_Temp.pdf", plot = pl, width = 6, height = 6)
