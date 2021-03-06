rm(list=ls(all=TRUE))

source("R/packages.R")
source("R/functions.R")

# iem <- read.csv("Data/WTC_IEM.csv", colClasses = c("Chamber" = "numeric", "Location" = "factor", "insertion" = "character", 
#                                                    "sampling" = "character", "Time" = "factor"))
# 
# iem$Chamber <- factor(ifelse(iem$Chamber < 10, paste("0", iem$Chamber, sep = ""), iem$Chamber)) #"01" is easier to organise than "1"
# iem$insertion <- as.Date(dmy(iem$insertion))
# iem$sampling <- as.Date(dmy(iem$sampling))
# iem <- iem[complete.cases(iem), ]
# iem <- droplevels(iem)
# iem$date <- as.Date(ave(apply(cbind(iem$insertion, iem$sampling), 1, mean), iem$Time), origin = origin) # same date for same date
# iem$id <- iem$Chamber:iem$Location
# # change unit from ug to ng
# iem$po <- iem$po * 1000 
# iem$no <- iem$no * 1000 
# iem$nh <- iem$nh * 1000 
# save(iem, file = "Output//Data/WTC_IEM.RData")
load("Output//Data/WTC_IEM.RData")

#################
# Summary table #
#################
source("R//SummaryExlTable.R")

#########
# Stats #
#########
source("R//Stats.R")

# save all ovjects
save.image(file = "Output/Data/AllObj.RData")

########
# Figs #
########
source("R//Figs.R")
