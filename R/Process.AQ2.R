library(plyr)
library(lubridate)
library(reshape)

############################
# Correct NO3 based on CCV #
############################

source("functions/functions.R")

fls <- dir(path = "Data/AQ2/NeedToBeCorrected/", pattern = ".csv$")

lapply(fls, function(x) write.csv(Crrtct.ccv.df(filename = x), 
                                  paste("Data/AQ2/ReadyToProcess/", "Corrected_", x, sep =""), row.names = TRUE))


###########
# Process #
###########
# remove unneccesary part
fils <- dir(path = "Data/AQ2/ReadyToProcess/", pattern = ".csv$")

write.csv(cmbn.fls(fils), "Data/AQ2/processed.dat.csv", row.names = FALSE) 
