#####################################
# merge soil moisture and temp data #
#####################################
load("Data/WTC_soilMoistTemp_Chamber_DailySummary.RData")
head(soilChmSmry)
head(iem)



###########
# Nitrate #
###########
source("R/Stats_NO.R")

############
# Ammonium #
############
source("R/Stats_NH.R")

#############
# Phosphate #
#############
source("R/Stats_P.R")
