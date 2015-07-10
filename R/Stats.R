#####################################
# merge soil moisture and temp data #
#####################################
load("Data/WTC_soilMoistTemp_Chamber_DailySummary.RData")

# restructure
names(soilChmSmry)[4] <- "probe"
SoilChMlt <- melt(soilChmSmry, id = c("Date", "Chamber", "temp", "probe"))
SoilCh <- cast(SoilChMlt, Date + Chamber + temp ~ probe + variable)


# chamber mean for IEM
IEM_ChMean <- ddply(iem, .(Time, insertion, sampling, date, Chamber, temp), 
                    function(x) colMeans(x[,c("no", "nh", "po")], na.rm = TRUE))
names(IEM_ChMean)[4] <- "Date"


# mean of soil vars during incubation period
head(SoilCh)

SoilIncSampMean <- function(insertion, sampling, Chm, data = SoilCh){
  a <- subset(data, Date >= insertion & Date >= sampling & Chamber == Chm)
  vars <- names(a)[which(!names(a) %in% c("Date", "Chamber", "temp"))]
  b <- ddply(a, .(Chamber), function(x) colMeans(x[, vars], na.rm = TRUE))
  return(cbind(insertion, sampling, b))
}


IEM_DF <- ddply(IEM_ChMean, .(Time, Date, insertion, sampling, Chamber, temp, no, nh, po),
                function(x) SoilIncSampMean(insertion= x$insertion, sampling= x$sampling, Chm = x$Chamber))

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
