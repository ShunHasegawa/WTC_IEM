#####################################
# merge soil moisture and temp data #
#####################################
load("Data/WTC_soilMoistTemp_Chamber_DailySummary.RData")

# restructure
names(soilChmSmry)[4] <- "probe"
SoilChMlt <- melt(soilChmSmry, id = c("Date", "Chamber", "temp", "probe"))
SoilCh <- cast(SoilChMlt, Date + Chamber + temp ~ probe + variable)

# chamber mean for IEM
plot(iem$nh)
# use iemExOl where outlier of nh is removed

# Add NP ratio
iemExOl$NP <- with(iemExOl, (no + nh)/po)
# remove Inf
iemExOl$NP[is.infinite(iemExOl$NP)] <- NA

IEM_ChMean <- ddply(iemExOl, .(Time, insertion, sampling, date, Chamber, temp), 
                    function(x) {
                      d1 <- colMeans(x[,c("no", "nh", "po", "NP")], na.rm = TRUE)
                      d2 <- with(x, gm_mean(NP, na.rm = TRUE))
                      return(c(d1, gmNP = d2))
                      })
ddply(iemExOl, .(Time, insertion, sampling, date, Chamber, temp), summarise, N=sum(!is.na(nh)))

names(IEM_ChMean)[4] <- "Date"

# mean of soil vars during incubation period
head(SoilCh)

SoilIncSampMean <- function(insertion, sampling, Chm, data = SoilCh){
  a <- subset(data, Date >= insertion & Date <= sampling & Chamber == Chm)
  vars <- names(a)[which(!names(a) %in% c("Date", "Chamber", "temp"))]
  b <- ddply(a, .(Chamber), function(x) colMeans(x[, vars], na.rm = TRUE))
  return(cbind(insertion, sampling, b))
}


IEM_DF <- ddply(IEM_ChMean, .(Time, Date, insertion, sampling, Chamber, temp, no, nh, po, NP, gmNP),
                function(x) SoilIncSampMean(insertion= x$insertion, sampling= x$sampling, Chm = x$Chamber))
IEM_DF$moist <- IEM_DF$SoilVW_5_25_Mean

p <- ggplot(SoilCh, aes(x = Date, y = SoilVW_5_25_Mean))
p2 <- p + 
  geom_line() +
  geom_point(data = IEM_DF, aes(x = Date, y = SoilVW_5_25_Mean), 
             col = "red", size = 2)+
  facet_wrap( ~ Chamber)+
  geom_vline(xintercept = as.numeric(unique(IEM_DF$insertion)), linetype = "dashed") +
  geom_vline(xintercept = as.numeric(unique(IEM_DF$sampling)), linetype = "dashed")
p2
# good

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

############
# NP Ratio #
############
source("R/Stats_NPRatio.R")

########################
## Result of contrast ##
########################
ContrastDF <- rbind(WTC_IEM_Nitrate_CntrstDf, WTC_IEM_Ammonium_CntrstDf)
save(ContrastDF, file = "output//data/WTC_IEM_ContrastDF.RData")

################
## CO2 x Time ##
################

# create stat summary table for LMM with CO2 and time
TempTimeStatList <- list(no = AnvF_no, nh = AnvF_nh, po = AnvF_po) 

Stat_TempTime <- ldply(names(TempTimeStatList), 
                      function(x) StatTable(TempTimeStatList[[x]], variable = x))
save(Stat_TempTime, file = "output//data/CO2Time_Stat.RData")
