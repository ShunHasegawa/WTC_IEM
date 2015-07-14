# summary data frame
ChMean <- ddply(iem.mlt, .(Time, date, temp, Chamber, variable), Crt_SmryDF) 
TrtMean <- ddply(ChMean, .(Time, date, temp, variable), function(x) Crt_SmryDF(x, val = "Mean"))

palette(c("blue2", "goldenrod1", "firebrick2", "chartreuse4", "deepskyblue1", "darkorange1", 
          "darkorchid3", "darkgrey", "mediumpurple1", "orangered2", "chocolate", "burlywood3"))

theme_set(theme_bw()) # graphic backgroud is white

# plot each nutrient separately
ChFg <- dlply(ChMean, .(variable), PltChmMean)
fls <- paste("Output/Figs/WTC_IEM_Chamber_", c("Nitrate", "Ammonium", "phosphate"),sep = "")
lapply(1:3, function(x) ggsavePP(filename = fls[x], plot = ChFg[[x]], width = 6, height = 3))

TrtFg <- dlply(TrtMean, .(variable), PltTmpMean)
fls <- paste("Output/Figs/WTC_IEM_Temp_", c("Nitrate", "Ammonium", "phosphate"),sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

########################
# Plot for publication #
########################
# theme
theme_set(theme_bw())

# ymax value for each variable
TrtMean$Date <- TrtMean$date
TrtMean$Time <- TrtMean$time
ymaxDF <- ddply(TrtMean, .(variable), function(x) max(x$Mean + x$SE, na.rm = TRUE))

# load contrastDF to annotate stat result and combine with max values from
# TrtMean as y position
load("output//data/WTC_IEM_ContrastDF.RData")

Antt_CntrstDF <- merge(ContrastDF, 
                       ddply(TrtMean, .(Date, variable), summarise, yval = max(Mean + SE)),
                       # this return maximum values
                       by = c("Date", "variable"), all.x = TRUE)


Antt_CntrstDF$temp <- "amb" # temp column is required as it's used for mapping

# Stat result
load("output//data//TempTime_Stat.RData")

# ylables for facet_grid
ylabs <- list(
  'no' = expression(NO[3]^"-"),
  'nh' = expression(NH[4]^"+"),
  'po' = expression(PO[4]^"3-"))


ylab_label <- function(variable, value){
  return(ylabs[value])
}

p <- WBFig(data = TrtMean, ylab = expression(IEM*-adsorbed~nutrients~(ng~cm^"-2"~d^"-1")),
           StatRes = Stat_TempTime, 
           StatY = ymaxDF[ , 2]*1.15) +
  geom_text(data = Antt_CntrstDF, aes(x = Date, y = yval, label = stars), vjust = 0)
  
p
ggsavePP(filename = "output//figs/Manuscript/WTC_IEM", plot = p, width = 6.65, height = 6.65)

## plot all nutrients ##
# labels for facet_grid
ylabs <- list(
  'no' = expression(NO[3]^"-"),
  'nh' = expression(NH[4]^"+"),
  'po' = expression(PO[4]^"3-"))

ylab_label <- function(variable, value){
  return(ylabs[value])
}

p <- ggplot(TrtMean, aes(x = date, y = Mean, col = temp))
pl <- p + geom_line(size = 1) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE, col = temp), width = 5) + 
  scale_color_manual(values = c("blue", "red"), "Temp trt", labels = c("Ambient", "eTemp")) +
  facet_grid(variable~., scales= "free_y", labeller= ylab_label) +
  labs(x = "Time", y = expression((mu*g~cm^-2~day^-1)))
ggsavePP(filename = "Output//Figs/WTC_IEM_Temp", plot = pl, width = 6, height = 6)
