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
fls <- paste("Output/Figs/WTC_IEM_Temp_", c("Nitrate", "Ammonium", "phosphate"), ".pdf",sep = "")
lapply(1:3, function(x) ggsave(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

## plot all nutrients ##
# labels for facet_grid
ylabs <- list(
  'no' = expression(NO[3]^"-"-N),
  'nh' = expression(NH[4]^"+"-N),
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
ggsavePP(filename = "Output//Figs/WTC_IEM_Temp", plot = pl, width = 6, height = 6)
