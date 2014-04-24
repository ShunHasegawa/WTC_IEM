rm(list=ls(all=TRUE))

library(plyr)
library(reshape)
library(car)

source("R//functions//functions.R")

dates <- dir("Data/Strips/")

results <- ldply(dates, cmbn.strp)

ftable(xtabs(~ time + chamber + variable + location, data = results))

# reshape table
names(results)[grep("Area", names(results))] <- "value"
results$row.names <- NULL # cast does not work the column name "row.names".
rst.cst <- cast(results, time + chamber + location ~ variable)

write.csv(rst.cst, "Output//Data/Result_WTC.IEM.SurfaceArea.csv", row.names = FALSE)
