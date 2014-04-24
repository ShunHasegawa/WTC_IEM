rm(list=ls(all=TRUE))

library(xlsx)
library(plyr)
library(reshape)
library(car)

source("R//functions//functions.R")

dates <- dir("Data/Strips/")

results <- ldply(dates, cmbn.strp)

ftable(xtabs(~ time + ring + variable + row.names, data = results))

# Dec2013 P_5.2 has two values so combine them
results <- ddply(results, .(time, ring, row.names, variable), summarise, Area = sum(Area))

# reshape table
names(results)[grep("Area", names(results))] <- "value"
names(results)[grep("row", names(results))] <- "plot" # cast does not work the column name "row.names".
rst.cst <- cast(results, time + ring + plot ~ variable)

write.csv(rst.cst, "Data//Strips/Result_IEM.SurfaceArea.csv", row.names = FALSE)
