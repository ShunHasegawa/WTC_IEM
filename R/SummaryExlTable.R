iem.mlt <- melt(iem, id = c("Time", "insertion", "sampling", "Chamber", "Location", "temp", "date"))

# chamber mean
ChSmmryTbl <- dlply(iem.mlt, .(variable), function(x) CreateTable(x, fac = "Chamber"))

# treat mean
ChMean <- ddply(iem.mlt, .(Time, date, temp, Chamber, variable), summarise, value = mean(value, na.rm = TRUE)) 
TrtSmmryTbl <- dlply(ChMean, .(variable), function(x) CreateTable(x, fac = "temp"))

# export as excel file

# create xcel workbook
wb <- createWorkbook()

# worksheet for rowdata
sheet <- createSheet(wb,sheetName="row_data")
addDataFrame(iem, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for chamber summary
shnames <- paste("Chamber_mean.",c("Nitrate","Ammonium","Phosphate", sep=""))
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = ChSmmryTbl[[x]]))

# worksheets for temp trt summary
shnames <- paste("Temp_mean.", c("Nitrate", "Ammonium", "Phosphate"), sep = "")
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = TrtSmmryTbl[[x]]))

#save file
saveWorkbook(wb,"Output/Table/WTC_IEM.xlsx")