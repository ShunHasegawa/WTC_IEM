###################################
# Coorect AQ2 result based on CCV #
###################################

# subset dataset beween each ccv
Crrct.ccv <- function(x, data, ccval = 7.5){
  # subset dataset between given ccvs
  ccv.ps <- grep("V$", as.character(data$Sample.ID))
  
  ts <- data[c(ccv.ps[x] : ccv.ps[x + 1]),]
  trng <- range(ts$times)
  xv <- as.numeric(trng)
  yv <- ts$Result[ts$times == trng[1] | ts$times == trng[2]] 
  
  b <- ccval * (1 / yv[1] - 1 / yv[2]) / (xv[1] - xv[2])
  a <- ccval / yv[1] - b * xv[1]
  
  ts$Result <- ts$Result * (a + b * as.numeric(ts$times))
  ts$times <- NULL
  return(ts)
}

# applied the function above for each subsets of ccvs
Crrtct.ccv.df <- function(filename, ccval = 7.5){
  data <- read.csv(paste("Data/AQ2/NeedToBeCorrected/", filename, sep = ""), header = TRUE)
  
  # make time factor as numeric
  a <- sapply(as.character(data$Time), strsplit, " ")
  b <- ldply(a, function(x) paste(x[c(5, 2, 3, 4)], collapse = "/"))
  
  b$V1 <- ymd_hms(b$V1)
  
  names(b) <- c("Time", "times")
  
  # merge
  mrg.res <- merge(data, b, by = "Time")
  
  # reorder accoding to time
  mrg.res <- mrg.res[order(mrg.res$times), ]
  
  ccv.ps <- grep("V$", as.character(mrg.res$Sample.ID))
  
  # re-caluculate the results
  a <- ldply(1:(length(ccv.ps)-1), function(x) Crrct.ccv(x, data = mrg.res, ccval))
  return(a)
}
######################################################
# process and combine aq 2 data, then create a table #
######################################################
prcsAQ2 <- function(data){
  # remove ccv, ccb, standard
  res <- data[-grep("^C|^STANDARD", as.character(data$Sample.ID)),]
  
  # remove dup, top, middle
  res <- res[-grep("dup$|top|middle", as.character(res$Sample.Details)),]
  
  # sample labels
  a <- strsplit(as.character(res$Sample.Details), "[.]")
  
  # turn this into data frame
  a.df <- ldply(a)
  names(a.df)[c(1, 4, 5)] <- c("Date", "ring", "plot")
  a.df$Date <- ymd(a.df$Date)
  res.df <- cbind(a.df, res)
  res.df <- res.df[c("Date", "ring", "plot", "Result")]
  return(res.df)
}

cmbn.fls <- function(file){
  # read files
  rd.fls <- lapply(file, function(x) read.csv(paste("Data/AQ2/ReadyToProcess/", x, sep = ""), header = TRUE))
  
  # process and make data frame for each test type
  pr.df <- ldply(rd.fls, function(x) ddply(x, .(Test.Name), prcsAQ2))
  
  # reshape
  names(pr.df)[5] <- "value"
  pr.cst <- cast(pr.df, Date + ring + plot ~ Test.Name)
  return(pr.cst)
}

###############################################
# read and combine strip surface area results #
###############################################
Process.strp <- function(file, time){
  b <- read.table(file, row.names = NULL)
  b.df <- ddply(b, .(row.names), summarise, Area = sum(Area)) 
  b.df$time <- time
  
  # chamber number
  a <- gsub("(_.{1,2}[.]xls)", "", file) # after "_" part is removed
  ch <- as.numeric(gsub(".*_", "", a)) # "_" and before part is removed
  b.df$chamber <- factor(ifelse(b.df$row.names %in%  c(1:3), ch, 
                                ifelse(b.df$row.names %in%  c(4:6), ch + 1, 
                                       ch + 2)))
  # variable type
  b.df$variable <- gsub(".*/|_.*", "", a) # before "/" and after "_" part are removed
  return(b.df)
}

# combine the processed results with their dates
cmbn.strp <- function(dates){
  files <- paste("Data//Strips//", dates, sep = "") # choose folder
  a <- dir(files, full.names = TRUE) # files in the folder to be read
  a.cmb <- ldply(a, function(x) Process.strp(x, time = gsub(".*//", "", files))) # process and combine them all
  return(a.cmb)
}
