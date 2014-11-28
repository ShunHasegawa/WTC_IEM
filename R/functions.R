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
  
  # add the latest ccv value at the end of data frame to make sure all measured values are placed between ccvs
  # if the last line is not ccv, vlues after last ccv will be turned into NA
  ccv.ps <- grep("V$", as.character(mrg.res$Sample.ID))
  lstTime <- mrg.res$times[nrow(mrg.res)]
  mrg.res[nrow(mrg.res) + 1, ] <- mrg.res[max(ccv.ps), ] 
  mrg.res$times[nrow(mrg.res)] <- lstTime + 1 # make the last time latest by adding 1 to the actual latest time
  
  ccv.ps <- grep("V$", as.character(mrg.res$Sample.ID)) # update ccv.position
  
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
  names(a.df)[c(1, 4, 5)] <- c("Date", "chamber", "location")
  a.df$Date <- ymd(a.df$Date)
  res.df <- cbind(a.df, res)
  res.df <- res.df[c("Date", "chamber", "location", "Result")]
  res.df$chamber <- as.numeric(res.df$chamber)
  res.df$location <- as.numeric(res.df$location)
  return(res.df)
}

cmbn.fls <- function(file){
  # read files
  rd.fls <- lapply(file, function(x) read.csv(paste("Data/AQ2/ReadyToProcess/", x, sep = ""), header = TRUE))
  
  # process and make data frame for each test type
  pr.df <- ldply(rd.fls, function(x) ddply(x, .(Test.Name), prcsAQ2))
  
  # reshape
  names(pr.df)[5] <- "value"
  pr.cst <- cast(pr.df, Date + chamber + location ~ Test.Name)
  return(pr.cst)
}

###############################################
# read and combine strip surface area results #
###############################################
Process.strp <- function(file, time){
  b <- read.table(file, row.names = NULL)
  b.df <- ddply(b, .(row.names), summarise, Area = sum(Area)) # some strips were ripped off so aggregate
  b.df$time <- time
  
  # chamber number
  a <- gsub("(_.{1,2}[.]xls)", "", file) # after "_" part is removed
  ch <- as.numeric(gsub(".*_", "", a)) # "_" and before part is removed
  b.df$chamber <- as.numeric(ifelse(b.df$row.names %in%  c(1:3), ch, 
                                ifelse(b.df$row.names %in%  c(4:6), ch + 1, 
                                       ch + 2)))
  b.df$location <- as.numeric(ifelse(b.df$row.names %in%  c(1, 4, 7), 1, 
                                 ifelse(b.df$row.names %in%  c(2, 5, 8), 2, 
                                        3)))
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

#######################
#model simplification #
#######################
MdlSmpl <- function(model){
  mod2 <- update(model, method = "ML") #change method from REML to ML
  stai <- stepAIC(mod2, trace = FALSE) #model simplification by AIC
  dr <- drop1(stai, test="Chisq") #test if removing a factor even more significantly lowers model
  model <- update(stai, method="REML")
  ifelse(all(dr[[4]] < 0.05, na.rm=TRUE), anr <- anova(model), anr<-NA) 
  #dr[[4]]<0.05-->unable to remove any more factors so finlize the results by changsing the method back to REML
  return(list(step.aic = stai$anova, drop1 = dr, anova.reml = anr, model.reml = model, model.ml = stai))
}

##########################
# Create a summary table #
##########################
CreateTable <- function(dataset, fac){
  #dataset=iem for chamber summary, ChMean for temp trt summary, fac = Chamber or temp, nutrient=no/nh/po
  a <- dataset[c("date", fac, "value")] #extract required columns
  colnames(a) <- c("date","variable","value") #change column names for cast
  means <- cast(a, date~variable, mean, na.rm = TRUE) 
  ses <- cast(a,date~variable,function(x) ci(x,na.rm=TRUE)[4])
  colnames(ses)[2:ncol(ses)] <- paste(colnames(ses)[2:ncol(ses)],"SE",sep=".")
  samples <- cast(a,date~variable,function(x) sum(!is.na(x))) #sample size
  colnames(samples)[2:ncol(samples)] <- paste(colnames(samples)[2:ncol(samples)],"N",sep=".")
  mer <- Reduce(function(...) merge(..., by = "date"), list(means, ses, samples)) #merge datasets
  mer <- mer[,c(1, order(names(mer)[-grep("date|N", names(mer))])+1, grep("N", names(mer)))] #re-order columns
  mer$date <- as.character(mer$date) # date is turned into character for knitr output 
  return(mer)
}

#function which creates excel worksheets
crSheet <- function(sheetname, dataset){
  #create sheet
  sheet <- createSheet(wb, sheetName = sheetname)
  
  #add data to the sheet
  addDataFrame(dataset, sheet, showNA = TRUE, row.names = FALSE, startRow = 2)
  
  #title of the sheet
  addDataFrame(t(c(sheetname, "unit=ug cm^(-2) day^(-1))")), sheet, startRow = 1, row.names = FALSE, col.names = FALSE)
}

#############################################
# compare different auto-correlation models #
#############################################

atcr.cmpr <- function(model, rndmFac){
  if(rndmFac == "Chamber/Location"){
    model2 <- update(model,corr=corCompSymm(form=~1|Chamber/Location)) 
  } else {
    if(rndmFac == "Chamber"){
      model2 <- update(model,corr=corCompSymm(form=~1|Chamber))
    } else {
      model2 <- update(model,corr=corCompSymm(form=~1|id))
    }
  }
  
  model3 <- update(model,correlation=corARMA(q=2))
  model4 <- update(model,correlation=corAR1()) 
  model5 <- update(model,correlation=corARMA(q=1))
  a <- anova(model,model2,model3,model4,model5)
  rownames(a) <- c("NULL", "corCompSymm", "corARMA(q=2)", "corAR1()", "corARMA(q=1)")
  models <- list(model, model2, model3, model4, model5, 'models' = a)
  return(models)
}

###########################################
# produce box plots with transformed data #
###########################################
# log OR sqrt OR power(1/3) OR inverse OR box-cox
bxplts <- function(value, ofst = 0, data, ...){
  data$y <- data[[value]] + ofst #ofst is added to make y >0
  a <- boxcox(y ~ temp * Time, data = data)
  par(mfrow = c(2, 3))
  boxplot(y ~ temp*Time, data, main = "row")
  boxplot(log(y) ~ temp*Time, main = "log", data)
  boxplot(sqrt(y) ~ temp*Time, main = "sqrt", data)
  boxplot(y^(1/3) ~ temp*Time, main = "power(1/3)", data)
  boxplot(1/y ~ temp*Time, main = "inverse", data)
  BCmax <- a$x[a$y == max(a$y)]
  texcol <- ifelse(BCmax < 0, "red", "black") 
  boxplot(y^(BCmax) ~ temp*Time, 
          main = "", sep = "=", 
          data = data)
  title(main = paste("Box Cox", round(BCmax, 4)), 
        col.main = texcol)
  par(mfrow = c(1,1))
}

# multiple box-cox power plot for different constant values
bxcxplts <- function(value, data, sval, fval){
  data$yval <- data[[value]]
  ranges <- seq(sval, fval, (fval - sval)/9)
  
  # store parameters given from box-cox plot
  par(mfrow = c(5, 2))
  BCmax <- vector()
  for (i in 1:10){
    data$y <- data$yval + ranges[i]
    a <- boxcox(y ~ temp * Time, data = data)
    BCmax[i] <- a$x[a$y == max(a$y)]
  }
  
  # plot box plot with poer given from box-box for 
  # each contstant value
  par(mfrow = c(5, 2))
  par(omi = c(0, 0, 0, 0), mai = c(0.4, 0.4, 0.4, 0))
  sapply(1:10, function(x) {
    boxplot((yval + ranges[x]) ^ BCmax[x] ~ temp * Time, 
            main = "", data = data)
    texcol <- ifelse(BCmax[x] < 0, "red", "black") 
    title(main = paste("constant=", round(ranges[x], 4), 
                       ", boxcox=", round(BCmax[x], 4)),
          col.main = texcol)
  })
  par(mfrow = c(1,1))
}


############################
# make a summary dataframe #
############################
Crt_SmryDF <- function(data, val = "value"){
  x <- data[ ,val]
  Mean <- mean(x, na.rm = TRUE)
  SE <- ci(x, na.rm = TRUE)[[4]]
  N  <- sum(!is.na(x))
  data.frame(Mean, SE, N)
}

############################
# Plot Chamber mean and SE #
############################
PltChmMean <- function(data){
  p <- ggplot(data, aes(x = date, y = Mean, col = Chamber, linetype = Chamber))
  ylab <- ifelse(unique(data$variable) == "no", expression(NO[3]^"-"-N~(mu*g~cm^-2~day^-1)), 
                 ifelse(unique(data$variable) == "nh", expression(NH[4]^"+"-N~(mu*g~cm^-2~day^-1)), 
                        expression(PO[4]^"3-"-P~(mu*g~cm^-2~day^-1))))
  p + geom_line(size = 1) + 
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE, col = Chamber), width = 5) + 
    scale_color_manual(values = palette(), "Chamber", labels = paste("Chamber", c(1:12), sep = "_")) +
    scale_linetype_manual(values = rep(c("solid", "dashed"), 6), 
                          "Chamber", labels = paste("Chamber", c(1:12), sep = "_")) +
    labs(x = "Time", y = ylab)
}

#############################
# plot Temp trt mean and SE #
#############################
PltTmpMean <- function(data){
  p <- ggplot(data, aes(x = date, y = Mean, col = temp))
  ylab <- ifelse(unique(data$variable) == "no", expression(NO[3]^"-"-N~(mu*g~cm^-2~day^-1)), 
                 ifelse(unique(data$variable) == "nh", expression(NH[4]^"+"-N~(mu*g~cm^-2~day^-1)), 
                        expression(PO[4]^"3-"-P~(mu*g~cm^-2~day^-1))))
  p + geom_line(size = 1) + 
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE, col = temp), width = 5) + 
    scale_color_manual(values = c("blue", "red"), "Temp trt", labels = c("Ambient", "eTemp")) +
    labs(x = "Time", y = ylab)
}

####################################
# create table of contrast results #
####################################
cntrstTbl <- function(cntrstRes, data){
  d <- unique(data$date)
  ds <- format(d, format = "%b-%Y")
  
  Df <- data.frame(
    date = ds,
    contrast  =  cntrst$Contrast,
    SE = cntrst$SE,
    t = cntrst$testStat,
    df = cntrst$df,
    P.value = cntrst$Pvalue)
  return(Df)
}

##############################
# Save ggplot in PDF and PNG #
##############################
ggsavePP <- function(filename, plot, width, height){
  ggsave(filename = paste(filename, ".pdf", sep = ""), 
         plot = plot, 
         width = width, 
         height = height)
  
  ggsave(filename = paste(filename, ".png", sep = ""), 
         plot = plot, 
         width = width, 
         height = height, 
         dpi = 600)
}

###########################
# step deletion with lmer #
###########################
stepLmer <- function(model, red.rndm = FALSE, ddf = "Kenward-Roger", ...){
  require(lmerTest)
  update(step(model, reduce.random = red.rndm, ddf = ddf,...)$model, 
         contrasts = NULL)
}
# use "Kenward-Roger" for approximation for denominator degrees of freedom. This
# is the same as the default DF given by Anova(model, test.statistic = "F). The
# default of step gives me a warning message for IEM-NO3 for some reasons (not
# sure why.. so changed it.)
