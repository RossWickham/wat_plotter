

### General ##############


splitByColName <- function(inDF,colName) split(inDF,inDF[,colName])

fastSplit<- function(inDF, colName, nCuts=10){
  #Performs the split operation, but faster by applying split
  #  only to subsets of the original dataset.
  #  Might be a litte more memory intensive, but worth it
  require(foreach)
  cutIndex <- cut(x = inDF[,colName], breaks=nCuts) #where to split rows
  out <-  #makes list of lists that need to be flattened
    foreach(cIndex = unique(cutIndex), .multicombine = T) %do%
    splitByColName(inDF[cutIndex==cIndex,],colName)
  do.call(c,out) #flattening to one big list
}

# recursiveFastSplit <- function(inDF, colNames){
#   if( length(colNames) == 1 ) return(inDF)
#   colName <- colNames[1]
#   colNames <- colNames[-1]
#   lapply(fastSplit(inDF, colName), recursiveFastSplit,colNames=colNames )
# }

# recursiveMerge <- function(splitDF1, splitDF2, colNames){
#   #recursively merges two nested lists that were created by 
#   #  the 'recursiveFastSplit' function.
#   
#   #correction so that column names don't get recycled in 'Map' function call
#   #  and are interpretted as vector
#   if(!is.list(colNames)) colNames <- list(colNames) 
# 
#   #If at the dataframe level, perform merge on paired dataframes.
#   #  otherwise, drill down another level and feed back into recursive function
#   if( is.data.frame(splitDF1) ) { #assuming same structure as splitDF2
#     return( base::merge(x=splitDF1,y=splitDF2, by=unlist(colNames), all=T) )
#   }else{
#     return( Map(recursiveMerge, splitDF1, splitDF2,colNames=colNames) )
#   }
# }
# 
# fastMerge <- function(inDF1, inDF2, colNames){
#   require(dplyr)
#   #merges two dataframes given the column names to be used
#   rSplit1 <- recursiveFastSplit(inDF1, colNames) #split each df
#   rSplit2 <- recursiveFastSplit(inDF2, colNames)
#   cat("\n\t\tfastMerge:\tCompleted split of dataframes")
#   #recursive merge to a list of dataframes, then bind rows
#   bind_rows(recursiveMerge(rSplit1, rSplit2, colNames=colNames))
# }


fastMerge <- function(dataList, colNames){
  require(reshape2) 
  #merges two dataframes given the column names to be used
  
  #Creating a new column with the list element's name
  dataList <- Map(function(x,y){x$variable=y;x},dataList, names(dataList))
  bData <- bind_rows(dataList) #merging to one big list
  
  #creating formula for casting
  castFormula <- as.formula(sprintf("%s ~ variable", paste0(colNames,collapse="+")))
  
  #performing cast
  out <- dcast(data = bData, formula = castFormula, value.var = "value")
  gc()
  return(out)
}

stripDFAttributes <- function(inDF){
  #strips all of the attributes, returns dataframe, preserves column names
  dfAttrNames <- names(attributes(inDF))
  for( a in dfAttrNames[dfAttrNames %!in% c("names")]  ) attr(inDF, a) <- NULL
  return(as.data.frame(inDF))
}

firstPeak <- function(values){
  # returns a logical with only one instance of TRUE 
  #  occurring at the first peak value
  peakIndex <- which.max(values)
  out <- logical(length(values))
  out[peakIndex] <- T
  return(out)
}

extractPeaksByEvent <- function(inDF){
  #Reduces to only the peaks. Input assumes the
  # standard time series dataframe used in the R project,
  #  with columns for the 'date', 'value', and 'event'
  require(dplyr)
  out <- inDF %>% group_by(event) %>% filter( firstPeak(value) )
  stripDFAttributes( out )
}

applyNamedFunctions <- function(df, fNames){
  #dataframe with a 'value' column which a series of
  #  functions is applied to
  #fNames defines the percentiles
  #  and mean or median.  It is a character vector that can have the following entries:
  #  perentile: e.g, "10%"
  #  "mean", "median"
  out <- vector("numeric",length(fNames))
  names(out) <- fNames
  
  for( f in fNames ){
    if( grepl("%",f) ){
      #assuming a quantile should be applied
      q <- as.numeric(str_extract(f,"\\d*"))
      out[f] <- quantile(x = df$value, probs = q/100)
    }else{
      #assuming it's a named function
      out[f] <- eval(call(name = f,df$value))
    }
  }
  return(out)
}

# mergeDFListByDateEvent <- function(eventList){
#   #merge a list of dataframes by date and event, with value columns
#   #  renamed so they are uniquely named after the alternatives 
#   eventList <- renameColsInDFList(eventList,"value",names(eventList))
#   #performing merge of list of dataframes
#   Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("event","date"), all = T),
#          eventList)
# }
# 
# 
mergeDFListByEvent <- function(eventList){
  #merge a list of dataframes by event, with columns value and date columns
  #  renamed so they are uniquely named after the alternatives
  eventList <- renameColsInDFList(eventList,"value",names(eventList))
  eventList <- renameColsInDFList(eventList,"date",sprintf("date %s",names(eventList)))
  #performing merge of list of dataframes
  Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("event"), all = T),
         eventList)
}



simplifyLine <- function(df,simplifyCol=names(df)[1],maxPts=10000){
  #Filters by skipping a set number of points to get (approximately) the maxPts 
  #  number of outputs
  
  #Error handling
  if( !is.data.frame(df) )
    stop(sprintf("\nError in simplifyLine function: argument 'df' needs to be a dataframe", simplifyCol))
  if( !is.numeric( df[,simplifyCol ]  ) ) 
    stop(sprintf("\nError in simplifyLine function: column '%s' needs to be numeric to sort", simplifyCol))
  
  # df <- df[order(df[, simplifyCol], decreasing=F),] 
  
  if( nrow(df) < maxPts) return(out)
  
  # maxPts <- min(nrow(df), maxPts) #Reducing maximum number of points if they're greater than the vector length
  
  nPtsToSkip <- max(1,as.integer(nrow(df)/maxPts))
  
  out <- df[seq(1,nrow(df), by=nPtsToSkip),]  #Reducing to the desired number of rows
  
  return(out)
}


### WY Groupings ##########


computeCumulativeP <- function(wyRank){
  wyRank$cumP <- cumsum(wyRank$percentage)
  if(wyRank$cumP[nrow(wyRank)] != 100)
    stop("\nNeed percentages of 'wyRanking' object to add to 100")
  return(wyRank)
}


### Summary Hydrograph ##########


SummaryHydrograph_Deterministic <- function(pathData,plotInfo,computeType){
  computeSummaryHydrograph(pathData,plotInfo$metrics)
}

SummaryHydrograph_FRA5k <- SummaryHydrograph_Deterministic

SummaryHydrograph_FRA50k <-SummaryHydrograph_Deterministic

computeSummaryHydrograph <- function(wyDF, metrics=c("min","mean","max")){
  #Expecting a dataframe input for wyDF that has columns called
  #  'day','month', 'wyCat', and numeric column called 'value'
  #An additional argument can be passed called 'metrics' that defines the percentiles
  #  and mean or median.  It is a character vector that can have the following entries:
  #  perentile: e.g, "10%"
  #  "mean", "median"
  #Output is the summary hydrograph by unique date/month with a column for each metric
  require(dplyr)
  if(any(c("day","month") %!in% names(wyDF))) {
    wyDF$day <- day(wyDF$date)
    wyDF$month <- month(wyDF$date)
  }
  convertDFToSameWY(
    ddply(.data = wyDF, .variables = .(day,month),
          .fun=applyNamedFunctions, fNames=metrics)
  )
}


### Quintile Forecast ############

computeSummaryHydrographByWYCat <- function(wyDF, metrics=c("min","mean","max")){
  #Expecting a dataframe input for wyDF that has columns called
  #  'day','month', 'wyCat', and numeric column called 'value'
  #An additional argument can be passed called 'metrics' that defines the percentiles
  #  and mean or median.  It is a character vector that can have the following entries:
  #  perentile: e.g, "10%"
  #  "mean", "median"
  #Output is the summary hydrograph by unique date/month with a column for each metric
  require(dplyr)
  ddply(.data = wyDF, .variables = .(day,month,wyCat),
        .fun=applyNamedFunctions, fNames=metrics)
}

QuintileByForecast_Deterministic <- function(pathData,plotInfo,computeType){
  characterizeTS(tsDF = pathData,
                 eventCharDF = fcsts$`80yr`[[plotInfo$fcstProj]],
                 eventCharCol = plotInfo$fcstDate,
                 wyCats = wyRanking[[plotInfo$wyGroup]])
}

QuintileByForecast_FRA5k <- function(pathData,plotInfo,computeType){
  characterizeTS(tsDF = pathData,
                 eventCharDF = fcsts$fiveK[[plotInfo$fcstProj]],
                 eventCharCol = plotInfo$fcstDate,
                 wyCats = wyRanking[[plotInfo$wyGroup]])
  
}

QuintileByForecast_FRA50k <- function(pathData,plotInfo,computeType){
  characterizeTS(tsDF = pathData,
                 eventCharDF = fcsts$fiftyK[[plotInfo$fcstProj]],
                 eventCharCol = plotInfo$fcstDate,
                 wyCats = wyRanking[[plotInfo$wyGroup]])
}


characterizeWYs <- function(eventCharDF,eventCharCol,wyCats){
  #charcaterizes the forecasts according to the percentiles
  #  provided in 'wyCats', using the column matching
  #  'eventCharCol'
  
  #reducing only to 'event', the column that matches the forecast, and 'incProbs' for 80 year
  out <- eventCharDF[,str_detect(names(eventCharDF),paste0(c("event",eventCharCol,"incProbs"),collapse="|"))]
  
  #Ranking based on percentile.  For 80 year, need to use the incremental probabilities - not the rank
  if( 'incProbs' %in% names(out) ){
    out$percentile[order(out[,eventCharCol])] <- 
      cumsum(out$incProbs[order(out[,eventCharCol])]) #cumulative some of incremental probabilities
    #normalizing to ensure adds to 100
    out$percentile <- out$percentile/max(out$percentile)*100
  }else{
    #normalizing to ensure adds to 100
    out$percentile <- rank(out[,eventCharCol])/nrow(out)*100
  }
  
  
  #Applying mapping of water year categories
  for( k in 1:nrow(wyCats)){
    if(k==1){
      out$category[ out$percentile <= wyCats$cumP[k] ] <- wyCats$category[k]
    }else if(k==nrow(wyCats)){
      out$category[ out$percentile <= wyCats$cumP[k] &
                      out$percentile > wyCats$cumP[k-1]  ] <- wyCats$category[k]
    }else{
      out$category[ out$percentile > wyCats$cumP[k-1]] <- wyCats$category[k]
    }
  }
  return(out)
}

characterizeTS <- function(tsDF,eventCharDF,eventCharCol,wyCats){
  #Wrapper function that computes a summary hydrograph broken out into the various water year categories
  #  (e.g., "dry","average","wet")
  #Characterizes an dataset using 
  #  1) eventCharDF: a paired dataframe of values for characterizing events
  #  2) eventCharCol: the column in the paired dataframe to use for characterization, and 
  #  3) wyCats: a dataframe defining water year characterizations (list element of 'wyRankings')
  #Required columns on 'tsDF':
  # event - numeric integer
  # date  - interprettable by lubridate month and day functions
  # value - value of the time series (e.g., flow or elev as float)
  #
  #Example output: for 5 groupings of 20%; 
  #  list names are category names as defined in wyRanking[[<grouping>]]$category
  #   See r_setup.R script for definition of these groupings
  # > str(sumHydro)
  # List of 5
  # $ 60-80%:'data.frame':	365 obs. of  7 variables:
  # ..$ day   : int [1:365] 1 2 3 4 5 6 7 8 9 10 ...
  # ..$ month : num [1:365] 10 10 10 10 10 10 10 10 10 10 ...
  # ..$ wyCat : chr [1:365] "60-80%" "60-80%" "60-80%" "60-80%" ...
  # ..$ min   : num [1:365] 6925 7733 7609 7559 6823 ...
  # ..$ mean  : num [1:365] 14640 14208 13899 12714 13068 ...
  # ..$ max   : num [1:365] 38864 51750 55566 39648 36440 ...
  # ..$ wyDate: Date[1:365], format: "3000-10-01" "3000-10-02" "3000-10-03" "3000-10-04" ...
  # ..- attr(*, "na.action")=Class 'omit'  Named int 338
  # .. .. ..- attr(*, "names")= chr "338"
  # $ 40-60%:'data.frame':	365 obs. of  7 variables:
  # ..$ day   : int [1:365] 1 2 3 4 5 6 7 8 9 10 ...
  # ..$ month : num [1:365] 10 10 10 10 10 10 10 10 10 10 ...
  # ... etc
  require(lubridate)
  nEvents <- length(unique(tsDF$event))
  
  #Assign a new 'category' column in 'eventCharDF' indicating wy 
  #  statistical categorization for each year
  eventCharDF <- characterizeWYs(eventCharDF,eventCharCol, wyCats)
  
  #Mapping category to time series by event
  tsDF$wyCat <- eventCharDF$category[match(x = tsDF$event, table = eventCharDF$event)]
  tsDF$month <- month(tsDF$date) #preparing times by month and day
  tsDF$day <- day(tsDF$date)
  tsDF <- na.omit(tsDF) #remove uncategorized values
  sumHydro <- list() #Initializing output list
  
  #assigning water year summary time series to each element output list by category
  for(wyCat in unique(tsDF$wyCat)){
    sumHydro[[wyCat]] <- computeSummaryHydrographByWYCat(wyDF = tsDF[tsDF$wyCat == wyCat,])
    sumHydro[[wyCat]] <- convertDFToSameWY(inDF = sumHydro[[wyCat]])
  }
  return(sumHydro)
}




### Base Year #############################################


BaseYear_Deterministic <- function(pathData,plotInfo,computeType){
  #Subset by specified base year and add a wyDate column
  baseYrData <- convertDFToSameWY(pathData[wateryear(pathData$date)%in%plotInfo$baseYr,])
  split(baseYrData,wateryear(baseYrData$date))
}

BaseYear_FRA5k <- function(pathData,plotInfo,computeType){
  #Pull all events associated with this base year and compute
  #  the min, median, and max
  subeventYr <- eventYr$fiveK #use 5k event-year pairings
  associatedEvents <- subeventYr$event[subeventYr$base_year %in% plotInfo$baseYr]
  #subset by events in base year
  baseYrData <- pathData[pathData$event %in% associatedEvents,]
  
  #splitting by base year and computing summary statistics
  baseYrData$baseYr <- subeventYr$base_year[match(baseYrData$event,subeventYr$event)]
  baseYrData <- split(baseYrData,baseYrData$baseYr)
  #compute min, median, and max by day
  Map(computeSummaryHydrograph,baseYrData,metrics=list(c("min","median","max")))
}

BaseYear_FRA50k <- function(pathData,plotInfo,computeType){
  #Pull all events associated with this base year and compute
  #  the min, median, and max
  subeventYr <- eventYr$fiftyK #use 50k event-year pairings
  associatedEvents <- subeventYr$event[subeventYr$base_year %in% plotInfo$baseYr]
  #subset by events in base year
  baseYrData <- pathData[pathData$event %in% associatedEvents,]
  
  #splitting by base year and computing summary statistics
  baseYrData$baseYr <- subeventYr$base_year[match(baseYrData$event,subeventYr$event)]
  baseYrData <- split(baseYrData,baseYrData$baseYr)
  #compute min, median, and max by day
  Map(computeSummaryHydrograph,baseYrData,metrics=list(c("min","median","max")))
}


### Frequency #############################################


subsetByTW <- function(pathData, tw){
  #Expected options for tw argument:
  #c("Annual",
  #"Spring (01Nov-30Apr)","Winter (01May-31Aug)",
  #"31Jan","28Feb","31Mar","10Apr", "30Apr","31May","30Jun","31Jul","31Aug","30Sep","31Oct","30Nov","31Dec",
  #month.abb)
  #
  winterMonths <- c(11:12,1:4) #Nov-Apr
  springMonths <- 5:8          #May-Aug
  
  if(tw %in% month.abb){ #tw = "Jan"-"Dec"
    #use abbreviated month in 'tw' against month function applied to date,
    #  using labe=T to return abbreviated month, and converting to character
    #  since returns factor for some reason?
    matchMonth <- which(month.abb == right(tw,3)) #extract numeric month
    return(pathData[month(pathData$date) == matchMonth,])
  }else if(grepl("Spring",tw)){ #tw = "Winter (01May-31Aug)"
    #nov-mar
    return(pathData[month(pathData$date) %in% springMonths,])
  }else if(grepl("Winter",tw)){ #tw = "Spring (01Nov-30Apr)"
    #apr-aug
    return(pathData[month(pathData$date) %in% winterMonths,])
  }else if(str_detect(tw,"\\d{2}[:alpha:]{3}")){ #tw is of format ddmmm. e.g., "30Apr"
    matchMonth <- which(month.abb == right(tw,3)) #extract numeric month
    matchDay <- left(tw,2)
    return(pathData[month(pathData$date) == matchMonth & day(pathData$date) == matchDay,])
  }else{ #assuming tw = "Annual"
    return(pathData)
  }
  
}

Frequency_Deterministic <- function(pathData,plotInfo,computeType){
  #Inputs from GUI are: freqTW, freqMetric (named elements in plotInfo)
  
  fName <- plotInfo$freqMetric
  
  out <- list()
  
  for( tw in plotInfo$freqTW  ){
    
    #subset by time window
    subPathData <- subsetByTW(pathData, tw)
    
    #Apply metric to results by water year
    metricData <- ddply(subPathData,.(event),applyNamedFunctions,fNames=fName)
    
    #append incremental probabilities and order lowest to highest
    metricData$p <- weibullProbs(Qs = metricData[,fName], exceedance = F)
    metricData$q <- qnorm(metricData$p)
    metricData <- metricData[order(metricData$p, decreasing=F),]
    
    #rename column named after 'fName' to 'y' to simplify plotly call
    names(metricData)[names(metricData) == fName ] <- "y"
    
    out[[tw]] <- metricData
  }
  return(out)
}

Frequency_FRA5k <- function(pathData,plotInfo,computeType){
  #Inputs from GUI are: freqTW, freqMetric (named elements in plotInfo)
  
  fName <- plotInfo$freqMetric
  
  out <- list()
  
  for( tw in plotInfo$freqTW  ){
    #subset by time window
    subPathData <- subsetByTW(pathData, tw)
    
    #Apply metric to results by event
    metricData <- ddply(subPathData,.(event),applyNamedFunctions,fNames=fName)
    
    #append incremental probabilities and order lowest to highest
    metricData$p <- weibullProbs(Qs = metricData[,fName], exceedance = F)
    metricData$q <- qnorm(metricData$p)
    metricData <- metricData[order(metricData$p, decreasing=F),]
    
    #rename column named after 'fName' to 'y' to simplify plotly call
    names(metricData)[names(metricData) == fName ] <- "y"
    
    out[[tw]] <- metricData
  }
  
  return(out)
}

Frequency_FRA50k <- 
  Frequency_FRA5k

### Duration ##############################################

Duration_Deterministic <- function(pathData,plotInfo,computeType){
  #anticipated inputs as durTW:
  # c("Annual","Spring (01Nov-30Apr)","Winter (01May-31Aug)",
  #   "31Jan","28Feb","31Mar","10Apr", "30Apr","31May","30Jun","31Jul","31Aug","30Sep","31Oct","30Nov","31Dec",
  #   month.abb)
  
  out <- list()
  
  for(tw in  plotInfo$durTW ){
    
    #subset by time window
    subPathData <- subsetByTW(pathData, tw)
    
    #rank decreasingly, compute incProbs and cumulative probability
    metricData <- subPathData
    metricData$incProbs <- 1/(nrow(metricData)-1)
    metricData <- metricData[order(metricData$value,decreasing = T),]
    
    #compute cumulative probabilities
    metricData$ace <- cumsum(metricData$incProbs)
    
    #If there are a lot of points, this will slow down plotting time
    #Reduce to just 1000 points using stride method
    if( nrow(metricData) > 1000 )
      metricData <- simplifyLine(df = metricData, simplifyCol = "ace",maxPts = 1000)
    
    out[[tw]] <- metricData
    
  }
  
  return(out)
}


#same function for all 
Duration_FRA50k <- 
  Duration_FRA5k <-
  Duration_Deterministic

### Metric vs Forecast ####################################

MetricvsForecast_Deterministic <- function(pathData,plotInfo,computeType){
  #Pair metricData with the project's forecast data
  #Arguments in plotInfo relavent to this compute type:
  # mFreqTW, mFreqMetric, mFcstProj, mFcstDate
  #output is a dataframe with columns for the forecast ('fcst')
  #  and the metric ('y')
  
  fName <- plotInfo$mFreqMetric
  out <- list()
  for(tw in  plotInfo$mFreqTW ){
    
    #subset by time window
    subPathData <- subsetByTW(pathData, tw)
    
    #Apply metric to results by event
    metricData <- ddply(subPathData,.(event),applyNamedFunctions,fNames=fName)
    
    #Pair with appropriate forecast
    fcstData <- fcsts$`80yr`[[plotInfo$mFcstProj]][, c("event",plotInfo$mFcstDate)]
    metricData <- merge(x = metricData, y = fcstData, by.x = "event",by.y = "event",all.x=T)
    
    metricData$baseYr <- metricData$event
    
    #rename column named after 'fName' to 'y' and 'mFcstDate' to 'fcst',
    #  simplifying plotly call
    names(metricData)[names(metricData) == fName ] <- "y"
    names(metricData)[names(metricData) == plotInfo$mFcstDate ] <- "fcst"
    
    out[[tw]] <- metricData
  }
  return(out)
}

MetricvsForecast_FRA5k <- function(pathData,plotInfo,computeType){
  #Pair metricData with the project's forecast data
  #Arguments in plotInfo relavent to this compute type:
  # mFreqTW, mFreqMetric, mFcstProj, mFcstDate
  #output is a dataframe with columns for the forecast ('fcst')
  #  and the metric ('y')
  
  fName <- plotInfo$mFreqMetric
  
  out <- list()
  for(tw in  plotInfo$mFreqTW ){
    
    #subset by time window
    subPathData <- subsetByTW(pathData, plotInfo$mFreqTW)
    
    #Apply metric to results by event
    metricData <- ddply(subPathData,.(event),applyNamedFunctions,fNames=fName)
    
    #Pair with appropriate forecast
    fcstData <- fcsts$fiveK[[plotInfo$mFcstProj]][, c("event",plotInfo$mFcstDate)]
    metricData <- merge(x = metricData, y = fcstData, by.x = "event",by.y = "event",all.x=T)
    
    metricData$baseYr <- eventYr$fiveK$base_year[match(metricData$event,eventYr$fiveK$event)]
    
    #rename column named after 'fName' to 'y' and 'mFcstDate' to 'fcst',
    #  simplifying plotly call
    names(metricData)[names(metricData) == fName ] <- "y"
    names(metricData)[names(metricData) == plotInfo$mFcstDate ] <- "fcst"
    
    out[[tw]] <- metricData
  }
  
  return(out)
}


MetricvsForecast_FRA50k <- function(pathData,plotInfo,computeType){
  #Pair metricData with the project's forecast data
  #Arguments in plotInfo relavent to this compute type:
  # mFreqTW, mFreqMetric, mFcstProj, mFcstDate
  #output is a dataframe with columns for the forecast ('fcst')
  #  and the metric ('y')
  
  fName <- plotInfo$mFreqMetric
  
  out <- list()
  for(tw in  plotInfo$mFreqTW ){
    
    #subset by time window
    subPathData <- subsetByTW(pathData, plotInfo$mFreqTW)
    
    #Apply metric to results by event
    metricData <- ddply(subPathData,.(event),applyNamedFunctions,fNames=fName)
    
    #Pair with appropriate forecast
    fcstData <- fcsts$fiftyK[[plotInfo$mFcstProj]][, c("event",plotInfo$mFcstDate)]
    metricData <- merge(x = metricData, y = fcstData, by.x = "event",by.y = "event",all.x=T)
    
    metricData$baseYr <- eventYr$fiveK$base_year[match(metricData$event,eventYr$fiveK$event)]
    
    #rename column named after 'fName' to 'y' and 'mFcstDate' to 'fcst',
    #  simplifying plotly call
    names(metricData)[names(metricData) == fName ] <- "y"
    names(metricData)[names(metricData) == plotInfo$mFcstDate ] <- "fcst"
    
    out[[tw]] <- metricData
  }
  return(out)
}

### Cost Computation #############################


computePrices <- function(energyDF, mp, computeType){
  #mp is the list of prices for the 80 year, FRA5k, and FRA50k runs
  #
  
  # #For testing 
  # computeType="FRA5k"
  # energyDF <- readSQLTbls(file_path = "Y:\\CRT\\CoordPowMF_woOnCall\\2019-02-01\\sql\\CoordPowMF (2019-02-01).sql",
  #                         paths = "//ARROW LAKES-POWER PLANT/ENERGY//1DAY/FLOODMODEL1/")
  # costs <- energyDF[[1]]    #probably won't need if passing 'energyDF' as dataframe, not list
  
  costs <- energyDF
  #prepping data
  prices <- mp[[computeType]] #extract the market price dataframe from list
  costs$month <- as.character(month(costs$date, label = T )) #create abbreviated month coolumn
  
  #merge by month and event
  costs <- merge(x = costs, y=prices, by.x=c("event","month"), by.y=c("Event","month"), all.x = T)
  costs$value <- costs$value.x*costs$value.y #computing the costs <MWh>*<$/MWh> -> $
  costs[, c("month","value.x","value.y")] <- NULL #removing now-extraneous columns
  
  #Correction for spinup time of models.  Set Oct01-Oct05 prices to zero
  costs$value[month(costs$date) == 10 & day(costs$date) %in% 1:5] <- 0
  
  #Ordering by event and date, so cumsum function works properly
  costs <- costs[with(costs,order(event, date)),]
  
  return(costs)
}

computeCumulativePrices <- function(energyDF){
  #cumulative sum by event - should work for both deterministic (event=wy) and FRA
  out <- energyDF %>% group_by(event) %>% mutate(value=cumsum(value))
  #stripping attributes
  outAttr <- attributes(out)
  for(nullAttr in names(outAttr)[names(outAttr) %!in% c("names", "class","row.names")]) attr(out, nullAttr) <- NULL
  return(out)
}

computePrices_fileWrapper <- function(file_path, allData=NULL){
  #Computes on allData object
  
  connIndex <- which(file_path==conns$file_path) #index for current file in 'conns'
  
  #Point to an SQL/DSS file
  alt <- conns$alt[connIndex]
  file_path <- conns$file_path[connIndex]
  computeType <- conns$compute_type[connIndex]
  connection_name <- conns$connection_name[connIndex]
  
  #search for /ENERGY/ paths
  ePaths <- pathConfig$allPaths[[connection_name]]$paths
  ePaths <- ePaths[grepl("/ENERGY/",ePaths)]
  if(length(ePaths)==0) return(allData) #skip if none
  
  #load energy data
  eData <- updateLoadedData(selectedAlts=alt, selectedPaths = ePaths, allData=allData,showProgress=F)
  
  #map 'computePrices' function to each loaded dataframe
  priceData <- Map(computePrices,eData$data[[alt]],list(mp), list(computeType))
  
  #renaming list elements from '/ENERGY/' to '/COST/'
  names(priceData) <- gsub("/ENERGY/","/COST/", names(priceData))
  
  #map 'computeCumulativePrices' function to each dataframe in 'priceData'
  cumPriceData <- Map(computeCumulativePrices,priceData)
  
  #renaming list elements from '/COST/' to '/COST-CUM/'
  names(cumPriceData) <- gsub("/COST/","/COST-CUM/", names(cumPriceData))
  
  #Appending to allData
  allData$data[[alt]] <- c(allData$data[[alt]], priceData, cumPriceData)
  
  return(allData)
}

