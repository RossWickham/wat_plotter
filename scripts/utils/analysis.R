

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










