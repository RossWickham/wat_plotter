### Saving ########


getSaveFileName <- function(ext){
  #Retrieves the file name to save to, using the appropriate extension
  saveFileName <- file.choose()
  
  if(!exists(saveFileName)) return(NULL)
  
  
  #concatenate extension if it isn't on the file
  if(right(saveFileName,nchar(ext)) != ext ) saveFileName <- cat(saveFileName,ext)
  return(saveFileName)
}


saveDataWrapper <- function(file_path, allData){
  #Updates the global object that stores the currently desired plot data ('allData')
  # Uses 'cPathConfig' to determine what needs to be updated
  #allData is a nested list by alternative that has both data and
  #  metadata info
  
  #Check if the data has been created yet.  If not, create
  # if( "data" %!in%  names(allData)){
  #   cat("\nInitializing time series data storage in memory")
  #   allData$data <- list()
  # }
  
  connIndex <- which(file_path == conns$file_path)
  
  connection_name <- conns$connection_name[connIndex]
  alt <- conns$alt[connIndex]
  file_format <- conns$file_format[connIndex]
  
  
  allDataPaths <- names(allData$data[[alt]])                 #paths in allData for current alt
  connPaths <- pathConfig$allPaths[[connection_name]]$paths  #connection path names
  
  #Get which paths need to be saved  Skip if none
  unSavedPaths <- allDataPaths[ allDataPaths %!in% connPaths ]
  if( length(unSavedPaths)==0 ){
    cat(sprintf("\nNo new data to save for %s", file_path))
    return(NULL)
  }
  
  if( file_format == "SQL" ){
    #Save to SQL if tables not yet created
    save_SQL( file_path=file_path, dfList=allData$data[[alt]] )
    
  }else if( file_format == "DSS" ){
    save_DSS( file_path=file_path, dfList=allData$data[[alt]] )
  }
  
} 


### _HTML ######################


### _Excel #################

loadXLTables <- function(xlFileName){
  #Loads the Qout - tailwater elevation rating 
  #  tables as defined in the 'qout_twelev.xlsx' Excel
  require(XLConnect)
  wb <- loadWorkbook(xlFileName)
  allSheets <- getSheets(wb)
  out <- lapply(X = allSheets, function(x) readWorksheet(wb,x))
  names(out) <- allSheets
  return(out)
}

convertDatesToChar <- function(saveDF){
  #converts dates to character prior to save, so Excel doesn't interpret the wrong
  saveDF[,names(saveDF) %in% c("wyDate")] <- as.character(saveDF[,names(saveDF) %in% c("wyDate")])
  return(saveDF)
}

### TODO #######
#Make Excel function for each compute_type - use copyFun if needed

saveExcel <- function(allData, plotData, opts,showProgress=F){
  #Iterate thoguh each plot, and save to Excel
  
  
  
  saveFileName <- opts$saveFileName
  if(saveFileName=="") saveFileName <- "test.xlsx"
  cat(sprintf("\nSaving Excel to:\t %s", saveFileName))
  saveFileName <- sprintf("%s/test_output/%s",getwd(),saveFileName)
  
  wb <- loadWorkbook(saveFileName,create=T)
  
  createSheet(wb,"all_data")
  
  #Increment to increase load bar
  #1/(<no. plots>*<no. alts>) : ignoring number of paths
  incPerPath <- 1/(length(plotData$meta)*length(opts$selectedAlts)*2)

  startCol <- 1 #iniitalizing start column
  for( k in 1:length(plotData$meta) ){   #iterate through each plot
    plotInfo <- plotData$meta[[k]]
    
    for(alt in opts$selectedAlts ){ #  and each alternative
      
      plotType <- plotInfo$plotType
      computeType <- allData$meta[[alt]]$compute_type
      
      # #call function by name onto the dataset, returns dataframe
      # plotFunName <- gsub(" ","",sprintf("plotly_%s_%s", plotType,computeType))
      # 
      # cat(sprintf("\nplotFunName:\t%s",plotFunName))
      # if( !exists(plotFunName) ){
      #   cat(sprintf("\nPlotly function for plot type '%s' and compute type '%s' does not yet exist, skipping",
      #               plotType, computeType))
      #   next
      # }
      # 
      
      
      
      #pull out the path data
      pData <- plotData$data[[plotInfo$plotType]][[plotInfo$paths[1]]][[alt]]$data
      
      if( is.list(pData) ){
        
        for( pDataName in names(pData) ){
          #write header info - alt, computeType, plotType, path
          writeWorksheet(object = wb, data = data.frame(x=c(alt,computeType,plotType,plotInfo$paths[1],pDataName)),
                         sheet = "all_data",startRow = 1, startCol = startCol, header = F, rownames = F)
          
          #write data to Excel sheet
          writeWorksheet(object = wb, data = convertDatesToChar(pData[[pDataName]]),
                         sheet = "all_data",startRow = 8, startCol = startCol, header = T, rownames = F)
          #advance start column
          startCol <- startCol + ncol(pData[[pDataName]]) + 2
        }
        
       
        
      }else{
        
        #write header info - alt, computeType, plotType, path
        writeWorksheet(object = wb, data = data.frame(x=c(alt,computeType,plotType,plotInfo$paths[1])),
                       sheet = "all_data",startRow = 1, startCol = startCol, header = F, rownames = F)
        
        #write data to Excel sheet
        writeWorksheet(object = wb, data = convertDatesToChar(pData),
                       sheet = "all_data",startRow = 8, startCol = startCol, header = T, rownames = F)
        #advance start column
        startCol <- startCol + ncol(pData) + 2
        
      }
      

      

      
      #Functions defined under associated section in 'plot_plotly' script
      # pList[[k]] <- eval(call(name =plotFunName,pList[[k]],plotData,alt,plotInfo,init))
      # pList[[k]] <- addXaxisLayout(pList[[k]],plotInfo)
      
      
      # if(showProgress) incProgress(incPerPath)
    }
    init <- T
  }
  
  saveWorkbook(wb)
  # return(
  #   plotly::subplot(pList,nrows = length(pList),
  #                   shareX = T, titleY = T, widths = 1)
  # )
  # }
}


### DSS - General ##############

getAllPathsFromFile <- function(dssFileName){
  require(dssrip)
  dssFile <- opendss(dssFileName)
  allPaths <- getPaths(dssFile)
  dssFile$done()
  return(allPaths)
}


### DSS -Save ####################


save_DSS <- function(file_path, dfList){
  cat("\nFunction to save to DSS is not yet completed")
}


### DSS - PDC ##################

## used to help with introspection on Java Objects
sigConversions = list(boolean="Z", byte="B", char="C", 
                      short="T", void="V", int="I", 
                      long="J", float="F", double="D")

fieldsDF <- function(jObject){ 
  #Creates a dataframe containing the field information for
  #  a Java object.
  require(plyr) 
  fields = ldply(.jfields(jObject), function(x) data.frame(CLASS=x,  
                                                           stringsAsFactors=FALSE)) 
  fields$SIGNATURE = llply(fields$CLASS, function(x){ 
    out = str_replace_all(x, "\\[\\]", "") 
    if(out %in% names(sigConversions)){ 
      out = sigConversions[[out]] 
    } else { 
      out = paste0("L", str_replace_all(out, fixed("."), "/"), ";") 
    } 
    ## If vector, add [ 
    if(grepl(fixed("\\[\\]"), x)){ 
      out = paste0("[", out) 
    } 
    return(out) 
  })
  
  #Extracting the name of the elements (last word, separated by period sign)
  fields$SHORTNAME = substring(text = fields$CLASS, 
                               first = unlist(lapply(gregexpr(pattern = "\\.", text = fields$CLASS),FUN=function(xFun) xFun[length(xFun)]))+1,
                               last=nchar(fields$CLASS))
  
  #Extracting the data type(e.g, double, integer,... second word in fullname)
  fields$DATATYPE = substring(text = fields$CLASS, 
                              first = unlist(lapply(gregexpr(pattern = " ", text = fields$CLASS),FUN=function(xFun) xFun[1]))+1,
                              last=unlist(lapply(gregexpr(pattern = " ", text = fields$CLASS),FUN=function(xFun) xFun[2]))-1)
  return(fields) 
}


makePairedDataContainer <-
  function (x,y=x, 
            fileName=paste0("Paired Data ",gsub(":","_",Sys.time()),".dss"),
            makeFile = T,
            APart="", BPart="", CPart = "",DPart = "",EPart = "", FPart="",
            xUnits="NO UNITS",yUnits="NO UNITS",
            yLabels = NULL, #Additional y labels
            xDataType="",yDataType="", #e.g., unitary or LOG
            xParameterName="x", yParameterName="y",...)
  {
    
    #This code will generate a paired time series in the DSS format
    #By default, if a file name is provided the function will save 
    #  to the current working directory with a timestamp in file name.
    #  A directory path may also be provided to fileName to save to
    #  a different directory. Otherwise, files would be saved to the 
    #  Java working directory.
    #At a minimum, x needs to be supplied.  If y is not supplied, y=x.
    
    #Variable descriptions:
    # x                    (numeric) vector containing the  x ordinate values
    # y                    (numeric) vector, matrix, or dataframe of paired data
    # fileName             (char) string containing file name or folder directory
    # makeFile              (logical) indicates whether a DSS file should be created
    # APart,BPart,CPart     (char, len=1) DSS path parts
    # xUnits,yUnits         (char, len=1) Units associated with x and y ordinates
    # yLabels                (char) The labels for y ordinates provided
    # xDataType,yDataTypes  (char) The type of the x ordinate (e.g., "LOG" or "UNT" for unitary)
    #                         These show up in the table
    # xParameterName,
    # yParameterName        (char) The name on the plot
    # ...                   Additional arguments, see below
    
    #For additional arguments that can be supplied to this function, enter the following into 
    #  the Jython.exe API:
    #from hec.io import *
    #pdc = PairedDataContainer()
    #for i in dir(pdc): print i
    
    #Author:  Ross Wickham, USACE
    #Date:    12/2/2016
    
    #Checking if fileName doesn't contain the current working directory and the fileName isn't a folder reference
    if( length(grep(pattern = getwd(),x = fileName))==0 &  #TRUE if the working directory is not in fileName
        gregexpr("/",text = fileName)[[1]][1] == -1 &      #TRUE if there are no forward slashes in fileName
        gregexpr("\\\\",text = fileName)[[1]][1] == -1){      #TRUE if there are no "\\" slashes in fileName
      fileName <- paste0(getwd(),"/",fileName)
      print(paste0("Saving the file to current working directory in R session (",getwd(),"); no directory specified in fileName argument."), quote=F)
    }
    
    #Checking if dssrip is installed
    if(!("dssrip" %in% installed.packages())){
      stop("The package dssrip has not been installed.  
           See installation instructions at https://github.com/eheisman/dssrip (link working as of 12/1/16)")
    }
    library(dssrip)
    
    #Obtaining additional arguments provided and checking
    additionalPars = list(...)
    
    if(any(names(additionalPars) == "supplementalInfo")) 
      warning("This version of the makePairedDataContainer function 
              does not support assigning the supplemental DSS Information.")
    if((exists("labels") & class(labels) != "function") | exists("xOrdinates") | exists("yOrdinates")) 
      warning("The argument for labels is replaced by yLabels, the argument for xOrdinate replaced by x, and the argument for yOrdinate is replaced by y.
              Arguments passed through variables 'labels', xOrdinate, or yOrdinate will not be assigned.")
    
    
    #Creating the Paired Data Container
    pdc = .jnew("hec/io/PairedDataContainer")
    
    pdc$watershed = APart
    pdc$location = BPart 
    pdc$version = FPart #version
    pdc$numberOrdinates = length(x)  #Number of data values
    
    pdc$labelsUsed = F   #Handling y labels if supplied
    if(!is.null(yLabels) & length(yLabels) > 1){
      pdc$labels = yLabels
      pdc$labelsUsed = T
    }
    
    if(length(yLabels) == 1) warning("The length of yLabels is equal to one. DSS 2.0 cannot add a label when there is only one y-ordinate.")
    
    #Assigning DSS pathname
    pdc$fullName = sprintf("/%s/%s/%s/%s/%s/%s/", APart,BPart,CPart,DPart,EPart,FPart)
    
    if(any(class(x) %in% c("integer","numeric"))){
      pdc$xOrdinates = .jarray(as.numeric(x), contents.class = "java/lang/Double")
    }else stop("x must be coercable to a numeric value.")
    
    #Creating a list of Java references for y ordinates
    if(is.null(y)) stop("Must supply numeric values for y or not pass as argument (default is y=x).")
    
    if(!is.null(yLabels) & length(y) != length(yLabels)) stop(paste0("Mismatch in length between y labels (",length(yLabels),") and number of columns in y (",length(y),")."))
    y <- try(as.data.frame(y), silent=T) #coercing to dataframe
    
    if(class(y) == "try-error") stop("y needs to be coercable into a dataframe.")
    
    if(length(y) > 50) stop("y cannot have more than 50 columns, this is a restriction imposed by DSS 2.0 for paired data containers.")
    
    #Number of y ordinates (number of time series in y)
    pdc$numberCurves = length(y)
    
    #Checking that all y columns are coercible to a numeric
    yColumnsNotNumber <- which(!(sapply(y,class) %in% c("numeric","integer")))
    if(length(yColumnsNotNumber) > 0) 
      stop(paste0("All y values must be coercible to a dataframe.  Columns with issues: ",paste(names(y[yColumnsNotNumber]),collapse=", ")))
    
    #Assigning each column in y to a java double array, and storing their java references in a list
    yAssign <- list()
    for(i in 1:length(y)){
      yAssign[[i]] <- .jarray(as.numeric(y[,i], contents.class = "java/lang/Double"))
    }
    
    pdc$yOrdinates = .jarray(unlist(yAssign), "[D")
    
    pdc$xunits = xUnits
    pdc$yunits = yUnits
    
    pdc$xtype = xDataType
    pdc$ytype = yDataType
    
    pdc$xparameter = xParameterName
    pdc$yparameter = yParameterName
    
    pdcFieldsDF <- fieldsDF(pdc)
    for (n in names(additionalPars)){
      
      if(!(n %in% pdcFieldsDF$SHORTNAME)){
        warning(paste0("The variable named ",n," passed to this function was not assigned because it is not an element of the
                       paired data container in DSS 2.0.  Check spelling and/or what what arguments can be passed to this function."))
        print("Arguments that can be supplied to this function, and corresponding data types:")
        print(pdcFieldsDF[,c("CLASS","SHORTNAME","DATATYPE")], quote=F)
        next}
      
      writeVal = additionalPars[[n]]
      
      if (is.na(writeVal) | writeVal == ""){warning(paste0("Insufficient data to assign value for ",n)); next}
      
      dataType <- pdcFieldsDF$DATATYPE[pdcFieldsDF$SHORTNAME == n]
      
      #Assigning to the correct location by data type.  Possible data types 
      #  for paired data containers include:
      #int, double, boolean, java.lang.String, 
      #double[] (xOrdinate), double[][] (yOrdinate), and java.lang.String[] (labels)
      #xOrdinate, yOrdinate, and labels are handled separately
      if(dataType == "int") .jfield(o = pdc, name = n) = as.integer(writeVal)
      if(dataType == "double") .jfield(o = pdc, name = n) = as.double(writeVal)
      if(dataType == "boolean") .jfield(o = pdc, name = n) = as.logical(writeVal)
      if(dataType == "java.lang.String") .jfield(o = pdc, name = n) = as.character(writeVal)
    }
    
    if(!is.null(fileName) & makeFile){
      
      #Deleting .dsc file if it exists
      if(file.exists(gsub(".dss",".dsc",fileName))) file.remove(gsub(".dss",".dsc",fileName))
      
      dssFileNew <- dssrip::opendss(filename=fileName, warnIfNew = F)
      dssFileNew$put(pdc)
      dssFileNew$close()
    }
    
    return(pdc)
  } #End function makePairedDataContainer




loadPDCtoDF_wildcardPath <- function(in_path, dssFileName) {
  #wrapper to read in paired data container as a dataframe with wildcard interpretation
  #  of the input DSS file
  
  dssFile <- opendss(dssFileName)
  allPaths <- getAllPaths(dssFile)
  
  selectedPaths <-fullPathByRegex(paths = allPaths, pattern = glob2rx_plus(in_path))
  if(length(selectedPaths) > 1)
    stop(sprintf( paste0("\n\tloadPDCtoDF_wildcardPath:\tNon-unique",
                         " pathname found when searching:\t%s\nDSS path matches:\n\t%s"),
                  in_path, paste0(selectedPaths, collapse="\n\t")))
  if(length(selectedPaths) == 0)
    stop(sprintf("\nNo unique matches found for wildcard DSS path:\t%s", in_path))
  
  pdcToDF(loadPDC(selectedPaths, dssFile))
}


#wrapper to read in paired data container as a dataframe
loadPDCtoDF <- function(in_path, dssFile) pdcToDF(loadPDC(in_path, dssFile))

loadPDC <- function(in_path, dssFile){
  #loads an individual paired data container with error handling
  out <- try(dssFile$get(in_path),silent = T)
  if(class(out) == "try-error"){
    return(NA)
  }else{
    return(out)
  }
}

pdcToDF <- function(pdc){
  #Input is a paired data contained from DSS as java object
  #Output is dataframe, preserving names of the pdc
  y <- pdc$yOrdinates
  out <- data.frame(x = pdc$xOrdinates, as.data.frame(t(y)))
  names(out) <- c("x",pdc$labels)
  return(out)
}

pdcToTS <- function(in_path, eventYr,dssFileName){
  #Converts a paired data container to a time series
  #  using the event-year pairings, like you'd want for URC data
  require(reshape2)
  dssFile <- opendss(dssFileName)
  pdcDF <- pdcToDF(loadPDC(in_path,dssFile))
  dssFile$done()
  #melting to a long dataframe
  meltDF <- melt(pdcDF,id.vars = "x", measure.vars = names(pdcDF)[names(pdcDF)!="x"])
  #scrubbing 'dates ' from column name, expecting this formatting from FRA extract process
  meltDF$variable <- gsub("dates |date ","",as.character(meltDF$variable))
  #getting base water years, creating date
  meltDF$wy <- eventYr$base_year[match(meltDF$x, eventYr$event)]
  meltDF$date <- as.Date(sprintf("%s%s",meltDF$wy,meltDF$variable),format="%Y%d%b")
  #adjusting for water year
  year(meltDF$date)[month(meltDF$date) >=10] <- year(meltDF$date)[month(meltDF$date) >=10] - 1
  
  meltDF <- meltDF[order(meltDF$x, meltDF$date),] #ordering
  
  meltDF <-  meltDF[,c("x","date","value")] #formatting to std time series dataframe
  names(meltDF) <- c("event","date","value")
  return(meltDF)
}

### DSS -TSC ##############################





loadMergedFRADSS <- function( in_paths, dssFileName, compute_type  ){
  #Loads from a merged FRA DSS file, where all data are in one 
  #  DSS file, given the fully qualified path of the file name
  #  e.g., like gatherpaths-extracted
  
  cat(sprintf("\nLoading path(s):\n\t%s",paste0(in_paths,collapse="\n\t")))
  
  #Iterate through paths
  if( toupper(compute_type)=="DETERMINISTIC"){
    pathRegExs <- formQAMetricRegEx(in_paths)
  }else if( toupper(compute_type)=="FRA"){
    pathRegExs <- formFRARegEx(in_paths)
  }else if(toupper(compute_type)=="POR"){
    #Form path regular expression from gatherpaths-like format
    pathRegExs <- formPORPathRegEx(in_paths) #only difference from POR load
  }
  tscList <- lapply(pathRegExs, extractTSCbyRegEx,dssFileName=dssFileName)
  
  #formatting to generic data format used in tool 
  nonNullElement <- sapply(tscList, function(x) !is.null(x))
  if( all(!nonNullElement) ){
    warning(paste0("\nNo data read for provided path expressions in DSS file (%s):\n\t%s",
                   dssFileName, paste0(in_paths,collapse="\n\t")))
    return(NULL)
  }
  
  if( str_detect(tscList[[which(nonNullElement)[1]]]$F[1],"\\d{6}") ){
    tscList <- lapply(tscList,formatMergedDSS)
  }else{
    tscList <- lapply(tscList,formatDSS)
  }
  
  #rename output to match associated name in in_path
  names(tscList) <- in_paths
  
  return(tscList)
}

formatMergedDSS <- function(df){
  #For gatherpaths DSS
  #input is a dataframe as read from the 'extractTSCbyRegEx' function
  #output is a dataframe with path part columns removed (A-F, abc)
  # and a new event column corresponding to the event
  if(is.null(df)) return(NA)
  if(nrow(df)==0 ) return(NA)
  #Convert to Date if 1DAY timesteps
  # if( diff(df$date[1:2]) == 86400) df$date <- as.Date(df$date)
  df$event <- as.numeric(str_extract_all(df$F,"\\d{6}",T))
  df[,c(LETTERS,"abc")] <- NULL
  df <- df[!duplicated(df$date),]
  return(df)
}

formatDSS <- function(df){
  #For QA Metrics and POR datasets
  #input is a dataframe as read from the 'extractTSCbyRegEx' function
  #output is a dataframe with path part columns removed (A-F, abc)
  # and a new event set to NA
  if(is.null(df)) return(NA)
  if(nrow(df)==0 ) return(NA)
  #Convert to Date if 1DAY timesteps
  # if( diff(df$date[1:2]) == 86400) df$date <- as.Date(df$date)
  df$event <- wateryear(df$date) #Map the year to the event
  df[,c(LETTERS,"abc")] <- NULL
  df <- df[!duplicated(df$date),]
  return(df)
}

formPORPathRegEx <- function(in_paths){
  #For reading the DSS files in the POR folders (e.g., POR 29-47)
  #From the gatherpaths-like format of pathnames
  # (e.g., //MCNARY_OUT/FLOW//1DAY/FLOODMODEL1/)
  # converts to a regular expression, that can 
  #  be used to match against POR DSS file paths
  #Example input: "//MCNARY_OUT/FLOW//1DAY/FLOODMODEL1/"
  #Example output: "//MCNARY_OUT/FLOW/*/1DAY/*F1*/"
  fParts <- getPathParts(in_paths,part="F")
  #prefix and suffix F part with wildcard operators for regular expressions
  newFParts <- paste0("*",sapply(fParts,replaceStringFromDict),"*")
  outPaths <- replacePathParts(in_paths,"f",newFParts) #New F parts
  replacePathParts(outPaths,"d","*")                   #D parts to wildcard
}
#These two are the same
formFRARegEx <- formPORPathRegEx


formQAMetricRegEx <- function(paths){
  #format paths with an asterisk in the D part and
  #  a wildcard operator prefix in the F part (e.g., *FLOODMODEL1)
  out <- replacePathParts(paths = paths,part = "D",replacements = "*")
  replacePathParts(paths = out,part = "F",replacements = paste0("*", getPathParts(out,"F")))
}


compileTSC <- function(paths, dssFile,valueColName="value"){
  #Retrieves all of the paths from the dss file object
  #valueColName is the default column name for the column with data
  #Note:
  # > str(dssFile)
  # Formal class 'jobjRef' [package "rJava"] with 2 slots
  # ..@ jobj  :<externalptr> 
  # ..@ jclass: chr "hec/heclib/dss/HecDss"
  
  pathSplit <- separatePathParts(paths)
  
  lenPaths <- length(paths)
  cat(sprintf("\nReading %g paths from %s",lenPaths,basename(dssFile$getFilename())))
  out <- list()
  for( k in 1:length(paths) ) {
    
    if(k %% 1000 == 0) cat(sprintf("\n[%6g/%6g]", k , lenPaths))
    
    path <- paths[k]
    
    tsc<-try(dssFile$get(path))
    
    if(length(tsc$times)==0) next #skipping if no data
    
    tscTimes <- as.POSIXct(tsc$times * 60, origin = "1899-12-31 00:00", tz = "UTC")
    
    out[[k]] <- data.frame(date = tscTimes,
                           A = pathSplit$A[k],
                           B = pathSplit$B[k],
                           C = pathSplit$C[k],
                           F = pathSplit$F[k],
                           value = tsc$values,
                           stringsAsFactors=F)
  }
  
  out <- bind_rows(out)
  
  names(out)[names(out) == "value"] <- valueColName
  
  return(out)
}



dssDataFromDSSFiles <- function(regExpr, dssFiles){
  #From an individual regular expression, retrieves the
  #  data from each DSS file
  Map(extractTSCbyRegEx, list(regExpr), dssFiles)
}





loadDSSDataFromRegEx <- function(regExprs, dssFiles){
  #wrapper for extractTSCbyRegEx to handle multiple regular expressions
  #  and DSS files.  Note: merges by unique A/B/C parts, so doesn't
  #  handle unique F parts right now, but it could
  #
  #'regExprs' is a string with regular expressions to be used
  #  to extract DSS paths
  #'dssFiles' are fully qualified paths to DSS files to load
  # data from.  Output is a single dataframe with a 'date' column
  #  and one column per regExprs
  require(reshape2)
  #raw read to nested lists of dfs, then merge into list of dfs, then
  rawList <- Map(dssDataFromDSSFiles, list(paste0(regExprs,collapse="|")), list(dssFiles))
  longDF <- bind_rows(lapply(rawList, bind_rows))
  
  #Adding column for the watAlt
  longDF$watAlt <- watAltFromFPart(longDF$F)
  longDF$abcAlt <- sprintf("%s_%s", longDF$abc, longDF$watAlt)
  
  #removing duplicated date/abc combos so dcast works properly
  longDF <- longDF[!duplicated(longDF[, c("date","abcAlt")]),]
  
  #expanding to dataframe
  dcast(data = longDF, formula = "date ~ abcAlt",value.var = "value")
}



#Merges selected paths regular expressions from a DSS file into a
#  long DF with columns for date, DSS path parts, merged A/B/C column,
#  and values
#
#The long DF can be converting to wide dataframe if needed:
# wideDF <- dcast(longDF, date ~ abc, value.var="value")   #if not concerned with F part
# wideDF <- dcast(longDF, date ~ abc+F, value.var="value") #if overlapping dates for some F parts

extractTSCbyRegEx <- function(pathRegEx, dssFileName){
  #dssFileName is fully qualified path to DSS File
  #pathRegEx is a vector of strings with regular expressions
  #  used to extract DSS paths
  
  #Example inputs:
  # dssFileName <- 
  #  "D:\\DSS\\1965W01\\NCS_FullYr_HighPool-1965W01.dss"
  # pathRegEx <- c("/*/*/STAGE/*/*/*/", #pulls all stage data
  #               "/*/*/FLOW/*/*/*/")   #pulls all flow data
  #
  #Example output:
  # > str(longDF)
  # 'data.frame':	3289 obs. of  7 variables:
  # $ date : POSIXct, format: "2007-10-01" "2007-10-02" "2007-10-03" "2007-10-04" ...
  # $ A    : chr  "" "" "" "" ...
  # $ B    : chr  "ALBENI FALLS-POOL" "ALBENI FALLS-POOL" "ALBENI FALLS-POOL" "ALBENI FALLS-POOL" ...
  # $ C    : chr  "ELEV" "ELEV" "ELEV" "ELEV" ...
  # $ F    : chr  "EXTNAA_FC:EOF POR WY:RESSIM-F1NAEFC" "EXTNAA_FC:EOF POR WY:RESSIM-F1NAEFC" "EXTNAA_FC:EOF POR WY:RESSIM-F1NAEFC" "EXTNAA_FC:EOF POR WY:RESSIM-F1NAEFC" ...
  # $ value: num  2064 2064 2064 2064 2064 ...
  # $ abc  : chr  "ALBENI FALLS-POOL__ELEV" "ALBENI FALLS-POOL__ELEV" "ALBENI FALLS-POOL__ELEV" "ALBENI FALLS-POOL__ELEV" ...
  
  require(dssrip)
  require(purrr)
  require(dplyr)
  require(reshape2)
  
  ### Functions ##############
  
  glob2rx_plus <- function (pattern, trim.head = FALSE, trim.tail = TRUE) 
  {
    #This is a special form of the glob2rx function in the utils package
    #  that treats "+" signs as characters.  There are some instances of "+" signs 
    #  in DSS paths, unfortunately
    if (!length(pattern)) 
      return(character())
    
    p <- gsub("\\.", "\\\\.", paste0("^", pattern, "$"))
    p <- gsub("\\?", ".", gsub("\\*", ".*", p))
    p <- gsub("([^\\])\\(", "\\1\\\\(", p)
    p <- gsub("([^\\])\\[", "\\1\\\\[", p)
    p <- gsub("\\+", "\\\\+", p)            #added by RSW
    p <- gsub("([^\\])\\{", "\\1\\\\{", p)
    if (trim.tail) 
      p <- sub("\\.\\*\\$$", "", p)
    if (trim.head) 
      p <- sub("\\^\\.\\*", "", p)
    p
  }
  
  formPaths <- function(a="",b="",c="",d="",e="",f=""){
    #Forms DSS paths from specified character in the form: /A/B/C/D/E/F/
    
    a <- as.character(a)
    b <- as.character(b)
    c <- as.character(c)
    d <- as.character(d)
    e <- as.character(e)
    f <- as.character(f)
    
    #At a minimum, A or B should be specified
    if( all(c(a,b) == "") ) stop("In functiion call: formPaths, need to at least specific an A or B part")
    if( !all(is.character(c(a,b,c,d,e,f))) ) stop("In functiion call: formPaths, all arguments need to be characters or convertible to characters")
    
    return(paste0("/",a,"/",b,"/",c,"/",d,"/",e,"/",f,"/"))
  }
  
  
  compileTSC <- function(paths, dssFile,valueColName="value"){
    #Retrieves all of the paths from the dss file object
    #valueColName is the default column name for the column with data
    #Note:
    # > str(dssFile)
    # Formal class 'jobjRef' [package "rJava"] with 2 slots
    # ..@ jobj  :<externalptr> 
    # ..@ jclass: chr "hec/heclib/dss/HecDss"
    
    pathSplit <- separatePathParts(paths)
    
    lenPaths <- length(paths)
    cat(sprintf("\nReading %g paths from %s",lenPaths,basename(dssFile$getFilename())))
    out <- list()
    for( k in 1:length(paths) ) {
      
      if(k %% 1000 == 0) cat(sprintf("\n[%6g/%6g]", k , lenPaths))
      
      path <- paths[k]
      
      
      tsc<-try(dssFile$get(path))
      
      #skipping if no data or not time series
      if(.jclass(tsc) != "hec.io.TimeSeriesContainer") next
      if(length(tsc$times)==0 ) next 
      
      tscTimes <- as.POSIXct(tsc$times * 60, origin = "1899-12-31 00:00", tz = "UTC")
      
      out[[k]] <- data.frame(date = tscTimes,
                             A = pathSplit$A[k],
                             B = pathSplit$B[k],
                             C = pathSplit$C[k],
                             F = pathSplit$F[k],
                             value = tsc$values,
                             stringsAsFactors=F)
      
      #Correction for daily data
      if(unique(pathSplit$E[k])=="1DAY")
        out[[k]]$date = out[[k]]$date-86400
    }
    
    out <- bind_rows(out)
    
    names(out)[names(out) == "value"] <- valueColName
    
    return(out)
  }
  
  addABCColumn <- function(inDF){
    #Input is a dataframe similar to the one created by
    #  the 'separatePathParts' function in dssrip  package,
    #  with an 'A', 'B', and 'C' column
    #Returns a sdataframe of the merged A, B, and C 
    # parts of the path, separated by '__' and 
    # missing any special characters
    
    inDF$abc <- NA
    noAPart <- inDF$A == ""
    inDF$abc[noAPart] = do.call(paste, c(inDF[noAPart,c("B", "C")], sep = "__"))
    inDF$abc[!noAPart] = do.call(paste, c(inDF[!noAPart,c("A", "B","C")], sep = "__"))
    
    inDF$abc <- gsub("\\*","",inDF$abc)
    
    return(inDF)
  }
  
  ### Main ################
  
  #opening dss and getting all paths
  dssFile <- opendss(dssFileName)
  allPaths <- separatePathParts(getAllPaths(dssFile))
  
  #Retrieving matching paths to read, returns matrix of strings
  selectedPaths <- sapply(pathRegEx,function(x) fullPathByRegex(paths = allPaths$PATH, pattern = glob2rx_plus(x)))
  selectedPaths <- as.vector(unlist(selectedPaths)) #Making vector instead of matrix of strings
  
  if(length(selectedPaths)==0) {
    warning(sprintf("\n\nNo data found in file '%s' for regular expression.", dssFileName))
    dssFile$close()
    return(NULL) #If no data to extract
  }else{
    
    #retrieving data
    longDF <- compileTSC(paths = selectedPaths, dssFile = dssFile)
    dssFile$done() #done with dss file
    
    #Making new column of merged A/B/C compenents, accounting for missing A parts
    longDF <- addABCColumn(longDF)
    
    return(longDF)
  }
  
  #converting to wide dataframe
  # wideDF <- dcast(longDF, date ~ abc, value.var="value")
}

### SQL #################


dbReadTable_tblNameFirst <- function(tblName, db) dbReadTable(db, tblName)


mergeSQLRawWithAddlInfo <- function(rawDF, addlInfoList){
  #Appends additional metadata colums as loaded from the sql file
  addlInfo <- addlInfoList[[sprintf("addlInfo_%s",as.character(nrow(rawDF)))]]
  #Making interprettable as dae
  addlInfo$date <- as.POSIXct(addlInfo$date,origin="1970-01-01",tz="GMT") 
  bind_cols(addlInfo,rawDF)
}

readSQLTbls <- function(file_path, paths){
  #Loads from an SQLite database
  db <- dbConnect(RSQLite::SQLite(),file_path)
  #check that the table exists in the file
  missingTbls <- paths[paths %!in% RSQLite::dbListTables(db)]
  if( length(missingTbls) > 0 ){
    cat(sprintf("Missing the following tables from database:\n\t%s",
                paste0(missingTbls,collapse="\n\t")))
    paths <- paths %!in% missingTbls #reduce to tables that do exist
    if(length(paths)==0) return(NA)     #return NA if no data
  }
  suppressWarnings( #suppress warnings to close db after each read
    out <- Map(dbReadTable_tblNameFirst, paths, list(db))
  )
  #Appending appropriate additional info, saved in sql
  uniqueRows <- as.character(unique(sapply(out,nrow)))
  addlInfoList <- Map(dbReadTable_tblNameFirst,
                      sprintf("addlInfo_%s",uniqueRows),
                      list(db))
  out <- lapply(out, mergeSQLRawWithAddlInfo,addlInfoList=addlInfoList )
  # if(length(paths)==1) out <- out[[1]]
  dbDisconnect(db) 
  return(out)
}


save_SQL <- function(file_path, dfList){
  #Saves a list of dataframes to SQL given the SQL file path
  
  #Opening connection to sql db, creating save directory if needed
  sqlDBFileName <- file_path
  if(!dir.exists(dirname(sqlDBFileName))) dir.create(dirname(sqlDBFileName),recursive = T)
  sqlDB <- dbConnect(SQLite(), dbname = sqlDBFileName)
  
  for( path in names(dfList) ){
    #extracting initial table names for comparison, returns character vector of length zero if no tables
    tblNames <- RSQLite::dbListTables(sqlDB) 
    
    cat(sprintf("\nPath:\t%s", path))
    if( path %in% tblNames ) next
    
    pathCPart <- getPathPart(path,"c")
    #rounding to desired decimal precision, matching in the 'roundPrecision' dataframe by C part 
    #  using regular expr
    prec <- roundPrecision$prec[roundPrecision$cPart=="DEFAULT"] #Setting to default initially
    #Checking if there is a partial C part match in the defined precision df
    for( cPart in roundPrecision$cPart  ) 
      if( grepl(cPart,pathCPart) & cPart != "DEFAULT")
        prec <- roundPrecision$prec[roundPrecision$cPart==cPart]
    
    saveData <- as.data.frame(dfList[[path]])
    saveData$value <- round(saveData$value,prec) #rounding, using specified precision
    
    #writing to database
    dbWriteTable(conn = sqlDB, name = path, value = data.frame(value=saveData$value),overwrite=F)
    
    #Writing additonal info if needed
    #appending number of rows as identifier to table name for loading later
    addlInfoTblName <- sprintf("addlInfo_%s",nrow(saveData)) #as string so doesn't write in scientific notation
    
    if( addlInfoTblName %!in% tblNames )
      dbWriteTable(conn = sqlDB, name = addlInfoTblName,
                   value = saveData[,addlInfoCols], overwrite=F) 
    
  }
  
  dbDisconnect(sqlDB) #close connection
}


### Load Available Data  #######################
#Retrieve connections ad determine available plottinn

getConnections <- function(sheetName){
  #Reads the config/connections Excel and reads in uncommented rows
  require(XLConnect)
  wb <- loadWorkbook("config/connections.xlsx")
  if(sheetName %!in% getSheets(wb))
    stop(sprintf("\ngetConnections:\tNo sheet '%s' found in Excel file config/connections.xlsx.", sheetName))
  
  #initial read of commented lines
  rawRead <-  readWorksheet(object = wb,sheet = sheetName,header=F)
  #re-read, dropping commented rows
  readWorksheet(object = wb,sheet = sheetName,startRow = sum(left(rawRead[,1],1)=="#",na.rm=T)+1,header=T)
}




### Forecast ###################



loadFcsts <- function(){
  #Returns a nested list of forecasts for both the
  # 5k and 50k runs by project.  primary list elements
  #  will be named after DSS files
  #Where the forecasts DSS data are relative to the base directory of R project
  #  modify here as needed if there are other versions of forecast files (i.e.,
  #  if the hydrologic sampler is changed for some reason)
  fcstDSSFiles <- c("dss/fcsts/fiveK.dss", "dss/fcsts/fiftyK.dss","dss/fcsts/80yr.dss")
  # fcstDSSFiles <- dir(path = "dss/fcsts",patter="*.dss$",full.names = T) #if new DSS are added
  
  #NOTE: forecast data are chaced in the config/cached folder.  If changes to this function
  #  are made, then it might be beneficial to remove/rename the fcsts.RData file
  
  #If cached forecast RData, load and check that nothing new has been added
  out <- NULL
  updateFcsts <- T
  if( file.exists(cachedFcstFile) ) {
    cat("\n\t\tLoading cached forecast data")
    load(cachedFcstFile)
    #Check that all of the files being loaded match
    if(any(removeFileExtension(fcstDSSFiles) %!in% names(out))){updateFcsts <- T; break}
    #Check all the last modified times match
    for( k in length(out) ){
      oldTime <- out[[k]]$mTime
      newTime <- file.mtime(fcstDSSFiles[removeFileExtension(fcstDSSFiles)==names(out)[k]])
      if(newTime > oldTime) {updateFcsts <- T; break}
    }
    updateFcsts <- F
  }
  
  
  #If needs updating, reload and re-save
  if(updateFcsts){
    cat("\n\t\tUpdating forecast data - detected change in file name(s) or modification of file")
    out <- list()
    for( dssFileName in fcstDSSFiles ){
      listIndex <- which(dssFileName == fcstDSSFiles) #list index in out object
      dssFile <- opendss(dssFileName) #loading DSS file and retrieving paths
      allPaths <- getPaths(dssFile)
      #reducing only to forecasts, assuming the all have the same B part format: <project name>_IN
      # fcstPaths <- grep(pattern = "_IN",x = allPaths, value = T)
      #removing text paths
      fcstPaths <- grep(pattern = "^(?!.*TEXT)",x = allPaths, perl = T,value=T)
      #removing event-year pairings from hydrologic sampler
      fcstPaths <- grep(pattern = "^(?!.*CRT_HS/EVENT)",x = fcstPaths, perl = T,value=T)
      
      #If 80 year, need to read in time series
      if( grepl("80yr|deterministic",basename(dssFileName)) ){
        #remove observed forecasts
        fcstPaths <- grep(pattern = "^(?!.*OBSV)",x = fcstPaths, perl = T,value=T)
        #Only read in paths associated with fcstBPartKey$det
        fcstPaths <- grep(pattern = paste0(fcstBPartKey$det,collapse="|"), fcstPaths,value=T)
        
        out[[listIndex]] <- getDeterminsticFcsts(fcstPaths,dssFile)
        
        #Removing 'dates ' part of column names
        out[[listIndex]] <- lapply(out[[listIndex]], function(x) {names(x) <- gsub("dates ","",names(x));return(x)})
        
        #If FRA, can read in as paired data containers
      }else{
        #reading in paired data containers as dataframes and naming list elements by B part of DSS path
        out[[listIndex]] <- lapply(fcstPaths, loadPDCtoDF, dssFile=dssFile)
        
        #renaming 'x' column to 'event' in each pdc
        out[[listIndex]] <- renameColsInDFList(out[[listIndex]],"x","event")
        names(out[[listIndex]]) <- getPathParts(fcstPaths, "b")
        
        #Removing 'dates ' part of column names
        out[[listIndex]] <- lapply(out[[listIndex]], function(x) {names(x) <- gsub("dates ","",names(x));return(x)})
      }
      
      
      
      out[[listIndex]]$mTime <- file.mtime(dssFileName)
      
      dssFile$done()
    }
    
    #Reducing only to common forecasts (and mTime element) between all compute types (list elements)
    namesInAllFcsts <- getCommonNamesInList(out)
    out <- lapply(out,function(x) x[namesInAllFcsts])
    
    #Divide by 1,000
    out <- lapply(out, function(dfList) lapply(dfList,
                                                function(x) {
                                                  if(is.data.frame(x)){
                                                    fcstCols <- grepl(paste0(month.abb,collapse="|"),names(x))
                                                    # x[,fcstCols] <- round(x[,fcstCols]/1000,3)
                                                    x[,fcstCols] <- x[,fcstCols]/1000
                                                  }
                                                  return(x)
                                                }  ))
    
    names(out) <- removeFileExtension(fcstDSSFiles) #naming after DSS files without file extensions
    save(out,file = cachedFcstFile)
  }
  
  return(out)
}


roundFcstTbl <- function(fcstTbl,prec=0){
  #Rounds a forecast table where the names
  fcstCols <- grepl(paste0(month.abb,collapse="|"),names(fcstTbl))
  fcstTbl[,fcstCols] <- round(fcstTbl[,fcstCols],prec)
  return(fcstTbl)
}



### General ##########


loadElevStor <- function(){
  elevWB <- loadWorkbook("config/elev_stor.xlsx")
  allSheets <- getSheets(elevWB)
  out <- lapply(allSheets, function(x) readWorksheet(elevWB,x))
  names(out) <- allSheets
  return(out)
}

mergeListOfDataFrames <- function(...){
  #Inputs are two lists of dataframes with identical names and columns in each df
  #Output is a single list of dataframes, with the rows binded between the 
  #  matching dataframes in the list element (i.e., how you'd want to merge two
  #  similar lists of dataframes)
  require(dplyr)
  library(purrr)
  input <- c(...)
  listSubNames <- unique(as.character(sapply(input,names, simplify = T)))
  out <- lapply(listSubNames, function(x) data.frame(bind_rows(map(input, x))) )
  names(out) <- listSubNames #naming list elements
  return(out)
}
#testing/demonstrating the mergeListOfDataFrames function
# testList <- list(part1= list(a1 = data.frame(b1=1:10, b2=11:20),
#                   a2= data.frame(b1=21:30, b2=31:40)),
#                  part2= list(a1 = data.frame(b1=41:50, b2=51:60),
#                              a2= data.frame(b1=61:70, b2=71:80)))
# mergeListOfDataFrames(testList)


getEventData <- function(event,altList_split){
  #Retrieves data for a given event, returning a
  #  nested list of dataframes, by location
  
  #reduce list to just the event specified, add 'wyDate' column
  #  for plotting in water year
  eventAltList <- lapply(altList_split,function(x)
    lapply(x, function(y) convertDFToSameWY(y[[as.character(event)]]) ))
  
  #add column of the alternative name, order by date
  for( alt in names(eventAltList) )
    eventAltList[[alt]] <-
      lapply(eventAltList[[alt]],
             function(x){x$alt <- alt; x <- x[order(x$wyDate),];return(x)})
  
  #merge the list of dataframes (by location), making sure they're sorted by date
  out <- mergeListOfDataFrames(eventAltList)
  lapply(out, function(x) x[order(x$wyDate),])
}

getDeterminsticFcsts <- function(fcstPaths,dssFile){
  
  #Read in time series container
  rawTSC <- compileTSC(fcstPaths,dssFile)
  rawTSC$monthday <- format(eom(rawTSC$date),"dates %d%b") #get end of month values
  rawTSC$monthday <- gsub("dates 29Feb","dates 28Feb",rawTSC$monthday)#correction for Feb29
  rawTSC$event <- wateryear(rawTSC$date)
  rawTSC <- rawTSC[minute(rawTSC$date)==1,] #removing the 00:00 hour, duplicated timestamps
  #Only want months from Jan-Jun
  rawTSC <- rawTSC[grepl(paste0(month.abb[1:6],collapse="|"),rawTSC$monthday),]
  # out[[listIndex]]
  rawTSCList <- split(rawTSC,rawTSC$B)
  out <- Map(function(x) dcast(data = unique(x),formula = "event~monthday",value.var="value"),
             rawTSCList)
  
  #Assigning incremental probabilities
  out <- lapply(out, function(x){x$incProbs = getIncProbs(x$event); return(na.omit(x))})
  
  #correcting list element names so they match FRA
  names(out) <- fcstBPartKey$fra[match(names(out),fcstBPartKey$det)]
  return(out)
}

getBaseYrData <- function(baseYr, altList_split, is5k=F){
  #Retrieves data for a given base year, returning a
  #  nested list of dataframes, by location
  if( is5k ){ #establishing the correct event-base year pair
    subEventYr <- eventYr$fiveK
  }else{
    subEventYr <- eventYr$fiftyK
  }
  #all events associated with the base year
  events <- subEventYr$event[subEventYr$base_year==baseYr]
  #returning a merged list of all events, merged by location
  mergeListOfDataFrames( lapply(events,getEventData, altList_split=altList_split)  )
}

### Stats ###########################

#Retrieves the deterministic, incremental probabilities for given water years
getIncProbs <- function(wys) eventYr[["deterministic_probs"]]$NEW.PROB[match(wys,eventYr[["deterministic_probs"]]$WY)]




### Market Prices ############################

meltPriceDF <- function(priceDF){
  #priceDF has an 'Event' column and other columns
  #  named after the months.
  #Output is a long dataframe that has the month and events paired
  #  with the price value ($/MWh)
  if(is.null(priceDF)) return(NULL)
  out <-  melt(data = priceDF, id.vars = "Event",measure.vars = month.abb, factorsAsStrings = F)
  out$variable <- as.character(out$variable)
  names(out)[names(out) == "variable"] <- "month"
  return(out)
}

#Load the market prices
getMP <- function(){
  
  mpFiles <- list(
    '80yr' = NULL,
    FRA5k= "X:\\CRT2014\\PDT\\WAT\\EntitySupport\\Negotiation Support 2019\\Econ_Losses\\market_prices\\MC_Energy_Prices.csv",
    FRA50k=NULL
  )
  #Retrieve raw data, and return as melted long dataframe
  rawMP <- lapply(mpFiles, function(x) {if(is.null(x)){return(x)}else{return(read_csv(x))}})
  lapply(rawMP, meltPriceDF) #melt to long dataframe
}

