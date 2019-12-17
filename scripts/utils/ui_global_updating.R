#handles alternatives and DSS path matching to ensure consistency for the UI

### Selectable DSS Paths #######


updateDSSPaths <- function(input,session){
  
  #Update a subset of pathConfig to only include currently selected alternatives
  cPathConfig <- updateSubPathConfig(input$selectedAlts,pathConfig)
  
  selectedPaths <- input$selectedPaths
  
  #Update selected DSS paths to ensure
  #  that they are in the currently selected alternatives
  newSelected <- selectedPaths[selectedPaths %in% cPathConfig$matchedPaths]
  if(length(newSelected) == 0) newSelected <- ""
  
  cat(sprintf("\nSelected DSS paths:\n\t%s", paste(selectedPaths,collapse="\n\t")))
  
  #Update DSS path choices
  updateSelectInput(session,"selectedPaths",
                    label="Select DSS paths",
                    choices=cPathConfig$matchedPaths,
                    selected=newSelected)
}

### pathConfig ########################

#match pairwise
getMatching <- function(xString,yString) xString[xString %in% yString]

#non-matches, pairwise
getMissing <- function(xString,yString) xString[xString %!in% yString]

#Matching paths between all list elements
getMatchingPaths <- function(pathConfig) Reduce(getMatching,lapply(pathConfig$allPaths,function(x)x$paths))

#Non-matching paths between all list elements
getMissingPaths <- function(pathConfig) Map(getMissing,lapply(pathConfig$allPaths,function(x)x$paths),list(pathConfig$matchedPaths))

getAvailablePaths <- function(pathConfig){
  #From a vector of all available paths in selected connections (SQL or DSS),
  #  returns a list indicates those that are matched between all current
  #  connection selections, and those that don't
  pathConfig$matchedPaths = getMatchingPaths(pathConfig)
  pathConfig$missingPaths = getMissingPaths(pathConfig)
  return(pathConfig)
}

getConnsToUpdate <- function(conns, pathConfig){
  #connection names that are either missing or not in the 
  #  pathConfig definition
  
  missingConnections <- which(conns$connection_name %!in% names(pathConfig$allPaths))
  
  outOfDateConnections <- NULL
  for( k in 1:length(pathConfig$allPaths) ){
    oldTime <- as.character(pathConfig$allPaths[[k]]$mTime)
    newTime <- as.character(file.mtime(pathConfig$allPaths[[k]]$file_path))
    if( newTime != oldTime ) outOfDateConnections <- c(outOfDateConnections,k)
  }
  
  return(conns[ unique(c(missingConnections,outOfDateConnections)),])
}

getSQLPaths <- function(sqlFileName){
  #load file and read table names
  require(RSQLite)
  sqlDB <- dbConnect(SQLite(), dbname = sqlDBFileName)
  tblNames <- RSQLite::dbListTables(sqlDB)
  dbDisconnect(sqlDB)
  return(tblNames)
}

updateSubPathConfig <- function(alts,pathConfig){
  #Given the current alternative selection, determines the
  #  available paths are in all of the selected alternatives
  #This returns a nested list of the same form as pathConfig,
  #  with elemnets for 'allPaths', 'missingPaths', and 'matchedPaths'
  
  if(length(alts)==0) {
    cat("\nNo alternatives selected.")
    return(NULL)
  }
  
  #reducing to just those elements in selected alternatives
  connsInAlts <- conns$connection_name[conns$alt %in% alts]
  
  subPathConfig <- list()
  subPathConfig$allPaths <- pathConfig$allPaths[ which(names(pathConfig$allPaths) %in% connsInAlts) ]
  
  subPathConfig <- getAvailablePaths(subPathConfig)
  return(subPathConfig)
}

getPathConfig <- function(conns){
  #Checks the file assocaited with each connection.  If it is out-of-date relative
  #  to what has previously been loaded, then update the available paths
  #
  #allPaths is a nested list organized as:
  #allPaths
  # - <connection> to be named after conns$connection_name
  #   - mTime: last modified date from file.mtime as numeric
  #   - paths: character strings of DSS paths/table names
  require(purrr)
  require(yaml)
  
  pathConfig <- NULL
  if(file.exists(pathConfigFile)) pathConfig <- read_yaml(file = pathConfigFile)
  
  if(is.null(pathConfig)){
    #Need to create the allPaths since it doesn't exist
    pathConfig <- list()
    
    #Organized by connection_name
    pathConfig$allPaths[conns$connection_name] <- pmap(conns,getMTimeAndPaths)
    
  }else{
    #Check if anything out of date or any new connections have been made
    
    #Check for new conns defined that aren't in allPaths, returns row(s) in conns dataframe
    connsToUpdate <- getConnsToUpdate(conns, pathConfig)
    
    #Update if needed
    if( nrow(connsToUpdate) > 0 ){
      cat(sprintf("\n\nDetected new or out-of-date connection files.  Updating:\n\t\t%s",
                  paste0(connsToUpdate$connection_name,collapse="\n\t")))
      pathConfig$allPaths[connsToUpdate$connection_name] <- pmap(connsToUpdate,getMTimeAndPaths)
    }
  }
  
  #Find all the matching paths amongst all connections and 
  #  tracking which paths are missing from matched paths in each connection
  pathConfig <- getAvailablePaths(pathConfig)
  
  write_yaml(x = pathConfig, file = pathConfigFile)
  
  return(pathConfig)
}


getSQLTblNames <- function(sqlDBFileName){
  #open sql connection, read table names, remove "addlInfo" tables from output,
  #  close connection
  sqlDB <- dbConnect(SQLite(), dbname = sqlDBFileName)
  tblNames <- RSQLite::dbListTables(sqlDB)
  tblNames <- tblNames[!grepl("addlInfo",tblNames)]
  dbDisconnect(sqlDB)
  return(tblNames)
}

getMTimeAndPaths <- function(connection_name=NULL,file_path, file_format, compute_type,alt,Notes,...){
  #For a given file path, format ("SQL" or "DSS"), and compute type ("FRA" or "80yr")
  #  returns a list of the last modified date ('mTime') and the DSS paths/table names
  #Using 'connection_name' as first argument so pmap function names list element after
  #  connection_name
  cat(sprintf("\n\t\t\t%s", file_path))
  mTime <- as.character(file.mtime(file_path))
  
  if(file_format=="SQL"){
    paths <- getSQLTblNames(file_path) #Assume file_format == "FRA"
  }else if(file_format=="DSS"){
    #compute_type can be either "Deterministic" (QA Metrics) or "FRA" (gatherpaths extract)
    
    allPaths <- separatePathParts(getAllPathsFromFile(file_path)) #get the separated path parts
    allPaths <- allPaths[!duplicated(allPaths[,c("A","B","C","E","F")]),] #reducing to unique abcef
    
    #Reducing F part to same as expected in standard path convention (i.e., gatherpaths)
    if(compute_type=="FRA"){
      
      warning("\nFor optimal performance, FRA data should be converted to SQL")
      
    }else if(compute_type=="Deterministic"){
      
      newFParts <- as.character(
        str_extract_all(string = allPaths$F,
                        pattern=paste0(modelDict$fPartDictionary,collapse="|"),
                        simplify=T)
      )
      
      paths <- formPaths(a = allPaths$A, b=allPaths$B,c=allPaths$C,e=allPaths$E,f=newFParts)
      
    }
    
  }else{
    warning(sprintf(paste0("\ngetMTimeAndPaths:\tError with file '%s'.",
                           "  File format '%s' currently does not have a method established to open",
                           file_path, file_format)))
  }
  
  return(list(mTime=mTime,paths=paths,file_path=file_path,file_format=file_format, compute_type=compute_type,alt=alt,Notes=Notes))
}

### input ##################



getOpts <- function(input, plotType="Quintile By Forecast"){
  #Checks whether or not to show progress bar (i.e., in server session)
  
  if(showProgress) opts <- getInputOptions(input)
  if(!showProgress){
    #In testing mode
    source("scripts/utils/testing.R")
    # opts <- getTestInput("Quintile By Forecast")
    # opts <- getTestInput("Summary Hydrograph")
    # opts <- getTestInput("Base Year")
    opts <- getTestInput(plotType)
    # opts <- getTestInput("Metric vs Forecast")
  }
  return(opts)
}


getInputOptions <- function(input){
  isolate(reactiveValuesToList(input))
}

### allData ####################

getAltMeta <- function(conns){
  #Retrieves alternative metadata from conns
  Map(function(alt){
    list(connDefinition=conns[conns$alt==alt,],
         compute_type=unique(conns$compute_type[conns$alt==alt]),
         file_format=unique(conns$file_format[conns$alt==alt]))
  },
  unique(conns$alt))
}


loadData <- function(file_path, paths,file_format,compute_type){
  
  if( file_format == "DSS"){
    
    if( toupper(compute_type) %in% c("DETERMINISTIC","FRA") ){
      #Reading determinstic (QA Metrics) or FRA (gatherpaths) DSS
      return(loadMergedFRADSS(in_paths =paths, compute_type = compute_type,dssFileName = file_path))
    }else {
      cat(sprintf("\nloadData:\tCurrently no read function for compute type '%s' and file format '%s'. Skipping",
                  compute_type, file_format))
      return(NULL)
    }
    
  }else if( toupper(file_format)=="SQL" ){
    #Reading SQL
    return(readSQLTbls(file_path, paths))
    
  }else{
    cat(sprintf("\nloadData:\tCurrently no read function for file format '%s'. Skipping", file_format))
  }
  
}

loadDataWrapper <- function(alt,paths,incPerPath,showProgress=F){
  #Load data given the alternative and paths to load
  #Get file format and compute type from metadata definition
  file_format=allData$meta[[alt]]$file_format
  compute_type=allData$meta[[alt]]$compute_type
  
  #Finding which file each path is in - there may be multiple DSS files
  #  for a given alternative (e.g., power, non-power QA metrics)
  #There can also be duplicated paths between DSS files - pick one
  altConnNames <- conns$connection_name[conns$alt==alt]
  # Map(function(connection_name) pathConfig$allPaths[[connection_name]]$paths )
  
  out <- NULL
  
  unloadedPaths <- paths
  for(connection_name in altConnNames){
    connPaths <- pathConfig$allPaths[[connection_name]]$paths
    pathsToLoad <- connPaths[connPaths %in% unloadedPaths]
    unloadedPaths <- unloadedPaths[unloadedPaths %!in% pathsToLoad]
    
    if(length(pathsToLoad)==0) next
    
    file_path <- pathConfig$allPaths[[connection_name]]$file_path
    
    out <- c(out, loadData(file_path,pathsToLoad,file_format,compute_type))
    
    if(showProgress) incProgress(incPerPath)
    
    if( length(unloadedPaths) ==0 ) break
  }
  
  return(out)
}

updateLoadedData <- function(selectedAlts, selectedPaths, allData=NULL,showProgress=F){
  #Updates the global object that stores the currently desired plot data ('allData')
  # Uses 'cPathConfig' to determine what needs to be updated
  #allData is a nested list by alternative that has both data and
  #  metadata info
  
  #Check if the data has been created yet.  If not, create
  if( "data" %!in%  names(allData)){
    cat("\nInitializing time series data storage in memory")
    allData$data <- list()
  }
  
  #Increment to increase load bar
  incPerPath <- 1/(length(selectedPaths)*length(selectedAlts))
  
  #Iterate through each selected alt
  for( alt in selectedAlts ){
    
    #Initialize alternative in dataset if needed
    if( alt %!in% names(allData$data) ) allData$data[[alt]] <- list() 
    
    #Get which paths need to be updated.  Skip if none
    unloadedPaths <- selectedPaths[ selectedPaths %!in% names(allData$data[[alt]]) ]
    if( length(unloadedPaths)==0 ){
      if(showProgress) incProgress(incPerPath)
      next
    }
    #Load into nested dataframe
    allData$data[[alt]] <- c(allData$data[[alt]],
                             loadDataWrapper(alt=alt,
                                             paths=unloadedPaths,
                                             incPerPath=incPerPath,
                                             showProgress=showProgress))
  } #End loop of alternatives
  return(allData)
} 


### plotData #######################



checkPathDataNeedsUpdate <- function(plotInfo,prevPlotData, opts){
  #prevPlotData contains the same list elements as 'plotInfo'
  #  Check that they match between the two or return
  #  If not, T, else F
  if(is.null(prevPlotData)) return(T) #If hasn't been created yet
  plotType <- plotInfo$plotType
  if(plotType == "Summary Hydrograph"){
    #check 'metrics'
    if( opts$metrics != prevPlotData$metrics) return(T)
    
  }else if(plotType == "Quintile By Forecast"){
    #check 'fcstProj', 'fcstDate', and 'wyGroup'
    if( opts$fcstProj != prevPlotData$fcstProj) return(T)
    if( opts$fcstDate != prevPlotData$fcstDate) return(T)
    if( opts$wyGroup != prevPlotData$wyGroup) return(T)
    
  }else if(plotType == "Base Year"){
    #check 'baseYr'
    if( opts$baseYr != prevPlotData$baseYr) return(T)
    
  }else if(plotType == "Frequency"){
    #check 'freqTW' and 'freqMetric'
    if( opts$freqTW != prevPlotData$freqTW) return(T)
    if( opts$freqMetric != prevPlotData$freqMetric) return(T)
    
  }else if(plotType == "Duration"){
    #check ''
    if( opts$durTW != prevPlotData$durTW) return(T)
    
  }else if(plotType == "Metric vs Forecast"){
    #check mFreqTW, mFreqMetric, mFcstProj, mFcstDate
    if( opts$mFreqTW != prevPlotData$mFreqTW) return(T)
    if( opts$mFreqMetric != prevPlotData$mFreqMetric) return(T)
    if( opts$mFcstProj != prevPlotData$mFcstProj) return(T)
    if( opts$mFcstDate != prevPlotData$mFcstDate) return(T)
    
  }
  return(F)
}

createDataset <- function(allData, plotInfo, path,alt){
  #Performs the compute on both deterministic and FRA datasets for each plotType
  
  #extracting dataframe (date, event, value columns), plotType and computeType
  pathData <- allData$data[[alt]][[path]]
  plotType <- plotInfo$plotType
  computeType <- allData$meta[[alt]]$compute_type
  
  #call function by name onto the dataset, returns dataframe
  fName <- gsub(" ","",sprintf("%s_%s", plotType,computeType))
  if(!exists(fName)){
    cat(sprintf("\nFunction for plot type '%s' and compute type '%s' does not yet exist, returning NULL",
                plotType, computeType))
    return(NULL)
  }else if(
    startup_checks( sprintf("createDataset %s", plotType), allData=allData, plotInfo=plotInfo)$failed
    ){
    return(NULL) #bad startup - return nothing
  }
  
  #Functions defined under associated section in 'analysis' script
  curveData <- eval(call(name =fName,pathData,plotInfo,computeType))
  
  #append plotInfo
  out <- plotInfo
  out$data <- curveData
  return(out)
}


updatePlotData <- function(allData, plotData, opts,showProgress=F){
  #Iterate thoguh each plot, and create dataset in plot$data
  #  under nested list defined by the plot's 'plotType'
  
  #creating new 'data' element if  not created yet
  if( "data" %!in% names(plotData) ) plotData$data <- list()
  
  #Increment to increase load bar
  #1/(<no. plots>*<no. alts>) : ignoring number of paths
  incPerPath <- 1/(length(plotData$meta)*length(opts$selectedAlts)*2)
  
  for( plotInfo in plotData$meta ){
    
    #Creating new 'plotType' element for plot type if not created yet
    plotType <- plotInfo$plotType
    
    if( plotType %!in% names(plotData$data) ) plotData$data[[plotType]] <- list()
    
    for(path in plotInfo$paths){
      
      #creating new 'path' element for the path if not created yet
      if( path %!in% names(plotData$data[[plotType]]) ) plotData$data[[plotType]][[path]] <- list()
      
      for(alt in opts$selectedAlts){
        #Checking if path data needs to be updated
        pathDataNeedsUpdate <- 
          checkPathDataNeedsUpdate(plotInfo, plotData$data[[plotType]][[path]][[alt]], opts)
        if( pathDataNeedsUpdate )
          plotData$data[[plotType]][[path]][[alt]] <- createDataset(allData, plotInfo, path,alt)
        if(showProgress) incProgress(incPerPath)
      }
    }
  }
  return(plotData)
}


getPlotData <- function(allData, plotData=NULL, opts,showProgress=F){
  #Creates a nested list structure similar to the allData, except the 'data'
  #  element is organized by plot type (e.g., summary hydro, annual freq, quintile)
  #It is updated in a similar way as allData, where a check is performed prior
  #  to computing so new plots are generated more quickly
  
  # use input <- getTestInput() for testing
  
  #Mapping instructions for each plot into list elements
  #  This is done for future flexibility in case we want the ability
  #  to plot multiple different plot types together.
  plotData$meta <- Map( function(x) c(paths=x,opts),opts$selectedPaths)
  plotData <- updatePlotData(allData, plotData, opts,showProgress)
  return(plotData)
}
