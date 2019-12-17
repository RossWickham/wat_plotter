#This function converts lifecycle DSS files from an FRA run
#  to SQL



### Testing Parameters #########

# baseDir = "Y:\\Joint_Modeling\\JM_COE_FRM\\2018-05-04_NewCRCs"
# extractCSVFiles = "power data_8projects_gatherpaths.csv"
# isFRA <- T


####***BELOW HERE IS ALL THAT SHOULD BE NEEDED FOR DEPLOYED TOOL***#######
#In addition to sourced, r_gatherpaths*.R script

### Config ###########

#Sending rounding digits for various paths
#Matches C part using regular expressions, and applies
#  rounding precision level (flow -> nearest cfs; elev/stage -> nearest 0.01 ft)
roundPrecision <- data.frame(cPart=c("FLOW","ELEV|STAGE","DEFAULT"),
                             prec=c(0,2,2))

#Number of rows in the extract CSV to iterate through at a time
#  Limits memory usage of script
nRowsAtTime <- 25

### Function #############

gatherpathsToSQL <- function(baseDir,extractCSVFiles,isFRA){
  #baseDir is the FRA directory, e.g.:
  #   "Y:\\Joint_Modeling\\JM_COE_FRM\\2018-05-04_NewCRCs")
  #extractCSVFiles is a character vector defining the extract CSV files, e.g.:
  #  c("power data_8projects_gatherpaths.csv", "extract_URCs_with_shifted.csv")
  #isFRA is a logical indicating whether or not this is an FRA dataset to be processed
  require(lubridate)
  require(sqldf)
  require(RSQLite)
  require(rjson)
  require(readr)
  require(dplyr)
  
  saveSQLDir <- paste0(baseDir,"\\sql") #saving sql data to it's own folder
  
  ### Functions ######
  
  #loading superfunction to compiled fra lifecycle data
  # source( "scripts/utils/r_gatherpaths_2019-07-30.R" ) 

  "%!in%" <- function(x,y) !(x %in% y)
  
  #Extracts from left side of character(similar to Excel function)
  left <- function(text, nchars)    substring(text, 1, nchars)
  
  #Extracts from right side of character (similar to Excel function)
  right <- function(text, nchars)    substring(text, nchar(text)-nchars+1, nchar(text))
  
  
  checkExistsRead <- function(filePath,readFun,fileArg=""){
    #Check that a file path exists and then performs
    #  a read function on the file and returns a dataframe
    #If the file needs to be input as a specific argument to the function
    #  that can be defined as a character string passed via 'fileArg' (e.g.,fileArg="file")
    if( !file.exists(filePath) ) stop(sprintf("\nFile not found:\t%s",filePath))
    if(fileArg!="") fileArg = sprintf("%s = ",fileArg)
    filePath <- gsub("\\\\","\\\\\\\\\\",filePath) #buffering with way too many backslashes for proper eval
    data.frame(eval(parse(text = sprintf("readFun(%s'%s')",fileArg,filePath))))
  }
  
  ### Loading extract config and csv

  extractConfigJsonFile <- paste0(baseDir,"\\extractConfig.json")
  extractConfig <- checkExistsRead(filePath = extractConfigJsonFile,  readFun = fromJSON, fileArg="file")
  
  #If no directory prefix, assume that the extract CSV is in the base FRA directory
  #Otherwise, will load from some other location
  # if(basename(extractCSVFile) == extractCSVFile) extractCSVFile <- paste0(baseDir,"\\",extractCSVFile)
  
  #Reading extract csv(s), keeping only rows that are needed for gatherpaths
  extractCSV <- lapply(extractCSVFiles,checkExistsRead, readFun = read_csv)
  extractCSV <- bind_rows(lapply(extractCSV, function(x) x[,c("COMMAND","SKIP","IN_PATH")]))
  
  #Reducing to gatherpaths and non-skip rows
  extractCSV <- extractCSV[ toupper(extractCSV$COMMAND) == "GATHERPATHS" & is.na(extractCSV$SKIP),]
  in_paths <- unique(extractCSV$IN_PATH) #unique DSS paths
  
  ### Checking paths don't already exist in sql
  
  #Opening connection to sql db, creating save directory if needed
  sqlDBFileName <- sprintf("%s\\%s (%s).sql", saveSQLDir, extractConfig$watAlt, basename(baseDir))
  if(!dir.exists(saveSQLDir)) dir.create(saveSQLDir)
  sqlDB <- dbConnect(SQLite(), dbname = sqlDBFileName)
  
  #extracting initial table names for comparison, returns character vector of length zero if no tables
  tblNames <- RSQLite::dbListTables(sqlDB) 
  
  if( any( in_paths %in% tblNames) )
    cat(sprintf("\n\nDetected some SQL tables that have already been processed, skipping:\n\t%s",
                    paste0(in_paths[in_paths %in% tblNames],collapse="\n\t")))
  in_paths <- in_paths[in_paths %!in% tblNames]
  if( length(in_paths) == 0 ) return(NULL) #breaking if already processed all required paths
  
  ### Main 
  
  #Iterating every 'nRowsAtTime' rows to preserve memory
  nIts <- ceiling(length(in_paths)/nRowsAtTime)
  sTime <- Sys.time()
  
  for( n in 1:nIts){
    
    ### Reading 

    tsc <- r_gatherpaths(baseDir =baseDir,
                           in_paths = in_paths[ ((n-1)*nRowsAtTime + 1):min(length(in_paths),n*nRowsAtTime)],
                           isFRA = isFRA)
    
    ### Saving
    
    addlInfoCols <- c("date","event") #columns to be saved as metadata

    for( i in 1:length(tsc) ){
      if(names(tsc)[i] %in% tblNames) next #Skipping if table already exists in sql database
      
      cat(sprintf("\nSaving %s\t[%4g/%4g]", names(tsc)[i], i, length(tsc)))
      
      
      tblName <- toupper(names(tsc)[i]) #the DSS path
      pathCPart <- strsplit(tblName,"/")[[1]][4] #extracting the C part from the DSS pathname
      saveData <- tsc[[i]] #data to be saved
      
      
      
      #rounding to desired decimal precision, matching in the 'roundPrecision' dataframe by C part 
      #  using regular expr
      prec <- roundPrecision$prec[roundPrecision$cPart=="DEFAULT"] #Setting to default initially
      #Checking if there is a partial C part match in the defined precision df
      for( cPart in roundPrecision$cPart  ) 
        if( grepl(cPart,pathCPart) & cPart != "DEFAULT")
          prec <- roundPrecision$prec[roundPrecision$cPart==cPart]
      
      saveData$value <- round(saveData$value,prec) #rounding, using specified precision
      
      #writing to database
      dbWriteTable(conn = sqlDB, name = tblName, value = data.frame(value=saveData$value),overwrite=F) 
      
      #Writing additonal info if needed
      #appending number of rows as identifier to table name for loading later
      addlInfoTblName <- sprintf("addlInfo_%s",nrow(saveData)) #as string so doesn't write in scientific notation
      
      if( addlInfoTblName %!in% tblNames)
        dbWriteTable(conn = sqlDB, name = addlInfoTblName,
                     value = saveData[,addlInfoCols], overwrite=F) 
      
      #in case duplicate paths (shouldn't be any), recreating table names string vector
      tblNames <- RSQLite::dbListTables(sqlDB) 
    }
  }
  
  dbDisconnect(sqlDB) #close connection
  
  eTime <- Sys.time() #Writing out how much time was required for the sql conversion
  tElapsed <- round(as.numeric(eTime)-as.numeric(sTime),0)
  cat(sprintf("\nFinished compute\n\tStart Time:\t%s\n\tEnd Time:\t%s\n\tElapsed:\t%s",
              sTime,eTime,
              as.character(seconds_to_period(tElapsed))) )
}





### TODO #############
#implement additional info
# save to table by addl_info_<nrows> to be paired when loading 
#Does create the risk of mixing metadata, but it seems very unlikely

#*The following section saves what was called 'additional information'.
#  An attempt was made to minimize the amount of redundant information (i.e,
#  dates and event numbers) saved with each extracted dataframe.  However,
#  there were some time series that were found to not have the same number of
#  rows in the dataframe, which caused issues.  A thought to fix this is to
#  have multiple 'addition information' sets that can the be merged by column
#  to associated time series, which would just be saved as vectors of values
#  (thus saving ~1/2 the disk space).
#  However, this functionality has not yet been established and would require 
#  a good amount of piping to fix.


###_Saving to SQL#########################

# 
# ###_Writing Additional Info ##########
# 
# #To save space, only writing the rounded values to db
# #WIll also want the additional data too, though (date, and fParts)
# #These **should** be the same between all FRA time series, but will want to check
# #  just in case.  Steps are to 1) check if the dateTimes and fParts align between
# #  all dataframes in the tsc list. 2) Save the date and fParts to the db,
# 
# addlInfoCols <- c("date","event")
# 
# #Checking that all of the additional info columns are the same between 
# #  each time series data frame, and match any previously saved.  If not saved,
# #  then create.  Otherwise, don't overwrite
# 
# if( !all(sapply(tsc,function(x) all_equal(x[,addlInfoCols],tsc[[1]][,addlInfoCols]))) )
#   stop(sprintf("Found a mismatch in additonal column information (%s) between FRA datasets.",
#                paste0(addlInfoCols,collapse=", ")))
# 
# addlInfo <- tsc[[1]][,addlInfoCols] #going with the first dataframe, since additional info all the same
# 
# if("addl_info" %in% tblNames ) {
#   oldAddlInfo <- RSQLite::dbReadTable(conn = sqlDB, name = "addl_info") #loading
#   oldAddlInfo$date <- as.POSIXct(oldAddlInfo$date,origin="1970-01-01",tz="GMT")
#   
#   if( !all_equal(addlInfo,oldAddlInfo) )
#     stop(sprintf("Found a mismatch in additonal column information (%s) between previous and newly generated FRA datasets.",
#                  paste0(addlInfoCols,collapse=", ")))
#   
# }else{
#   #writing to database, since additional info not yet created
#   dbWriteTable(conn = sqlDB, name = "addl_info", value = addlInfo, overwrite=F) 
# }
# 
# dbDisconnect(sqlDB)

### Reading in  Saved Data ##################

# sqlDB <- dbConnect(SQLite(), dbname = sqlDBFileName)
# addlInfo <-  RSQLite::dbReadTable(conn = sqlDB, name = "addl_info")
# addlInfo <- addlInfo[,addlInfoCols]
# # tsc <- RSQLite::dbReadTable(conn = sqlDB, name = tblName)
# # cBindData <- addlInfo[addlInfo[,getSQLAddlInfoColName(tblName)]==1, addlInfoCols]
# # tsc <- cbind(cBindData, tsc)
# 
# tsc <- sapply( tblNames[tblNames!="addl_info"],
#                function(x) cbind(addlInfo,RSQLite::dbReadTable(conn = sqlDB, name = x)), simplify=F)
# 
# dbDisconnect(sqlDB)

