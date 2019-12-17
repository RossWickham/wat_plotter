#Reads either deterministic or FRA DSS files and returns
#  a list of dataframes containing the parsed-out results.
#
#For FRA data, the names of each list element correspond to 
#  elements in the character string vector. The extractConfig.json
#  file is needed to read FRA data.
#  DSS files are expected to be the raw output from the WAT.
#  e.g., (line indents indicate subfolders)
#  ../2019.05.16/   <- base directory ('baseDir' argument in function)
#      FRA_50_TWM/
#         realization 1/
#           lifecycle 1/
#             NAA_FC-FRA_50_TWM.dss
#           lifecycle 2
#             NAA_FC-FRA_50_TWM.dss
#             ...

#
#For deterministic data, wildcard operators are accepted in the
#  input strings to read in (e.g.) all outflow data at once
#  wanted (e.g., in_paths = "//*-POOL/FLOW-OUT/*/*/*F1*/")
#  DSS files are expected to be the raw output from the WAT.
#  e.g., (line indents indicate subfolders)
#  ../NAA_FC/    <- base directory ('baseDir' argument in function)
#      POR_29-47/
#        NAA_FC-POR_29-47.dss <- one folder beneath base directory
#      POR_48-64/
#        NAA_FC-POR_48-64.dss
#      ...
#
#FRA output will have the 'event' column in output dataframes filled in with
#  a 6-digit character string (e.g., '0000001'), while deterministic data will
#  have this column filled with NA values.
#
#Deterministic output example:
# List of 2
# $ //ALBENI FALLS-POOL/FLOW-OUT//1DAY/NAA_FC:RESSIM-F1NAFC/    :'data.frame':	29230 obs. of  3 variables:
# ..$ date : POSIXct[1:29230], format: "1928-09-30" "1928-10-01" "1928-10-02" "1928-10-03" ...
# ..$ event: logi [1:29230] NA NA NA NA NA NA ...
# ..$ value: num [1:29230] 4000 4000 4000 16671 24559 ...
# $ //ARROW LAKES-POOL/FLOW-OUT//1DAY/NAA_FC:RESSIM-F1NAFC/     :'data.frame':	29230 obs. of  3 variables:
# ..$ date : POSIXct[1:29230], format: "1928-09-30" "1928-10-01" "1928-10-02" "1928-10-03" ...
# ..$ event: logi [1:29230] NA NA NA NA NA NA ...
# ..$ value: num [1:29230] 45000 45000 26052 26050 26050 ...

r_gatherpaths <- function(baseDir, in_paths, isFRA = T){
  # baseDir:
  #  - For FRA, Contains the extractconfig.json file and the FRA main results dir (e.g., "FRA_50_TWM")
  #      e.g., baseDir <- "\\coe-nwpnv005por.nwp.ds.usace.army.mil\CRT_HH\CRSO\NAA_FC\2019.05.16"
  #  - For Deterministic, is the folder path of the directory with the POR DSS files.
  #      e/g/. baseDir = "\\coe-nwpnv005por.nwp.ds.usace.army.mil\CRT_HH\CRT2014\PDT\WAT\CRSO\Data\HEC-WAT_Det\NAA_FC\27Sep2018_PostVarQBugFix"
  
  #in_paths:
  #  - For FRA, this is a character vector of the extract-formatted paths to read, as would be entered into the
  #     'IN_PATH' column of an extract csv file.
  #     e.g.:
  #     > str(in_paths)
  #     chr [1:2] "//MICA-POWER PLANT/EFFICIENCY//1DAY/FLOODMODEL1/" "//MICA-POWER PLANT/POWER//1DAY/FLOODMODEL1/"
  #  - For Deterministic, this is a character vector of regular exrpressions to be evaluated to find the correct model paths
  #     This is needed because there is no extractConfig.json file that can be used to easily reference the exact paths to read in
  #     e.g., getting Grand Coulee and Mica FLOODMODEL1 pool elevations:
  # > str(in_paths)
  # chr [1:2] "//GRAND COULEE-POOL/ELEV/*/*/*F1*/" "//MICA-POOL/ELEV/*/*/*F1*/"
  
  #isFRA: logical to indicate if this is FRA data or not
  
  options( java.parameters = "-Xms64m" ) #Increasing java JVM heap size for memory limits (not sure this actually helps)
  
  require(dssrip)
  # require(XLConnect)
  require(stringr)
  require(rjson)
  require(readr)
  require(dplyr)
  require(foreach)
  
  
  ###_Establishing Directories and Files #########
  
  extractConfigJsonFile <- paste0(baseDir,"\\extractConfig.json") #for FRA
  
  ### Functions ########################
  
  ###_String Handling ##############
  
  #Extracts from left side of character(similar to Excel function)
  left <- function(text, nchars){
    substring(text, 1, nchars)
  }
  
  #Extracts from right side of character (similar to Excel function)
  right <- function(text, nchars){
    substring(text, nchar(text)-nchars+1, nchar(text))
  }
  
  formFraFpartTemplate <- function(fParts, extractConfig){
    #Input is character vector of F parts from an extract csv e.g. c("FLOODMODEL1","FLOODMODEL0")
    #  and nested extractConfig list read from json file
    #Output is the FRA-formatted version with '%06g' for catalog. e.g.:
    #  //ALBENI FALLS/CONSTRAINTID/01JAN1930/1DAY/C:%06g|NAA_FC:FRA_50_TWM:RESSIM-F0NAFC_F/
    
    matchedFParts <- match(x = fParts, table = names(extractConfig$fPartDictionary))
    
    if( any(is.na(matchedFParts)) )
      warning(sprintf(paste0("\nNo match found in extractConfig.json file for specified F part.",
                             "\nCheck extract csv or extract config json:\n\t%s"),
                      paste0(unique(fParts[is.na(matchedFParts)]),collapse="\n\t")))
    
    out <- sapply(matchedFParts, function(x) extractConfig$fPartDictionary[[x]], simplify=T)
    
    #Setting NULL to NA so they are concatenated and vector length is preserved
    out <- sapply(out, function(x) {if(is.null(x))x<-NA; return(x)} ) 
    
    out[!is.na(out)] <- paste0("C:%06g|",out[!is.na(out)]) #adding catalog component
    
    return(out)
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
    if( all(c(a,b) == "") ) stop("In functiion call: formPaths, need to at least specify an A or B part")
    if( !all(is.character(c(a,b,c,d,e,f))) ) stop("In functiion call: formPaths, all arguments need to be characters or convertible to characters")
    
    return(paste0("/",a,"/",b,"/",c,"/",d,"/",e,"/",f,"/"))
  }
  
  createAllPathsToRead <- function(pathTemplate, extractConfig){
    #pathParts is data frame with the columns 'fPartTemplate' 
    
    sWY  <- extractConfig$startWaterYear
    nWYs <- extractConfig$eventsPerLifecycle
    
    allWYs <- c((sWY-1):(sWY+nWYs-2),(sWY):(sWY+nWYs-1))
    #Creating event-year pairings
    eventYrCombos = data.frame(yr = allWYs[order(allWYs)],
                               event=rep(1:nWYs,each=2))
    
    apply(X = eventYrCombos, MARGIN = 1, FUN = function(x){
      sprintf(pathTemplate,x[1],x[2])
    })
  }
  
  multiCharReplace <- function(chars2Replace, char2Sub, strings){
    #Replaces 1:1 matches in 'chars2Replace' with 'char2Sub' in each element of 'strings'
    #  character vector
    for(k in 1:length(chars2Replace))strings <- gsub(chars2Replace[k], char2Sub[min(k,length(char2Sub))], strings)
    return(strings)
  }
  
  ### _DSS/FRA ####################
  
  getAllFRAPathsToRead <- function(rawPaths){
    #Making all of the paths to be read in at each lifecycle file
    
    #Creating templates out of the paths
    pathParts <- separatePathParts(rawPaths)
    
    pathParts$D <- "01JAN%s" #setting D part to formattable
    
    #making the F part template
    pathParts$fPartTemplate <- formFraFpartTemplate(fParts = pathParts$F, extractConfig = extractConfig)
    
    pathParts <- na.omit(pathParts) #removing paths with bad F parts
    
    #Forming a complete DSS path to use
    pathParts$pathTemplate <- with(pathParts,
                                   formPaths(a=A, b=B, c=C, d=D,e=E,f=fPartTemplate))
    
    #Creating a vector of all the paths to be read in for each lifecycle file
    return(as.character(sapply(pathParts$pathTemplate,createAllPathsToRead,
                               extractConfig=extractConfig)))
  }
  
  compileTSC <- function(paths, dssFile,isfra=T, valueColName="value"){
    #Retrieves all of the paths from the dss file object
    #valueColName is the default column name for the column with data
    #Note: dssFile is the java object of the DSS file as read in by opendss function
    # > str(dssFile)
    # Formal class 'jobjRef' [package "rJava"] with 2 slots
    # ..@ jobj  :<externalptr> 
    # ..@ jclass: chr "hec/heclib/dss/HecDss"
    require(dssrip)
    require(dplyr)
    
    pathSplit <- separatePathParts(paths) #separating path parts
    
    lenPaths <- length(paths)
    cat(sprintf("\nReading %g paths: %s\t%s...",lenPaths,basename(dirname(dssFile$getFilename())),paths[1]) )
    out <- list()
    for( k in 1:length(paths) ) {
      
      if(k %% 1000 == 0) cat(sprintf("\n[%6g/%6g]\t%s\n", k , lenPaths,Sys.time()))
      
      path <- paths[k]
      
      tsc<-try(dssFile$get(path), silent = T)
      
      if(class(tsc) == "try-error") next #skipping if error in read
      if(length(tsc$times)==0) next #skipping if no data
      
      # out[[k]] <- data.frame(date = as.POSIXct(tsc$times * 60, origin = "1899-12-31 00:00", tz = "UTC"),
      #                        aPart = pathSplit$A[k],
      #                        bPart = pathSplit$B[k],
      #                        cPart = pathSplit$C[k],
      #                        ePart = pathSplit$E[k],
      #                        fPart = pathSplit$F[k],
      #                        value = tsc$values,
      #                        stringsAsFactors=F)
      
      getFRAEvent <- function(fParts) str_extract(string =fParts,pattern = "\\d{6}")
      
      out[[k]] <- data.frame(date = as.POSIXct(tsc$times * 60, origin = "1899-12-31 00:00", tz = "UTC"),
                             event = ifelse(isfra,getFRAEvent( pathSplit$F[k] ),NA),
                             value = tsc$values,
                             stringsAsFactors=F)
      
      #Correction for daily data - this is actually wrong.  Don't adjust the timestamp
      # if(unique(pathSplit$E[k])=="1DAY")
      #   out[[k]]$date = out[[k]]$date-86400
    }
    
    out <- bind_rows(out)
    
    names(out)[names(out) == "value"] <- valueColName
    
    return(out)
  }
  
  ###_Metadata Management ############
  
  getEventIds <- function(lc, extractConfig){
    #Gets the appropriate event IDs given the lifecycle number
    nWY <- extractConfig$eventsPerLifecycle
    return(sprintf("%06g",1:nWY + nWY*(lc-1)) )
  }
  
  recursiveDirName <- function(filePath,nDeep){
    #Retrieve the fully qualified parent directory up 'nDeep' levels
    nDeep = nDeep-1
    dirName <- dirname(filePath)
    ifelse(nDeep==0,
           return(dirName),
           return(recursiveDirName(dirName,nDeep)))
  }
  
  checkExistsRead <- function(filePath,readFun,fileArg=""){
    #Check that a file path exists and then performs
    #  a read function on the file
    #If the file needs to be input as a specific argument to the function
    #  that can be defined as a character string passed via 'fileArg' (e.g.,fileArg="file")
    if( !file.exists(filePath) ) stop(sprintf("\nFile not found:\t%s",filePath))
    if(fileArg!="") fileArg = sprintf("%s = ",fileArg)
    filePath <- gsub("\\\\","\\\\\\\\\\",filePath) #buffering with way too many backslashes for proper read
    eval(parse(text = sprintf("readFun(%s'%s')",fileArg,filePath)))
  }
  
  rDataFilesFromPathParts <- function(allPaths){
    out <- NULL
    for( k in 1:nrow(allPaths)) {
      abc=allPaths[k,]
      saveRDataFile <- sprintf("%s\\rdata\\%s__%s__%s",baseFraDir,abc$A,abc$B,abc$C)
      if(abc$A=="") saveRDataFile <- sprintf("%s\\rdata\\%s__%s.RData",baseFraDir,abc$B,abc$C)
      out <- c(out,saveRDataFile)
    }
    return(out)
  }
  
  
  mergeListOfDataFrames <- function(list1, list2){
    #Inputs are two lists of dataframes with identical names and columns in each df
    #Output is a single list of dataframes, with the rows binded between the 
    #  matching dataframes in the list element (i.e., how you'd want to merge two similar lists
    #  of dataframes)
    Map(bind_rows, list1, list2)
  }
  
  ###_For Deterministic ##########
  
  glob2rx_plus <- function (pattern, trim.head = FALSE, trim.tail = TRUE) {
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
  
  splitByUniqueABCEF <- function(pathsToRead){
    #There is a chance that when using regular expressions, different project data could be assigned to 
    #  the same dataframe, which is dangerous.  e.g., if in_path is '/*/*/*/*/*/*' (all wildcard), then literally
    #  all of the data in the DSS file will be merged into one dataframe.  So, this step filters out uniqe 
    #  paths by A, B, C, E, and F parts
    
    splitPaths <- separatePathParts(pathsToRead)
    
    splitPaths$newFPart <- sapply(splitPaths$F,makeNewDeterminsticFParts,simplify = T)
    
    #checking if there are more than one unique A,B, C, or E parts of all paths
    uniqueABCEF <- unique(splitPaths[,c("A","B","C","E","newFPart")])
    
    if( nrow(uniqueABCEF) != 1 ){
      #If not, the need to form new list elements that contain all of the unique data
      out <- list()
      for( v in 1:nrow(uniqueABCEF) )
        #Extracting paths to be read and adding to their own element in a list
        out[[v]] <- with(uniqueABCEF[v,],splitPaths$PATH[splitPaths$A==A &
                                                      splitPaths$B==B &
                                                      splitPaths$C==C & 
                                                      splitPaths$E==E &
                                                      splitPaths$newFPart==newFPart])
      return(out)
    }else{
      return(list(pathsToRead)) #return input, 
    }
    
  }
  
  makeNewDeterminsticFParts <- function(fPart){
    #Takes out the POR-unique identifier in the deterministic path's F part
    #  e.g., 'NAA_FC:POR 92-08:RESSIM-F1NAFC' becomes NAA_FC:RESSIM-F1NAFC
    splitParts <- str_split(string = fPart, pattern = ":",simplify = T)
    #If there aren't 3 elements in split string, then just return the original
    #  b/c unsure what to do with it
    ifelse(length(splitParts)==3,paste0(splitParts[c(1,3)],collapse=":"),fPart)
  }
  
  #Rename list elements to standardized format
  makeStdDeterministicNames <- function(allPathsToRead){
    #Expecting a list of strings containing paths that have already been split to 
    #  be unique by ABCE components.  Takes the first string (b/c it should match
    #  all others in the vector) and developes a standardized name to be saved
    #  to in the SQL output tbl
    firstPaths <- sapply(allPathsToRead, function(x) x[1], simplify=T)
    firstPathsSplit <- separatePathParts(firstPaths)
    
    newFParts <- sapply(firstPathsSplit$F, makeNewDeterminsticFParts,simplify = T)
    
    return( with(firstPathsSplit,
                 formPaths(a = A, b = B, c = C, e = E,
                           f=newFParts)) )
    
  }
  
  filterPathsWithRegExStrings <- function(regExStrings,paths)
    sapply(regExStrings, function(x) fullPathByRegex(paths = paths, pattern = glob2rx_plus(x)),simplify=F)
  
  ### Establishing paths and files to Read ################
  #Rolling out one big dataframe the defines the files, their associated
  #  paths to be read, and also checks for existence of those files
  
  #Reading in json file
  if( isFRA ) {
    #Reading FRA
    #  - find all lifecycle DSS files
    #  - guess at paths to be read in, informed by extractconfig.json file
    #  - iterate through each lifecycle file, reading in list of paths
    #    - assign each unique path (i.e., unique in_paths element) to dataframe in list
    #    - rename F parts to correspond to FRA unique events (e.g., 1:5000)

      extractConfig <- checkExistsRead(filePath = extractConfigJsonFile,  readFun = fromJSON, fileArg="file")
      allPathsToRead <- sapply(unique(in_paths), getAllFRAPathsToRead, simplify=F)
      
      
      #Retrieving the lifecycle DSS files
      lifecycleDSSFiles <- with(extractConfig,
                                dir(path = sprintf("%s\\%s",baseDir,analysisPeriod),
                                    pattern = sprintf("%s-%s.dss",watAlt,analysisPeriod),
                                    full.names = T, recursive = T))
      #Reducing only to files inside of lifecycle folders
      lifecycleDSSFiles <- lifecycleDSSFiles[grepl("lifecycle",lifecycleDSSFiles)]
      
      if(length(lifecycleDSSFiles) == 0)
        with(extractConfig,
             stop(sprintf("\nNo lifecycle DSS files found.  Expecting lifecycle files to have the following name:\t%s",
                          sprintf("%s-%s.dss",watAlt,analysisPeriod))))
      
      #Ordering by lifecycle
      lifecycleDSSFiles <- lifecycleDSSFiles[order(as.numeric(gsub("lifecycle ","",basename(dirname(lifecycleDSSFiles)))))]
      
      cat(sprintf("\n\nReading the following files:\n\t%s\n\n",paste0(lifecycleDSSFiles,collapse="\n\t")))
      
      ### Iterating through lifecycle files #############

      tsc <- foreach(lifecycleDSS = lifecycleDSSFiles,.combine = mergeListOfDataFrames) %do% {
        
        dssFile <- try(opendss(filename = lifecycleDSS,F,T))
        
        if(class(dssFile) == "try-error") return(NULL) #if file wrong version or corrupted
        
        #Reading in TSCs
        rawTSC <- sapply(allPathsToRead, compileTSC,isfra=T,dssFile=dssFile, simplify=F)
        
        #pulling out lifecycle number 
        lcString <- basename(recursiveDirName(lifecycleDSS,1))
        lc <- as.numeric(gsub("lifecycle ","",lcString))
        
        #Renaming F parts so they are in sequence (e.g., lifecycle 2 event IDs are 51:100, not 1:50)
        oldEventIDs <- unique(rawTSC[[1]]$event)
        
        newEventIDs <- getEventIds(lc, extractConfig) #6-digit character strings
        
        #Performing replacement
        rawTSC <- sapply(rawTSC,
                         function(x) {x$event <- as.numeric(newEventIDs[match(x = x$event,table = oldEventIDs)]); return(x)},
                         simplify=F)
        
        dssFile$close()
        
        return(rawTSC)
      } #End foreach loop

    
  }else{
    #Reading deterministic
    #  - find all DSS files
    #  - iterate through each DSS file
    #    - read in all paths from DSS catalog files
    #    - subset to only paths that match in_paths (via regular expressions)
    #    - assign each unique path (i.e., unique in_paths element) to dataframe in list
    
    deterministicDSSFiles <- dir(path = baseDir, pattern = "*.dss", recursive = T, full.names = T)
    
    #There are lots of other DSS files that are in deterministic computes, but assuming that the
    #  ones we want to read are down two directories from the base directory fed into this function
    # e.g., if the file we want to read is:
    #   "D:\\crt_crso\\dss\\deterministic\\NAA_FC\\80yr/POR_29-47/NAA_FC-POR_29-47.dss"
    #  then up two levels from that is the 'baseDir' folder:
    #   "D:\\crt_crso\\dss\\deterministic\\NAA_FC\\80yr"
    #Looking up two directories from selected DSS files and checking that they match the base directory
    deterministicDSSFiles <- 
      deterministicDSSFiles[recursiveDirName(deterministicDSSFiles,2) == gsub("\\\\","/",baseDir)]
    cat(sprintf("\n\nReading the following files:\n\t%s\n\n",paste0(deterministicDSSFiles,collapse="\n\t")))
    
    tsc <- foreach(dssFileName = deterministicDSSFiles,.combine = mergeListOfDataFrames) %do% {
      
      dssFile <- try(opendss(filename = dssFileName,F,T))
      
      if(class(dssFile) == "try-error") return(NULL) #if file wrong version or corrupted
      
      allPaths <- separatePathParts(getAllPaths(dssFile))
      
      #First, get all possible matches given the input regular expressions
      allPathsToRead <- filterPathsWithRegExStrings(in_paths,allPaths$PATH)
      
      #Parse out by unique ABCEF parts (ignoring D) - also, the F parts being compared  
      #  aren't the raw F part, but one that has the period of record indicator
      #  removed (e.g., 'NAA_FC:RESSIM-F1NAFC', not 'NAA_FC:POR 29-47:RESSIM-F1NAFC')
      allPathsToRead <- sapply(allPathsToRead,splitByUniqueABCEF)
      
      #Renaming with standardized deterministic path names.  e.g, 'NAA_FC:RESSIM-F1NAFC', not 'NAA_FC:POR 29-47:RESSIM-F1NAFC'
      names(allPathsToRead) <- makeStdDeterministicNames(allPathsToRead)
      
      #Reading in TSCs
      rawTSC <- sapply(allPathsToRead, compileTSC,isfra=F,dssFile=dssFile, simplify=F)
      
      dssFile$close()
      
      return(rawTSC)
    } #End foreach loop
  }

  #Assigning metadata attributes (why not)
  attr(x = tsc, which = "baseDir") <- baseDir
  attr(x = tsc, which = "in_paths") <- in_paths
  
  return(tsc)
}


### Testing #######

# tsc <- extractFRAwithExtractCSV(baseFraDir = baseFraDir, extractCSVFile = extractCSVFile)
