#Checks config, data formatting, etc. to ensure tool will run properly


startup_checks <- function(whichSection,...){
  
  outMsg=""
  failed=F
  
  args <- list(...) #converting input to ist
  
  #For debug, shows all arguments passed by name
  cat(sprintf("\nstartup_checks\t%s\nArgs:\n\t%s",
              whichSection,
              paste0(names(args),collapse="\n\t")))
  
  ### Connections #########################################
  
  if(whichSection=="Connections"){
    
    #No NA values in all but "Notes" and "connection_name" columns
    if( any(is.na(conns[, names(conns) %!in% c("Notes","connection_name")])) )
      stop("\nBlank cells are only allowed in 'Notes' and 'connection_name' columns.  Check config/connections.xlsx")
    
    #Check that there are no duplicate connection names and indicate those that don't match
    dupConns <- conns$connection_name[duplicated(conns$connection_name)]
    if( length(dupConns) > 0 )
      stop(sprintf("\nConnection name '%s' is duplicated in the connections.xlsx file.  Connection names need to be unique",
                   dupConns))
    

    #Connections exist.  Ignore if they don't
    allFiles <- conns$file_path
    nonExistentFiles <- file.exists(allFiles) & !dir.exists(allFiles) #also checks not a directory
    if( sum(!nonExistentFiles) > 0 ){
      outMsg = sprintf("\nNon-existent file(s) provided or direcory set in 'file_path' of Check config/connections.xlsx.  These alternative data will be skipped:\n\t%s",
                      paste0(nonExistentFiles,collapse="\n\t"))
      conns <- conns[conns$dir_path %!in% nonExistentFiles,]
      failed=T
    }
    
    #Check correct pairings of file_format and compute_type
    #  Currently accepted pairings are:
    acceptableConnectionPairings <- data.frame(file_format= c("SQL",  "SQL",   "DSS",  "DSS"),
                                               compute_type=c("FRA5k","FRA50k","FRA5k","Deterministic"),
                                               stringsAsFactors = F)
    #  Checking that all connections match a configuration in the acceptable pairings df
    validConnectionPairings <- pmap_lgl(conns,function(file_format,compute_type,...){
      any(acceptableConnectionPairings$file_format %in% file_format  &
            acceptableConnectionPairings$compute_type %in%  compute_type   )
    })
    
    if(any(!validConnectionPairings)){
      out=sprintf(paste0("\nInvalid 'file_format' and 'compute_type' pairing for ",
                             "connection(s):\n\t%s\n\nAcceptable parings are:\n"),
                      paste0(conns$connection_name[!validConnectionPairings],collapse="\n\t"))
      print(as.matrix(acceptableConnectionPairings))
      failed=T
      stop()
    }
    
    ### Forecast and HS ####################
  }else if(whichSection=="Forecast and HS"){
    
    #Check that all of the 

    ### All Data ####################
  }else if(whichSection=="All Data"){
    
    
    # No mixing and matching of FRA and deterministic data for given alternatives - must use different connection name
    #Prompt about appropriate way to name deterministic alts (e.g., 'NAA_FC (Det)')
    for( alt in names(allData$meta)){
      if( length(allData$meta[[alt]]$compute_type) > 1 | length(allData$meta[[alt]]$file_format) > 1 ){
        outMsg=sprintf(paste0("\nstartup_checks (%s):\tMore than one type of",
                           " 'compute_type' or 'file_format' specified for alternative '%s'.",
                           "Removing this alternative from plotting\n\tCheck connections.xlsx.",
                           "  If an alternative has both deterministic and FRA data, use different",
                           " connection names (e.g., 'NAA_FC' and 'NAA_FC (Det)"),
                    whichSection, alt)
        conns <- conns[conns$alt != alt,]
        failed=T
      }
    }
      
    
    ### createDataset #######
  }else if(whichSection == "createDataset Base Year"){
    
     
    if( length(args$plotInfo$baseYr) ==0 ){ #no base year selected
      outMsg = "\nNeed to select a base year"
      failed=T
    }
    
    
    ### makePlotly ##############
  }else if(whichSection == "makePlotly"){
    
    #No path selected to plot
    # if( length(args$opts$selectedPaths) ==0 ){
    #   outMsg="\nMust select path(s) to plot"
    #   failed=T
    # }
    # 
    failed =F
    
    ### No checks defined ###############
  }else{
    #No checks for this section yet - assume all good
    failed =F
    
  }
  
  #Just outputting to the console for now
  if(outMsg != "") cat(outMsg)
  
  return(list(failed=failed,outMsg=outMsg))
}
