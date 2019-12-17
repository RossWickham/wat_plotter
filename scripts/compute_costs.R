#Computes the $ amount associated with the power market, using 


source("webapp/global.R")

mp <- getMP()


### MAIN ######################

#For testing
# i=3

#Loop for iterating through files
for( i in 2:3  ){
  file_path <- conns$file_path[i]
  cat(sprintf("\nComputing costs for %s", file_path))
  #compute costs and cumulative costs by event/wy
  allData <- computePrices_fileWrapper(file_path, allData)
  saveDataWrapper(file_path, allData) #save out to file
}


### FIXING MISTAKES ####################


# deleting bad data
deleteTbls <- F

if( deleteTbls ){
  sqlDB <- dbConnect(SQLite(), dbname = file_path)
  tblNames <- RSQLite::dbListTables(sqlDB) 
  deleteTbls <- grep(pattern = "/COST",x = tblNames,value = T)
  cat(sprintf("\nDeleting from %s:\n\t%s", file_path, paste0(deleteTbls,collapse="\n\t")))
  for(tblName in deleteTbls) dbRemoveTable(conn = sqlDB, name = tblName)
  dbDisconnect(sqlDB) #close connection
}

test <- dbReadTable(sqlDB,"addlInfo_1826300")
