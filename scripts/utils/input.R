library(tcltk)

### Fra to SQL ##############################

chooseFRADir <- function(){
  #Selects a directory
  root = tktoplevel("width" = 1, "height" = 1)
  tkraise(root)
  Sys.sleep(2) #pause just a little for dailogs
  selectedDir <- 
    tclvalue(tkchooseDirectory(
      initialdir="\\\\coe-nwpnv005por.nwp.ds.usace.army.mil\\FRA_Current",
      title="Select the main FRA Directory (one level up from extractConfig.json)"))
  tkdestroy(root)
  if( selectedDir == "" )  stop("\nCancelled FRA directory selection")
  return(selectedDir)
}


chooseExtractCSV <- function(baseDir){
  #Selects a csv file given the starting directory for search
  root = tktoplevel("width" = 1, "height" = 1)
  tkraise(root)
  Sys.sleep(1)
  extractCSVFiles <- 
    tk_choose.files(default = paste0(baseDir,"/extract.csv"),
                    caption=paste0("Select the extract csv files with gatherpaths",
                                   " commands to dump to SQL (Hold Ctrl to Select Multiple)"),
                    filters=matrix(c("CSV", ".csv"),byrow = 2,ncol=2))
  tkdestroy(root)
  if( length(extractCSVFiles) == 0 ) stop("\nCancelled extract csv file selection")
  return(extractCSVFiles)
} 



