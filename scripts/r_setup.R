
"%!in%" <- function(x,y) !(x %in% y) #not in operator

options(stringsAsFactors=F)
options(max.print=1000) #limiting number of printouts for lists

cat("\nrunning r_setup")

### Java Configuration ######################
#Establishing java configuration prior to loading dssrip

options( java.parameters = "-Xms64m" )

#For testing - only using libraries from Y drive to ensure portable
if(Sys.getenv("USERNAME") == "g4ecjrsw") .libPaths("Y:\\Dataprocessing_tools\\resources\\R-3.4.2\\library")


if(R.Version()$arch=="x86_64"){
  # use 64-bit .jar and .dll
  #Linking to 64-bit java on 'V' drive
  #baseJavaDir = "\\\\nww-netapp1\\prjmgnt\\CWMS\\software\\java64"
  # baseJavaDir = "D:\\Programs\\CWMS-v3.1.1.126\\HEC-ResSim\\3.4"
  
  options(dss_location=baseJavaDir )
  options(dss_jre_location= paste0(baseJavaDir ,"\\java") )
  
  Sys.setenv(JAVA_HOME= paste0(baseJavaDir, "\\java") )
  
} else {
  stop("\n\nThis script needs to be ran using 64-bit R\n\n")
}

#QC checks for java and resource directories
if(!dir.exists(baseJavaDir)) stop(sprintf("\n\nExpected java directory could not be found here:\n\t%s",baseJavaDir))


#Establishing environmental variables
options(dss_location=baseJavaDir )
options(dss_jre_location= paste0(baseJavaDir ,"\\java") )
Sys.setenv(JAVA_HOME= paste0(baseJavaDir, "\\java") )


### Loading Packages ##############

library(dssrip) #Loading dssrip first

findPckgs <- function(scriptText){
  scriptText <- scriptText[!grepl("^#",gsub(" ","",scriptText))] #skipping commented packages
  #searching for require and library formatting
  #NOTE: only works if there isn't a comment after the load call
  scriptText <- scriptText[grepl("require\\(.*\\)$|library\\(.*\\)$",scriptText)]
  unique(gsub("require|library|\\(|\\)| ","",scriptText)) #scrubbing code to just the package name
}

findAllPckgs <- function(){
  #Looking at scripts to find needed packages
  scriptTextList <- sapply(dir("scripts",".R$",recursive = T,full.names = T),readLines)
  scriptTextList <- scriptTextList[!grepl("archive",names(scriptTextList))]
  lapply(scriptTextList,findPckgs)
}

pckgsList <- findAllPckgs()

pckgsToLoad <- unique(unlist(pckgsList))

#Or just used a manual entry list:
# pckgsToLoad <- c('lubridate', 'sqldf', 'RSQLite',
#                  'rjson', 'readr', 'dplyr',
#                  'foreach', 'dssrip', 'purrr',
#                  'reshape2', 'stringr', 'XLConnect',
#                  'tcltk')


missingPckgs <- pckgsToLoad[pckgsToLoad %!in% rownames(installed.packages())]

if( length(missingPckgs) > 0  ){
  missingPckgList <- pckgsList[sapply(pckgsList,function(x) any(missingPckgs %in% x))]
  missingPckgList <- lapply(missingPckgList,
                            function(x) {x[grepl(paste0(missingPckgs,collapse="|"),x)] <- 
                              paste0(x[grepl(paste0(missingPckgs,collapse="|"),x)]," <- missing"); return(x) })
  Map(f = function(x,y) cat(sprintf("\n%s\n\t%s\n",x,paste0(y,collapse="\n\t"))),
      names(missingPckgList),missingPckgList)
  stop(sprintf("\n\nMissing required packages in R installation:\n\t%s",
               paste0(missingPckgs,collapse = "\n\t")) )
}

suppressPackageStartupMessages( #loading all packages
  a <- sapply(pckgsToLoad,library,verbose=F, character.only=T)
)


### Sourcing all Utility scripts ###################

#sources all R scripts in the scripts/utils without the word 'test' in them
suppressPackageStartupMessages(
a <- sapply(grep("^(?=.*R)(?!.*test)", dir("scripts/utils",full.names = T,recursive = T), perl=TRUE,value = T),
            source))




