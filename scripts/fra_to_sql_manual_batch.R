#Takes user input about CSV files and the FRA directory and 
#  converts lifecycle DSS files to SQL, saving in the same
#  FRA directory under an 'sql' folder. SQL datasets will have
#  a standard structure, with 'date', 'event', and 'value'
#  columns

#Function dependencies:
# chooseFRADir and chooseExtractCSV function in :
#   scripts/utils/input.R

#gatherpathsToSQL:
#   scripts/utils/gatherpaths_to_sql_2019-08-23.R
#   scripts/utils/r_gatherpaths_2019-07-30.R


#going up one level if running from scripts directory
# (i.e., executed from .bat file in scripts directory)
if(basename(getwd()) == "scripts") setwd("..")

# source("scripts/r_setup.R") #source configuration setup
source("webapp/global.R")

### Main #########

#get FRA directory path
# baseDir <- chooseFRADir()
baseDirs <- c(
  # "Y:\\CRT\\SISSyNP",
  # "Y:\\Joint_Modeling\\JM_COE_FRM\\2018-05-04_NewCRCs",
  # "Y:\\CRT\\JM_COE_NP\\2019-01-22",
  "Y:\\CRSO\\PA\\09Dec2019",
  "Y:\\CRSO\\NAA_FC\\2019.05.16"
  )

#Get extract CSV(s) file path
# extractCSVFiles <- chooseExtractCSV(baseDir) 
# extractCSVFiles <- "Y:\\CRT\\SISSyNP\\extract_URCs_with_shifted_and_gatherpaths.csv"
extractCSVFiles <- "Y:\\Dataprocessing_tools\\wat_plotter\\config\\extract_crso_gatherpaths.csv"

#Convert to SQL where 'gatherpaths' listed as command in extract CSV(s)
for(baseDir in baseDirs){
  gatherpathsToSQL(baseDir,extractCSVFiles,T)
  gc()
  }











