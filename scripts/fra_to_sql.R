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

source("scripts/r_setup.R") #source configuration setup

### Main #########

baseDir <- chooseFRADir()                    #get FRA directory path
extractCSVFiles <- chooseExtractCSV(baseDir) #Get extract CSV(s) file path

#Convert to SQL where 'gatherpaths' listed as command in extract CSV(s)
gatherpathsToSQL(baseDir,extractCSVFiles,T)  
