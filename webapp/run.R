#opening shiny app for cwms esp viewer from batch file

options(warn=-1)
suppressPackageStartupMessages(library(shiny))
options(warn=1)

#Retrieving the arguments passes via batch file and assigning to objects
args = commandArgs(trailingOnly=TRUE)
defaultHost <- args[1] #as string
defaultPort <- as.integer(args[2]) #needs to be integer
baseJavaDir <- args[3]

#Location of shiny app
dirName = sprintf("%s/webapp", getwd())

#running app
runApp(appDir = dirName, launch.browser=F,
       port = defaultPort, host = defaultHost,
       quiet = T)

