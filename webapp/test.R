

# cat(sprintf("\n\nCurrent working directory:\n\t%s",getwd()))

suppressPackageStartupMessages(require(shiny,quietly = T))

#Retrieving the arguments passes via batch file and assigning to objects
args = commandArgs(trailingOnly=TRUE)
defaultHost <- args[1] #as string
defaultPort <- as.integer(args[2]) #needs to be integer
baseJavaDir <- args[3]

#if launching from different directory
if(basename(getwd()) == "webapp") setwd("..")

source("scripts/r_setup.R")


cat("\n\nThe test worked!\n\n")