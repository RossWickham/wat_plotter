
cat("\nrunning global")
options(warn=-1) #no warnings, they clutters command prompt window.  output uses 'cat'

"%!in%" <- function(x,y) !(x %in% y) #not in operator

#assuming in webapp folder if not in base project directory
if("scripts" %!in% dir()) setwd("..") 

#Testing - only using libraries from Y drive to ensure portable
if(Sys.getenv("USERNAME") == "g4ecjrsw") .libPaths("Y:\\Dataprocessing_tools\\resources\\R-3.4.2\\library")


#loading from project main directory
suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(plotly)
  source("scripts/r_setup.R")
})


### Connections ######################

#Deterministic results DSS data
dssConns <- getConnections("DSS_Connections")

#FRA results directories
fraConns <- getConnections("SQL")

### Metrics #####################

#The available quantiles for the dropdown menu, and their associated line types for plotly
metricsToCompute <- data.frame(metric=    c("min", "1%",        "10%", "25%",    "median",
                                            "mean",    "75%",    "90%", "99%",        "max"),
                               #see the plotly reference here for the line type to be used under the scatter>lines>dash
                               #options are: "solid", "dot", "dash", "longdash", "dashdot", or "longdashdot"
                               lty_plotly=c("dot","longdashdot","dash","dashdot","solid",
                                            "longdash", "dashdot","dash","longdashdot","dot" ),
                               stringsAsFactors = F)

#Retrieving the base R equivalent line types from plotly (this function defined in plot.R)
metricsToCompute$baseRLty <- plotlyLtyToGraphicsLty(metricsToCompute$lty_plotly)



### PDF Save Settings ########

#color and line type settings for minor grid line
minGridCol= grey(0.3,0.3)
minLty = 3


### Forecast and HS  ###############

#hydrologic sampler event-year pairing
hsWb <- loadWorkbook("config/HS_Event_Year_Mapping.xlsx")
eventYr <- list()
eventYr[["fiveK"]] <- readWorksheet(hsWb,"event-year (5k)")
eventYr[["fiftyK"]] <- readWorksheet(hsWb,"event-year (50k)")

#Reading in deterministic probabilities
eventYr[["deterministic_probs"]] <- readWorksheet(hsWb,"probs")

#Retrieving forecast datasets
fcsts <- loadFcsts()


### WY Ranking ##############################

#wyRanking:
#list various probability groupings, add as needed
# category   = names of the groups
# percentage = incremental percentage of each group
# html_lty   = plotly line type
# lty        = R plot line type (also for pdf)
wyRanking <- list(
  #5 groupings of 20% 
  five_groups = data.frame(category=c('<20%','20-40%','40-60%','60-80%','>80%'),
                           percentage=rep(20,5),
                           html_lty=c("dash","dot","solid","dot","dash"),
                           lty=c(3,2,1,2,3),
                           stringsAsFactors = F),
  #3 groups: <20% (dry); 20-80% (avg); >80% (wet)
  three_groups =  data.frame(category=c("dry","average","wet"),
                             percentage=c(20,60,20),
                             lty=c("dash","solid","dot"),
                             stringsAsFactors = F)
)


### F Part Handling ###########################################

#modelDict and replaceStringFromDict:
#Defines how name matching is done between the POR and FRA paths or
#  merged QA metrics files.  e.g., this helps the mapping between
#  these two pathnames:
# deterministic, WAT output: //ALBENI FALLS_IN/FLOW/01JAN1928/1DAY/TTPC:POR 29-47:RESSIM-F1PCMF/
# fra/merged:                //ALBENI FALLS_IN/FLOW/01JAN1928/1DAY/TTPC~FLOODMODEL1/
#
#Info extracted from WAT watesrhed: CRSO_Master_2019-05_17_RAGNAROK_kNNo
# Script:    runs/mergeResultsWithDatumShiftUserInput.py (line 17, 'modelDict' object):
# modelDict = {"URC BASE":"URCBASE", "ECC BASE":"ECCBASE", "F0":"FLOODMODEL0",  \
#   "P1":"POWERMODEL1", "F1":"FLOODMODEL1", "P2":"POWERMODEL2",
#   "Fx0": "BASIC_FLOOD", "C1": "CANADIAN_OPS", "D1": "DAILY_OPS"}
#
#If no match in modelDict, the logic is to simply carry the name forward. See 'replaceStringFromDict' function:
# def replaceStringFromDict(stringObj, dictObj):
#   # If stringObj starts with any one of the keys of dictObj, return the value corresponding to the key
#   # otherwise, return the original string
#   for key in dictObj.keys():
#   if stringObj.find(key) == 0: # starts with the key
#     stringObj = dictObj[key]
#     break 
#   return stringObj
#
modelDict <- data.frame( #converting to R dataframe
  fPartDictionary = c('FLOODMODEL0', 'FLOODMODEL1', 'POWERMODEL1',
                      'URC BASE', 'URC ADJ', 'ECC BASE',
                      'URC GCL DWR SHIFT', 'HYDROLOGIC SAMPLER', 'HYDRO SAMPLE',
                      'YAKIMAMODEL', 'UPPERSNAKEMODEL', 'FIA_MODEL',
                      "FIA_SPRING", "FIA_WINTER",'KNNMODEL',
                      "BASIC_FLOOD", "CANADIAN_OPS", "DAILY_OPS"),
  watFPart = c("F0","F1","P1",
               "URC-BASE", NA, "ECC BASE",
               "URC-GCL DWR SHIFT", NA, NA, #URC GCL, Hydro Samplers
               NA, NA, NA, #YAKIMA, Upper Snake
               NA, NA ,NA, #FIA, KNN
               "Fx0", "C1", "D1"),
  stringsAsFactors=F 
)

replaceStringFromDict <- function(fPartDictionary){
  #Performs matching between F part in FRA runs and F part in POR DSS runs
  watFPart <- modelDict$watFPart[modelDict$fPartDictionary==fPartDictionary]
  if( length(watFPart) == 0 ) watFPart <- NA #setting to NA if no match
  if( is.na(watFPart)  )
    warning(sprintf(paste0("\n\tNo F part match could be found for model",
                           " '%s' in 'modelDict' object (defined in r_setup.R script).\n\t",
                           "  Using original string ('%s') to match F parts in in DSS file."),
                    fPartDictionary,fPartDictionary))
  ifelse( is.na(watFPart),fPartDictionary, watFPart)
}



### Plotly Settings ######################

#margins.  See layout>margins in plotly references:
#  https://plot.ly/r/reference/#layout-margin
#  https://plot.ly/r/setting-graph-size/
#Setting margin in pixels
pMargins <- list(l=100)


### Startup Checks #########################
#Scripts to check that necessary data are available and config is defined correctly
# source("scripts/startup_checks.R")

cat("\n\nBrowser is ready - refresh or open new browser session")
