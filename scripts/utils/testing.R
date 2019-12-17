



getTestInput <- function(plotType){
  #Creates input for testing
  # opts:
  # fcstProj
  # plotType
  # durTW
  # mFcstProj
  # fcstDate
  # saveHTMLButton
  # wyGroup
  # refreshButton
  # freqMetric
  # plotTemplateName
  # freqTW
  # savePDFButton
  # metrics
  # mFreqMetric
  # mFreqTW
  # baseYr
  # selectedPaths
  # selectedAlts
  # plotTitle
  # mFcstDate
  
  #basic info
  input <- list()
  input$selectedAlts <- c("JM_COE_NP (Det)",  #deterministic
                          "JM_COE_FRM") #FRA
  input$selectedPaths <- c("//ARROW LAKES-POOL/ELEV//1DAY/FLOODMODEL1/")
  input$plotTitle <- "Test Plot"
  input$plotTemplateName <- "testing"
  
  input$saveFileName="text.xlsx"
  
  #assigning plot type
  input$plotType <- plotType
  
  #frequency
  input$freqTW <- c("Jun","Jul")
  input$freqMetric <- "max"
  
  #duration
  input$durTW <- "Jun"
  
  #metric vs forecast
  input$mFreqTW <- "Jun"
  input$mFreqMetric <- "max"
  input$mFcstProj <- "THE DALLES_IN"
  input$mFcstDate <- "30Apr"
  
  #base year
  input$baseYr <- c(1929,1996)
  
  #quintile by forecast
  input$fcstProj <- "THE DALLES_IN"
  input$fcstDate <- "30Apr"
  input$wyGroup <- "five_groups"
  
  #summary hydrographs
  input$metrics <- c("10%","median","90%")

  return(input)
}

