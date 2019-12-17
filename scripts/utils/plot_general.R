#Functions that are useful for both plotly and non-plotly plots


### Colors ################################################

colToHex <- function(colName, alpha=0.5) mapToRGBA(col2rgb(col = colName),alpha=alpha)

mapToRGBA <- function(rgbaVector,alpha){
  #input is a vector defining the red, green, blue,
  #  and alpha values (in that order) to be passed
  #  to the rgb function
  rgb(rgbaVector[1]/255, rgbaVector[2]/255, rgbaVector[3]/255, alpha)
}


### Line Type #############################################


# forming colors for given set of inputs using Set1
getColor <- function(currentSel,allSel){
  #Getting the full pallette given current selection and all selections
  pal <- RColorBrewer::brewer.pal(length(allSel),"Set1")
  pal[which(currentSel==allSel)]
}

getLty <- function(plotType,plotColName,opts,htmlLty=T){
  #Given the plot type and the name of the column being plotted,
  #  returns the line type
  #Plot types: "Summary Hydrograph","Quintile By Forecast",
  #   "Base Year","Frequency","Metric vs Forecast"
  #
  #By default returns the line type as used in plotly,
  #  otherwise returns line type for R base plots
  
  if( plotType %in% c("Base Year","Frequency")){
    if(htmlLty){
      outLty <- "solid"
    }else{
      outLty <- 1
    }
  }else if( plotType %in% c("Summary Hydrograph")){
    
    if(htmlLty){
      outLty <- metricsToCompute$lty_plotly[metricsToCompute$metric==plotColName]
    }else{
      outLty <- metricsToCompute$baseRLty[metricsToCompute$metric==plotColName]
    }
    
  }else if( plotType %in% "Quintile By Forecast"){
    
    if(htmlLty){
      outLty <- wyRanking[[opts$wyGroup]]$lty_plotly[wyRanking[[opts$wyGroup]]$category==plotColName]
    }else{
      outLty <- wyRanking[[opts$wyGroup]]$baseRLty[wyRanking[[opts$wyGroup]]$category==plotColName]
    }
    
  }
  return(outLty)
}
