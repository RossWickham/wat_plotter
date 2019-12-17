


### Plotly General ##############################################

copyfun <- function(f, ...){
  l <- pairlist(...)
  a <- formals(f)
  n <- names(l)[names(l) %in% names(a)]
  a[n] <- l[n]
  formals(f) <- a
  f
}

makePlotly <- function(allData, plotData, opts,showProgress=F){
  #Iterate thoguh each plot, and create plotly subplot
  
  # sChecks <- startup_checks("makePlotly",allData=allData, plotData=plotData, opts=opts)
  # 
  # if(sChecks$failed){
  #   cat(sChecks$outMsg)
  #   return(plot_ly())
  # }
  
  #Increment to increase load bar
  #1/(<no. plots>*<no. alts>) : ignoring number of paths
  incPerPath <- 1/(length(plotData$meta)*length(opts$selectedAlts)*2)
  
  pList <- list() #initializing output plotly
  
  init <- F #have initialized plotly yet?
  for( k in 1:length(plotData$meta) ){   #iterate through each plot
    plotInfo <- plotData$meta[[k]]
    pList[[k]] <- plot_ly()
    
    for(alt in opts$selectedAlts){ #  and each alternative
      
      # for(path in plotData$meta)
      #extracting plotType and computeType
      plotType <- plotInfo$plotType
      computeType <- allData$meta[[alt]]$compute_type
      
      #call function by name onto the dataset, returns dataframe
      plotFunName <- gsub(" ","",sprintf("plotly_%s_%s", plotType,computeType))
      
      cat(sprintf("\nplotFunName:\t%s",plotFunName))
      if( !exists(plotFunName) ){
        cat(sprintf("\nPlotly function for plot type '%s' and compute type '%s' does not yet exist, skipping",
                    plotType, computeType))
        next
      }
      
      
      #Functions defined under associated section in 'plot_plotly' script
      pList[[k]] <- eval(call(name =plotFunName,pList[[k]],plotData,alt,plotInfo,init))
      pList[[k]] <- addXaxisLayout(pList[[k]],plotInfo)

      
      if(showProgress) incProgress(incPerPath)
    }
    init <- T
  }
  
  cat(sprintf("\nlength(pList):\t%g", length(length(pList))))
  
  
  #merging into subplot
  # if(length(pList)==1){
  #   return(pList[[1]])
  # }else{
    return(
      plotly::subplot(pList,nrows = length(pList),
                      shareX = T, titleY = T, widths = 1)
    )
  # }
}


### xaxis layout ##########################################


addXaxisLayout <- function(inP,plotInfo){
  plotType <- plotInfo$plotType
  
  
  if( "yLab" %!in% names(plotInfo)) plotInfo$yLab <- extractPathParts(plotInfo$paths[1],"abc")
  
  if( plotType %in% c("Summary Hydrograph", "Quintile By Forecast", "Base Year")){
    #water year starting 3000-10-01.  Use 'convertDFToSameWY' to crate a
    #  'wyDate' column that starts in water year 3001

      inP <- inP %>% plotly::layout(xaxis=list(title="",tickformat="%b %d", tick0="3000-10-01", dtick="M1"),
                             yaxis=list(title=plotInfo$yLab),
                             title=plotInfo$plotTitle)
    
  }else if( plotType %in% c("Frequency")){
    
    inP <- inP %>% plotly::layout(xaxis=list(title="Annual Chance Exceedance (%)"),
                           yaxis=list(title=plotInfo$yLab),
                           title=plotInfo$plotTitle)
    
    
  }else if( plotType %in% "Duration"){
    
    inP <- inP %>% plotly::layout(xaxis=list(title="Percent of Time Exceeded"),
                           yaxis=list(title=plotInfo$yLab),
                           title=plotInfo$plotTitle)
    
  }else if(plotType %in% "Metric vs Forecast"){
    
    inP <- inP %>% plotly::layout(xaxis=list(title=sprintf("%s, %s Forecast (Maf)",
                                                    plotInfo$mFcstProj, plotInfo$mFcstDate)),
                           yaxis=list(title=plotInfo$yLab),
                           title=plotInfo$plotTitle)

  }    
  return(inP)
}

### Quintile By Forecast ##################################


plotly_QuintileByForecast_Deterministic <- function(inP,plotData,alt,plotInfo,init,...){
  
  pData <-  plotData$data[[plotInfo$plotType]][[plotInfo$paths[1]]][[alt]]$data
  
  for( wyCat in names(pData) ){
    
    inP <- inP %>% add_trace(data = pData[[wyCat]],
                             x=~wyDate, y=~mean,
                             type="scatter",mode="lines",
                             name=sprintf("%s: %s", alt, wyCat),
                             hoverinfo="x+y+text",
                             text=sprintf("%s: %s", alt, wyCat),
                             legendgroup=wyCat,
                             showlegend=!init,
                             line=list(color=getColor(alt,plotInfo$selectedAlts),
                                       dash = getLty(plotInfo$plotType, wyCat, plotInfo)))
    
    if( plotInfo$qAddRibbons )
      inP <- inP %>% add_ribbons(x=~wyDate,ymin=~min,ymax=~max,
                                 name=sprintf("%s: %s", alt, wyCat),
                                 hoverinfo="x+y+text",
                                 text=sprintf("%s: %s", alt, wyCat),
                                 legendgroup=wyCat,
                                 showlegend=F,
                                 fillcolor=colToHex(getColor(alt,plotInfo$selectedAlts),0.3), #applying transparency
                                 line=list(color="black"))
    
  }
  
  return(inP)
}

#All functions are the same for qunitle by forecast
plotly_QuintileByForecast_FRA5k <- 
  plotly_QuintileByForecast_FRA50k <- 
  plotly_QuintileByForecast_Deterministic



### Summary Hydrograph ####################################

plotly_SummaryHydrograph_Deterministic <- function(inP,plotData,alt,plotInfo,init,...){
  
  pData <- plotData$data[[plotInfo$plotType]][[plotInfo$paths[1]]][[alt]]$data
  
  for( metric in names(pData) ){
    
    if( metric %!in% metricsToCompute$metric ) next
    
    subData <- pData[, c("wyDate",metric)]
    names(subData) <- c("x","y")
    
    inP <- inP %>% add_trace(data = subData,
                             x=~x, y=~y,
                             type="scatter",mode="lines",
                             name=sprintf("%s: %s", alt, metric),
                             legendgroup=metric,
                             hoverinfo="x+y+text",
                             text=sprintf("%s: %s", alt, metric),
                             showlegend=!init,
                             line=list(color=getColor(alt,plotInfo$selectedAlts),
                                       dash = getLty(plotInfo$plotType, metric, plotInfo)))
  }
  return(inP)
}

#All functions are the same for sum hydro
plotly_SummaryHydrograph_FRA5k <-
  plotly_SummaryHydrograph_FRA50k <-
  plotly_SummaryHydrograph_Deterministic

### Base Year #############################################

plotly_BaseYear_Deterministic <- function(inP,plotData,alt,plotInfo,init,...){
  
  pData <- plotData$data[[plotInfo$plotType]][[plotInfo$paths[1]]][[alt]]$data

  alt_BaseYrPairings <- sprintf("%s_%s",
                                rep(plotInfo$selectedAlts,each=length(names(pData))),
                                rep(names(pData),times=length(plotInfo$selectedAlts))
                                )
  
  for(baseYr in names(pData)){
    alt_BaseYr <- sprintf("%s_%s",alt,baseYr)
    #plots individual line trace for base year
    inP <- inP %>% add_trace(data = pData[[baseYr]],
                             x=~wyDate, y=~value,
                             type="scatter",mode="lines",
                             name=sprintf("%s: %s", alt, baseYr),
                             hoverinfo="x+y+text",
                             text=sprintf("%s: %s", alt, baseYr),
                             showlegend=!init,
                             legendgroup=sprintf("%s: %s", alt, baseYr),
                             line=list(color=getColor(alt_BaseYr,alt_BaseYrPairings),
                                       dash = getLty(plotInfo$plotType, metric, plotInfo)))
  }
  
  
  return(inP)
}

plotly_BaseYear_FRA5k <- function(inP,plotData,alt,plotInfo,init,...){
  
  pData <- plotData$data[[plotInfo$plotType]][[plotInfo$paths[1]]][[alt]]$data
  
  alt_BaseYrPairings <- sprintf("%s_%s",
                                rep(plotInfo$selectedAlts,each=length(names(pData))),
                                rep(names(pData),times=length(plotInfo$selectedAlts))
                                )

  for(baseYr in names(pData)){
    alt_BaseYr <- sprintf("%s_%s",alt,baseYr)
    #plots as a filled area
    inP <- inP %>% add_ribbons(data = pData[[baseYr]],
                               x=~wyDate,
                               ymin=~min,ymax~max, #key difference from deterministic plots
                               name=sprintf("%s: %s", alt, baseYr),
                               legendgroup=sprintf("%s: %s", alt, baseYr),
                               showlegend=!init,
                               hoverinfo="x+y+text",
                               text=sprintf("%s: %s", alt, baseYr),
                               fillcolor=colToHex(getColor(alt_BaseYr,alt_BaseYrPairings),0.3), #applying transparency
                               line=list(color="black"))
  }
  return(inP)
}

#use 5k function for 50k
plotly_BaseYear_FRA50k <- plotly_BaseYear_FRA5k

### Frequency ######################################


plotly_Frequency_Deterministic <- function(inP,plotData,alt,plotInfo,init,...){
  #expecting pData to have columns for the metric that was computed ('y'), by water year
  #  with a 'q' column indicating the z-score of the probabilities
  
  pData <- plotData$data[[plotInfo$plotType]][[plotInfo$paths[1]]][[alt]]$data
  
  alt_tw_Pairings <- sprintf("%s_%s",
                             rep(plotInfo$selectedAlts,each=length(plotInfo$freqMetric)),
                             rep(plotInfo$freqMetric,times=length(plotInfo$selectedAlts)))
  
  for( tw in plotInfo$freqTW  ){
    
    alt_tw <- sprintf("%s_%s", alt, tw)
    
    inP <- inP %>% 
      add_trace(data = pData[[tw]],
                x=~q, y=~y,
                type="scatter",mode="lines",
                name=sprintf("%s: %s %s", alt, tw, plotInfo$freqMetric),
                hoverinfo="x+y+text",
                text=~paste0(sprintf("%s: %s %s", alt, tw, plotInfo$freqMetric),
                             "<br>P: ",round((1-p)*100,3)," %"),
                showlegend=!init,
                legendgroup=sprintf("%s: %s %s", alt, tw, plotInfo$freqMetric),
                line=list(color=getColor(alt_tw,alt_tw_Pairings)))
  }
  
  return(inP)
}

plotly_Frequency_FRA5k <-
  plotly_Frequency_FRA50k <- 
  plotly_Frequency_Deterministic

### Duration ##############################################

plotly_Duration_Deterministic <- function(inP,plotData,alt,plotInfo,init,...){
  #Expecting pData to have columns for the individual values ('value') and the
  #  annual chance exceedance ('ace')
  
  pData <- plotData$data[[plotInfo$plotType]][[plotInfo$paths[1]]][[alt]]$data
  
  alt_tw_Pairings <- sprintf("%s_%s",
                             rep(plotInfo$selectedAlts,each=length(plotInfo$durTW)),
                             rep(plotInfo$durTW,times=length(plotInfo$selectedAlts)))
  
  for( tw in plotInfo$durTW){
    
    alt_tw <- sprintf("%s_%s", alt, tw)
    
    inP <- inP %>% 
      add_trace(data = pData[[tw]],
                x=~ace, y=~value,
                type="scatter",mode="lines",
                name=sprintf("%s: %s", alt, tw),
                hoverinfo="x+y+text",
                text=~paste0(sprintf("%s: %s", alt, tw),
                             "<br>P: ",round(ace*100,3)," %"),
                showlegend=!init,
                legendgroup=sprintf("%s: %s", alt, tw),
                line=list(color=getColor(alt_tw,alt_tw_Pairings)))
  }
  
  return(inP)
}

plotly_Duration_FRA5k <-
  plotly_Duration_FRA50k <- 
  plotly_Duration_Deterministic


### Metric vs Forecast ####################################



plotly_MetricvsForecast_Deterministic <- function(inP,plotData,alt,plotInfo,init,ptSize=6){
  #Expecting pData to have columns for the forecast ('fcst'), metric ('y'), 
  #  and event ('event')
  #plotting as individual points - no lines
  
  pData <- plotData$data[[plotInfo$plotType]][[plotInfo$paths[1]]][[alt]]$data
  
  alt_tw_Pairings <- sprintf("%s_%s",
                                  rep(plotInfo$selectedAlts,each=length(plotInfo$mFreqTW)),
                                  rep(plotInfo$mFreqTW,times=length(plotInfo$selectedAlts)))
  
  for( tw in plotInfo$mFreqTW  ){
  
    alt_tw <- sprintf("%s_%s", alt, tw)
    
    inP <- inP %>% 
      add_trace(data = pData[[tw]],
                x=~fcst, y=~y,
                type="scatter",mode="markers",
                name=sprintf("%s: %s %s", alt, tw, plotInfo$mFreqMetric),
                hoverinfo="x+y+text",
                text=~paste0(sprintf("%s: %s %s", alt, tw, plotInfo$mFreqMetric),
                             "<br>Event: ",event,
                             "<br>Base Year: ", baseYr),
                showlegend=!init,
                legendgroup=sprintf("%s: %s", alt, tw),
                marker=list(color=getColor(alt_tw,alt_tw_Pairings),
                            symbol="cross",
                            size=ptSize))
  }
  
  return(inP)
}


#Using same function, but different default point size
plotly_MetricvsForecast_FRA5k <-
  plotly_MetricvsForecast_FRA50k <- 
  copyfun(plotly_MetricvsForecast_Deterministic,ptSize=2)
