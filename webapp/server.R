#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#




shinyServer(function(input, output,session) {
  
  #Indicates whether executing in a shiny app or not
  showProgress <<- is.environment(getDefaultReactiveDomain())
  
  
  
  
  #Update possible path names to select as alternative changes
  observeEvent(input$selectedAlts, {
    #Update alternatives and matching paths
    cat("\nUpdating Alternatives and Matching Paths")
    withProgress(message = "Updating DSS paths",value=0.5,{
      updateDSSPaths(input,session)
    })
    
  })
  
  ### Refresh ########
  observeEvent(input$refreshButton, {
    #Update plotly given current alternative and DSS path selection
    cat("\nRefreshing Plotly")
    
    
    opts <- getOpts(input)

    selectedPaths <- opts$selectedPaths
    selectedAlts <- opts$selectedAlts
    
    #Provide progress for loading data
    withProgress(message="Loading Data",value=0,{
      allData <<- updateLoadedData(selectedAlts, selectedPaths, allData,showProgress=showProgress)
    })
    
    withProgress(message="Creating Plots",value=0,{
      
      plotData <<- getPlotData(allData, plotData, opts,showProgress=showProgress)
      
      cat("\nMade plotData")
      
      #making global
      p <<- makePlotly(allData, plotData, opts,showProgress)
      output$verticalTable <- renderPlotly(p)
    })
  })
  
  
  
  ### Saving ##########
  observeEvent(input$savePDFButton,{
    #Saves to PDF
    saveFileName <- getSaveFileName(ext=".pdf")
    saveToPdf()
  })
  
  observeEvent(input$saveHTMLButton,{ #save to html
    # saveFileName <- getSaveFileName(ext=".html")
    cat("\nSaving to html")
    saveFileName <- input$saveFileName
    saveFileName <- sprintf("%s/test_output/%s",getwd(),saveFileName)
    if(!is.null(saveFileName)) htmlwidgets::saveWidget(widget = p,file = saveFileName, selfcontained = F)
    cat(sprintf("\nDone saving to html:\t%s",saveFileName ))
    #
    # (config,espData, espQData, input$metrics, input$selectedYrs, fcstInfo[[input$selectedFcst]])
  })
  
  observeEvent(input$saveExcelLButton,{ #save to Excel
    cat("\nSaving to Excel")
    #Indicates whether executing in a shiny app or not
    # showProgress <<- is.environment(getDefaultReactiveDomain())
    opts <- getOpts(input)
    saveExcel(allData, plotData, opts,showProgress)
    cat(sprintf("\nDone saving to Excel:\t%s",opts$saveFileName ))
  })
  
  ### Additional Tabs #######
  
  output$runMatrix <- renderDataTable(
    runMatrix,
    options=list(scrollX=T, pageLength=nrow(runMatrix))
  )
  
  output$fcstTbl <- renderDataTable( #show the rounded determinstic TDA fcst tables
    roundedFcstTable,
    options=list(pageLength=nrow(roundedFcstTable))
  )
  
  ### Custom Plots #####################
  
  ## keep track of elements inserted and not yet removed
  inserted <- cPlotPathInserted <- cPlotTypeInserted <- cBaseYrInserted <- c()
  
  ### TODO #####
  #Need to somehow add elements to cPlotTbl, which is used as ui 
  
  observeEvent(input$cInsertBtn, {
    btn <- input$cInsertBtn
    id <- paste0('cPlotRow', btn)
    opts <- getOpts(input)
    
    cat(sprintf("\nCustom button ID:\t%s", id))
    insertUI(
      selector = "#cPlotTbl",
      ## wrap element in a div with id for ease of removal
      ui = 
        # tags$div(  #<- original code from example
        #   tags$p(paste('Element number', btn)),
        #   id = id
        # )
        fluidRow(


          tags$div(
            id=id,
            fluidRow(
              style="border: 4px double black;",
            column(
              #Path to plot
              selectInput(inputId = sprintf('cPlotPath%g',btn),
                          label =  'Path:',
                          choices = opts$selectedPaths,
                          multiple=F, selectize=F),
              #Plot type - changes conditional panel for plot options
              selectInput(inputId = sprintf('cPlotType%g',btn),
                          label =  'Plot Type:',
                          choices = c("Summary Hydrograph","Quintile By Forecast",
                                      "Base Year","Frequency","Duration","Metric vs Forecast"),
                          multiple=F, selectize=F),
              width=2
            ),

            #y-axis label
            column(
              textInput(inputId = sprintf('cPlotYaxLabel%g',btn),value = "",label = "Y-Axis Label"),
              width=2
            ),

            #Datum Shift
            column(
              numericInput(inputId = sprintf('cPlotDatumShift%g',btn),value = 0,label = "Datum Shift"),
              #Divisor (for e.g., cfs -> kcfs)
              numericInput(inputId = sprintf('cPlotDivisor%g',btn),value = 1,label = "Divisor"),
              width=2
            ),
            #Base Year
            column(

              ## Custom Plot - Base Year #########
              conditionalPanel(
                condition= sprintf("input.%s == 'Base Year'",sprintf('cPlotType%g',btn)),

                selectInput(sprintf('cBaseYr%g',btn), 'Base Year:',
                            c(1929:2008,3013:3028,3101:3109),
                            multiple=T, selectize=T)
              ),
              width=2
            )

          )))
    )
    
    inserted <<- c(id,inserted)
  })
  
  #Removing a row
  observeEvent(input$cRemoveBtn, {
    removeUI(
      selector=paste0("#",inserted[length(inserted)])
    )
    
    inserted <<- inserted[-length(inserted)]
  })
  
  
  ### Debug #############
  observeEvent(input$printInput, {
    
    #Checks whether or not to show progress bar (i.e., in server session)
    showProgress <- is.environment(getDefaultReactiveDomain())
    if(showProgress) opts <- getInputOptions(input)
    if(!showProgress){
      #In testing mode
      source("scripts/utils/testing.R")
      # opts <- getTestInput("Quintile By Forecast")
      # opts <- getTestInput("Summary Hydrograph")
      # opts <- getTestInput("Base Year")
      opts <- getTestInput("Frequency")
      # opts <- getTestInput("Metric vs Forecast")
    }
    
    opts <- opts[order(names(opts))] #ordering by element names
    
    for (i in 1:length(opts)) {
      cat("\n",names(opts)[i],":\n")
      message(opts[[i]])
    }
    # lapply(opts, message)
    
  })
  
  #for debug, re-source the global script
  observeEvent(input$reloadGlobals, {
    #Update alternatives and matching paths
    source("webapp/global.R")
    
  })
  
})
