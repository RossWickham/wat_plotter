#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/




sideBarWidth <- 4

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("WAT Plotter"),
  
  ### TODO ########
  #Warning messages prompt:
  #https://stackoverflow.com/questions/34422342/show-warning-to-user-in-shiny-in-r
  #Something like this, but want to eval in different sections

  
  sidebarPanel(
    
    
    titlePanel("Alternative File Selection"),
    
    #Alternative selection
    multiInput(
      inputId = "selectedAlts", label = "Alternatives :",
      choices = unique(conns$alt),
      selected = NULL,
      # width = validateCssUnit(sprintf("%.0f%%",100*sideBarWidth/12)),
      options = list(
        enable_search = T,
        non_selected_header = "Choose between:",
        selected_header = "You have selected:"
      )
    ),
    
    #DSS path selection - dynamically changes based upon what is currently available in 
    #  the 'Alternatives' selection
    multiInput(
      inputId = "selectedPaths", label = "Matched Paths Between Selected Alternatives:",
      choices = "",
      selected = NULL,
      # width = "800px",
      options = list(
        enable_search = T,
        non_selected_header = "Choose between:",
        selected_header = "You have selected:"
      )
    ),
    
    #update 'conns' (from connections.xlsx) and 'pathConfig'
    # actionButton("updateButton","Update Selectable DSS Paths"),
    
    #Creates new plots
    actionButton("refreshButton","Refresh Plots"),
    br(),br(),
    
    textInput(inputId="plotTitle", label="Plot Title:", value = "",
              # width = "400px",
              placeholder = "CRT Alternative Comparisons"),
    
    ### Plot Template ###################
    selectInput('plotTemplateName', 'Plot Template (Not working yet):', state.name, multiple=F, selectize=T),
    
    ### TODO ##########
    #able to select template from dropdown
    #Load button
    #save button that prompts to define current template's name
    
    ### Plot Type #########
    selectInput('plotType', 'Plot Type:',
                c("Summary Hydrograph","Quintile By Forecast",
                  "Base Year","Frequency","Duration","Metric vs Forecast"),
                selected = "Quintile By Forecast",
                multiple=F, selectize=T),
    
    
    ### Base Year ######
    #Base Year Options Selection
    conditionalPanel(
      condition="input.plotType == 'Base Year'",
      tags$div(
        tags$h1("Base Year Selection:")
      ),
      br(),
      
      selectInput('baseYr', 'Base Year:',
                  c(1929:2008,3013:3028,3101:3109),
                  multiple=T, selectize=T)
    ),
    
    ### Quintile By Forecast ####
    #Quntile Plot Options Selection
    conditionalPanel(
      condition="input.plotType == 'Quintile By Forecast'",
      
      #Selection for project and date used in quintile plot 
      
      tags$div(
        tags$h1("Quintile Plot Options:"),
        tags$h6(" - Deterministic plots grouped using incremental probabilities for each water year"),
        tags$h6(" - Lines indicate median value within each grouping")
      ),
      
      br(),
      
      fluidRow(
        column(12,
               fluidRow(
                 column(6,
                        selectInput('fcstProj', 'Project',
                                    getCommonNamesInList_recursive(fcsts,nLevel=1,exclude = c("mTime","event","incProbs")),
                                    multiple=F, selectize=T,selected = "THE DALLES_IN")
                 )),
               
               fluidRow(
                 column(4,
                        selectInput('fcstDate', 'Date',
                                    getCommonNamesInList_recursive(fcsts,nLevel=2,exclude = c("mTime","event","incProbs")),
                                    multiple=F, selectize=T, selected="30Apr")
                 ),
                 column(4,
                        #Also need to select the grouping definition for quintile plots
                        selectInput(inputId = 'wyGroup',label =  'Grouping',
                                    choices = names(wyRanking), multiple=F, selectize=T, selected = names(wyRanking)[1])
                 ))
        )
      ),
      
      checkboxInput(inputId="qAddRibbons", label="Add Within-Group Ranges")
      
      
      
    ),
    ### Summary Hydrograph ####
    conditionalPanel(
      condition="input.plotType == 'Summary Hydrograph'",
      
      tags$div(
        tags$h1("Summary Hydrograph Plot Options:")
      ),
      
      #Summary Hydrographs: Dropdown for quantiles of interest
      selectInput(inputId = 'metrics',label =  'Metrics:',
                  choices = metricsToCompute$metric,
                  multiple=T, selectize=T, selected = c("10%","median","90%"))
    ),
    
    #Frequency  #####
    conditionalPanel(
      
      condition="input.plotType == 'Frequency'",
      tags$div(
        tags$h1("Frequency Plot Options:")
      ),
      
      selectInput('freqTW', 'Time Window:',
                  c("Annual","Spring (01Nov-30Apr)","Winter (01May-31Aug)",
                    "31Jan","28Feb","31Mar","10Apr", "30Apr","31May","30Jun","31Jul","31Aug","30Sep","31Oct","30Nov","31Dec",
                    month.abb),
                  selected="Annual",
                  multiple=T, selectize=T),
      
      selectInput('freqMetric', 'Metric:',
                  c("min","median","max"),
                  selected = "max",
                  multiple=F, selectize=T)
    ),
    
    
    #Duration  #####
    conditionalPanel(
      
      condition="input.plotType == 'Duration'",
      tags$div(
        tags$h1("Duration Plot Options:")
      ),
      
      selectInput('durTW', 'Time Window:',
                  c("Annual","Spring (01Nov-30Apr)","Winter (01May-31Aug)",
                    month.abb),
                  selected="Annual",
                  multiple=T, selectize=T)
    ),
    
    
    ### Metric vs Forecast #####
    #"Metric vs Forecast" Plot Options Selection
    conditionalPanel(
      
      condition="input.plotType == 'Metric vs Forecast'",
      
      tags$div(
        tags$h1("Metric vs Forecast Plot Options:")
      ),
      fluidRow(
        column(6,
               selectInput('mFreqTW', 'Time Window:',
                           c("Annual","Spring (01Nov-30Apr)","Winter (01May-31Aug)",
                             "31Jan","28Feb","31Mar","10Apr", "30Apr","31May","30Jun","31Jul","31Aug","30Sep","31Oct","30Nov","31Dec",
                             selected="Annual",
                             month.abb),
                           multiple=T, selectize=T)
        ),
        column(4,
               selectInput('mFreqMetric', 'Metric:',
                           c("min","median","max"),
                           selected = "median",
                           multiple=F, selectize=T)
        )
      ),
      fluidRow(
        column(6,
               selectInput('mFcstProj', 'Project',
                           getCommonNamesInList_recursive(fcsts,nLevel=1,exclude = c("mTime","event","incProbs")),
                           multiple=F, selectize=T,selected = "THE DALLES_IN")
        ),
        column(4,
               selectInput('mFcstDate', 'Date',
                           getCommonNamesInList_recursive(fcsts,nLevel=2,exclude = c("mTime","event","incProbs")),
                           multiple=F, selectize=T, selected="30Apr")
        )
      )
    ),
    
    ### Saving ###########
    fluidRow(
      
      tags$h4("Save Options:"),
     
      # checkboxGroupInput("saveOptions","",
      #                    c("html" = "saveHTMLBox",
      #                      "Excel" = "saveExcelLBox",
      #                      "pdf" = "savePDFBox"),
      #                    width="20%"
      # ),
      
      #Make sure these widths add up to 12 or wrap in a new fluidRow
      column(
        fluidRow(
          checkboxInput("saveHTMLBox","html",width="20%")
        ),width=2
      ),
      column(
        fluidRow(
          checkboxInput("saveExcelBox","Excel", width="20%")
        ),width=2
      ),
      column(
        fluidRow(
          checkboxInput("savePDFBox","pdf", width="20%")
        ),width=2
      ),
      column(
        fluidRow(
          checkboxInput("savePNGBox","png", width="20%")
        ),width=2
      ),
      br(),br(),br(),
      
      textInput(inputId="saveFileName", label="Save File Name:", value = "",
                placeholder = "Dworshak_NAA_FC_ThreeGroups.html"),
      
      actionButton("saveButton","Save")
    
    ),
    
    width=sideBarWidth
  ),  #End sidepanel definition
  
  
  # mainPanel(plotlyOutput("verticalTable"))
  
  # navbarMenu("",
  #                tabPanel("Plot",
  #                         mainPanel(plotlyOutput("verticalTable"))),
  #                tabPanel("CRT Run Matrix",dataTableOutput("runMatrix")),
  #                tabPanel("TDA 80 yr Forecasts",dataTableOutput("fcstTbl"))
  #            )
  
  ### Main Panel Tabs ###############
  mainPanel( #Tabs
    tabsetPanel(
      tabPanel("Plot",
               mainPanel(plotlyOutput("verticalTable",width = "1000px",height="1200px")),width=12),
      tabPanel("CRT Run Matrix",dataTableOutput("runMatrix")),
      tabPanel("TDA Deterministic Forecasts",dataTableOutput("fcstTbl")),
      tabPanel("Custom Plot",
               fixedRow(
                 actionButton('cInsertBtn', 'Insert Row'), 
                 actionButton('cRemoveBtn', 'Remove Row')
               ),
               tags$div(id = 'cPlotTbl') 
               
               )
    )
  ),
  
  actionButton("reloadGlobals","Reload Functions (debug)"),
  
  actionButton("printInput","Print Input")
  
)) #end fluidPage, shinyUI
