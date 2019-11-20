#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# "%!in%" <- function(x,y) !(x %in% y) #not in operator
# 
# #assuming in webapp folder if not in base project directory
# if("scripts" %!in% dir()) setwd("..") 
# 
# cat(sprintf("\nCurrent working directory:\t%s", getwd()))
# 
# #loading from project main directory
# source("scripts/r_setup.R")
# library(shiny)
# library(plotly)
# 
# #Loading all data to save time
# availableFcsts <- getAvailabelForecasts()



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  titlePanel("WAT Plotter"),
  
  
  sidebarPanel(
    
    titlePanel("Alternative File Selection"),
    
    multiInput(
      inputId = "DSS File", label = "Fruits :",
      choices = c("Banana", "Blueberry", "Cherry",
                  "Coconut", "Grapefruit", "Kiwi",
                  "Lemon", "Lime", "Mango", "Orange",
                  "Papaya"),
      selected = "Banana", width = "400px",
      options = list(
        enable_search = FALSE,
        non_selected_header = "Choose between:",
        selected_header = "You have selected:"
      )
    ),
    
    actionButton("add_fileConfigRow", "Add Row"),
    actionButton("delete_fileConfigRow", "Delete Row")
    
  ),
  # actionButton("reset", "Reset"),
  # tags$hr(),
  # modFunctionUI("editable")
  
  # 
  #     #input options taken from these examples: http://shiny.rstudio.com/gallery/selectize-vs-select.html
  #     #select the forecast to look at
  #     selectInput(inputId = 'selectedFcst',label =  'Available Forecasts',
  #                 choices = getAvailabelForecasts(T), multiple=F, selectize=F),
  # 
  #     #Text box or radio buttons to input the quantiles of interest
  #     selectInput(inputId = 'metrics',label =  'Metrics',
  #                 choices = metricsToCompute$metric, multiple=T, selectize=T, selected = c("10%","median","90%")),
  # 
  #     #drop down to select an ESP year
  #     selectInput('selectedYrs', 'ESP Years', availableYrs, multiple=TRUE, selectize=TRUE),
  # 
  #     #update plot button and save to PDF button
  #     actionButton("updateButton","Update"),
  #     actionButton("savePDFButton","Save to pdf"),
  # 
  #     #Current forecast information
  #     titlePanel("Forecast Information"),
  #     htmlOutput("fcst_info"),
  # 
  #     #Current CWMS database information
  #     titlePanel("Current CWMS Data"),
  #     htmlOutput("current_data_tbl"),
  # 
  #     #Forecast comparison table
  #     titlePanel("Forecast Comparisons"),
  # 
  #     #use DT library to get a filter table
  #     htmlOutput( "fcstNote" ),
  #     dataTableOutput("fcst_table"),
  
  
  
  
  
  #one big, long plot
  mainPanel(
    # plotlyOutput("bigPlot",width = "800px",height="1200px"),
    DTOutput("shiny_table")
  )
  
  
  
  
)) #end fluidPage, shinyUI
