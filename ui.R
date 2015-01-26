#ideas: navbar so we can have a separate page for uncommon disease plots and for p&i data
library(shiny)

#read in disease and location name data
disease_names <- read.table("disease_names.txt", header=T)
location_names <- read.table("location_names.txt", header=T)
 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("CDC Weekly Case Count"), 
 
  #sidebar which houses all user interface elements
    column(4, wellPanel(
      
      #a drop down menu to choose the disease name of interest
      selectInput('disease_name', 'Disease Name', as.character(levels(disease_names$x)), selected="Hepatitis B, Acute"),
      br(),
      
      # a row with two columns: one to choose the plot type, and one to choose from a few display options
      fluidRow(
        column(7,radioButtons("locty", "Location Type",
                   c("State" = "state",
                   "Single region" = "region",
                   "All states within a region"="stregion",
                   "All regions"="aregion",
                   "Country" = "country"), selected="aregion")),
        column(5,checkboxInput('alert_line', 'Include alert thresholds'),checkboxInput('cumulative', 'Cumulative counts'),uiOutput("frees"))
      ),
      
      #a drop down menu to choose location.  menu will be populated based on which location type was chosen.
      uiOutput("location")
    )),
  
    #plot
    column(8, plotOutput('plot1'))
))