#ideas: navbar so we can have a separate page for uncommon disease plots and for p&i data
library(shiny)

#read in disease and location name data
disease_names <- read.table("disease_names.csv", header=T)
location_names <- read.table("location_names.csv", header=T)
 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("MMWR Disease Time Series Plot"),
 
 
  #sidebar which houses all user interface elements
  sidebarLayout(
    sidebarPanel(
      selectInput('disease_name', 'Disease Name', as.character(levels(disease_names$x))),
      br(),
      fluidRow(
        column(8,radioButtons("locty", "Location Type",
                   c("State" = "state",
                   "Region" = "region",
                   "Country" = "country"))),
        column(4,checkboxInput('show_all', 'Plot all locations (can be slow)'),
               checkboxInput('alert_line', 'Include alert thresholds'))
      ),
      uiOutput("ui")
    ),
  
    #plot
    mainPanel(plotOutput('plot1'))
  )
))