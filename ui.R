#ideas: navbar so we can have a separate page for uncommon disease plots and for p&i data
library(shiny)

#read in disease and location name data
disease_names <- read.table("disease_names.txt", header=T)
location_names <- read.table("location_names.txt", header=T)
 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("CDC Weekly Case Count"), 
 
  #The shiny app is made up of two columns: the first column houses all of the user interface,
  #including disease name selection, location type, location name, and plot options.  
    column(4, wellPanel(
      
      # A drop down menu to choose the disease name of interest.  
      # Reads in disease names from disease_names.txt.  Can choose which name to default to.  
      selectInput('disease_name', 'Disease Name', as.character(levels(disease_names$x)), selected="Salmonellosis"),
    
      #A line break to make the interface clearer 
      br(),
      
      # A row with two columns: one to choose the location type, and one to choose from a few display options.
      # uiOutput("frees") creates a checkbox for whether the y-axis scale should be the same for all plots
      # This checkbox only appears for certain location type selections, and is defined in the server.R file. 
      fluidRow(
        column(7,radioButtons("locty", "Location Type",
                   c("State" = "state",
                   "Single region" = "region",
                   "All states within a region"="stregion",
                   "All regions"="aregion",
                   "Country" = "country"), selected="aregion")),
        column(5,checkboxInput('alert_line', 'Include alert thresholds'),
               checkboxInput('cumulative', 'Cumulative counts'),
               uiOutput("frees"))
      ),
      
      # A drop down menu to choose location.  The menu will be populated based on which location type was chosen.
      # The checkbox is defined in the server.R file
      uiOutput("location")
    ),
    
    # Information about data collection.
    "Please visit", 
    a("this site", href="http://wwwn.cdc.gov/nndss/document/ProvisionalNationaNotifiableDiseasesSurveillanceData20100927.pdf"),
    "for more information on how the data were collected.  All data are provisional.",
    br(),
    br(),
    a("See the code", href="https://github.com/NLMichaud/WeeklyCDCPlot"),
    br(),
    br(),
    "Any questions or comments can be sent to Nick Michaud: ",
    a("michaud@iastate.edu", href="mailto:michaud@iastate.edu")),
  
    # The second column houses the plot(s) of the data that was selected.  These plots are defined in the server.R file.
    column(8, plotOutput('plot1'))
))