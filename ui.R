#ideas: navbar so we can have a separate page for uncommon disease plots and for p&i data
library(shiny)
library(dplyr)
#read in disease and location name data
disease_names <- read.table("disease_names.txt", header=T)
disease_names <- droplevels(filter(disease_names, x!="P&I MORT"))
infrequent <- read.table("inf_dis.txt", header=T)


shinyUI(navbarPage("CDC Data Visualization",
                   tabPanel("Disease Plots",
                            
                            # Application title
                            titlePanel("CDC Weekly Case Count"), 
                            
                            #The shiny app is made up of two columns: the first column houses all of the user interface,
                            #including disease name selection, location type, location name, and plot options.  
                            column(4, wellPanel( 
                              # A drop down menu to choose the disease name of interest.  
                              # Reads in disease names from disease_names.txt.  Can choose which name to default to.  
                              selectInput('disease_name', 'Disease Name', as.character(levels(disease_names$x)), selected="Salmonellosis"),
                              
                              # A drop down menu to choose location.  The menu will be populated based on which location type was chosen.
                              # The checkbox is defined in the server.R file
                              dateRangeInput('years','Choose date range', start= "2015-01-01", end=Sys.Date(), 
                                             min = "2008-01-01", max=Sys.Date() ),
                              
                              uiOutput("location"),
                              
                              
                              #A line break to make the interface clearer 
                              br(),
                              
                              # A row with two columns: one to choose the location type, and one to choose a plot type.
                              fluidRow(
                                column(7, radioButtons("locty", "Location Type",
                                                       c("State" = "state",
                                                         "Single region" = "region",
                                                         "All states within a region"="stregion",
                                                         "All regions"="aregion",
                                                         "Country" = "country"), selected="aregion")
                                ),
                                column(5, radioButtons("plotty", "Plot Type",
                                                       c("Weekly data" = "week",
                                                         "Weekly data by year" = "weeky",
                                                         "Cumulative data by year" = "cumuy"), selected="week"))
                              ),
                              
                              # A row with some plot options. uiOutput("frees") creates a checkbox for
                              # whether the y-axis scale should be the same for all plots.
                              # This checkbox only appears for certain location type selections, and is defined in the server.R file. 
                              fluidRow(
                                h5(strong('Plot Options')),
                                checkboxInput('alert_line', 'Include alert thresholds (experimental)'),
                                uiOutput("frees")
                              ))
                            ),
                            
                            
                            
                            # The second column houses the plot(s) of the data that was selected.  These plots are defined in the server.R file.
                            column(8, plotOutput('plot1'))
                   ),
                   
                   tabPanel("Inf. Reported Diseases",
                            titlePanel("CDC Weekly Case Count for Infrequently Reported Diseases"), 
                            column(4, wellPanel( 
                              # A drop down menu to choose the disease name of interest.  
                              # Reads in disease names from disease_names.txt.  Can choose which name to default to.  
                              selectInput('inf_name', 'Disease Name', as.character(levels(infrequent$x)), selected="Typhoid fever"),
                              radioButtons("plottyI", "Plot Type",
                                           c("Weekly data" = "week",
                                             "Weekly data by year" = "weeky",
                                             "Cumulative data" = "cumu",
                                             "Cumulative data by year" = "cumuy"), selected="week"),
                              
                              #A line break to make the interface clearer 
                              br(),
                              fluidRow(
                                h5(strong('Plot Options')),
                                checkboxInput('alert_lineI', 'Include alert thresholds (experimental)')
                              ))
                            ),
                            
                            column(8, plotOutput('plot2'))
                   ),
                   
                   tabPanel("CDC Pneumonia and Influenza Mortality",
                            
                            titlePanel("CDC Weekly Pneumonia and Influenza mortality"),
                            column(4, wellPanel( 
                              # A row with two columns: one to choose the location type, and one to choose from a few display options.
                              # uiOutput("frees") creates a checkbox for whether the y-axis scale should be the same for all plots
                              # This checkbox only appears for certain location type selections, and is defined in the server.R file. 
                              uiOutput("locationP"),
                              
                              fluidRow(
                                column(7,radioButtons("loctyP", "Location Type",
                                                      c("City" = "city",
                                                        "Single region" = "regionP",
                                                        "All cities within a state" = "stateP",
                                                        "All cities within a region"="ctregion",
                                                        "All regions"="aregionP",
                                                        "Total" = "totalP"), selected="aregionP")),
                                column(5, radioButtons("plottyP", "Plot Type",
                                                       c("Weekly data" = "week",
                                                         "Weekly data by year" = "weeky",
                                                         "Cumulative data by year" = "cumuy"), selected="week"))
                              ),
                              
                              fluidRow(
                                h5(strong('Plot Options')),
                                checkboxInput('alert_lineP', 'Include alert thresholds (experimental)'),
                                uiOutput("freesP")
                              ))
                            ),
                            
                            column(8, plotOutput('plot3'))
                   ),
                   
                   tabPanel("More Information",   # Information about data collection.
                            "Data are updated weekly on Thursday at 20:00 CT.",
                            br(),
                            br(),
                            "Please visit", 
                            a("this site", href="http://wwwn.cdc.gov/nndss/document/ProvisionalNationaNotifiableDiseasesSurveillanceData20100927.pdf"),
                            "for more information on how the data were collected.  All data are provisional.",
                            br(),
                            br(),
                            a("See the code", href="https://github.com/NLMichaud/WeeklyCDCPlot"),
                            br(),
                            br(),
                            "Any questions or comments can be sent to",
                            br(),
                            "Aaron Kite-Powell: " ,
                            a("akitepowell@gmail.com", href="mailto:akitepowell@gmail.com"),
                            br(),
                            "Nick Michaud: ",
                            a("michaud@iastate.edu", href="mailto:michaud@iastate.edu"))
                   
)
)