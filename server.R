library(shiny)
library(ggplot2)
library(dplyr)

## Sometimes as different options are chosen, Shiny tries to create a plot without first reading in new user input, 
## which can cause ggplot to throw an error.  For that reason, there's a lot of code like if(is.null(input$locty)) return()
## This code checks to see if the correct input has been read yet, and if not, it prevents ggplot from trying to plot anything. 

# Load in cdc data and location names.
cdcdata <- read.table("plotdat.txt", header=T)
location_names <- read.table("location_names.txt", header=T, colClasses=c("character","character"))
cdcdata$MMWR.Week <- as.numeric(cdcdata$MMWR.Week)
cdcdata$MMWR.Year <- as.factor(cdcdata$MMWR.Year)

shinyServer(function(input, output) {

  # The first reactive element of the UI is a drop down menu which filters locations based on whether
  # the user has selected states, regions, states in regions, or country
  # if "all regions" is selected, no location choices are displayed
  output$location <- renderUI({     
    if(is.null(input$locty))return()
     switch(input$locty,
           "state" =     return(selectInput('location_name', 'State Name',sort(filter(location_names, type=="state")$location))),
           "stregion" =  return(selectInput('location_name', 'Region Name', sort(filter(location_names, type=="region")$location))),
           "region" =  return(selectInput('location_name', 'Region Name', sort(filter(location_names, type=="region")$location))),
           "country" =  return(selectInput('location_name', 'Country Name',sort(filter(location_names, type=="country")$location)))
      )
    })
  
  # The second reactive element of the UI is a checkbox which forces the same y-axis scale for all plots.
  # This element only shows up if multiple plots are being shown.
  output$frees <- renderUI({
    if(input$locty=="stregion"||input$locty=="aregion"){
      return(checkboxInput('fixed_scales','Force same scale for y-axis', value=T))
    }
     return()
  })
  
  # We select data to plot based on which location type and location was chosen.  
  # The reactive function filters the data to return only rows from cdc data which correspond to either the state,
  # region, or country selected.  For some reason, need to put in extra error check for the "states within region" option to prevent ggplot error message 
  selectedData <- reactive({
    if(input$locty=="aregion") return(filter(cdcdata, display_name == input$disease_name, Reporting.Area %in% location_names$location[which(location_names$type=="region")]))
    if(is.null(input$location_name))return()
    if(input$locty=="state"||input$locty=="region"||input$locty=="country") return(filter(cdcdata, display_name == input$disease_name, Reporting.Area == input$location_name))
    if(input$locty=="stregion"){
      if(!(input$location_name %in% location_names$region)){return()}
      return(filter(cdcdata, display_name == input$disease_name, Reporting.Area %in% location_names$location[location_names$region==input$location_name]))}
   })
  
  # Plot data - either a single plot for one location, or faceted plots for all locations of a single type
  output$plot1 <- renderPlot({
    if(is.null(input$locty)||is.null(selectedData()))return()    
    scaletype = "fixed"
    
    # Depending on whether the "Cumulative" checkbox is checked, set plot aesthetics to either weekly or cumulative counts
    if(input$cumulative==F){ aesthetics1 = aes(x=MMWR.Week, y=c, group=MMWR.Year, colour=MMWR.Year)
                             aesthetics2 = aes(x=MMWR.Week, y=fourteenwk.thresh,colour=MMWR.Year)}
    if(input$cumulative==T){ aesthetics1 = aes(x=MMWR.Week, y=cumulate, group=MMWR.Year, colour=MMWR.Year)
                             aesthetics2 = aes(x=MMWR.Week, y=cumu14,colour=MMWR.Year)}
   
    # Create the main ggplot
    p <- ggplot(selectedData(), aesthetics1)+geom_line(stat="identity",position="identity",size=1)+
                ylab("Number Reported")+scale_color_brewer(palette="Set1",name="Weekly case counts")
    
    # For state, region, or country location types, only a single plot will be displayed
    if(input$locty=="state"||input$locty=="region"||input$locty=="country"){
      
      # If the alert threshold box was checked, include a line on the plots.  Otherwise, plot with no line.
      if(input$alert_line){
        return(p+geom_line(aesthetics2, linetype="dashed")+
               geom_point(data=subset(selectedData(),fourteenwk.alert == T),colour="black")+
               ggtitle(paste("MMWR",input$disease_name, "Reports for", input$location_name, "2014 & 2015")))
      }
      if(!input$alert_line){
        return(p+ggtitle(paste("MMWR",input$disease_name, "Reports for", input$location_name, "2014 & 2015"))) 
      }
    }
    if(is.null(input$fixed_scales)){return()}
    
    # For states within a region or all region location types, the plot will be faceted.  
    # First check to see whether the user has chosen fixed or free scales, then create plots.
    if(input$fixed_scales==F) {scaletype = "free"}
    if(input$locty=="stregion"||input$locty=="aregion"){
      if(input$alert_line){
        return(p+facet_wrap(~ Reporting.Area, scales=scaletype)+geom_line(aesthetics2, linetype="dashed")+
               geom_point(data=subset(selectedData(),fourteenwk.alert == T),colour="black")) 
      }
      if(!input$alert_line){
        return(p+facet_wrap(~ Reporting.Area, scales=scaletype)) 
      }
    }
  })
})





