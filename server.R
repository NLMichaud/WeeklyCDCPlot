library(shiny)
library(ggplot2)
library(dplyr)

#load in cdc data and location names
cdcdata <- read.table("plotdat.txt", header=T)
location_names <- read.table("location_names.txt", header=T, colClasses=c("character","character"))
cdcdata$MMWR.Week <- as.numeric(cdcdata$MMWR.Week)
cdcdata$MMWR.Year <- as.factor(cdcdata$MMWR.Year)

shinyServer(function(input, output) {

  #include a reactive user interface which lets the user select from states, regions or country
  # if "all regions" is selected, no location choices are displayed
  output$location <- renderUI({     
    if(is.null(input$locty))return()
     switch(input$locty,
           "state" =     return(selectInput('location_name', 'State Name',sort(filter(location_names, type=="state")$location))),
           "stregion" =  return(selectInput('location_name', 'Region Name', sort(filter(location_names, type=="region")$location), selected="S. ATLANTIC")),
           "region" =  return(selectInput('location_name', 'Region Name', sort(filter(location_names, type=="region")$location))),
           "country" =  return(selectInput('location_name', 'Country Name',sort(filter(location_names, type=="country")$location)))
      )
    })
  
  output$frees <- renderUI({
    if(input$locty=="stregion"||input$locty=="aregion"){
      return(checkboxInput('fixed_scales','Force same scale for y-axis', value=T))
    }
     return()
  })
  
  #select data based on which location type was chosen.  the reactive function filters the data to return only rows from cdc data
  # which correspond to either the state, region, or country selected.  for some reason, need to put in extra error check for stregion to prevent ggplot2 error message 
  selectedData <- reactive({
    if(input$locty=="aregion") return(filter(cdcdata, display_name == input$disease_name, Reporting.Area %in% location_names$location[which(location_names$type=="region")]))
    if(is.null(input$location_name))return()
    if(input$locty=="state"||input$locty=="region"||input$locty=="country") return( cdcdata[(as.character(cdcdata$display_name) == input$disease_name ) &
               (as.character(cdcdata$Reporting.Area) == input$location_name),])
    if(input$locty=="stregion"){
      if(!(input$location_name %in% location_names$region)){return()}
    return(cdcdata[(as.character(cdcdata$display_name) == input$disease_name ) &
                                                 (as.character(cdcdata$Reporting.Area) %in% location_names$location[location_names$region==input$location_name]),])}
   })
  
  #plot data - either a single plot for one location, or faceted plots for all locations of a single type
  output$plot1 <- renderPlot({
    if(is.null(input$locty)||is.null(selectedData()))return()    
    #the generic plot
    scaletype = "fixed"
    if(input$cumulative==F){ aesthetics1 = aes(x=MMWR.Week, y=c, group=MMWR.Year, colour=MMWR.Year)
                             aesthetics2 = aes(x=MMWR.Week, y=fourteenwk.thresh,colour=MMWR.Year)}
    if(input$cumulative==T){ aesthetics1 = aes(x=MMWR.Week, y=cumulate, group=MMWR.Year, colour=MMWR.Year)
                             aesthetics2 = aes(x=MMWR.Week, y=cumu14,colour=MMWR.Year)}
    p <- ggplot(selectedData(), aesthetics1) +
      geom_line(stat="identity",position="identity",size=1)+ylab("Number Reported")+  scale_color_brewer(palette="Set1",
                                                                                                         name="Weekly case counts")
    #for these location types, only a single plot will be displayed (as opposed to faceted plots)
    if(input$locty=="state"||input$locty=="region"||input$locty=="country"){
      if(input$alert_line){
        #add alert line
        return(p+geom_line(aesthetics2, linetype="dashed")+
                 geom_point(data=subset(selectedData(),fourteenwk.alert == T),colour="black")+ggtitle(paste("MMWR",input$disease_name, "Reports for", input$location_name, "2014 & 2015")))
      }
      if(!input$alert_line){
        return(p+ggtitle(paste("MMWR",input$disease_name, "Reports for", input$location_name, "2014 & 2015"))) 
      }
    }
    
    if(is.null(input$fixed_scales)){return()}
    if(input$fixed_scales==F) {scaletype = "free"}
    #for these location types, plot will be faceted.
    if(input$locty=="stregion"||input$locty=="aregion"){
      if(input$alert_line){
        #add alert line
        return(p+facet_wrap(~ Reporting.Area, scales=scaletype)+geom_line(aesthetics2, linetype="dashed")+geom_point(data=subset(selectedData(),fourteenwk.alert == T),colour="black")) 
      }
      if(!input$alert_line){
        return(p+facet_wrap(~ Reporting.Area, scales=scaletype)) 
      }
    }
  })
  
})





