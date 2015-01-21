library(shiny)
library(ggplot2)
library(dplyr)

#load in cdc data and location names
cdcdata <- read.table("plotdat.csv", header=T)
location_names <- read.table("location_names.csv", header=T, colClasses=c("character","character"))


shinyServer(function(input, output) {

  #include a reactive user interface which lets the user select from states, regions or country
  output$ui <- renderUI({     
    if(is.null(input$show_all)||input$show_all){
      return()
    }  
    if(!input$show_all){
     switch(input$locty,
           "state" =     selectInput('location_name', 'Location Name',sort(filter(location_names, type=="state")$location)),
           "region" =  selectInput('location_name', 'Location Name', sort(filter(location_names, type=="region")$location)),
           "country" =  selectInput('location_name', 'Location Name',sort(filter(location_names, type=="country")$location)),
      )
    }
  })

  #select data based on which location was chosen, or if all locations was checked
  selectedData <- reactive({
    if(!input$show_all) return( cdcdata[(as.character(cdcdata$display_name) == input$disease_name ) &
               (as.character(cdcdata$Reporting.Area) == input$location_name),])
    if(input$show_all) return(filter(cdcdata, display_name == input$disease_name, Reporting.Area %in% location_names$location[which(location_names$type==input$locty)]))
    
  })
 
  
  cdcdata$MMWR.Week <- as.numeric(cdcdata$MMWR.Week)
  cdcdata$MMWR.Year <- as.factor(cdcdata$MMWR.Year)
  #plot data - either a single plot for one location, or faceted plots for all locations of a single type
  output$plot1 <- renderPlot({
    if(is.null(input$locty)||is.null(input$show_all))return()
    if(!input$show_all){
      if(input$alert_line){
    p <- ggplot(selectedData(), aes(x=MMWR.Week, y=c, group=MMWR.Year, fill=MMWR.Year)) +  geom_histogram( alpha=.5, stat="identity",position="identity") +ggtitle(paste("MMWR",input$disease_name, "Reports for", input$location_name, "2014 & 2015"))    
    return(p+geom_line(aes(x=MMWR.Week, y=fourteenwk.thresh,colour=MMWR.Year))+geom_point(data=subset(selectedData(),fourteenwk.alert == T),aes(colour=MMWR.Year))+ylab("Number Reported"))
        }
      if(!input$alert_line){
      p <- ggplot(selectedData(), aes(x=MMWR.Week, y=c, group=MMWR.Year, fill=MMWR.Year)) +  geom_histogram( alpha=.5, stat="identity",position="identity") +ggtitle(paste("MMWR",input$disease_name, "Reports for", input$location_name, "2014 & 2015"))    
      return(p+ylab("Number Reported")) 
        }
    }
    if(input$show_all){
      if(input$alert_line){
        p <- ggplot(selectedData(), aes(x=MMWR.Week, y=c, group=MMWR.Year, fill=MMWR.Year)) +  geom_histogram( alpha=.5, stat="identity",position="dodge") +ggtitle(paste("MMWR",input$disease_name, "Reports for", input$location_name, "2014 & 2015"))    
        return(p+ylab("Number Reported")+facet_wrap(~ Reporting.Area)+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+scale_x_continuous()+geom_point(data=subset(selectedData(),fourteenwk.alert == T), aes(colour=MMWR.Year))+geom_line(aes(x=MMWR.Week, y=fourteenwk.thresh,colour=MMWR.Year))) 
      }
      if(!input$alert_line){
        p <- ggplot(selectedData(), aes(x=MMWR.Week, y=c, group=MMWR.Year, fill=MMWR.Year)) +  geom_histogram( alpha=.5, stat="identity",position="dodge") +ggtitle(paste("MMWR",input$disease_name, "Reports for", input$location_name, "2014 & 2015"))    
        return(p+ylab("Number Reported")+facet_wrap(~ Reporting.Area)+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+scale_x_continuous()) 
      }
    }
  })
})





