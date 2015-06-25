library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(grid)
## Sometimes as different options are chosen, Shiny tries to create a plot without first reading in new user input, 
## which can cause ggplot to throw an error.  For that reason, there's a lot of code like if(is.null(input$locty)) return()
## This code checks to see if the correct input has been read yet, and if not, it prevents ggplot from trying to plot anything. 

# Load in cdc data, P&I data, and infreq disease data along with location names.
cdcdata <- read.table("plotdat.txt", header=T)
# Have to convert some region names to all uppercase since they were recorded differently.  May want
# to move this into CDCScrape.R
location_names <- read.table("location_names.txt", header=T, colClasses=c("character","character"))
cdcdata$year <- as.factor(cdcdata$year)
cdcdata$MMWR.Year <- as.factor(cdcdata$MMWR.Year)
cdcdata$Reporting.Area[toupper(cdcdata$Reporting.Area)%in%location_names$region]<- toupper(cdcdata$Reporting.Area[toupper(cdcdata$Reporting.Area)%in%location_names$region])
cdcdata$rdate <- as.Date(cdcdata$rdate, format="%m/%d/%Y")
cdcdata$week <- as.Date(cdcdata$week, format="%m/%d")

pi_names <- read.table("pi_names.txt", header=T,colClasses=c("character","character"))
pi_names$location[which(pi_names$type=="region")]<- toupper(pi_names$location[which(pi_names$type=="region")])

infreq <- read.table("infreq.txt", header=T)
infreq$MMWR.year <- as.factor(infreq$MMWR.year)
infreq$date <- as.Date(infreq$date, format="%m/%d/%Y")

datetrans <- read.table("week.csv", header=T, sep=",")



shinyServer(function(input, output, session) {

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
    
  
  output$locationP <- renderUI({     
    if(is.null(input$loctyP))return()
    switch(input$loctyP,
           "city" =     return(selectInput('location_nameP', 'City Name',sort(filter(pi_names, type=="city")$location))),
           "ctregion" =  return(selectInput('location_nameP', 'Region Name', sort(filter(pi_names, type=="region")$location))),
           "regionP" =  return(selectInput('location_nameP', 'Region Name', sort(filter(pi_names, type=="region")$location))),
           "stateP" =  return(selectInput('location_nameP', 'State Name', sort(unique(filter(pi_names, type=="city")$state))))
    )
  })
  
  # The second reactive element of the UI is a checkbox which forces the same y-axis scale for all plots.
  # This element only shows up if multiple plots are being shown.
  output$frees <- renderUI({
    if(input$locty=="stregion"||input$locty=="aregion"){
      return(checkboxInput('fixed_scales','Force same scale for y-axis', value=F))
    }
     return()
  })
  


  output$freesP <- renderUI({
    if(input$loctyP=="ctregion"||input$loctyP=="aregionP"){
      return(checkboxInput('fixed_scalesP','Force same scale for y-axis', value=F))
    }
    return()
  })
  

  
  
  observe({
    x <- input$disease_name
    miny <-  min(filter(cdcdata, display_name==x)$rdate)
    updateDateRangeInput(session, "years", min = miny)
  })
  
  # We select data to plot based on which location type and location was chosen.  
  # The reactive function filters the data to return only rows from cdc data which correspond to either the state,
  # region, or country selected.  For some reason, need to put in extra error check for the "states within region" option to prevent ggplot error message 
  selectedData <- reactive({
    if(input$locty=="aregion") return(filter(cdcdata, display_name == input$disease_name, Reporting.Area %in% location_names$location[which(location_names$type=="region")],
                                            rdate >= input$years[1], rdate<=input$years[2] ))
    if(is.null(input$location_name))return()
    if(input$locty=="state"||input$locty=="region"||input$locty=="country") return(filter(cdcdata, display_name == input$disease_name, Reporting.Area == input$location_name, 
                                                                                          rdate >= input$years[1], rdate<=input$years[2] ))
    if(input$locty=="stregion"){
      if(!(input$location_name %in% location_names$region)){return()}
      return(filter(cdcdata, display_name == input$disease_name, Reporting.Area %in% location_names$location[location_names$region==input$location_name], 
                    rdate >= input$years[1], rdate<=input$years[2] ))}
   })
  

  selectedDataI <- reactive({
    if(is.null(input$inf_name))return()
    return(filter(infreq, Disease==input$inf_name))
  })
  
  selectedDataP <- reactive({
    if(input$loctyP=="aregionP") return(filter(cdcdata, display_name == "P&I MORT", Reporting.Area %in% pi_names$location[which(pi_names$type=="region")]))
    if(input$loctyP=="totalP") return(filter(cdcdata, display_name=="P&I MORT", Reporting.Area == "Total"))
    if(is.null(input$location_nameP))return()
    if(input$loctyP=="city"||input$loctyP=="regionP") return(filter(cdcdata, display_name == "P&I MORT", Reporting.Area == input$location_nameP))
    if(input$loctyP=="stateP"){
      if(!(input$location_nameP %in% pi_names$state)){return()}
      return(filter(cdcdata, display_name == "P&I MORT", Reporting.Area %in% pi_names$location[pi_names$state==input$location_nameP]))}
    if(input$loctyP=="ctregion"){
      if(!(input$location_nameP %in% pi_names$region)){return()}
      return(filter(cdcdata, display_name == "P&I MORT", Reporting.Area %in% pi_names$location[pi_names$region==input$location_nameP]))}
  })
  
    
  
  # Plot data - either a single plot for one location, or faceted plots for all locations of a single type
  output$plot1 <- renderPlot({
    if(is.null(input$locty)||is.null(selectedData()))return()    
    scaletype = "fixed"
    
    # Depending on whether the "Cumulative" checkbox is checked, set plot aesthetics to either weekly or cumulative counts
    switch(input$plotty,
           "week"  =  {aesthetics1 = aes(x=rdate, y=c, group=group)
                       aesthetics2 = aes(x=rdate, y=fourteenwk.thresh, group=group)},
           "weeky"  =  {aesthetics1 = aes(x=MMWR.Week, y=c, group=MMWR.Year, colour=MMWR.Year)
                        aesthetics2 = aes(x=MMWR.Week, y=fourteenwk.thresh,colour=MMWR.Year)} ,
           "cumuy" =   {aesthetics1 = aes(x=MMWR.Week, y=ycumulate, group=year, colour=year)
                        aesthetics2 = aes(x=MMWR.Week, y=ycumu14,colour=year)}
    )
    
    # Create the main ggplot
    p <- ggplot(selectedData(), aesthetics1)+geom_line(stat="identity",position="identity",size=1)+
      ylab("Number Reported")+scale_color_brewer(palette="Set2",name="Weekly case counts")+
      ggtitle(paste("MMWR",input$disease_name, "Reports"))+xlab("Date")
    
    #if(input$plotty=="weeky"||input$plotty=="cumuy") p <- p  + scale_x_date(breaks="3 months",limits=c(as.Date("1/1", format="%m/%d"),as.Date("12/31", format="%m/%d")),
    #                                                                                     labels=date_format("%b"))
    

    
    # If the alert threshold box was checked, include a line on the plots.  Otherwise, plot with no line.
    if(input$alert_line){
      p <- p +  geom_point(data=subset(selectedData(),fourteenwk.alert == T),colour='RED')
      if(input$plotty=="week") p <- p+geom_line( mapping=aesthetics2, linetype="dashed", colour='RED')
      if(input$plotty=="weeky"||input$plotty=="cumuy") p <- p+geom_line( mapping=aesthetics2, linetype="dashed")
    }
    
    if(input$locty=="state"||input$locty=="region"||input$locty=="country") return(p)
    
    if(is.null(input$fixed_scales)){return()}
    if(input$fixed_scales==F) scaletype="free"
    
    return(p + facet_wrap(~ Reporting.Area, scales=scaletype)+theme(panel.margin = unit(1, "lines")))
  })
  
  
  output$plot2 <- renderPlot({
    if(is.null(selectedDataI()))return()    
    scaletype = "fixed"
    
    # Depending on whether the "Cumulative" checkbox is checked, set plot aesthetics to either weekly or cumulative counts
    switch(input$plottyI,
           "week"  =  {aesthetics1 = aes(x=date, y=c)
                       aesthetics2 = aes(x=date, y=threshold)},
           "weeky"  =  {aesthetics1 = aes(x=MMWR.week, y=c, group=MMWR.year, colour=MMWR.year)
                        aesthetics2 = aes(x=MMWR.week, y=threshold,colour=MMWR.year)} ,
           "cumuy" =   {aesthetics1 = aes(x=MMWR.week, y=ycumulate, group=MMWR.year, colour=MMWR.year)
                        aesthetics2 = aes(x=MMWR.week, y=ycumu14,colour=MMWR.year)}
    )
    
    # Create the main ggplot
    p <- ggplot(selectedDataI(), aesthetics1)+geom_line(stat="identity",position="identity",size=1)+
      ylab("Number Reported")+scale_color_brewer(palette="Set2",name="Weekly case counts")+
      ggtitle(paste("MMWR",input$inf_name, "Reports for 2014 & 2015"))
    
    if(input$plottyI=="week") p <- p + scale_x_date(breaks = "3 month", minor_breaks = "1 month", labels=date_format("%m/%Y"))+xlab("Date")
    
    # If the alert threshold box was checked, include a line on the plots.  Otherwise, plot with no line.
    if(input$alert_lineI){
      p <- p+  geom_point(data=subset(selectedDataI(),alert == T),colour='RED')
      if(input$plottyI=="week") p <- p+geom_line( mapping=aesthetics2, linetype="dashed", colour='RED')
      if(input$plottyI=="weeky"||input$plottyI=="cumuy") p <- p+geom_line( mapping=aesthetics2, linetype="dashed")
    }
    
    return(p)
  })
  
  output$plot3 <- renderPlot({
    if(is.null(input$loctyP)||is.null(selectedDataP()))return()    
    scaletype = "fixed"
    
    # Depending on whether the "Cumulative" checkbox is checked, set plot aesthetics to either weekly or cumulative counts
    switch(input$plottyP,
           "week"  =  {aesthetics1 = aes(x=rdate, y=c)
                       aesthetics2 = aes(x=rdate, y=fourteenwk.thresh)},
           "weeky"  =  {aesthetics1 = aes(x=MMWR.Week, y=c, group=MMWR.Year, colour=MMWR.Year)
                        aesthetics2 = aes(x=MMWR.Week, y=fourteenwk.thresh,colour=MMWR.Year)} ,
           "cumuy" =   {aesthetics1 = aes(x=MMWR.Week, y=ycumulate, group=MMWR.Year, colour=MMWR.Year)
                        aesthetics2 = aes(x=MMWR.Week, y=ycumu14,colour=MMWR.Year)}
    )
    
    # Create the main ggplot
    p <- ggplot(selectedDataP(), aesthetics1)+geom_line(stat="identity",position="identity",size=1)+
      ylab("Number Reported")+scale_color_brewer(palette="Set2",name="Weekly case counts")+
      ggtitle(paste("MMWR P&I Mortality Reports for 2014 & 2015"))
    
    if(input$plottyP=="week") p <- p + scale_x_date(breaks = "3 month", minor_breaks = "1 month", labels=date_format("%m/%Y"))+xlab("Date")
    
    # If the alert threshold box was checked, include a line on the plots.  Otherwise, plot with no line.
    if(input$alert_lineP){
      p <- p+  geom_point(data=subset(selectedDataP(),fourteenwk.alert == T),colour='RED')
      if(input$plottyP=="week"||input$plottyP=="cumu") p <- p+geom_line( mapping=aesthetics2, linetype="dashed", colour='RED')
      if(input$plottyP=="weeky"||input$plottyP=="cumuy") p <- p+geom_line( mapping=aesthetics2, linetype="dashed")
    }
    
    if(input$loctyP=="state"||input$loctyP=="region"||input$loctyP=="country") return(p)
    
    if(is.null(input$fixed_scalesP)){return()}
    if(input$fixed_scalesP==F) scaletype="free"
    
    return(p + facet_wrap(~ Reporting.Area, scales=scaletype))
  })

})




  

##still to do:
##infreq and p&i mort
#recode automatic update - create file just for old data, then rescrape new data, reformat, and attach each week.