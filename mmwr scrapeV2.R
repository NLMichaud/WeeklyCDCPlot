library(plyr)
library(RCurl)
library(dplyr)


setwd("/home/nick/Documents/PhD/Spring 15/mmwr scrape/MMWRPlot/")


#for each different url, get url data from CDC and apply dis_func to each disease
url_func <- function(url_data){
  
  #construct actual CDC website url name and get data for 2014 and 2015
  curl <- url_data$url
  URL <- paste( "https://data.cdc.gov/api/views/",curl, "/rows.csv?accessType=DOWNLOAD",sep="")
  
  nndss14 <-read.csv(textConnection(getURL(URL[1],ssl.verifypeer=FALSE)),strip.white=T,stringsAsFactors=F)
  nndss15 <- read.csv(textConnection(getURL(URL[2],ssl.verifypeer=FALSE)),strip.white=T,stringsAsFactors=F)
  
  #crypto has a slightly different name for mmwr.week and mmwr.year, change it
  if("MMWRWeek"%in%names(nndss14)){nndss14<- rename(nndss14,  c("MMWRWeek"="MMWR.Week" ))}
  if("MMWRYear"%in%names(nndss14)){nndss14<- rename(nndss14,  c("MMWRYear"="MMWR.Year" ))}
  if("MMWRWeek"%in%names(nndss15)){nndss15<- rename(nndss15,  c("MMWRWeek"="MMWR.Week" ))}
  if("MMWRYear"%in%names(nndss15)){nndss15<- rename(nndss15,  c("MMWRYear"="MMWR.Year" ))}
  
  #get the name of the column of interest in the cdc files
  dname <- c(paste(url_data$data_name[1],"..Current.week",sep=""))
 
  #combine 2014 and 2015 data
 nndss <- rbind(select(nndss14, contains(dname), contains("MMWR"), contains("Reporting"), -contains("flag")),
 select(nndss15, contains(dname), contains("MMWR"), contains("Reporting"), -contains("flag")))
 
 
 names(nndss)[which(dname==names(nndss))] <- "c"
 nndss$c <- as.numeric(nndss$c)
 nndss$c[is.na(nndss$c)]<-0
 nndss$display_name <- url_data$display_name[1]
 
 #create columns for 10 and 14 week thresholds, and 10 and 14 week alerts
 cdc1 <- nndss %>% group_by(Reporting.Area) %>% transmute("fourteenwk.thresh"=runmean(c, 14, align="right")+2*runsd(c, 14,align="right"),
                                                             "tenwk.thresh"=runmean(c,10,align="right")+2*runsd(c, 10,align="right"),
                                                             "fourteenwk.alert"=c>fourteenwk.thresh,
                                                             "tenwk.alert"=c>tenwk.thresh)
 
 #select relevant columns of data table
 nndss<- cbind(select(nndss, one_of("c","Reporting.Area", "MMWR.Year", "MMWR.Week","display_name")), cdc1)
 
 #add 53 to MMWR.Week in 2015 for ggplot2 purposes.  not a good long-term solution.
 #nndss$MMWR.Week[nndss$MMWR.Year==2015] <- nndss$MMWR.Week[nndss$MMWR.Year==2015]+53
 return(nndss)
}


#read in data with disease names and corresponding urls
url_data <- read.table("urldat.csv", header=T)

#scrape data from cdc website for all diseases
output <- ddply(url_data, .(data_name), url_func)

#write output as plotdat.csv
write.table(output, file="plotdat.csv", row.names=FALSE, col.names=TRUE)

#separate output file which contains all disease names called disease_names.csv
write.table(unique(output$display_name), file="disease_names.csv", row.names=FALSE, col.names=TRUE)

#separate output file which contains locations and location types (state, region, country) called location_names.cdv
regions <-c("NEW ENGLAND", "MID. ATLANTIC", "E.N. CENTRAL", "W.N. CENTRAL", "S. ATLANTIC", 
           "E.S. CENTRAL", "W.S. CENTRAL", "MOUNTAIN", "PACIFIC", "C.N.M.I.")
loc_type <- rep("state", length(unique(output$Reporting.Area)))
loc_type[which(unique(output$Reporting.Area)%in%regions)] <- "region"
loc_type[1] <- "country"
all_locs<-data.frame(location=unique(output$Reporting.Area),type=loc_type)
write.table(all_locs, file="location_names.csv", row.names=FALSE, col.names=TRUE)



#still have to include infrequent diseases and P&I data
#new inf url :https://data.cdc.gov/api/views/pb4z-432k/rows.csv?accessType=DOWNLOAD


