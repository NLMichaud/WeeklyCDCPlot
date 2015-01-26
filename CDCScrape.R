library(plyr)
library(RCurl)
library(dplyr)
library(caTools)

setwd("/home/nick/Documents/PhD/Spring 15/mmwr scrape/MMWRPlot/")

# a function to help deal with NA values when calculating thresholds.  NA's occur when we try to 
#calculate running standard deviations with only one data point, and cause an error in the cumsum function
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}

#for each different url, get url data from CDC and apply dis_func to each disease
url_func <- function(url_data){
  
  #construct actual CDC website url name and get data for 2014 and 2015
  curl <- url_data$url
  URL <- paste( "https://data.cdc.gov/api/views/",curl, "/rows.csv?accessType=DOWNLOAD",sep="")
  
  nndss14 <-read.csv(textConnection(getURL(URL[1],ssl.verifypeer=FALSE)),strip.white=T,stringsAsFactors=F)
  nndss15 <- read.csv(textConnection(getURL(URL[2],ssl.verifypeer=FALSE)),strip.white=T,stringsAsFactors=F)
    
  #crypto has a slightly different name for mmwr.week and mmwr.year, change it
  if("MMWRWeek"%in%names(nndss14)){nndss14<- dplyr::rename(nndss14,MMWR.Week=MMWRWeek )}
  if("MMWRYear"%in%names(nndss14)){nndss14<- dplyr::rename(nndss14, MMWR.Year=MMWRYear )}
  if("MMWRWeek"%in%names(nndss15)){nndss15<- dplyr::rename(nndss15, MMWR.Week=MMWRWeek  )}
  if("MMWRYear"%in%names(nndss15)){nndss15<- dplyr::rename(nndss15,  MMWR.Year=MMWRYear )}

  #get the name of the column of interest in the cdc files
  dname <- c(paste(url_data$data_name[1],"..Current.week",sep=""))
 
  #combine 2014 and 2015 data
 nndss <- rbind(select(nndss14, contains(dname), contains("MMWR"), contains("Reporting"), -contains("flag")),
 select(nndss15, contains(dname), contains("MMWR"), contains("Reporting"), -contains("flag")))
 
 # set NA values to 0, maybe not a great idea
 names(nndss)[which(dname==names(nndss))] <- "c"
 nndss$c <- as.numeric(nndss$c)
 nndss$c[is.na(nndss$c)]<-0
 nndss$display_name <- url_data$display_name[1]
 
 #create columns for 10 and 14 week thresholds, and 10 and 14 week alerts
 nndss <- nndss %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                                             tenwk.thresh=newthresh(c,10),
                                                          fourteenwk.alert=c>fourteenwk.thresh,
                                                             tenwk.alert=c>tenwk.thresh)
                                                         
 #create new columns for cumulative sum (by year) along with cumulative threshold values
 nndss <- group_by(nndss, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                                  cumu10=cumulate+(tenwk.thresh-c),
                                                                  cumu14=cumulate+(fourteenwk.thresh-c))
 
 #select relevant columns of data table
 nndss<- select(nndss, one_of("c","Reporting.Area", "MMWR.Year", "MMWR.Week","display_name"), contains("thresh"),contains("cumu"),contains("alert"))
 
 return(nndss)
}


#read in data with disease names and corresponding urls
url_data <- read.table("urldat.txt", header=T)

#scrape data from cdc website for all diseases
output <- ddply(url_data, .(data_name), url_func)

#write output as plotdat.csv
write.table(output, file="plotdat.txt", row.names=FALSE, col.names=TRUE)

#separate output file which contains all disease names called disease_names.csv
write.table(unique(output$display_name), file="disease_names.txt", row.names=FALSE, col.names=TRUE)

#separate output file which contains locations and location types (state, region, country) called location_names.cdv
regions <-c("NEW ENGLAND", "MID. ATLANTIC", "E.N. CENTRAL", "W.N. CENTRAL", "S. ATLANTIC", 
           "E.S. CENTRAL", "W.S. CENTRAL", "MOUNTAIN", "PACIFIC", "TERRITORIES")
loc_type <- rep("state", length(unique(output$Reporting.Area)))
loc_type[which(unique(output$Reporting.Area)%in%regions)] <- "region"
loc_type[1] <- "country"
region_num=0
loc_reg <- rep("NONE", length(loc_type))
for(i in 1:62){
  if(loc_type[i]=="region"){
    region_num = region_num+1
  }
  if(loc_type[i]=="state"){
    loc_reg[i]=regions[region_num]
  }
}
loc_reg[63:67] <- "TERRITORIES"
all_locs<-data.frame(location=unique(output$Reporting.Area),type=loc_type, region=loc_reg)
write.table(all_locs, file="location_names.txt", row.names=FALSE, col.names=TRUE)



#still have to include infrequent diseases and P&I data
#new inf url :https://data.cdc.gov/api/views/pb4z-432k/rows.csv?accessType=DOWNLOAD


