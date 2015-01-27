library(plyr)
library(RCurl)
library(dplyr)
library(caTools)

# Read in data with disease names and corresponding urls.  This data is created from the url_names.R file, which should be run first.
urldat <- read.table("urldat.txt", header=T)

# A function to help deal with NA values when calculating thresholds.  NA's occur when we try to 
# calculate running standard deviations with only one data point, and cause an error in the cumsum function.
# Args:
#   x: A vector of disease occurance data that we wish to calculate an alert threshold for
#   days: an integer for the number of days to calculate the threshold over

newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}

# This function takes each url and corresponding disease name and gets data from CDC.  It then combines multiple years worth of data,
# calculates alert thresholds and cumulative sums and returns the columns of interest from the CDC data.
# Args:
#   url_data: the rows of the url_data.txt file which contain the urls for a given disease

url_func <- function(url_data){
  
  # Construct actual CDC website url name and get data for 2014 and 2015
  curl <- url_data$url
  URL <- paste( "https://data.cdc.gov/api/views/",curl, "/rows.csv?accessType=DOWNLOAD",sep="")
  
  nndss14 <-read.csv(textConnection(getURL(URL[1],ssl.verifypeer=FALSE)),strip.white=T,stringsAsFactors=F)
  nndss15 <- read.csv(textConnection(getURL(URL[2],ssl.verifypeer=FALSE)),strip.white=T,stringsAsFactors=F)
    
  # Some diseases have a slightly different name for MMWR.Week and MMWR.Year, so we standardize the names here
  if("MMWRWeek"%in%names(nndss14)){nndss14<- dplyr::rename(nndss14,MMWR.Week=MMWRWeek )}
  if("MMWRYear"%in%names(nndss14)){nndss14<- dplyr::rename(nndss14, MMWR.Year=MMWRYear )}
  if("MMWRWeek"%in%names(nndss15)){nndss15<- dplyr::rename(nndss15, MMWR.Week=MMWRWeek  )}
  if("MMWRYear"%in%names(nndss15)){nndss15<- dplyr::rename(nndss15,  MMWR.Year=MMWRYear )}

  # dname is the name of the column in the nndss file which contains weekly data for the disease of interest
  dname <- c(paste(url_data$data_name[1],"..Current.week",sep=""))
 
  # Select relevant columns from both the 2014 and 2015 data and rbind them together
 nndss <- rbind(select(nndss14, contains(dname), contains("MMWR"), contains("Reporting"), -contains("flag")),
 select(nndss15, contains(dname), contains("MMWR"), contains("Reporting"), -contains("flag")))
 
 # set NA values to 0, maybe not a great idea, but useful for calculating thresholds and cumulative sums
 names(nndss)[which(dname==names(nndss))] <- "c"
 nndss$c <- as.numeric(nndss$c)
 nndss$c[is.na(nndss$c)]<-0
 nndss$display_name <- url_data$display_name[1]
 
 # Create columns for 10 and 14 week thresholds and 10 and 14 week alerts, grouping by reporting area.
 nndss <- nndss %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                                             tenwk.thresh=newthresh(c,10),
                                                          fourteenwk.alert=c>fourteenwk.thresh,
                                                             tenwk.alert=c>tenwk.thresh)
                                                         
 # Create columns for cumulative sum along with cumulative threshold values, grouping both by reporting area and year
 nndss <- group_by(nndss, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                                  cumu10=cumulate+(tenwk.thresh-c),
                                                                  cumu14=cumulate+(fourteenwk.thresh-c))
 
 #select and return relevant columns of data table
 nndss<- select(nndss, one_of("c","Reporting.Area", "MMWR.Year", "MMWR.Week","display_name"), contains("thresh"),contains("cumu"),contains("alert"))
 
 return(nndss)
}




# Run the url_func function for each different disease name in our urldat.txt data file
output <- ddply(urldat, .(data_name), url_func)

# Write output as plotdat.csv
write.table(output, file="plotdat.txt", row.names=FALSE, col.names=TRUE)

# Separate output file which contains all disease names called disease_names.csv
write.table(unique(output$display_name), file="disease_names.txt", row.names=FALSE, col.names=TRUE)

# Separate output file which contains locations and location types (state, region, or country) called location_names.cdv
regions <-c("NEW ENGLAND", "MID. ATLANTIC", "E.N. CENTRAL", "W.N. CENTRAL", "S. ATLANTIC", 
           "E.S. CENTRAL", "W.S. CENTRAL", "MOUNTAIN", "PACIFIC", "TERRITORIES")
loc_type <- rep("state", length(unique(output$Reporting.Area)))
loc_type[which(unique(output$Reporting.Area)%in%regions)] <- "region"
loc_type[1] <- "country"

# Also include, for state locations, which region the state falls under.  Thankfully, the CDC data table is ordered so that it first lists a region, then
# all the states in that region, then the next region, and so on.  So, between each region name, all states will be in the same region  
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


