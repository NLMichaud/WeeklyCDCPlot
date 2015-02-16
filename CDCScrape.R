library(plyr)
library(RCurl)
library(dplyr)
library(caTools)

# Read in data with disease names and corresponding urls.  This data is created from the url_names.R file, which should be run first.
urldat <- read.table("urldat.txt", header=T)
dates <- read.table("dates.txt", header=T)

# A function to help deal with NA values when calculating thresholds.  NA's occur when we try to 
# calculate running standard deviations with only one data point, and cause an error in the cumsum function.
# Args:
#   x: A vector of disease occurance data that we wish to calculate an alert threshold for
#   days: an integer for the number of days to calculate the threshold over

newthresh <- function(x,days){
  thresh <-runmean(x,days, align="right")+2*runsd(x, days, align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
# A function to convert MMWR week and Year info to calendar dates, uses dates.txt file
# 
# Args:
#   x: a data.frame with columns titled "MMWR.Week" and "MMWR.Year"
 
getdate <- function(x){
  if(!is.na(x['MMWR.Week'])){
  return(filter(dates, MMWR.Week==as.numeric(x['MMWR.Week']))[5+as.numeric(x['MMWR.Year'])-2014][[1]])
  }
  return(filter(dates, MMWR.Week==as.numeric(x['MMWR.week']))[5+as.numeric(x['MMWR.year'])-2014][[1]])
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
  if("MMWR.WEEK"%in%names(nndss14)){nndss14<- dplyr::rename(nndss14,MMWR.Week=MMWR.WEEK )}
  if("MMWRYear"%in%names(nndss14)){nndss14<- dplyr::rename(nndss14, MMWR.Year=MMWRYear )}
  if("MMWR.YEAR"%in%names(nndss14)){nndss14<- dplyr::rename(nndss14, MMWR.Year=MMWR.YEAR )}
  if("MMWRWeek"%in%names(nndss15)){nndss15<- dplyr::rename(nndss15, MMWR.Week=MMWRWeek  )}
  if("MMWR.WEEK"%in%names(nndss15)){nndss15<- dplyr::rename(nndss15, MMWR.Week=MMWR.WEEK  )}
  if("MMWRYear"%in%names(nndss15)){nndss15<- dplyr::rename(nndss15,  MMWR.Year=MMWRYear )}
  if("MMWR.YEAR"%in%names(nndss15)){nndss15<- dplyr::rename(nndss15,  MMWR.Year=MMWR.YEAR )}

  # dname is the name of the column in the nndss file which contains weekly data for the disease of interest
  dname <- c(paste(url_data$data_name[1],"..Current.week",sep=""))
  
  #special column name for P&I mortality data
  if(url_data$data_name[1]=="P&I MORT")dname <- "P.I..Total"
 
  # Select relevant columns from both the 2014 and 2015 data and rbind them together
 nndss <- rbind(select(nndss14, contains(dname), contains("MMWR"), contains("Reporting"), -contains("flag")),
 select(nndss15, contains(dname), contains("MMWR"), contains("Reporting"), -contains("flag")))

 # set NA values to 0, maybe not a great idea, but useful for calculating thresholds and cumulative sums
 names(nndss)[which(dname==names(nndss))] <- "c"
 nndss$c <- as.numeric(nndss$c)
 nndss$c[is.na(nndss$c)]<-0
 nndss$display_name <- url_data$display_name[1]
 
 #Create columns for 10 and 14 week thresholds and 10 and 14 week alerts, grouping by reporting area.
 nndss<-  group_by(nndss, Reporting.Area) %>% do(mutate(., fourteenwk.thresh=newthresh(c,14),
                                                       tenwk.thresh=newthresh(c,10),
                                                        fourteenwk.alert=c>fourteenwk.thresh,
                                                        tenwk.alert=c>tenwk.thresh,
                                                        cumulate=cumsum(c),
                                                        cumu10=cumulate+(tenwk.thresh-c),
                                                        cumu14=cumulate+(fourteenwk.thresh-c)))

 
 # Create columns for cumulative sum along with cumulative threshold values, grouping both by reporting area and year
 nndss <- group_by(nndss, Reporting.Area, MMWR.Year) %>% do(mutate(., ycumulate=cumsum(c),
                                                                  ycumu10=ycumulate+(tenwk.thresh-c),
                                                                  ycumu14=ycumulate+(fourteenwk.thresh-c)))
 # Add date information for the MMWR week/year combination
 nndss$date<- apply(nndss, 1,  getdate)
 
 #select and return relevant columns of data table
 nndss<- select(nndss, one_of("c","Reporting.Area", "MMWR.Year", "MMWR.Week","display_name","date"),contains("cumu"), contains("alert"),contains("thresh"))
 return(nndss)
}

# Run the url_func function for each different disease name in our urldat.txt data file.  Use re-encoding to remove some
# illegible characters
output <- ddply(urldat, .(data_name), url_func)
output$Reporting.Area <- as.character(output$Reporting.Area)
Encoding(output$Reporting.Area) <- "latin1"
output$Reporting.Area <- iconv(output$Reporting.Area, "latin1", "ASCII", sub="")

# Write output as plotdat.csv
write.table(output, file="plotdat.txt", row.names=FALSE, col.names=TRUE)

# Separate output file which contains all disease names called disease_names.csv
write.table(unique(output$display_name), file="disease_names.txt", row.names=FALSE, col.names=TRUE)


URL <- paste( "https://data.cdc.gov/api/views/",urldat$url[1], "/rows.csv?accessType=DOWNLOAD",sep="")
names <- unique(read.csv(textConnection(getURL(URL,ssl.verifypeer=FALSE)),strip.white=T,stringsAsFactors=F)$Reporting.Area)
# Separate output file which contains locations and location types (state, region, or country) called location_names.cdv
regions <-c("NEW ENGLAND", "MID. ATLANTIC", "E.N. CENTRAL", "W.N. CENTRAL", "S. ATLANTIC", 
            "E.S. CENTRAL", "W.S. CENTRAL", "MOUNTAIN", "PACIFIC", "TERRITORIES")
loc_type <- rep("state", length(names))
loc_type[which(names%in%regions)] <- "region"
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
all_locs<-data.frame(location=names,type=loc_type, region=loc_reg)
write.table(all_locs, file="location_names.txt", row.names=FALSE, col.names=TRUE)

##now a location file for P&I mortality data
URL <- URL <- paste( "https://data.cdc.gov/api/views/7esm-uptm/rows.csv?accessType=DOWNLOAD",sep="")
pi_names <- unique(read.csv(textConnection(getURL(URL,ssl.verifypeer=FALSE)),strip.white=T,stringsAsFactors=F)$Reporting.Area)
Encoding(pi_names) <- "latin1"
pi_names <- iconv(pi_names, "latin1", "ASCII", sub="")
pi_names[length(pi_names)]<-"Total"
loc_type <- rep("city", length(pi_names))
loc_type[which(tolower(pi_names)%in%tolower(regions))] <- "region"
loc_type[length(pi_names)]<-"total"
city_st<- sapply(strsplit(pi_names[loc_type=="city"], ",", fixed=T,), function(x){return(state.name[which(state.abb==gsub(" ", "",x[2],fixed=T))])})
city_st[[75]] <- NA
city_st <- unlist(city_st)
rcity_st <- rep(NA, length(loc_type))
rcity_st[which(loc_type=="city")]<-city_st
# Also include, for state locations, which region the state falls under.  Thankfully, the CDC data table is ordered so that it first lists a region, then
# all the states in that region, then the next region, and so on.  So, between each region name, all states will be in the same region  
region_num=0
loc_reg <- rep("NONE", length(loc_type))
for(i in 1:length(pi_names)){
  if(loc_type[i]=="region"){
    region_num = region_num+1
  }
  if(loc_type[i]=="city"){
    loc_reg[i]=regions[region_num]
  }
}
all_locs<-data.frame(location=pi_names,type=loc_type, region=loc_reg, state=rcity_st)
write.table(all_locs, file="pi_names.txt", row.names=FALSE, col.names=TRUE)

#separate code for infrequent diseases.
URL <- c("https://data.cdc.gov/api/views/wcwi-x3uk/rows.csv?accessType=DOWNLOAD",
         "https://data.cdc.gov/api/views/pb4z-432k/rows.csv?accessType=DOWNLOAD") 
nndss14 <-read.csv(textConnection(getURL(URL[1],ssl.verifypeer=FALSE)),strip.white=T,stringsAsFactors=F)
nndss15 <- read.csv(textConnection(getURL(URL[2],ssl.verifypeer=FALSE)),strip.white=T,stringsAsFactors=F)
nndss <- rbind(select(nndss14, contains("Current.week"), contains("MMWR"), contains("Disease"), -contains("flag")),
               select(nndss15,  contains("Current.week"),contains("MMWR"), contains("Disease"), -contains("flag")))


#disease names are different bewteen years, try to clean some disease names up
Encoding(nndss$Disease) <- "latin1"
nndss$Disease <- iconv(nndss$Disease, "latin1", "ASCII", sub="")
nndss$Disease <- gsub(":","",nndss$Disease)
nndss$Disease <- gsub(",","",nndss$Disease)

nndss$Disease <- gsub("*","",nndss$Disease, fixed=T)

#remove all disease names which aren't present in both years
nndss <- nndss[-which(nndss$Disease%in%names(which(table(nndss$Disease)<54))),]
d <- nndss
d$c <- d$Current.week
d$c <- as.numeric(d$c)

#calculate 14 week thresholds and alerts
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
d<-  group_by(d, Disease) %>% do(mutate(., threshold=newthresh(c,14),
                                        alert=c>threshold,
                                        cumulate=cumsum(c),
                                        cumu14=cumulate+(threshold-c)))

# Create columns for cumulative sum along with cumulative threshold values, grouping both by reporting area and year
d <- group_by(d, Disease, MMWR.year) %>% do(mutate(., ycumulate=cumsum(c),
                                                                  ycumu14=ycumulate+(threshold-c)))
#get dates
d$date<- apply(d, 1,  getdate)

#rename some diseases 
d$Disease <- as.factor(d$Disease)
levels(d$Disease)[3] <- "Arbo,EEE"
levels(d$Disease)[2] <- "Arbo,CA serogroup"
levels(d$Disease)[4] <- "Arbo,Powassan"
levels(d$Disease)[5] <- "Arbo,St Louis"
levels(d$Disease)[6] <- "Arbo,WEE"
levels(d$Disease)[9] <- "Botulism other"
levels(d$Disease)[14] <- "Cyclosporiasis"
levels(d$Disease)[16] <- "H flu <5 non-b"
levels(d$Disease)[17] <- "H flu <5 b"
levels(d$Disease)[18] <- "H flu <5 unknown"
levels(d$Disease)[19] <- "Hansen Disease"
levels(d$Disease)[20] <- "HUS,postdiarrheal"
levels(d$Disease)[21] <- "HBV,perinatal"
levels(d$Disease)[22] <- "Influenza ped mort"
levels(d$Disease)[25] <- "Measles"
levels(d$Disease)[26] <- "Mening a,c,y,w-135"
levels(d$Disease)[27] <- "Mening other"
levels(d$Disease)[28] <- "Mening serogroup b"
levels(d$Disease)[29] <- "Mening unknown"
levels(d$Disease)[30] <- "Novel influenza A"
levels(d$Disease)[32] <- "Polio nonparalytic"
levels(d$Disease)[34] <- "Psittacosis"
levels(d$Disease)[37] <- "Q Fever, Total"
levels(d$Disease)[39] <- "SARS-CoV"
levels(d$Disease)[40] <- "Smallpox"
levels(d$Disease)[41] <- "Strep toxic shock synd"
levels(d$Disease)[42] <- "Syphilis congenital <1yr"
levels(d$Disease)[42] <- "Toxic shock synd staph"
levels(d$Disease)[47] <- "Vanco Interm Staph A"
levels(d$Disease)[48] <- "Vanco Resist Staph A"

d$alert[is.na(d$alert)]<-"N"

write.table(d, file="infreq.txt", row.names=FALSE, col.names=TRUE)
write.table(unique(d$Disease), file="inf_dis.txt",row.names=FALSE, col.names=TRUE)
