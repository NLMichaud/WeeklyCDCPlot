#First column of matrix is the display name for each disease - to be used in plots
#Second column is name used by CDC in downloaded tables, so that disease data can be accessed by appending ..Current.week to the end 
#Third column is specific url that the disease data can be located at.  
#to include additional diseases, add a row the the matrix.

setwd("/home/nick/Documents/PhD/Spring 15/mmwr scrape/MMWRPlot/")

all <- matrix(c(
c("Cryptosporidiosis","Cryptosporidiosis", "b36e-ru3r", "2014"),
c("Cryptosporidiosis","Cryptosporidiosis", "9n3x-apcd", "2015"),
c("Salmonellosis", "Salmonellosis", "52cr-rw4k", "2014"),
c("Salmonellosis", "Salmonellosis", "d6kj-devz", "2105"),
c("Shigellosis","Shigellosis", "52cr-rw4k", "2014"),
c("Shigellosis","Shigellosis", "n3wf-wtep", "2015"),
c("Pertussis","Pertussis", "8rkx-vimh", "2014"),
c("Pertussis","Pertussis", "d69q-iyrb", "2015"),
c("Malaria","Malaria", "y6uv-t34t", "2014"),
c("Malaria","Malaria", "7pb7-w9us", "2015"),
c("Legionellosis","Legionellosis", "23gt-ssfe", "2014"),
c("Legionellosis","Legionellosis", "ydsy-yh5w", "2015"),
c("Hepatitis A", "Hepatitis..viral..acute...type.A","rg4j-6mcc", "2014"),
c("Hepatitis A", "Hepatitis..viral..acute...type.A","65xe-6neq", "2015"),
c("Hepatitis B, Acute", "Hepatitis..viral..acute...type.B","rg4j-6mcc","2014"),
c("Hepatitis B, Acute", "Hepatitis..viral..acute...type.B","65xe-6neq","2015"),
c("Hepatitis C, Acute", "Hepatitis..viral..acute...type.C","rg4j-6mcc","2014"),
c("Hepatitis C, Acute", "Hepatitis..viral..acute...type.C","65xe-6neq","2015"),
c("Giardiasis", "Giardiasis", "9ix3-ryt6","2014"),
c("Giardiasis", "Giardiasis", "mpdg-hf57","2015"),
c("Meningococcal Disease Invasive (all serogroups)", "Meningococcal.disease..invasive...All.serogroups", "y6uv-t34t","2014"),
c("Meningococcal Disease Invasive (all serogroups)", "Meningococcal.disease..invasive...All.serogroups", "7pb7-w9us","2015"),
c("Mumps", "Mumps", "8rkx-vimh","2014"),
c("Mumps", "Mumps", "d69q-iyrb","2015"),
#c("Pneumonia and Influenza Mortality Reports by City/Region, 2014", "P.I..Total","qpap-3u8w"),
#leave out pneumonia for now, format is too different
c("Shiga toxin-producing E. coli (STEC)", "Shiga.toxin.producing.E..coli..STEC..", "52cr-rw4k","2014"),
c("Shiga toxin-producing E. coli (STEC)", "Shiga.toxin.producing.E..coli..STEC..", "n3wf-wtep","2015")
)
,ncol=4, byrow=T)

#name matrix columns and write to csv file
URL_NAMES <- data.frame(display_name=all[,1],data_name=all[,2],url=all[,3],year=all[,4])
write.table(URL_NAMES, file="urldat.txt", row.names=FALSE, col.names=TRUE)
