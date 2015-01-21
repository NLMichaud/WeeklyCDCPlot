#First column of matrix is the display name for each disease - to be used in plots
#Second column is name used by CDC in downloaded tables, so that disease data can be accessed by appending ..Current.week to the end 
#Third column is specific url that the disease data can be located at.  
#to include additional diseases, add a row the the matrix.

setwd("/home/nick/Documents/PhD/Spring 15/mmwr scrape/MMWRPlot/")

all <- matrix(c(
c("Cryptosporidiosis","Cryptosporidiosis", "b36e-ru3r"),
c("Cryptosporidiosis","Cryptosporidiosis", "9n3x-apcd"),
c("Salmonellosis", "Salmonellosis", "52cr-rw4k"),
c("Salmonellosis", "Salmonellosis", "d6kj-devz"),
c("Shigellosis","Shigellosis", "52cr-rw4k"),
c("Shigellosis","Shigellosis", "n3wf-wtep"),
c("Pertussis","Pertussis", "8rkx-vimh"),
c("Pertussis","Pertussis", "d69q-iyrb"),
c("Malaria","Malaria", "y6uv-t34t"),
c("Malaria","Malaria", "7pb7-w9us"),
c("Legionellosis","Legionellosis", "23gt-ssfe"),
c("Legionellosis","Legionellosis", "ydsy-yh5w"),
c("Hepatitis A", "Hepatitis..viral..acute...type.A","rg4j-6mcc"),
c("Hepatitis A", "Hepatitis..viral..acute...type.A","65xe-6neq"),
c("Hepatitis B, Acute", "Hepatitis..viral..acute...type.B","rg4j-6mcc"),
c("Hepatitis B, Acute", "Hepatitis..viral..acute...type.B","65xe-6neq"),
c("Hepatitis C, Acute", "Hepatitis..viral..acute...type.C","rg4j-6mcc"),
c("Hepatitis C, Acute", "Hepatitis..viral..acute...type.C","65xe-6neq"),
c("Giardiasis", "Giardiasis", "9ix3-ryt6"),
c("Giardiasis", "Giardiasis", "mpdg-hf57"),
c("Meningococcal Disease Invasive (all serogroups)", "Meningococcal.disease..invasive...All.serogroups", "y6uv-t34t"),
c("Meningococcal Disease Invasive (all serogroups)", "Meningococcal.disease..invasive...All.serogroups", "7pb7-w9us"),
c("Mumps", "Mumps", "8rkx-vimh"),
c("Mumps", "Mumps", "d69q-iyrb"),
#c("Pneumonia and Influenza Mortality Reports by City/Region, 2014", "P.I..Total","qpap-3u8w"),
#leave out pneumonia for now, format is too different
c("Shiga toxin-producing E. coli (STEC)", "Shiga.toxin.producing.E..coli..STEC..", "52cr-rw4k"),
c("Shiga toxin-producing E. coli (STEC)", "Shiga.toxin.producing.E..coli..STEC..", "n3wf-wtep")

)
,ncol=3, byrow=T)

#name matrix columns and write to csv file
URL_NAMES <- data.frame(display_name=all[,1],data_name=all[,2],url=all[,3])
write.table(URL_NAMES, file="urldat.csv", row.names=FALSE, col.names=TRUE)

