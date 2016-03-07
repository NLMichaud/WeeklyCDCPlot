# First column of matrix is the display name for each disease - to be used in plots
# Second column is name used by CDC in downloaded tables, so that disease data can be accessed by appending ..Current.week to the end 
# Third column is specific url that the disease data can be located at.
# Fourth column is the year of the data in the url.
# To include additional diseases, add a row the the matrix.

all <- matrix(c(
c("Cryptosporidiosis","Cryptosporidiosis", "b36e-ru3r", "2014"),
c("Cryptosporidiosis","Cryptosporidiosis", "9n3x-apcd", "2015"),
c("Cryptosporidiosis","Cryptosporidiosis", "kikd-77zw", "2016"),
c("Salmonellosis", "Salmonellosis", "52cr-rw4k", "2014"),
c("Salmonellosis", "Salmonellosis", "d6kj-devz", "2105"),
c("Salmonellosis", "Salmonellosis", "4qb4-rsd8", "2016"),
c("Shigellosis","Shigellosis", "52cr-rw4k", "2014"),
c("Shigellosis","Shigellosis", "n3wf-wtep", "2015"),
c("Shigellosis","Shigellosis", "xv7k-8e7s", "2016"),
c("Pertussis","Pertussis", "8rkx-vimh", "2014"),
c("Pertussis","Pertussis", "d69q-iyrb", "2015"),
c("Pertussis","Pertussis", "bfe6-2gyq", "2016"),
c("Malaria","Malaria", "y6uv-t34t", "2014"),
c("Malaria","Malaria", "7pb7-w9us", "2015"),
c("Malaria","Malaria", "93k9-hy54", "2016"),
c("Legionellosis","Legionellosis", "23gt-ssfe", "2014"),
c("Legionellosis","Legionellosis", "ydsy-yh5w", "2015"),
c("Legionellosis","Legionellosis", "yqwx-bvu7", "2016"),
c("Hepatitis A", "Hepatitis..viral..acute...type.A","rg4j-6mcc", "2014"),
c("Hepatitis A", "Hepatitis..viral..acute...type.A","65xe-6neq", "2015"),
c("Hepatitis A", "Hepatitis..viral..acute...type.A","7vnz-2mjz", "2016"),
c("Hepatitis B, Acute", "Hepatitis..viral..acute...type.B","rg4j-6mcc","2014"),
c("Hepatitis B, Acute", "Hepatitis..viral..acute...type.B","65xe-6neq","2015"),
c("Hepatitis B, Acute", "Hepatitis..viral..acute...type.B","7vnz-2mjz","2016"),
c("Hepatitis C, Acute", "Hepatitis..viral..acute...type.C","rg4j-6mcc","2014"),
c("Hepatitis C, Acute", "Hepatitis..viral..acute...type.C","65xe-6neq","2015"),
c("Hepatitis C, Acute", "Hepatitis..viral..acute...type.C","7vnz-2mjz","2016"),
c("Giardiasis", "Giardiasis", "9ix3-ryt6","2014"),
c("Giardiasis", "Giardiasis", "mpdg-hf57","2015"),
c("Giardiasis", "Giardiasis", "afja-b25e","2016"),
c("Meningococcal Disease Invasive (all serogroups)", "Meningococcal.disease..invasive...All.serogroups", "y6uv-t34t","2014"),
c("Meningococcal Disease Invasive (all serogroups)", "Meningococcal.disease..invasive...All.serogroups", "7pb7-w9us","2015"),
c("Meningococcal Disease Invasive (all serogroups)", "Meningococcal.disease..invasive...All.serogroups", "93k9-hy54","2016"),
c("Mumps", "Mumps", "8rkx-vimh","2014"),
c("Mumps", "Mumps", "d69q-iyrb","2015"),
c("Mumps", "Mumps", "bfe6-2gyq","2016"),
#c("Pneumonia and Influenza Mortality Reports by City/Region, 2014", "P.I..Total","qpap-3u8w"),
#leave out pneumonia for now, format is too different
c("Shiga toxin-producing E. coli (STEC)", "Shiga.toxin.producing.E..coli..STEC..", "52cr-rw4k","2014"),
c("Shiga toxin-producing E. coli (STEC)", "Shiga.toxin.producing.E..coli..STEC..", "n3wf-wtep","2015"),
c("Shiga toxin-producing E. coli (STEC)", "Shiga.toxin.producing.E..coli..STEC..", "xv7k-8e7s","2016"),
c("P&I MORT", "P&I MORT", "qpap-3u8w", "2014"),
c("P&I MORT", "P&I MORT", "7esm-uptm", "2015"),
c("P&I MORT", "P&I MORT", "rpjd-ejph", "2016")
)
,ncol=4, byrow=T)

#Name matrix columns and write to csv file
URL_NAMES <- data.frame(display_name=all[,1],data_name=all[,2],url=all[,3],year=all[,4])
write.table(URL_NAMES, file="urldat.txt", row.names=FALSE, col.names=TRUE)

