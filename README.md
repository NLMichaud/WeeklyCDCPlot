# MMWRPlot
Contains R files for making a Shiny app which displays MMWR data by state and disease.

-url_names.R creates urldat.csv, which contains the names of each disease of interest, and the url location for the data of that disease

-mmwr scrapeV2.R uses the urldat.csv to scrape the data from the CDC website, and creates three .csv files:
    - plotdat.csv, which is the disease data scraped from the CDC
    -location_names.csv, which is a file with the names of all locations we have data for, and the type of each location (state, region, country)
    -disease_names.csv, which is a file with the names of all diseases we have data for
    Note: the last two .csv files are used only to help create the user interface for the shiny app
    

-ui.R and server.R are the two .R files which run the shiny app using runApp("MMWRPlot")
