tags$html(
  tags$head(
    tags$title('About page')
  ),
  tags$body(
    h3("About the data"),
    p("This app provides a user-friendly interface to download meteorological data from the Open Data Suedtirol."),
    p("The Open Data Suedtirol API and its documentation can be found at the Province of Bozen web site."),
    p(a( href='http://daten.buergernetz.bz.it/it/dataset/misure-meteo-e-idrografiche',
         target='_blank','Open Data Suedtirol - Meteo data web service' )),
    p("Once selected the stations and sensors by filtering the table and/or drawing a polygon on the map and chosen 
a date range you are ready to download (the dataset starts in the beginning of 2016)."), 
p("When download is complete, a \x22save\x22 icon pops up and you can choose to save a csv or a json file. 
You can choose between different time aggregations. \x22raw\x22 preserves the timestamp of the sensor you are 
downloading (usually 5 or 10 minutes). For the other time aggregation options you will get different statistics
for each measurement in the selected timespan."), 
p("All measurements willa have a count (the sum) of No Data in the timespan selected."),
p("All measurements besides precipitation and wind direction will also have a mean value. "),
p("Precipitation and Air temperature will also have the sum value."), 
p("Air temperature and relative humidity will also have min and max values."),
p("Wind direction will be reclassified with the following values and the most frequent value will be displayed:"), 
p("1 = N; 2 = NE; 3 = E; 4 = SE; 5 = S; 6 = SW; 7 = W; 8 = NW."), 
p("You can also choose between \x22wide\x22 or \x22long\x22 data.
In wide data you get a column for every sensor, in long data you get a factor column called \x22Sensor\x22 and a column
\x22Value\x22 which stores the measurements."),
    br(),
  
    h3("Disclaimer"),
    p("Meteorologial data downloaded form this app comes with no warranty and has not been validated by the data 
provider nor by the authors of the app. This app is intended only to facilitate retrieving data from the Meteorologial 
Open Data Suedtirol API"),
  
    br(),
    
    h3("Run the app locally: Download from Github"),
    p("The source code is stored here:"),
    p(a( href='https://github.com/GiulioGenova/ODBZ/',
         target='_blank','Giulio Genova - ODBZ' )),
    p("To run the application locally run the following code:"),
    p("=========================================================================="),
    p("if (!require(\x22shiny\x22)) install.packages(\x22shiny\x22)"),
    p("shiny::runGitHub('GiulioGenova/ODBZ',subdir=\x22app\x22,launch.browser = TRUE)"),
    p("=========================================================================="),
    p("This will automatically install the packages needed on your machine (the first time you run it only) and then run the app."),
    br(),
    
    h3("Author"),
    p(a( href='https://github.com/GiulioGenova/',
         target='_blank','Giulio Genova' )),
    p("Eurac - Institute for Alpine Environment"),
    p("mail: giulio.genova@eurac.edu"),
    p("2018-06-18"),
    br(),
    h3("Acknowledgements"),
    p("Special thanks to Mattia Rossi who has developed the R package \x22MonalisR\x22 which plays and important role in the backend of this application. You can find the package here:"),
    
    p(a( href='https://gitlab.inf.unibz.it/earth_observation_public/MonalisR', 
         target='_blank','MonalisR'))
    
  )
)
