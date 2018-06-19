tags$html(
  tags$head(
    tags$title('About page')
  ),
  tags$body(
    h3("About the data"),
    p("This app provides a user-friendly interface to download data from the Open Data S\uumldtirol."),
    p("The Open Data S\uumldtirol API and its documentation can be found at the Province of Bozen web site."),
    p(a( href='http://daten.buergernetz.bz.it/it/dataset/misure-meteo-e-idrografiche',
         target='_blank','Open Data S\u252dtirol - Meteo data web service' )),
    p("Once selected the stations and sensors by filtering the table and chosen a date range you are ready to download (the dataset starts in the beginning of 2016). When download is complete, a save csv icon pops up. You can choose between different time aggregations. raw preserves the timestamp of the sensor you are downloading (usually 5 or 10 minutes). For the other time aggregation options you will get the mean, maximum, minimum and sum values for each sensor in the selected timespan. You can also choose between wide or long data. In wide data you get a column for every sensor, in long data you get a factor column called Sensor and a column Value which stores the measurements."),
    br(),
    h3("Acknowledgements"),
    p("Special thanks to Mattia Rossi who has developed the R package MonalisR which plays and important role in the backend of this application. You can find the package here:"),
    
    p(a( href='https://gitlab.inf.unibz.it/earth_observation_public/MonalisR', 
         target='_blank','MonalisR'))
    
  )
)
