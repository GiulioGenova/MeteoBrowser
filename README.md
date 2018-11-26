# Meteo Browser South Tyrol
A Shiny App to download the meteorological time series from the Open Data - Province of Bozen 

To run the application install the library:

```R
if(!require(devtools)) install.packages("devtools")

devtools::install_github("GiulioGenova/MeteoBrowser",ref = "package")
```
and then use the function run_app

```R
run_app('MeteoBrowser')
```

A stable online version is available here:

https://euracalpenv.shinyapps.io/MeteoBrowserSouthTyrol/

And a dev version here:

https://giuliogenova.shinyapps.io/ODBZ/


How to:

Once selected the stations and sensors by filtering the table and/or drawing a polygon on the map and chosen a date range you are ready to download (the dataset starts
in the beginning of 2016). When download is complete, a "save as csv" icon pops up. You can choose between different time
aggregations. "raw" preserves the timestamp of the sensor you are downloading (usually 5 or 10 minutes). For the other time
aggregation options you will get the mean, maximum, minimum and sum values for each sensor in the selected timespan. You can also
choose between "wide" or "long" data. In wide data you get a column for every sensor, in long data you get a factor column called
"Sensor" and a column "Value" which stores the measurements.


Acknowledgements:

The application retrieves meteorological and hydrological time series from the South Tyrolean Meteo Open Data 
API (further documentation http://daten.buergernetz.bz.it/it/dataset/misure-meteo-e-idrografiche)

Special thanks to Mattia Rossi who has developed the R package "MonalisR" which plays and important role in the backend of this
application. You can find the package here:
https://gitlab.inf.unibz.it/earth_observation_public/MonalisR

Disclaimer:

Meteorologial data downloaded form this app comes with no warranty and has not been validated by the data provider nor by the authors of the app.
This app is intended only to facilitate retrieving data from the Meteorologial Open Data Suedtirol API