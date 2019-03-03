Meteo Browser South Tyrol
========================


A Shiny App to download the meteorological time series from the Open Data - Province of Bozen 

To run the application install the package:

```R
if(!require(devtools)) install.packages("devtools")

devtools::install_github("GiulioGenova/MeteoBrowser")
```
and then use the function run_app

```R
MeteoBrowser::run_app('MeteoBrowser')
```

A stable online version is available here:

http://meteobrowser.eurac.edu/


How to:
-------

Once selected the stations and measurements (also possible to draw a polygon on the map) and chosen a date range, 
you are ready to download (the dataset available refers to the last three years).
When download is complete, a "save" icon pops up and you can choose to save a csv or a json file.
You can choose between different time aggregations. "raw" preserves the timestamp of the sensor you are
downloading (usually 5 or 10 minutes). For the other time aggregation options you will get different statistics
for each measurement in the selected timespan.

![](https://github.com/GiulioGenova/MeteoBrowser/blob/master/MeteoBrowser.PNG)

In the "Readme" page of the application you will find further information about the app and the data source.


## Getting help

If you encounter a clear bug, please describe it and file it on 
[github](https://github.com/GiulioGenova/MeteoBrowser/issues).

## Contributing

If you would like to contribute, please create a [Pull
Request](https://github.com/GiulioGenova/MeteoBrowser/pulls) and make
appropriate applicable changes for review.

Acknowledgements:
-------

The application retrieves meteorological and hydrological time series from the South Tyrolean Meteo Open Data 
API (further documentation http://daten.buergernetz.bz.it/it/dataset/misure-meteo-e-idrografiche)

Special thanks to Mattia Rossi who has developed the R package "MonalisR" which plays and important role in the backend of this
application. You can find the package here:
https://gitlab.inf.unibz.it/earth_observation_public/MonalisR

Disclaimer:
-------

Meteorologial data downloaded form this app comes with no warranty and has not been validated by the data provider nor by the authors of the app.
This app is intended only to facilitate retrieving data from the Meteorologial Open Data Suedtirol API
