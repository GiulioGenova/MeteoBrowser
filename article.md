---
title: 'SoilCalibrationRegression: R interface to retrieve soil moisture regression to calibrate sensor measurement vs soil moisture gravimetric measurement.'
tags:
  - Open Data
  - R
  - Shiny
authors:
 - name: Giulio Genova
   orcid: 0000-0003-0872-7098
   affiliation: 1
 - name: Mattia Rossi
   orcid: 0000-0000-0000-0000
   affiliation: 2
 - name: Georg Niedrist
   orcid: 0000-0003-0872-7098
   affiliation: 1
 - name: Stefano Della Chiesa
   orcid: 0000-0000-0000-0000
   affiliation: 1
affiliations:
 - name: eurac research, Institute for Alpine Environment, Bolzano/Bozen, Italy.
   index: 1
 - name: eurac research, Institute for Earth Observation, Bolzano/Bozen, Italy.
   index: 2
date: 02 October 2018
bibliography: article.bib
---

# Summary

Promoting open data framework with a comprehensive data infrastructure increases
publicly available knowledge and provide new unexplored benefits [@Janssen2012]. Moreover, combining meteorological and
hydrological open data with other datasets unlock a big potential, with larger
impact in environmental application [@Zuiderwijk2014].

The Open Data catalogue of South Tyrol (<http://daten.buergernetz.bz.it/de/>) is
a bilingual (Italian and German) public database with a large variety of local
data and aims to facilitate data search and data reusability. The Meteo section
of the Open Data catalogue contains several meteorological and hydrological
variables coming from 120 real-time monitoring stations. Precisely, measurements
of air temperature, air humidity, precipitation, wind speed, wind direction,
solar radiation, hours of sunlight, river discharge, water level, ground water
level. The time series begin in 2015 onwards.

MeteoBrowserSouthTyrol is a web application based on the statistical programming
language R [@Team2018] and the package Shiny [@Chang2015], which provides the framework for building responsive web based
applications using javascript. Its purpose is to access and retrieve
meteorological and hydrological time series from the South Tyrolean Meteo Open
Data API (further documentation
<http://daten.buergernetz.bz.it/de/dataset/misure-meteo-e-idrografiche>).

The tool creates multiple queries that filter and resample the available data
based on the user's need. User can interact with leaflet graphical
representation of all stations, calculate distances and draw polygons to select
the target stations. Filter by station, date range, altitude and type of
measurement. Data time series can be exported as comma separated values (.csv)
or JSON and the format is compliant to tidy data frames [@Wickham2014], both
long and wide tables can be downloaded. The app is available in English, German
and Italian.

MeteoBrowserSouthTyrol is a user-friendly web tool beneficial for a wide range
of users, from common citizens to students as well as researchers, private
companies and public administration.

Thus, the MeteoBrowserSouthTyrol allows to freely and easily access hydrological
and meteorological data, promoting dissemination for a wide range of users, from
common citizens to students as well as researcher, private company and public
administration. Overall this promote the development of services in several
fields such as, education, hydrology and agriculture [@carolan2015can].

Finally, the developed web application provides a fast and comprehensible
solution to access, visualize and download openly available databases exposed to
a JSON based API”. This can be further developed and possibly expanded to other
networks based on similar APIs and Open Data standardization.

The application is at the following link:

<https://euracalpenv.shinyapps.io/MeteoBrowserSouthTyrol/>

The repository is at the following link:

[https://github.com/GiulioGenova/MeteoBrowser](https://github.com/GiulioGenova/MeteoBrowser)


# References
