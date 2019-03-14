---
title: 'Meteo Browser South Tyrol: A Shiny App to download the meteorological time series from the Open Data Catalogue of the Province of Bolzano/Bozen - Italy'
tags:
  - Open Data
  - R
  - Shiny
authors:
 - name: Giulio Genova
   orcid: 0000-0001-9412-8651
   affiliation: 1
 - name: Mattia Rossi
   orcid: 0000-0003-4760-1607
   affiliation: 2
 - name: Georg Niedrist
   orcid: 0000-0002-7511-6273
   affiliation: 1
 - name: Stefano Della Chiesa
   orcid: 0000-0002-6693-2199
   affiliation: 1
affiliations:
 - name: eurac research, Institute for Alpine Environment, Bolzano/Bozen, Italy.
   index: 1
 - name: eurac research, Institute for Earth Observation, Bolzano/Bozen, Italy.
   index: 2
date: 14 March 2019
bibliography: paper.bib
---

# Summary

Promoting open data framework with a comprehensive data infrastructure increases
publicly available knowledge and provides new unexplored benefits [@Janssen2012]. Moreover, combining meteorological and
hydrological open data with other datasets can have a positive 
impact in environmental application [@Zuiderwijk2014].

The Open Data catalogue of South Tyrol (<http://daten.buergernetz.bz.it/de/>) is
a bilingual (Italian and German) public database with a large variety of local
data and aims to facilitate data search and data re-usability. The Meteo section
of the Open Data catalogue contains several meteorological and hydrological
variables coming from 120 real-time monitoring stations. Precisely, measurements
of air temperature, air humidity, precipitation, wind speed, wind direction,
solar radiation, hours of sunlight, river discharge, water level, ground water
level. The dataset available refers to the last three years.

Meteo Browser South Tyrol is a web application based on the statistical programming
language R [@Team2018] and the package Shiny [@Chang2015], which provides the framework for building responsive web based
applications using javascript. Its purpose is to access and retrieve
meteorological and hydrological time series from the South Tyrolean Meteo Open
Data API (further documentation
<http://daten.buergernetz.bz.it/de/dataset/misure-meteo-e-idrografiche>).

The tool creates multiple queries that filter and resample the available data
based on the user's need. Users can interact with leaflet graphical
representation of all stations, calculate distances and draw polygons to select
the target stations. They can also filter by station, date range, altitude and type of
measurement. Data time series can be exported as comma separated values (.csv)
or JSON and the format is compliant to tidy data frames [@Wickham2014], both
long and wide tables can be downloaded. The app is available in English, German
and Italian.

Meteo Browser South Tyrol is a user-friendly web tool beneficial for a wide range
of users, from common citizens to students as well as researchers, private
companies and public administration.

Thus, the Meteo Browser South Tyrol allows to freely and easily access hydrological
and meteorological data, promoting dissemination for a wide range of users, from
common citizens to students as well as researchers, private companies and the public
administration. Overall this promotes the development of services in several
fields such as, education, hydrology and agriculture [@carolan2015can].

Finally, the developed web application provides a fast and comprehensible
solution to access, visualize and download openly available databases exposed to
a JSON-based API. This can be further developed and possibly expanded to other
networks based on similar APIs and Open Data standardization.

The application is at the following link:

<http://meteobrowser.eurac.edu/>

The repository is at the following link:

[https://github.com/GiulioGenova/MeteoBrowser](https://github.com/GiulioGenova/MeteoBrowser)


# References
