if (!require("devtools")) install.packages("devtools")
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("DT")) install.packages("DT")
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("geojsonio")) install.packages("geojsonio")
if (!require("stringr")) install.packages("stringr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("tibble")) install.packages("tibble")
if (!require("tidyr")) install.packages("tidyr")
if (!require("spdplyr")) install.packages("spdplyr")
if (!require("dbplyr")) install.packages("dbplyr")
if (!require("pbapply")) install.packages("pbapply")
if (!require("lubridate")) install.packages("lubridate")
if (!require("leaflet")) devtools::install_github("rstudio/leaflet")
if (!require("leaflet.extras")) devtools::install_github("bhaskarvk/leaflet.extras")
#if (!require("leaflet")) install.packages("leaflet")
#if (!require("leaflet.extras")) install.packages("leaflet.extras")
if (!require("sp")) install.packages("sp")
if (!require("raster")) install.packages("raster")

library(raster)
library(pbapply)
library(httr)
library(jsonlite)
library(geojsonio)
library(stringr)
library(tibble)
library(shinyjs)
library(sp)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(readr)
library(DT)

source(file.path(getwd(),"download_resample.R"))
source(file.path(getwd(),"MonalisR.R"))

header <- dashboardHeader(titleWidth = 480)

anchor <- tags$a(href='http://www.eurac.edu/',
                 tags$img(style="vertical-align: bottom;width: 270px;padding-right: 10px;",
                          #src='http://www.eurac.edu/Style%20Library/logoEURAC.jpg'),#, height='60', width='50'
                          src='LogoProvEurac.PNG'),
                 'Meteo Browser South Tyrol',class='tit')

header$children[[2]]$children <- tags$div(anchor,class = 'name')




translation<-read.csv(file.path(getwd(),"translationUtf.csv"),header = T,sep = ",",stringsAsFactors = F,encoding = "UTF-8")#

tr <- function(key,l,t=translation){ # translates text into current language
  x<-as.character(t[grep(key,t$key),l])
  return(x)
}

leaf.proj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
LL <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

url <- "http://daten.buergernetz.bz.it/services/meteo/v1/sensors"
u <- GET(url) %>% content
se<-bind_rows(u)
st<-getMeteoStat()
legend_tab<-full_join(st,se)%>%#dplyr::select(-NAME_L,-NAME_E,-DESC_L,-DATE,VALUE,-LAT,-LONG,-VALUE)%>%
  mutate_if(is.character, funs(as.factor(.)))%>%
  dplyr::mutate(
    id=row_number(),
    DESC_E=as.factor(ifelse(DESC_D=="relative Luftfeuchte","Relative humidity",
                            ifelse(DESC_D=="Niederschlag","Precipitation",
                                   ifelse(DESC_D=="Windgeschwindigkeit","Wind speed",
                                          ifelse(DESC_D=="Windrichtung","Wind direction",
                                                 ifelse(DESC_D=="Windgeschwindigkeit Böe","Wind direction",
                                                        ifelse(DESC_D=="Luftdruck","Atmospheric pression",
                                                               ifelse(DESC_D=="Globalstrahlung","Solar Radiation",
                                                                      ifelse(DESC_D=="Sonnenscheindauer","Sunshine hours",
                                                                             ifelse(DESC_D=="Schneehöhe","Snow height",
                                                                                    ifelse(DESC_D=="Wassertemperatur","Water temperature",
                                                                                           ifelse(DESC_D=="Lufttemperatur","Air temperature",
                                                                                                  ifelse(DESC_D=="Durchfluss","Water flow",
                                                                                                         ifelse(DESC_D=="Wasserstand","Water level",
                                                                                                                ifelse(DESC_D=="Grundwasserstand","Groundwater level","unknown"))))))))))))))))


tot_tab<-legend_tab%>%dplyr::select(-NAME_L,-DESC_L,-DATE,-LAT,-LONG,-VALUE) %>%
  dplyr::mutate(NAME = paste(NAME_D,NAME_I,sep=" / ")) %>%
  dplyr::mutate(DESC_D = paste(DESC_D,UNIT,sep=" - ")) %>%
  dplyr::mutate(DESC_I = paste(DESC_I,UNIT,sep=" - ")) %>%
  unite(DESC_E,DESC_E,UNIT,sep=" - ")

legend_tab<-legend_tab%>%dplyr::select(-DATE,-VALUE)

se_spread<-se %>% dplyr::select(SCODE,TYPE,UNIT,VALUE,DATE) %>% 
  dplyr::mutate(DATE=paste0("(",as_datetime(DATE),")"))%>%
  unite(VALUE,VALUE,UNIT,sep=" ") %>% 
  dplyr::mutate(VALUE=paste0("<b>",VALUE,"</b>"))%>%
  unite(VALUE,VALUE,DATE,sep=" ") %>% 
  spread(TYPE,VALUE)

sspat<-getMeteoStat(format = "spatial")
se_spread<-left_join(sspat,se_spread)


green <- awesomeIcons(icon = "ios-checkmark", iconColor = "black",
                      library = "ion", markerColor = "green")

grey <- awesomeIcons(icon = "ios-close", iconColor = "black", 
                     library = "ion", markerColor = "lightgray")

blu <- awesomeIcons(icon = "ios-close", iconColor = "black", 
                    library = "ion", markerColor = "blue")

refcols <- c("TimeStamp", "NAME")#,"SCODE"