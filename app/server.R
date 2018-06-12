
#if (!require("pacman")) install.packages("pacman")

#pacman::p_load(package1, package2, package_n)
#pac=c("dplyr","readr","tibble","tidyr","spdplyr","lubridate","pbapply",
  #    "shiny","shinydashboard","shinyjs","DT","httr","leaflet","leaflet.extras",
  #    "jsonlite","geojsonio","stringr")
#pacman::p_load(char = pac)
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("tibble")) install.packages("tibble")
if (!require("tidyr")) install.packages("tidyr")

if (!require("spdplyr")) install.packages("spdplyr")
if (!require("dbplyr")) install.packages("dbplyr")
if (!require("pbapply")) install.packages("pbapply")
if (!require("lubridate")) install.packages("lubridate")
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("DT")) install.packages("DT")
if (!require("httr")) install.packages("httr")
if (!require("leaflet")) install.packages("leaflet")
if (!require("leaflet.extras")) install.packages("leaflet.extras")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("geojsonio")) install.packages("geojsonio")
if (!require("stringr")) install.packages("stringr")

#install.packages("pbapply")
#install.packages("DT")
#devtools::install_github("ropensci/plotly")

#devtools::install_github("ropensci/geojsonio")
#install.packages("rgdal", type = "source")
#install.packages("rgeos", type = "source")
#devtools::install_github("ropensci/geojsonio")

#library(ggplot2)
#library(DBI)
#library(RMariaDB)
#library(MonalisR)
#library(dplyr)
#library(dbplyr)
#library(lubridate)
#library(plotly)
#library(shinydashboard)
#library(shiny)
#library(leaflet)
#library(leaflet.extras)
#library(readr)
#library(rgdal)
library(pbapply)
#library(tidyr)
#library(spdplyr)
#library(DT)
library(httr)
library(jsonlite)
library(geojsonio)
library(stringr)
library(tibble)
library(shinyjs)
#source("//ABZ02FST.EURAC.EDU/AlpEnv/Projekte/SBR/01_Workspace/GeG/resample_SBR.R")
#source("H:/Projekte/SBR/09_R_Script/resample_SBR_func.R")
#wd<-setwd("H:/Projekte/SBR/01_Workspace/GeG/Shiny_wd")
#setwd("C:/temp_R/shiny_prov/")
#setwd("H:/Projekte/SBR/01_Workspace/GeG/ODBZ")
source(file.path(getwd(),"download_resample.R"))
source(file.path(getwd(),"MonalisR.R"))
#file<-file.path(wd,"Orchards_monitoring_station_MONALISA-SBR_Project_21032018.kml")
#file<-"~/Orchards_monitoring_station_MONALISA-SBR_Project_21032018.kml"
#getwd()
# y axis title
RHy <- list(title = "Air RH [%]")
Airy <- list(title = "Air T [°C]")
Py <- list(title = "P & Irrig [mm]")
windy <- list(title = "Wind speed [m/s]")
SWCy <- list(title = "SWC [%]")
SWPy<- list(title = "SWP [%]")
STy <- list(title = "Soil T [°C]")

# colors
RHcolor<-"#0188AE"
AirTmeancolor<-"#000A10"
AirTmaxncolor<-"#D81159"
AirTmincolor<-"#0496FF"
Pcolor<-"#1778AE"
Irrigcolor<-"#00273D"
Windcolor<-"#0D0508"
SWC20color<-'red'#"#8F2D56"#same for SWP
SWC40color<-'blue'#"#006BA6"#same for SWP
ST20color<-'red'#"#8F2D56"
ST40color<-'blue'#"#006BA6"

# linetype
STline<-"dash"
RHline<-"solid"
AirTline<-"soild"

url <- "http://daten.buergernetz.bz.it/services/meteo/v1/sensors"
u <- GET(url) %>% content
se<-bind_rows(u)
st<-getMeteoStat()
tot_tab<-full_join(st,se)%>%select(-NAME_L,-NAME_E,-DESC_L,-DATE,-VALUE,-LAT,-LONG)%>%
  mutate_if(is.character, funs(as.factor(.)))#%>%as.data.frame()


server <- function(input, output,session) {
  #observeEvent(input$stop,{
   # session$reload()
    #return()
  #})
  output$table<-renderDT(tot_tab, filter = 'top',rownames=F,selection="none",
                         #,server = T,
                         options = list(autoWidth = F,scrollX=T)
                         )#,server = TRUE,options=list(pageLength = 100,scrollX = T),  escape = FALSE
  
  ## connenction and query on province database
  D <- reactiveValues(documents = NULL)
  #inputdb=reactive({
    observeEvent(input$refresh,{
    ids<-input$table_rows_all
    #nstations<-length(ids)%>%as.numeric
    #nstations<-10
    #ids<-c(1,3,7,24)
    
    #station<-tot_tab[ids,'SCODE']%>%unique#%>%as.vector
    #sensors<-tot_tab[ids,'TYPE']%>%unique
    station<-unique(tot_tab$SCODE[ids])%>%as.character
    sensors<-unique(tot_tab$TYPE[ids])%>%as.character
    nstations<-length(station)%>%as.numeric*length(sensors)%>%as.numeric
    
    #tab_station<-getMeteoStat()%>%filter(NAME_D%in%input$Station)#st
    ##tab_station<-getMeteoStat()%>%filter(NAME_D%in%st)#st
    
    #tab_sensor<-getMeteoSensor()%>%filter(Sensor%in%input$Sensor)#sn
    ##tab_sensor<-getMeteoSensor()%>%filter(Sensor%in%sn)#sn
    
    #tab<-left_join(tab_station,tab_sensor,"SCODE")
    #nstations<-tab$NAME_D
    
    #statname=input$Station
    #stations<-getMeteoStat()
    #stat_code<-stations%>%filter(NAME_D%in%statname)
    #station<-stat_code$SCODE
    
    #sensors<-input$Sensor
    
    datestart<-as.character(input$daterange[1])
    dateend<-as.character(input$daterange[2])
    #out_dir="H:/Projekte/SBR/04_Data/06_daily_resample"
    round=input$round
    
    if(as_date(datestart)<=dateend){
    withProgress(message = 'Getting data', value = 0, {
    db<-dwnld(station=station,datestart=datestart,dateend=dateend,sensors=sensors,nstations=nstations)#round=round,
    })#
    }else{db<-"Error in selecting the date range. First date must be earlier than last date"}
    D$documents <- list(db)
    
    })
  
  #downloaded <- eventReactive(input$refresh,{#,input$daterange,input$round,input$Station
   # as.data.frame(inputdb())
    #})
  
    observeEvent(input$stop,{
      session$reload()
      return()
    })
    
    
  output$map<-renderLeaflet({
    ids<-input$table_rows_all
    station<-unique(tot_tab$SCODE[ids])%>%as.character
    stations_sel<-getMeteoStat(format = "spatial")%>%filter(SCODE%in%station)#NAME_D%in%input$Station get spatial stations database (Province) with the seleced SCODEs
    plotMeteoLeaflet(stations_sel)
    
  })
  
  output$message<-renderText({
    datestart<-as.character(input$daterange[1])
    dateend<-as.character(input$daterange[2])
    if(as_date(datestart)>as_date(dateend)){
    messagedate<-"Error in selecting the date range. First date must be earlier than last date"}else{NULL}
  })
  
  output$selected<-renderText({
    ids<-input$table_rows_all
    nstation<-unique(tot_tab$SCODE[ids])%>%as.character %>% length
    nsensors<-unique(tot_tab$TYPE[ids])%>%as.character %>% length
    if(nstation==1) stat<-" station" else{stat<- " stations"}
    if(nsensors==1) param<-" parameter" else{param<- " parameters"}
    mssg<- paste("You have selected n° ", nstation,stat," and ",nsensors,param)
    
  })
  #observe({
  #  click<-input$map_marker_click
   # if(is.null(click))
   #   return()
    
   # text2<-paste("You've selected point ", click$id,click$lat,click$lng)
   # output$Click_text<-renderText({
   # text2
   # })
 # })
   
  output$rightdate <-reactive({
    datestart<- input$daterange[1] %>% as.character %>% as_date
    dateend<- input$daterange[2] %>% as.character %>% as_date
    return(datestart<=dateend)
    
  })
  
  output$tablebuilt <-reactive({
    return(!is.null(D$documents))
    #return(any(D$documents[[1]]))
  })
  
  output$downloadData <- downloadHandler(
  filename = function() {
    #station=as.character(input$Station)
    #startdate<-as.character(input$daterange[1])
    #enddate<-as.character(input$daterange[2])
    
    startdate<-as.character(min(as_date(D$documents[[1]]$TimeStamp)))
    enddate<-as.character(max(as_date(D$documents[[1]]$TimeStamp)))
    gather<-input$gather
    round=input$round
    nstat=D$documents[[1]]$Station %>% unique %>% length %>% as.character#                     as.character(length(unique(D$documents[[1]]$Station))) 
    #paste0(station,'_',round,'_',startdate,'_',enddate,'.csv')
    paste0(nstat,'stat','_',startdate,'_',enddate,'_',round,'_',gather,'.csv')#,round
  },
  content = function(con) {
    round=input$round
    df=D$documents[[1]] 
    
    if(input$gather=="wide"){
      spread=TRUE}else{
    
      spread=FALSE
    }
    db=resample_provBz_data(df=df,round=round,spread=spread)
    
    write.csv(x=db,file =  con,quote = F,row.names = F,na = "NA",sep = ",",dec = ".")
    
  }
  
  
)
  
  
  outputOptions(output, 'tablebuilt', suspendWhenHidden=FALSE)
  outputOptions(output, 'rightdate', suspendWhenHidden=FALSE)
}
