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
if (!require("sp")) install.packages("sp")
if (!require("raster")) install.packages("raster")
#if (!require("rgdal")) install.packages("rgdal")


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
library(raster)
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
library(sp)
#library(rgdal)


source(file.path(getwd(),"download_resample.R"))
source(file.path(getwd(),"MonalisR.R"))
#load(file.path(getwd(),"translationNew.RDATA"))
#translation<-read.csv(file.path(getwd(),"translationNew.csv"),header = T,sep = ",",stringsAsFactors = F,encoding = "UTF-8")#
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
tot_tab<-full_join(st,se)%>%dplyr::select(-NAME_L,-NAME_E,-DESC_L,-DATE,VALUE,-LAT,-LONG,-VALUE)%>%
  mutate_if(is.character, funs(as.factor(.)))#%>%as.data.frame()

se_spread<-se %>% dplyr::select(SCODE,TYPE,UNIT,VALUE,DATE) %>% 
  dplyr::mutate(DATE=paste0("(",as_datetime(DATE),")"))%>%
  unite(VALUE,VALUE,UNIT,sep=" ") %>% 
  dplyr::mutate(VALUE=paste0("<b>",VALUE,"</b>"))%>%
  unite(VALUE,VALUE,DATE,sep=" ") %>% 
  spread(TYPE,VALUE)

    green <- awesomeIcons(icon = "ios-close", iconColor = "black", 
library = "ion", markerColor = "green")
  grey <- awesomeIcons(icon = "ios-close", iconColor = "black", 
library = "ion", markerColor = "lightgray")
    blu <- awesomeIcons(icon = "ios-close", iconColor = "black", 
library = "ion", markerColor = "blue")

server <- function(input, output,session) {
  
  #output$about_out  <- renderUI({
  #if(input$language=="en"){
  #about = source(file.path(getwd(),'about_EN.R'))
  #}else if(input$language=="de"){
  #about = source(file.path(getwd(),'aboutDe.R'))
  #}else if(input$language=="it"){
  #about = source(file.path(getwd(),'about_IT.R'))
   
  #}
  #about$value
  #})

  output$about_out  <- renderUI({
  if(input$language=="en"){
  x <- includeHTML(file.path(getwd(),"about_EN.html"))
  }else if(input$language=="de"){
  x <- includeHTML(file.path(getwd(),"about_DE.html"))
  }else if(input$language=="it"){
  x <- includeHTML(file.path(getwd(),"about_IT.html"))
  } 
  x
  })
  
  ### moved from UI due to multilanguage
  output$save  <- renderMenu({
  conditionalPanel(condition = "output.tablebuilt",#br(),#"input.daterange[1]<=input.daterange[2]"
                 div(style="display: inline-block;vertical-align:top; width: 40%;",
                     downloadButton('downloadData', h4(tr("savecsvjson",input$language)) ,class="butt")),#
                     div(style="display: inline-block;vertical-align:top; width: 45%;",
                         radioButtons(inputId = "csvjson",label = tr("tableType",input$language),
                                      choices = list("csv","json"))))

})
    
  output$Data  <- renderMenu({
    sidebarMenu(
  menuItem(tr("menuData",input$language), tabName = "Data", icon = icon("bar-chart-o"))
    )
  })
  
  output$about  <- renderMenu({
    sidebarMenu(
  menuItem(tr("menuReadme",input$language), tabName = "about", icon = icon("info-circle"))
      )
  })
  
  output$refresh  <- renderUI({
    #datestart<- input$daterange[1] %>% as.character %>% as_date
    #dateend<- input$daterange[2] %>% as.character %>% as_date
    #if(datestart<=dateend){actionButton(label= as.character(translation[grep("refresh",translation$key),input$language]),"refresh")}
    
    conditionalPanel(condition = "output.rightdate",#br(),
                     actionButton(label= tr("refresh",input$language),"refresh")) 
  })
  
  output$deletebtn  <- renderUI({
  if(input$spatialSelection==TRUE){
    actionButton("deletebtn", tr("deletebtn",input$language))}
  })
  
  #output$deletebtn  <- renderUI({
  #conditionalPanel(condition = "output.spatialSelection",br(),
  #  actionButton("deletebtn", as.character(translation[grep("deletebtn",translation$key),input$language])))
  #})
  
  
  output$downloadInstructions <- renderText({
    tr("downloadInstructions",input$language)
    })
  
  output$tableInstructions  <- renderText({
    tr("tableInstructions",input$language)
    
    #tr(text="tableInstructions",lenguage=as.character(input$lenguage),translation=translation)
    })
  
  output$daterange<-renderUI({
    
   dateRangeInput(label = h4(tr("daterange",input$language)),inputId = "daterange",separator = " - ",min = "2000-01-01",#
                                                             start = Sys.Date()-3,
                                                             end = Sys.Date()+1,language=input$language)
   })
  
  output$round<-renderUI({
    
   selectInput("round",label = h4(tr("roundLabel",input$language)),
                               choices = list(tr("raw",input$language),
                                              tr("hour",input$language),
                                              tr("day",input$language),
                                              tr("week",input$language),
                                              tr("month",input$language),
                                              tr("year",input$language)
                                             ))
    })
  
   output$gather<-renderUI({
    
   selectInput("gather", label= h4(tr("gatherLabel",input$language)),
                                choices = list(tr("wide",input$language),
                                                tr("long",input$language)
                                              ))
   })
  
  output$spatSel<-renderText({
    
  tr("spatialSelection",input$language)
                              
  })
  #output$spatialSelection<-renderUI({
  #checkboxInput("spatialSelection", label = as.character(translation[grep("spatialSelection",translation$key),input$language]),
  #                              value = FALSE)
  #})
  # output.spatialSelection
  #
  output$message<-renderUI({
    #datestart<-as.character(input$daterange[1])
    #dateend<-as.character(input$daterange[2])
    #if(as_date(datestart)>as_date(dateend)){
      #messagedate<-as.character(translation[grep("messageDate",translation$key),input$language])
    #}else{messagedate<-NULL}
     # messagedate
      
      #conditionalPanel(condition = "output.rightdate==false",br(),
      #as.character(translation[grep("messageDate",translation$key),input$language]))
      
      conditionalPanel(condition = "output.rightdate==false",br(),
      renderText(tr("messageDate",input$language)))
                             
                             
                             
  })
  
  #output$message<-renderText({
   # datestart<-as.character(input$daterange[1])
    #dateend<-as.character(input$daterange[2])
    #if(as_date(datestart)>as_date(dateend)){
     # messagedate<-"Error in selecting the date range. First date must be earlier than last date"}else{NULL}
#})
  
  output$selected<-renderText({
    
    #station<-StatSens()[[1]]
    #sensors<-StatSens()[[2]]
    
    station <- StatSens$station
    sensors <- StatSens$sensors
    
    nstation <- length(station)%>%as.numeric
    nsensors <- length(sensors)%>%as.numeric
    
    if(nstation==1){ stat <- tr("aaa",input$language)}
    else{stat <- tr("bbb",input$language)}
    
    if(nsensors==1){param<-tr("ccc",input$language)}
    else{param <- tr("ddd",input$language)}
    
    youHave <- tr("youvSelected",input$language)
    and <- tr("Sensorsand",input$language)
    mssg <- paste(youHave,
                 nstation,stat,
                 and,
                 nsensors,param
                )
    mssg
    #return(mssg)
  })
  
  
    output$selected_list<-renderText({
    
    #station<-StatSens()[[3]]
    station<-StatSens$stationName
    
    mssg<- paste(station,collapse="; ")
    
  })
  
  output$selected_listSensors<-renderText({
    
    #station<-StatSens()[[3]]
    sensors<-StatSens$sensors
    
    mssg<- paste(sensors,collapse="; ")
    
  })
  
  
  output$youSelStations <- renderText({
    
    tr("youSelStations",input$language)
    
  })
  
  output$andSensors <- renderText({
    
    tr("andSensors",input$language)
    
  })
  
  ### end of moved from UI due to multilanguage
  ###############
  
  output$table<-DT::renderDT({
    
    if(input$language=="it"){
    tot_tab<-tot_tab%>%dplyr::select(-NAME_D,-DESC_D)#%>%
      #rename(NAME_I=NOME,TYPE=SENSORE,ALT=ALTITUDINE)
    }else if(input$language=="de"){
    tot_tab<-tot_tab%>%dplyr::select(-NAME_I,-DESC_I)#%>%
      #rename(TYPE=SENSOR)
    }else{
    tot_tab<-tot_tab%>%dplyr::select(-DESC_I)%>%#,-NAME_I
      #dplyr::mutate(DESC_D=translation[which(substr(translation$de,start = 1,stop = nchar(translation$de)-1)==DESC_D),"en"])
      dplyr::mutate(DESC_D=as.factor(ifelse(DESC_D=="relative Luftfeuchte","Relative humidity",
                              ifelse(DESC_D=="Niederschlag","Precipitation",
                                     ifelse(DESC_D=="Windgeschwindigkeit","Wind speed",
                                            ifelse(DESC_D=="Windrichtung","Wind direction",
                                                   ifelse(DESC_D=="Windgeschwindigkeit Böe","Wind direction",
                                                          ifelse(DESC_D=="Luftdruck","Atmospheric perssion",
                                                                 ifelse(DESC_D=="Globalstrahlung","Solar Radiation",
                                                                        ifelse(DESC_D=="Sonnenscheindauer","Sunshine hours",
                                                                               ifelse(DESC_D=="Schneehöhe","Snow height",
                                                                               ifelse(DESC_D=="Wassertemperatur","Air temperature",
                                                                                      ifelse(DESC_D=="Lufttemperatur","Water flow",
                                                                                             ifelse(DESC_D=="Durchfluss","Water flow",
                                                                                                    ifelse(DESC_D=="Wasserstand","Water level",
                                                                                                           ifelse(DESC_D=="Grundwasserstand","Groundwater level","unknown"))))))))))))))))%>%
      dplyr::rename(DESC_E = DESC_D)
      
    }
    
    dt<-datatable(tot_tab, filter = 'top',rownames=F,selection="none",
              options = list(autoWidth = F,scrollX=T,
                            searchCols = list(list(search = '["83200MS"]'), NULL, NULL, NULL,#
                                      NULL, NULL, NULL )
                            )
    ) 
    
    if(input$language=="it"){
    dt<-dt%>% 
      formatStyle(c("TYPE",  "DESC_I","UNIT"),
                  backgroundColor = "#edf5e1")
    }else if(input$language=="de"){
    dt<-dt%>% 
      formatStyle(c("TYPE", "DESC_D", "UNIT"),
                  backgroundColor = "#edf5e1")
    }else{
    dt<-dt%>%formatStyle(c("TYPE", "DESC_E", "UNIT"),
                  backgroundColor = "#edf5e1")
    }
    
    dt
  
  })
  
  #polyCoord <- reactive({
   # polygon_coordinates = input$map_draw_new_feature$geometry$coordinates[[1]]
   # return(polygon_coordinates)
  #})
  polyCoord <- reactiveVal(NULL)
  
  observeEvent(input$deletebtn,{
    polygon_coordinates = NULL
    polyCoord(polygon_coordinates)
    })
  
  #observeEvent(req(input$map_draw_stop),{
    observe({
    req(input$map_draw_stop)
      polygon_coordinates = input$map_draw_new_feature$geometry$coordinates[[1]]
    polyCoord(polygon_coordinates)
    })
  
  

  
  D <- reactiveValues(documents = NULL)
  
  StatSens<-reactiveValues(station= c(),sensors= c(),stationName = c())#station= NULL,sensors= NULL,stationName = NULL  
  
  #reactive({
  observe({#input$updateSelection
    input$tot_tab_state
    #input$tot_tab_search_columns
    #req(input$map_draw_stop)
    observe({
    ids<-input$table_rows_all

    
    station<-unique(tot_tab$SCODE[ids])%>%as.character
    sensors<-unique(tot_tab$TYPE[ids])%>%as.character
    if(input$language=="it"){
      stationName<-unique(tot_tab$NAME_I[ids])%>%as.character
      }else {
       stationName<-unique(tot_tab$NAME_D[ids])%>%as.character
    
    }
    #########################################################
    #input$spatialSelection
      if(input$spatialSelection){#FALSE
      #req(input$map_draw_stop)
      stations_sp <- getMeteoStat(format = "spatial")%>%filter(SCODE%in%station)
      
      
      
      #get the coordinates of the polygon
      #polygon_coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
      #polygon_coordinates <- polyCoord$polygon_coordinates
      polygon_coordinates <-polyCoord()
      if(is.null(polygon_coordinates)|is.null(ids)){
          station<-NULL;sensors<-NULL;stationName<-NULL
          }else{
      drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
      sp <- SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))
      
      # set coords as latlong then transform to leaflet projection
      proj4string(sp) <- LL
      polyre <- spTransform(sp, leaf.proj)
      
      stations_sp<-spTransform(stations_sp,leaf.proj)
      
      selected_stats <- stations_sp %over% polyre
      
      sp_sel<-stations_sp %>% dplyr::filter(row_number()%in%which(!is.na(selected_stats)))
      
      station<-unique(sp_sel$SCODE) %>% as.character
      
        if(input$language=="it"){
      stationName<-unique(sp_sel$NAME_I) %>% as.character
      }else {
       stationName<-unique(sp_sel$NAME_D) %>% as.character
        }
      
      filterForSensor<-tot_tab[ids,]%>% dplyr::filter(SCODE%in%station)
      sensors<-unique(filterForSensor$TYPE)%>%as.character
    }
      }
    #return(list(station,sensors,stationName))
    StatSens$station<-station
    StatSens$sensors<-sensors
    StatSens$stationName<-stationName
  })
    })
  
  #})
  
  
  
  observeEvent(input$refresh,{
    ids<-input$table_rows_all
    
    #station<-StatSens()[[1]]
    #sensors<-StatSens()[[2]]
    
    station<-StatSens$station
    sensors<-StatSens$sensors
    
    datestart<-as.character(input$daterange[1])
    dateend<-as.character(as_date(input$daterange[2])+1)
    
    round=input$round
    
    nstations<-length(station)%>%as.numeric*length(sensors)%>%as.numeric
    
    
    if(as_date(datestart)<=dateend){
      withProgress(message = 'Getting data', value = 0, {
        db<-dwnld(station=station,datestart=datestart,dateend=dateend,sensors=sensors,nstations=nstations)#round=round,
      })#
    }else{db<-"Error in selecting the date range. First date must be earlier than last date"}
    D$documents <- list(db)
    
  })
  
  
  observeEvent(input$stop,{
    session$reload()
    return()
  })
  
  
 observe({

  ids<-input$table_rows_all
  stationTab<-unique(tot_tab$SCODE[ids])%>%as.character
  station<-StatSens$station
  stations_sel<-getMeteoStat(format = "spatial")%>%filter(SCODE%in%station)#NAME_D%in%input$Station get spatial stations database (Province) with the seleced SCODEs
  stations_selNot<-getMeteoStat(format = "spatial")%>%filter(!SCODE%in%station)#NAME_D%in%input$Station get spatial stations database (Province) with the seleced SCODEs
  stations_selNotTab<-stations_selNot%>%filter(SCODE%in%stationTab)#NAME_D%in%input$Station get spatial stations database (Province) with the seleced SCODEs
  stations_selNot<-stations_selNot%>%filter(!SCODE%in%stationTab)#NAME_D%in%input$Station get spatial stations database (Province) with the seleced SCODEs
  
    stations<-left_join(stations_sel,se_spread)
  stationsSelNot<-left_join(stations_selNot,se_spread) 
  
   proxy <- leafletProxy("map")%>% clearMarkers()%>% removeDrawToolbar('draw')
   
   #proxy %>% removeControl('draw')
   #proxy %>% clearGroup('draw')
   
   # "output.spatialSelection"
  # input$spatialSelection 
  if(input$spatialSelection){#FALSE
    polygon_coordinates <-polyCoord()
    
    
    proxy <- proxy %>% addDrawToolbar(
      
      #targetLayerId ='draw',
      targetGroup='draw',
      polylineOptions=FALSE,
      markerOptions = FALSE,
      circleOptions = FALSE,
      rectangleOptions =FALSE,
      circleMarkerOptions =FALSE)%>%#
      addMeasure(position = "topleft",primaryLengthUnit = "meters")%>%
      addLayersControl(baseGroups = c("OSM","SAT"),#overlayGroups = c('draw'),
                       options = layersControlOptions(collapsed = FALSE),position = "topleft")
    
    if(is.null(polygon_coordinates)){
      
    }else{  
      drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
      sp <- SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))
      
      proxy <- proxy %>% addPolygons(data=sp,fillOpacity=0.4)
    }
    
  }
  proxy<- proxy %>% 
    addAwesomeMarkers(lng = stationsSelNot$LONG %>% as.character %>% as.numeric, lat = stationsSelNot$LAT %>% 
                        as.character %>% as.numeric, icon = grey, 
                      popup = paste(tr("code",input$language),stationsSelNot$SCODE, "<br>", 
                                    tr("nameDe",input$language),stationsSelNot$NAME_D,"<br>",
                                    tr("nameIt",input$language), stationsSelNot$NAME_I, "<br>",
                                    tr("altitude",input$language),stationsSelNot$ALT, "<br>",
                                    "<br>",
                                    tr("latestRecorded",input$language), 
                                    "<br>",
                                    tr("airTemp",input$language),stationsSelNot$LT, "<br>",
                                    tr("reHum",input$language),stationsSelNot$LF, "<br>",
                                    tr("precipitation",input$language),stationsSelNot$N, "<br>",
                                    tr("windSpeed",input$language),stationsSelNot$WG, "<br>",
                                    tr("windDir",input$language),stationsSelNot$WR, "<br>",
                                    tr("windGuts",input$language),stationsSelNot$WG.BOE, "<br>",
                                    tr("atmPress",input$language),stationsSelNot$LD.RED, "<br>",
                                    tr("solarRad",input$language),stationsSelNot$GS, "<br>",
                                    tr("sunsHour",input$language),stationsSelNot$SD, "<br>",
                                    tr("snowH",input$language),stationsSelNot$HS, "<br>",
                                    tr("watTemp",input$language),stationsSelNot$WT, "<br>",
                                    tr("watFlow",input$language),stationsSelNot$Q, "<br>",
                                    tr("watLevel",input$language),stationsSelNot$W, "<br>"#,
                                    #"Ground water level:",stations$WT, "W.ABST"
    
                      ))%>%
    
    addAwesomeMarkers(lng = stations_selNotTab$LONG %>% as.character %>% as.numeric, lat = stations_selNotTab$LAT %>% 
                        as.character %>% as.numeric, icon = blu, 
                      popup = paste(tr("code",input$language),stations_selNotTab$SCODE, "<br>", 
                                    tr("nameDe",input$language),stations_selNotTab$NAME_D,"<br>",
                                    tr("nameIt",input$language), stations_selNotTab$NAME_I, "<br>",
                                    tr("altitude",input$language),stations_selNotTab$ALT, "<br>",
                                    "<br>",
                                    tr("latestRecorded",input$language), 
                                    "<br>",
                                    tr("airTemp",input$language),stations_selNotTab$LT, "<br>",
                                    tr("reHum",input$language),stations_selNotTab$LF, "<br>",
                                    tr("precipitation",input$language),stations_selNotTab$N, "<br>",
                                    tr("windSpeed",input$language),stations_selNotTab$WG, "<br>",
                                    tr("windDir",input$language),stations_selNotTab$WR, "<br>",
                                    tr("windGuts",input$language),stations_selNotTab$WG.BOE, "<br>",
                                    tr("atmPress",input$language),stations_selNotTab$LD.RED, "<br>",
                                    tr("solarRad",input$language),stations_selNotTab$GS, "<br>",
                                    tr("sunsHour",input$language),stations_selNotTab$SD, "<br>",
                                    tr("snowH",input$language),stations_selNotTab$HS, "<br>",
                                    tr("watTemp",input$language),stations_selNotTab$WT, "<br>",
                                    tr("watFlow",input$language),stations_selNotTab$Q, "<br>",
                                    tr("watLevel",input$language),stations_selNotTab$W, "<br>"#,
                                    #"Ground water level:",stations$WT, "W.ABST"
   
                      ))%>%
    addAwesomeMarkers(lng = stations$LONG %>% as.character %>% as.numeric, lat = stations$LAT %>% 
                                as.character %>% as.numeric, icon = green, 
                              popup = paste(tr("code",input$language),stations$SCODE, "<br>", 
                                            tr("nameDe",input$language),stations$NAME_D,"<br>",
                                            tr("nameIt",input$language), stations$NAME_I, "<br>",
                                            tr("altitude",input$language),stations$ALT, "<br>",
                                            "<br>",
                                            tr("latestRecorded",input$language), 
                                            "<br>",
                                            tr("airTemp",input$language),stations$LT, "<br>",
                                            tr("reHum",input$language),stations$LF, "<br>",
                                            tr("precipitation",input$language),stations$N, "<br>",
                                            tr("windSpeed",input$language),stations$WG, "<br>",
                                            tr("windDir",input$language),stations$WR, "<br>",
                                            tr("windGuts",input$language),stations$WG.BOE, "<br>",
                                            tr("atmPress",input$language),stations$LD.RED, "<br>",
                                            tr("solarRad",input$language),stations$GS, "<br>",
                                            tr("sunsHour",input$language),stations$SD, "<br>",
                                            tr("snowH",input$language),stations$HS, "<br>",
                                            tr("watTemp",input$language),stations$WT, "<br>",
                                            tr("watFlow",input$language),stations$Q, "<br>",
                                            tr("watLevel",input$language),stations$W, "<br>"#,
                                            #"Ground water level:",stations$WT, "W.ABST"
                              ))
  
  
  })
  
  output$map<-renderLeaflet({

  m<-plotMeteoLeaflet()#stations_sel
    m
})
  
  #######
  
  drawnshapes <- list()
  
  # we are fortunate here since we get an event
  #   draw_all_features
  observeEvent(
    input$map_draw_all_features,
    {
      drawnshapes <<- lapply(
        input$map_draw_all_features$features,
        function(ftr) {
          ftr$properties$`_leaflet_id`
        }
      )
      # seeing is believing
      str(drawnshapes)
    }
  )
  
  # observe our simple little button to remove
  observeEvent(
    input$deletebtn,
    {
      print(drawnshapes)
      lapply(
        drawnshapes,
        function(todelete) {
          session$sendCustomMessage(
            "removeleaflet",
            list(elid="map", layerid=todelete)
          )
        }
      )
    }
  )
  
  
  
  
  

  
  
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
      tryCatch({
        
        #station=as.character(input$Station)
        startdate<-as.character(min(as_date(D$documents[[1]]$TimeStamp)))
        enddate<-as.character(max(as_date(D$documents[[1]]$TimeStamp)))
        gather<-as.character(translation[grep(input$gather,translation[,input$language]),"key"])
        #gather<-input$gather
        round=input$round
        nstat=D$documents[[1]]$SCODE %>% unique %>% length %>% as.character 
        #paste0(station,'_',round,'_',startdate,'_',enddate,'.csv')
        if(input$csvjson=="csv"){
        paste0(nstat,'stat','_',startdate,'_',enddate,'_',round,'_',gather,'.csv')#,round
        }else{
          paste0(nstat,'stat','_',startdate,'_',enddate,'_',round,'_',gather,'.json')#,round
          }
      }, error = function(e){"error.csv"})#
      
    },
    content = function(con) {
      round<-as.character(translation[grep(input$round,translation[,input$language]),"key"])
      #round<-input$round
      gather<-as.character(translation[grep(input$gather,translation[,input$language]),"key"])
      #gather<-input$gather
      df=D$documents[[1]] 
      
      if(gather=="wide"){
        spread=TRUE}else{
          spread=FALSE
        }
      
      db=resample_provBz_data(df=df,round=round,spread=spread)
      if(input$csvjson=="csv"){
      write.csv(x=db,file =  con,quote = F,row.names = F,na = "NA")
        }else{
      write_json(db,con)
      }
      
    }
    
  )
  
  
  
  outputOptions(output, 'tablebuilt', suspendWhenHidden=FALSE)
  outputOptions(output, 'rightdate', suspendWhenHidden=FALSE)
}
