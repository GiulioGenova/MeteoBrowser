if (!require("devtools")) install.packages("devtools")
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
if (!require("leaflet")) devtools::install_github("rstudio/leaflet")
if (!require("leaflet.extras")) devtools::install_github("bhaskarvk/leaflet.extras")
#if (!require("leaflet")) install.packages("leaflet")
#if (!require("leaflet.extras")) install.packages("leaflet.extras")
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

server <- function(input, output,session) {
  
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
  output$d1scla1mer <- renderUI({
    tags$b(tr("d1scla1mer",input$language))
  })
  
  output$Disclaimer <- renderText({
    tr("Disclaimer",input$language)
  })
  
  output$save  <- renderUI({
    conditionalPanel(condition = "output.tablebuilt",#br(),#"input.daterange[1]<=input.daterange[2]"
                     #div(style="width: 100%;",
                     #div(style="width: 30%;",#display: inline-block;vertical-align:top; 
                     downloadButton('downloadData', h4(tr("savecsvjson",input$language)) ,class="butt")
                     #  )
                     
    )
    #)
    
  })
  
  output$tabChoice  <- renderUI({
    conditionalPanel(condition = "output.tablebuilt",
                     #div(style=" width: 25%;",#display: inline-block;vertical-align:top;
                     radioButtons(inputId = "csvjson",label = tr("tableType",input$language),
                                  choices = list("csv","json"))
                     #)
    )
    #)
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
  
  output$legend  <- renderMenu({
    sidebarMenu(
      menuItem(tr("LegTab",input$language), tabName = "legend", icon = icon("info-circle"))
    )
  })
  
  output$refresh  <- renderUI({
    #datestart<- input$daterange[1] %>% as.character %>% as_date
    #dateend<- input$daterange[2] %>% as.character %>% as_date
    #if(datestart<=dateend){actionButton(label= as.character(translation[grep("refresh",translation$key),input$language]),"refresh")}
    #div(style="width: 40%;",
    conditionalPanel(condition = "output.rightdate",#br(),
                     actionButton(label= tr("refresh",input$language),"refresh"))
    #   )
  })
  
  output$downloadInstructions <- renderText({
    tr("downloadInstructions",input$language)
  })
  
  output$tableInstructions  <- renderText({
    tr("tableInstructions",input$language)
    
    #tr(text="tableInstructions",lenguage=as.character(input$lenguage),translation=translation)
  })
  
  output$daterange<-renderUI({
    
    dateRangeInput(label = h4(tags$b(tr("daterange",input$language))),inputId = "daterange",separator = " - ",min = "2000-01-01",#
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
  
  
  output$message<-renderUI({
    
    conditionalPanel(condition = "output.rightdate==false",br(),
                     renderText(tr("messageDate",input$language)))
    
    
    
  })
  
  
  
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
    sensors<-StatSens$sensorsName
    
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
  
  output$legend_tab <- DT::renderDT({
    
    legendDt<-datatable(legend_tab, filter = 'top',rownames=F,selection="none",
                        options = list(autoWidth = F,scrollX=T))
    legendDt
  })
  
  output$statlist <- renderUI({
    
    statlist <- sort(unique(as.vector(tot_tab$NAME)), decreasing = FALSE)
    #statlist <- append(statlist, "All", after =  0)
    selectizeInput("selStation", h4(tags$b(tr("s3l3ctStatTitle",input$language))), statlist,
                   multiple = TRUE,
                   selected = "Salurn / Salorno",
                   options = list(
                     placeholder = tr("d3falutStat",input$language)))
    
  })
  
  
  output$sensorlist <- renderUI({
    if(input$language=="it"){
      
      sensorlist <- sort(unique(as.vector(tot_tab$DESC_I)), decreasing = FALSE)
      
    }else if(input$language=="de"){
      
      sensorlist <- sort(unique(as.vector(tot_tab$DESC_D)), decreasing = FALSE)
      
    }
    else{
      
      sensorlist <- sort(unique(as.vector(tot_tab$DESC_E)), decreasing = FALSE)
    }
    
    #sensorlist <- append(sensorlist, "All", 0)
    selectizeInput("selSensor", h4(tags$b(tr("s3l3ctParamTitle",input$language))), sensorlist,
                   multiple = TRUE,
                   options = list(
                     placeholder = tr("d3falutParam",input$language)))
    
  }) 
  
  output$altitudelist <- renderUI({
    
    sliderInput("selAltitude", label = h4(tags$b(tr("s3lelevrange",input$language))), min = 0, #, h4(),tags$sub("[m]"))
                max = max(tot_tab$ALT,na.rm = T), value = c(0, max(tot_tab$ALT,na.rm = T)))
    
    
  })
  
  
  ids <- reactive({
    raw <- tot_tab
    #req(input$colorchoose)
    input$selStation
    input$selSensor
    input$selAltitude
    
    
    #if ("All"%in%input$selStation) {
    if (is.null(input$selStation)) {
      
      filt1 <- quote(NAME != "@?><")
      
      
    } else {
      
      
      filt1 <- quote(NAME %in% input$selStation)
      
    }
    
    
    #if ("All"%in%input$selSensor) {
    if (is.null(input$selSensor)) {
      
      if(input$language=="it"){
        
        filt2 <- quote(DESC_I != "@?><")
        
      }else if(input$language=="de"){
        
        filt2 <- quote(DESC_D != "@?><")
        
      }
      else{
        
        filt2 <- quote(DESC_E != "@?><")
      }
      
      
      
    } else {
      
      if(input$language=="it"){
        
        filt2 <- quote(DESC_I %in% input$selSensor)
        
      }else if(input$language=="de"){
        
        filt2 <- quote(DESC_D %in% input$selSensor)
        
      }
      else{
        
        filt2 <- quote(DESC_E %in% input$selSensor)
      }
      
      
    }
    
    #if (input$cutchoose == "All") {
    if (is.null(input$selAltitude)) {
      
      filt3 <- quote(ALT != "@?><")
      
      
    } else {
      
      
      filt3 <- quote(ALT >= input$selAltitude[1] & ALT <= input$selAltitude[2])
      
    }
    
    
    raw <- raw %>%
      filter_(filt1) %>%
      filter_(filt2) %>%
      filter_(filt3)
    
    if(length(raw$id) == 0){
      
      return(NULL)
      
    }else{
      
      return(raw$id)
      
    }
  })
  
  
  polyCoord <- reactiveVal(NULL)
  
  observe({
    req(input$map_draw_stop)
    
    input$map_draw_new_feature
    
    res <- input$map_draw_all_features
    
    if (is.null(res) || length(res[['features']]) == 0 ) {
      res <-(NULL)
    } else {
      res <-(res[['features']][[1]])
    }
    
    polygon_coordinates = res$geometry$coordinates[[1]]
    
    polyCoord(polygon_coordinates)
    
  })
  
  
  D <- reactiveValues(documents = NULL)
  
  StatSens<-reactiveValues(station= c(),sensors= c(),stationName = c(),sensorsName=c())#station= NULL,sensors= NULL,stationName = NULL  
  
  
  observe({
    #input$tot_tab_state
    
    input$selStation
    input$selSensor
    input$selAltitude
    
    observe({
      ids<-ids()
      #ids<-input$table_rows_all
      
      station<-unique(tot_tab$SCODE[ids])%>%as.character
      sensors<-unique(tot_tab$TYPE[ids])%>%as.character
      if(input$language=="it"){
        stationName<-unique(tot_tab$NAME_I[ids])%>%as.character
      }else {
        stationName<-unique(tot_tab$NAME_D[ids])%>%as.character
        
      }
      
      if(input$language=="it"){
        sensorsName<-unique(tot_tab$DESC_I[ids])%>%as.character
      }else if(input$language=="de"){
        sensorsName<-unique(tot_tab$DESC_D[ids])%>%as.character
      }
      else{
        sensorsName<-unique(tot_tab$DESC_E[ids])%>%as.character
      }
      
      #########################################################
      #input$spatialSelection
      if(!is.null(polyCoord()) ){#FALSE
        #req(input$map_draw_stop)
        #stations_sp <- getMeteoStat(format = "spatial")%>%filter(SCODE%in%station)
        stations_sp <- sspat %>% filter(SCODE%in%station)
        
        
        polygon_coordinates <-polyCoord()
        if(is.null(ids)){#is.null(polygon_coordinates)|
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
          
          if(input$language=="it"){
            sensorsName<-unique(filterForSensor$DESC_I)%>%as.character
          }else if(input$language=="de"){
            sensorsName<-unique(filterForSensor$DESC_D)%>%as.character
          }
          else{
            sensorsName<-unique(filterForSensor$DESC_E)%>%as.character
          }
          
          
        }
      }
      #return(list(station,sensors,stationName))
      StatSens$station<-station
      StatSens$sensors<-sensors
      StatSens$stationName<-stationName
      StatSens$sensorsName<-sensorsName
    })
  })
  
  #})
  
  
  
  observeEvent(input$refresh,{
    #ids<-input$table_rows_all
    ids <- ids()
    
    #station<-StatSens()[[1]]
    #sensors<-StatSens()[[2]]
    
    station<-StatSens$station
    sensors<-StatSens$sensors
    
    datestart<-as.character(input$daterange[1])
    dateend<-as.character(as_date(input$daterange[2])+1)
    
    round<-as.character(translation[grep(input$round,translation[,input$language]),"key"])
    #round<-input$round
    gather<-as.character(translation[grep(input$gather,translation[,input$language]),"key"])
    if(gather=="wide"){
      spread=TRUE}else{
        spread=FALSE
      }
    nstations<-length(station)%>%as.numeric*length(sensors)%>%as.numeric
    
    
    if(as_date(datestart)<=dateend){
      withProgress(message = 'Getting data', value = 0, {
        db<-dwnld(station=station,datestart=datestart,
                  dateend=dateend,sensors=sensors,nstations=nstations,
                  round=round,spread=spread)#
        
        
        tab<-tot_tab %>% dplyr::select(SCODE,NAME)
        db<-left_join(db,tab,.before=2)
        
      })#
    }else{db<-"Error in selecting the date range. First date must be earlier than last date"}
    D$documents <- list(db)
    
  })
  
  
  # observeEvent(input$stop,{
  #   session$reload()
  #   return()
  # })
  
  
  observe({
    
    #ids<-input$table_rows_all
    ids <- ids()
    
    stationTab<-unique(tot_tab$SCODE[ids])%>%as.character
    station<-StatSens$station
    
    stations<-se_spread%>%filter(SCODE%in%station)#NAME_D%in%input$Station get spatial stations database (Province) with the seleced SCODEs
    stations_selNot<-se_spread%>%filter(!SCODE%in%station)#NAME_D%in%input$Station get spatial stations database (Province) with the seleced SCODEs
    
    stations_selNotTab<-stations_selNot%>%filter(SCODE%in%stationTab)#NAME_D%in%input$Station get spatial stations database (Province) with the seleced SCODEs
    stationsSelNot<-stations_selNot%>%filter(!SCODE%in%stationTab)#NAME_D%in%input$Station get spatial stations database (Province) with the seleced SCODEs
    
    #stations<-left_join(stations_sel,se_spread)
    #stationsSelNot<-left_join(stations_selNot,se_spread) 
    
    proxy <- leafletProxy("map")%>% clearMarkers() #%>% removeDrawToolbar(clearFeatures = TRUE)
    
    #proxy %>% removeControl('draw')
    #proxy %>% clearGroup('draw')
    
    # "output.spatialSelection"
    # input$spatialSelection 
    # #if(input$spatialSelection){#FALSE
    #polygon_coordinates <-polyCoord()
    
    
    #proxy <- proxy #%>%
    # addMeasure(position = "topleft",primaryLengthUnit = "meters")%>%
    #addLayersControl(baseGroups = c("OSM","SAT"),#overlayGroups = c('draw'),
    #                options = layersControlOptions(collapsed = FALSE),position = "topleft")
    
    #if(is.null(polygon_coordinates)){
    
    #}else{  
    #  drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
    #  sp <- SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))
    
    #  proxy <- proxy %>% addPolygons(data=sp,fillOpacity=0.4)
    #}
    
    ## }
    proxy<- proxy %>% 
      addAwesomeMarkers(lng = stationsSelNot$LONG %>% as.character %>% as.numeric, lat = stationsSelNot$LAT %>% 
                          as.character %>% as.numeric, icon = grey, 
                        popup = paste(tr("nameDe",input$language),stationsSelNot$NAME_D,"<br>",
                                      tr("nameIt",input$language), stationsSelNot$NAME_I, "<br>",
                                      tr("altitude",input$language),stationsSelNot$ALT, "<br>",
                                      tr("code",input$language),stationsSelNot$SCODE, "<br>", 
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
                        popup = paste(tr("nameDe",input$language),stations_selNotTab$NAME_D,"<br>",
                                      tr("nameIt",input$language), stations_selNotTab$NAME_I, "<br>",
                                      tr("altitude",input$language),stations_selNotTab$ALT, "<br>",
                                      tr("code",input$language),stations_selNotTab$SCODE, "<br>", 
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
                        popup = paste(tr("nameDe",input$language),stations$NAME_D,"<br>",
                                      tr("nameIt",input$language), stations$NAME_I, "<br>",
                                      tr("altitude",input$language),stations$ALT, "<br>",
                                      tr("code",input$language),stations$SCODE, "<br>", 
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
    m%>% addDrawToolbar(
      
      #targetLayerId ='draw',
      targetGroup='draw',
      polygonOptions = drawPolygonOptions(),
      editOptions = editToolbarOptions(edit=FALSE),
      polylineOptions=FALSE,
      markerOptions = FALSE,
      circleOptions = FALSE,
      rectangleOptions =FALSE,
      circleMarkerOptions =FALSE)
  })
  
  #######
  
  
  
  
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
      
      #if(gather=="wide"){
      #  spread=TRUE}else{
      #    spread=FALSE
      #  }
      
      #db=resample_provBz_data(df=df,round=round,spread=spread)
      db=df
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
