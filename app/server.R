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
translation<-read.csv(file.path(getwd(),"translation.csv"),header = T,sep = ",",stringsAsFactors = F)

leaf.proj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
LL <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

url <- "http://daten.buergernetz.bz.it/services/meteo/v1/sensors"
u <- GET(url) %>% content
se<-bind_rows(u)
st<-getMeteoStat()
tot_tab<-full_join(st,se)%>%dplyr::select(-NAME_L,-NAME_E,-DESC_L,-DATE,VALUE,-LAT,-LONG,-VALUE)%>%
  mutate_if(is.character, funs(as.factor(.)))#%>%as.data.frame()

se_spread<-se %>% dplyr::select(SCODE,TYPE,UNIT,VALUE) %>% 
  unite(VALUE,VALUE,UNIT,sep=" ") %>% 
  spread(TYPE,VALUE)



server <- function(input, output,session) {
  
  
  #tr <- function(text,lenguage,translation){ # translates text into current language
  #x<-as.character(translation[grep(text,translation$key),lenguage])
  #return(x)
  #}
  
  # UI
  
  output$refresh  <- renderUI({
    conditionalPanel(condition = "output.rightdate",br(),
                     actionButton(label= as.character(translation[grep("refresh",translation$key),input$language]),"refresh")) 
  })
  
  output$refresh  <- renderUI({
  conditionalPanel(condition ="output.spatialSelection",actionButton("deletebtn", as.character(translation[grep("refresh",translation$key),input$language])))
  })
  
  output$downloadInstructions <- renderText({
    as.character(translation[grep("downloadInstructions",translation$key),input$language])
    })
  
  output$tableInstructions  <- renderText({
    as.character(translation[grep("tableInstructions",translation$key),input$language])
    
    #tr(text="tableInstructions",lenguage=as.character(input$lenguage),translation=translation)
    })
  
  output$daterange<-renderUI({
    
   dateRangeInput(label = h4(as.character(translation[grep("daterange",translation$key),input$language])),inputId = "daterange",separator = " - ",min = "2000-01-01",#
                                                             start = Sys.Date()-3,
                                                             end = Sys.Date()+1,language=input$language)
   })
  
  output$round<-renderUI({
    
   selectInput("round",label = h4(as.character(translation[grep("roundLabel",translation$key),input$language])),
                               choices = list(as.character(translation[grep("raw",translation$key),input$language]),
                                              as.character(translation[grep("hour",translation$key),input$language]),
                                              as.character(translation[grep("day",translation$key),input$language]),
                                              as.character(translation[grep("week",translation$key),input$language]),
                                              as.character(translation[grep("month",translation$key),input$language]),
                                              as.character(translation[grep("year",translation$key),input$language])
                                             ))
    })
  
   output$gather<-renderUI({
    
   selectInput("gather", label= h4(as.character(translation[grep("gatherLabel",translation$key),input$language])),
                                choices = list(as.character(translation[grep("wide",translation$key),input$language]),
                                                as.character(translation[grep("long",translation$key),input$language])
                                              ))
   })
  
  output$spatSel<-renderText({
    
  as.character(translation[grep("spatialSelection",translation$key),input$language])
                              
  })
  
  
  output$message<-renderText({
    datestart<-as.character(input$daterange[1])
    dateend<-as.character(input$daterange[2])
    if(as_date(datestart)>as_date(dateend)){
      messagedate<-as.character(translation[grep("messageDate",translation$key),input$language])
    }else{messagedate<-NULL}
      messagedate
      
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
    
    station<-StatSens$station
    sensors<-StatSens$sensors
    
    nstation<-length(station)%>%as.numeric
    nsensors<-length(sensors)%>%as.numeric
    
    if(nstation==1) stat<-as.character(translation[grep("aaa",translation$key),input$language]) else{stat<- as.character(translation[grep("bbb",translation$key),input$language])}
    
    if(nsensors==1)param<-as.character(translation[grep("ccc",translation$key),input$language])else{param<- as.character(translation[grep("ddd",translation$key),input$language])}
    
    
    mssg<- paste(as.character(translation[grep("youvSelected",translation$key),input$language]),
                 nstation,stat,
                 as.character(translation[grep("and",translation$key),input$language]),
                 nsensors,param)
    
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
  
  
  output$youSelStations<-renderText({
    
    as.character(translation[grep("youSelStations",translation$key),input$language])
    
  })
  
  output$andSensors<-renderText({
    
    as.character(translation[grep("andSensors",translation$key),input$language])
    
  })
  
  ###############
  
  output$table<-DT::renderDT({
    
    if(input$language=="it"){
    tot_tab<-tot_tab%>%dplyr::select(-NAME_D,-DESC_D)
    }else{
    tot_tab<-tot_tab%>%dplyr::select(-NAME_I,-DESC_I)
    }
    
    dt<-datatable(tot_tab, filter = 'top',rownames=F,selection="none",
              options = list(autoWidth = F,scrollX=T)
    ) 
    
    if(input$language=="it"){
    dt<-dt%>% 
      formatStyle(c("TYPE",  "DESC_I","UNIT"),
                  backgroundColor = "#edf5e1")
    }else{
    dt<-dt%>% 
      formatStyle(c("TYPE", "DESC_D", "UNIT"),
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
    dateend<-as.character(input$daterange[2])
    
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
  
  
  output$map<-renderLeaflet({
    ids<-input$table_rows_all
    station<-unique(tot_tab$SCODE[ids])%>%as.character
    stations_sel<-getMeteoStat(format = "spatial")%>%filter(SCODE%in%station)#NAME_D%in%input$Station get spatial stations database (Province) with the seleced SCODEs
    stations_sel<-left_join(stations_sel,se_spread)
    m<-plotMeteoLeaflet(stations_sel)
    

    if(input$spatialSelection){#FALSE
      polygon_coordinates <-polyCoord()
      

      m <- m %>% addDrawToolbar(
        targetGroup='draw',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions =FALSE,
        circleMarkerOptions =FALSE)%>%#
        addMeasure(position = "topleft",primaryLengthUnit = "meters")%>%
        addLayersControl(overlayGroups = c('draw'),
                         options = layersControlOptions(collapsed = FALSE),position = "topleft")
       
      if(is.null(polygon_coordinates)){
          
          }else{  
        drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
        sp <- SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))
        
        m <- m %>% addPolygons(data=sp,fillOpacity=0.4)
          }
      
    }
    m
    
  })
  
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
        paste0(nstat,'stat','_',startdate,'_',enddate,'_',round,'_',gather,'.csv')#,round
        
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
      write.csv(x=db,file =  con,quote = F,row.names = F,na = "NA")
      
    }
    
  )
  
  
  
  outputOptions(output, 'tablebuilt', suspendWhenHidden=FALSE)
  outputOptions(output, 'rightdate', suspendWhenHidden=FALSE)
}
