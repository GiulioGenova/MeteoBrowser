
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

leaf.proj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
LL <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

url <- "http://daten.buergernetz.bz.it/services/meteo/v1/sensors"
u <- GET(url) %>% content
se<-bind_rows(u)
st<-getMeteoStat()
tot_tab<-full_join(st,se)%>%dplyr::select(-NAME_L,-NAME_E,-DESC_L,-DATE,-VALUE,-LAT,-LONG)%>%
  mutate_if(is.character, funs(as.factor(.)))#%>%as.data.frame()

stations_sp <- getMeteoStat(format = "spatial")

server <- function(input, output,session) {
  #observeEvent(input$stop,{
   # session$reload()
    #return()
  #})
  
  output$table<-DT::renderDT({
  datatable(tot_tab, filter = 'top',rownames=F,selection="none",
                         options = list(autoWidth = F,scrollX=T)
                         ) %>% 
    formatStyle(c("TYPE", "DESC_D", "DESC_I","UNIT"),
                                     backgroundColor = "#edf5e1")
})
  
  #"#c3cfe6"
  #output$table<-renderDT(tot_tab, filter = 'top',rownames=F,selection="none",
   #                      options = list(autoWidth = F,scrollX=T)
    #                     )%>%formatStyle(c("TYPE", "DESC_D", "DESC_I","UNIT"),
     #                                 backgroundColor = "grey")
  #,server = TRUE,options=list(pageLength = 100,scrollX = T),  escape = FALSE
  
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
    
    datestart<-as.character(input$daterange[1])
    dateend<-as.character(input$daterange[2])
    #out_dir="H:/Projekte/SBR/04_Data/06_daily_resample"
    round=input$round
      
    #################################################################################
     #Spatial selection
     if(FALSE){#input$spatialSelection=="YES"
     
     stations_sp <- getMeteoStat(format = "spatial")
     req(input$map_draw_stop)
     
     
     #get the coordinates of the polygon
     polygon_coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
      
    drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
    sp <- SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))

    # set coords as latlong then transform to leaflet projection
    proj4string(sp) <- LL
    polyre <- spTransform(sp, leaf.proj)
    
    stations_sp<-spTransform(stations_sp,leaf.proj)
       
    selected_stats <- stations_sp %over% polyre

    sp_sel<-stations_sp %>% dplyr::filter(row_number()%in%which(!is.na(selected_stats)))

    station<-unique(sp_sel$SCODE) %>% as.character
       #}
    
      }
    #################################################################################  
    nstations<-length(station)%>%as.numeric*length(sensors)%>%as.numeric
      
      
      
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
    #########################################################
    if(FALSE){#input$spatialSelection=="YES"
     
     stations_sp <- getMeteoStat(format = "spatial")
     req(input$map_draw_stop)
     
     
     #get the coordinates of the polygon
     polygon_coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
      
    drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
    sp <- SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))

    # set coords as latlong then transform to leaflet projection
    proj4string(sp) <- LL
    polyre <- spTransform(sp, leaf.proj)
    
    stations_sp<-spTransform(stations_sp,leaf.proj)
       
    selected_stats <- stations_sp %over% polyre

    sp_sel<-stations_sp %>% dplyr::filter(row_number()%in%which(!is.na(selected_stats)))

    station<-unique(sp_sel$SCODE) %>% as.character
       #}
    nstation<-length(station)%>%as.character
      }
    
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
    tryCatch({
    
    #station=as.character(input$Station)
    startdate<-as.character(min(as_date(D$documents[[1]]$TimeStamp)))
    enddate<-as.character(max(as_date(D$documents[[1]]$TimeStamp)))
    gather<-input$gather
    round=input$round
    nstat=D$documents[[1]]$SCODE %>% unique %>% length %>% as.character 
    #paste0(station,'_',round,'_',startdate,'_',enddate,'.csv')
    paste0(nstat,'stat','_',startdate,'_',enddate,'_',round,'_',gather,'.csv')#,round
      
    }, error = function(e){"error.csv"})#
  
  },
  content = function(con) {
    
    round<-input$round
    gather<-input$gather
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
