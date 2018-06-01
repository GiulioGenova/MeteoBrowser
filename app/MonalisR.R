getMeteoStat<-function (url = NA, format = "table") 
{
  if (is.na(url)) 
    url <- "http://daten.buergernetz.bz.it/services/meteo/v1/stations"
  if (format == "table") {
    js1 <- fromJSON(url)
    js1 <- js1$features$properties %>% as_tibble
    return(js1)
  }
  else if (format == "spatial") {
    js1 <- geojson_read(paste0(url, ".geojson"), method = "web", 
                        what = "sp")
    
    #js1 <- fromJSON(url)
    return(js1)
  }
}

downloadMeteo<-function (dburl = NULL, station_code, sensor_code, datestart, 
          dateend, path = "", csv = FALSE) 
{
  if (is.null(dburl)) 
    dburl <- "http://daten.buergernetz.bz.it/services/meteo/v1/timeseries"
  datestart1 <- datestart %>% convertDate(., db = "Meteo")
  dateend1 <- dateend %>% convertDate(., db = "Meteo")
  dates <- c(datestart %>% as.Date, dateend %>% as.Date) %>% 
    str_replace_all(., "-", "")
  req1 <- dburl %>% paste0(., "?station_code=", station_code) %>% 
    paste0(., "&output_format=JSON") %>% paste0(., "&sensor_code=", 
                                                sensor_code) %>% paste0(., "&date_from=", datestart1) %>% 
    paste0(., "&date_to=", dateend1)
  DAT <- fromJSON(req1)
  if (length(DAT) != 0) {
    DAT <- DAT %>% add_column(rep(sensor_code, times = nrow(.)), 
                              .before = 2) %>% add_column(rep(station_code, times = nrow(.)), 
                                                          .before = 2)
    colnames(DAT) <- c("TimeStamp", "Station", "Sensor", 
                       "Value")
    if (path != "") {
      myfile = paste0(path, "/", station_code, "_", sensor_code, 
                      "_", dates[1], "_", dates[2])
      save(DAT, file = paste0(myfile, ".RData"))
      if (csv == TRUE) {
        write.csv(x = DAT, paste0(myfile, ".csv"))
      }
    }
    return(DAT)
  }
}

plotMeteoLeaflet<-function (stations = NULL, addPoints = NULL, addBuff = F, widthBuff = 10000) 
{
  if (is.null(stations)) 
    stations <- getMeteoStat(format = "spatial")
  c1 <- awesomeIcons(icon = "ios-close", iconColor = "black", 
                     library = "ion", markerColor = "blue")
  c2 <- awesomeIcons(icon = "ios-close", iconColor = "black", 
                     library = "ion", markerColor = "red")
  
  m <- leaflet() %>%addSearchOSM()%>%
    #htmlwidgets::onRender(".leaflet-control {
     #                     float: left;
      #                    clear: both;}")%>% 
    #addTiles()%>% 
    addProviderTiles("OpenStreetMap.Mapnik", group = "OSM")%>% 
    addProviderTiles("Esri.WorldImagery", group = "SAT") %>%
    addAwesomeMarkers(lng = stations$LONG %>% as.character %>% as.numeric, lat = stations$LAT %>% 
                                                        as.character %>% as.numeric, icon = c1, popup = paste("Code:", 
                                                                                                              stations$SCODE, "<br>", "Name GER:", stations$NAME_D, 
                                                                                                              "<br>", "Name ITA:", stations$NAME_I, "<br>", "Altitude:", 
                                                                                                              stations$ALT))%>%
    #addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me",
     #                        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
    addMeasure(position = "topleft",primaryLengthUnit = "meters")%>%
    addLayersControl(baseGroups = c("OSM","SAT"),
                     options = layersControlOptions(collapsed = FALSE),position = "topleft")
  
  if (!is.null(addPoints)) {
    coords <- coordinates(addPoints)
    m <- m %>% addAwesomeMarkers(coords[, 1], coords[, 2], 
                                 icon = c2)
  }
  if (addBuff == T) {
    ref <- getMeteoStat(format = "spatial")
    shp <- spTransform(addPoints, CRS = CRS(projection(ref)))
    buff1 <- gBuffer(shp, byid = T, width = widthBuff)
    buff1 <- spTransform(buff1, CRS = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    m <- m %>% addPolygons(data = buff1, fillColor = "red", 
                           weight = 0)
  }
  return(m)
}

convertDate<-function (date, db = "Monalisa") 
{
  if (db == "Monalisa") {
    date <- date/1000
    date <- as.POSIXct(date, origin = "1970-01-01")
    return(date)
  }
  if (db == "Meteo") {
    date <- date %>% str_replace_all(., "-", "") %>% str_replace(., 
                                                                 ":", "") %>% str_replace(., " ", "")
    return(date)
  }
}
 
