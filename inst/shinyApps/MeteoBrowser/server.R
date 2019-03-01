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

  output$dst <- renderUI({

    conditionalPanel(condition = "output.tablebuilt",

                     checkboxInput(inputId = "isdst",
                                   label = tr("d5t",input$language),value = T)

    )

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
    conditionalPanel(condition = "output.rightdate && output.rightStationSensor",#br(),
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

    dateRangeInput(label = h4(tags$b(tr("daterange",input$language))),
                   inputId = "daterange",separator = " - ",
                   min = Sys.Date()-1100,#"2000-01-01"
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
                               tr("year",input$language)),
                selected = tr("hour",input$language))
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



  output$messageStatSens<-renderUI({

    conditionalPanel(condition = "output.rightStationSensor==false",br(),
                     renderText(tr("messageStatSens",input$language)))



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

  # output$legend_tab <- DT::renderDT({
  #
  #   legendDt<-datatable(legend_tab, filter = 'top',rownames=F,selection="none",
  #                       options = list(autoWidth = F,scrollX=T))
  #   legendDt
  # })


  output$statlist <- renderUI({

    statlist <- sort(unique(as.vector(tot_tab$NAME)), decreasing = FALSE)
    #statlist <- append(statlist, "All", after =  0)
    selectizeInput("selStation", tags$div(h4(tags$b(tr("s3l3ctStatTitle",input$language))),h6(tr("a11st",input$language))),#tags$div(h4(tags$b(tr("s3l3ctStatTitle",input$language))),"casta")
                   c("All",statlist),
                   multiple = TRUE,
                   #selected = "Salurn / Salorno",
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


  observe({

    if(input$language=="it"){
      column="DESC_I"
      vectorSens=tot_tab$DESC_I
    }else if(input$language=="de"){
      column="DESC_D"
      vectorSens=tot_tab$DESC_D
    }else{
      column="DESC_E"
      vectorSens=tot_tab$DESC_E
    }



    if ((is.null(input$selStation) ) & is.null(input$selSensor)) {#|| input$selStation=="All"


      selectedStat <- sort(unique(as.vector(tot_tab$NAME)), decreasing = FALSE)

      selectedSens <- sort(unique(as.vector(vectorSens)), decreasing = FALSE)


      updateSelectInput(session, "selStation",
                        choices = c("All",selectedStat),selected = NULL
      )


      updateSelectInput(session, "selSensor",
                        choices = selectedSens,selected = NULL
      )



    }else if(!is.null(input$selStation) & is.null(input$selSensor)){
      #all
      if(any(c("All")%in%input$selStation)){

        selectedStat <- sort(unique(as.vector(tot_tab$NAME)), decreasing = FALSE)

        selectedSens <- sort(unique(as.vector(vectorSens)), decreasing = FALSE)


        updateSelectInput(session, "selStation",
                          choices = c("All",selectedStat),selected = "All"
        )


        updateSelectInput(session, "selSensor",
                          choices = selectedSens,selected = NULL
        )


      }else{

        selectedStat <- sort(unique(as.vector(tot_tab$NAME)), decreasing = FALSE)

        sel = tot_tab$NAME %in% input$selStation
        sel = tot_tab[sel,column]
        selectedSens <- sort(unique(as.vector(sel[[1]])), decreasing = FALSE)


        updateSelectInput(session, "selStation",
                          choices = c("All",selectedStat),selected = input$selStation
        )


        updateSelectInput(session, "selSensor",
                          choices = selectedSens,selected = NULL
        )

      }

    }else if(is.null(input$selStation) & !is.null(input$selSensor)){

      if(input$language=="it"){
        sel = tot_tab %>% filter(DESC_I %in% input$selSensor)
      }else if(input$language=="de"){
        sel = tot_tab %>% filter(DESC_D %in% input$selSensor)
      }else{
        sel = tot_tab %>% filter(DESC_E %in% input$selSensor)
      }

      sel = sel$NAME

      selectedStat <- sort(unique(as.vector(sel)), decreasing = FALSE)

      selectedSens <- sort(unique(as.vector(vectorSens)), decreasing = FALSE)


      updateSelectInput(session, "selStation",
                        choices = c("All",selectedStat),selected = NULL
      )


      updateSelectInput(session, "selSensor",
                        choices = selectedSens,selected = input$selSensor
      )


    }else if(!is.null(input$selStation) & !is.null(input$selSensor)){
      #all
      if(any(c("All")%in%input$selStation)){

        selectedStat <- sort(unique(as.vector(tot_tab$NAME)), decreasing = FALSE)

        selectedSens <- sort(unique(as.vector(vectorSens)), decreasing = FALSE)


        updateSelectInput(session, "selStation",
                          choices = c("All",selectedStat),selected = "All"
        )


        updateSelectInput(session, "selSensor",
                          choices = selectedSens,selected = input$selSensor
        )

      }else {


        if(input$language=="it"){
          sel1 = tot_tab %>% filter(DESC_I %in% input$selSensor)
        }else if(input$language=="de"){
          sel1 = tot_tab %>% filter(DESC_D %in% input$selSensor)
        }else{
          sel1 = tot_tab %>% filter(DESC_E %in% input$selSensor)
        }

        sel1 = sel1$NAME

        selectedStat <- sort(unique(as.vector(sel1)), decreasing = FALSE)

        sel2 = tot_tab$NAME %in% input$selStation
        sel2 = tot_tab[sel2,column]
        selectedSens <- sort(unique(as.vector(sel2[[1]])), decreasing = FALSE)


        updateSelectInput(session, "selStation",
                          choices = c("All",selectedStat),selected = input$selStation
        )


        updateSelectInput(session, "selSensor",
                          choices = selectedSens,selected = input$selSensor
        )
      }

    }

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



    if (is.null(input$selStation)) {

      filt1 <- quote(NAME == "@?><")

    } else if("All"%in%input$selStation) {

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
          proj4string(sp) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
          polyre <- spTransform(sp, "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

          stations_sp<-spTransform(stations_sp,"+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

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

        #stri_encode(stationName, "", "UTF-8")
        #stri_encode(sensorsName, "", "UTF-8")

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
    dateend<-as.character(input$daterange[2])
    #dateend<-as.character(as_date(input$daterange[2])+1)
    round<-as.character(translation[grep(input$round,translation[,input$language]),"key"])
    #round<-input$round
    gather<-as.character(translation[grep(input$gather,translation[,input$language]),"key"])
    # if(gather=="wide"){
    #   spread=TRUE}else{
    #     spread=FALSE
    #   }
    nstations<-length(station)%>%as.numeric*length(sensors)%>%as.numeric

    station_sensor<- get_provBz_sensors() %>%
      dplyr::filter(SCODE %in% station,Sensor %in% sensors) %>%
      dplyr::select(SCODE,Sensor)

    if(as_date(datestart)<=dateend & length(station)!=0){
      withProgress(message = 'Getting data', value = 0, {
        db<-get_provBz_data(station_sensor=station_sensor,
                            datestart=datestart,
                            dateend=dateend,nstations=nstations,
                            round=round,#spread=spread,
                            inshiny=TRUE)#


        tab<-tot_tab %>% dplyr::select(SCODE,NAME)

        db<-left_join(db,tab,.before=2) %>% dplyr::select(-SCODE) %>%
          dplyr::arrange(NAME,TimeStamp) %>% unique()

        db <- db[, c(refcols, setdiff(names(db), refcols))]

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

    proxy <- leafletProxy("map")%>% clearMarkers()



    if(length(stations)!= 0){

      proxy <- proxy %>%
        flyToBounds(lng1 = max(stations$LONG),lat1 = max(stations$LAT),
                    lng2 = min(stations$LONG),lat2 = min(stations$LAT),
                    options = list(maxZoom = 12))

    }else{

      proxy <- proxy %>%
        fitBounds(lng1 = max(stationsSelNot$LONG),lat1 = max(stationsSelNot$LAT),
                  lng2 = min(stationsSelNot$LONG),lat2 = min(stationsSelNot$LAT))

    }
    #%>% removeDrawToolbar(clearFeatures = TRUE)

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

  output$rightStationSensor <-reactive({

    return(length(StatSens$station)!=0 & length(StatSens$sensors)!=0)

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
        nstat=D$documents[[1]]$NAME %>% unique %>% length %>% as.character
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
      db=D$documents[[1]]

      if(gather=="wide"){
        spread=TRUE}else{
          spread=FALSE
        }

      if(spread){

        db<-db %>%
          spread(Sensor, Value)

      }

      if(input$isdst){
        db$TimeStamp <- with_tz(db$TimeStamp,tzone = "Europe/Berlin")
      }
      #db=resample_provBz_data(df=df,round=round,spread=spread)

      if(input$csvjson=="csv"){
        write.csv(x=db,file =  con,quote = F,row.names = F,na = "NA",fileEncoding = "UTF-8")
      }else{
        write_json(db,con)
      }

    }

  )



  outputOptions(output, 'tablebuilt', suspendWhenHidden=FALSE)
  outputOptions(output, 'rightdate', suspendWhenHidden=FALSE)
  outputOptions(output, 'rightStationSensor', suspendWhenHidden=FALSE)
  outputOptions(output, "statlist", priority = 1)
}
