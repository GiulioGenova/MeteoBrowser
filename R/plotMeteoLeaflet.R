#' display a leaflet map with all the metereological stations of the province of bozen
#'
#' @export
#' @importFrom MonalisR getMeteoStat
#' @import leaflet
#' @import leaflet.extras
#' @importFrom magrittr %>%
#' @import spdplyr
#'

plotMeteoLeaflet<-function (){

  stations <- getMeteoStat(format = "spatial")
  c1 <- awesomeIcons(icon = "ios-close", iconColor = "black",
                     library = "ion", markerColor = "blue")
  c2 <- awesomeIcons(icon = "ios-close", iconColor = "black",
                     library = "ion", markerColor = "grey")

  m <- leaflet() %>%
    addSearchOSM()%>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Locate Me",
      onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
    addFullscreenControl()%>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Street Map")%>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addAwesomeMarkers(lng = stations$LONG %>% as.character %>% as.numeric,
                      lat = stations$LAT %>%
                        as.character %>% as.numeric,
                      icon = c1,
                      popup = paste("Code:",stations$SCODE, "<br>",
                                    "Name GER:", stations$NAME_D,"<br>",
                                    "Name ITA:", stations$NAME_I, "<br>",
                                    "Altitude:",stations$ALT, "<br>",
                                    "<br>",
                                    "Latest recorded measurements:", "<br>",
                                    "Air temperature:",stations$LT, "<br>",
                                    "Relative humidity:",stations$LF, "<br>",

                                    "Precipitation:",stations$N, "<br>",
                                    "Wind speed:",stations$WG, "<br>",
                                    "Wind direction:",stations$WR, "<br>",
                                    "Wind guts:",stations$WG.BOE, "<br>",
                                    "Atmospheric perssion:",stations$LD.RED, "<br>",
                                    "Solar Radiation:",stations$GS, "<br>",
                                    "Sunshine hours:",stations$SD, "<br>",
                                    "Snow height:",stations$HS, "<br>",
                                    "Water temperature:",stations$WT, "<br>",
                                    "Water flow:",stations$Q, "<br>",
                                    "Water level:",stations$W, "<br>"#,
                                    #"Ground water level:",stations$WT, "W.ABST"
                      ))%>%
    addMeasure(position = "topleft",primaryLengthUnit = "meters")%>%
    addLayersControl(baseGroups = c("Street Map","Satellite"),
                     options = layersControlOptions(collapsed = FALSE),position = "topright")

  return(m)
}
