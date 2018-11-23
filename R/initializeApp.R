#' initialize variables for the app
#'
#' @export
#' @importFrom MonalisR getMeteoStat
#' @importFrom  dplyr row_number mutate select left_join mutate_if funs
#' @importFrom tidyr unite
#' @import shiny
#' @importFrom magrittr %>%
#' @import spdplyr
#' @importFrom httr GET
#' @import leaflet
#' @import leaflet.extras
#'


initializeApp <- function(){

  header <<- dashboardHeader(titleWidth = 260)

  anchor <<- tags$a(href='http://www.eurac.edu/',
                    tags$img(style="vertical-align: bottom;width: 250px;",#padding-right: 10px;
                             #src='http://www.eurac.edu/Style%20Library/logoEURAC.jpg'),#, height='60', width='50'
                             src='LogoProvEuracN.PNG'),
                    class='tit')

  header$children[[2]]$children <<- tags$div(anchor,class = 'name')




  translation<<-read.csv(file.path(getwd(),"translationUtf.csv"),header = T,sep = ",",stringsAsFactors = F,encoding = "UTF-8")#

  tr <<- function(key,l,t=translation){ # translates text into current language
    x<-as.character(t[grep(key,t$key),l])
    return(x)
  }

  leaf.proj <<- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  LL <<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

  url <- "http://daten.buergernetz.bz.it/services/meteo/v1/sensors"
  u <- GET(url) %>% content
  se<-bind_rows(u)
  se <-se[!duplicated(se[ , 1:2 ]), ]
  st<-getMeteoStat()
  legend_tab<-full_join(st,se)%>%#dplyr::select(-NAME_L,-NAME_E,-DESC_L,-DATE,VALUE,-LAT,-LONG,-VALUE)%>%
    mutate_if(is.character, funs(as.factor(.)))%>%
    mutate(
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


  tot_tab<<-legend_tab%>%select(-NAME_L,-DESC_L,-DATE,-LAT,-LONG,-VALUE) %>%
    mutate(NAME = paste(NAME_D,NAME_I,sep=" / ")) %>%
    mutate(DESC_D = paste(DESC_D,UNIT,sep=" - ")) %>%
    mutate(DESC_I = paste(DESC_I,UNIT,sep=" - ")) %>%
    unite(DESC_E,DESC_E,UNIT,sep=" - ")

  legend_tab<<-legend_tab %>% select(-DATE,-VALUE)

  se_spread <<- se  %>%
    select(SCODE,TYPE,UNIT,VALUE,DATE) %>%
    mutate(DATE=paste0("(",as_datetime(DATE),")"))%>%
    unite(VALUE,VALUE,UNIT,sep=" ") %>%
    mutate(VALUE=paste0("<b>",VALUE,"</b>"))%>%
    unite(VALUE,VALUE,DATE,sep=" ") %>%
    spread(TYPE,VALUE)

  sspat<<-getMeteoStat(format = "spatial")
  se_spread<<-left_join(sspat,se_spread)




  refcols <<- c("TimeStamp", "NAME")#,"SCODE"
}
