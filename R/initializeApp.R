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
  # the translation key file
  translation<<-read.csv(file.path(getwd(),"translationUtf.csv"),header = T,sep = ",",stringsAsFactors = F,encoding = "UTF-8")#

  # tr <<- function(key,l,t=translation){ # translates text into current language
  #   x<-as.character(t[grep(key,t$key),l])
  #   return(x)
  # }

  # Building the table of stations

  checkStat <<- tryCatch({
    GET("http://daten.buergernetz.bz.it/services/meteo/v1/stations") %>% content
  }, error = function(e){NULL})#

  u <<- tryCatch({
    GET("http://daten.buergernetz.bz.it/services/meteo/v1/sensors") %>% content
  }, error = function(e){NULL})#

  if(!is.null(u)){
    se<-bind_rows(u)
    se <-se[!duplicated(se[ , 1:2 ]), ]
    st<-getMeteoStat()
    legend_tab<-full_join(st,se)%>%#dplyr::select(-NAME_L,-NAME_E,-DESC_L,-DATE,VALUE,-LAT,-LONG,-VALUE)%>%
      mutate_if(is.character, funs(as.factor(.)))%>%
      mutate(
        id=row_number(),
        DESC_E=as.factor(case_when(DESC_D=="relative Luftfeuchte"~"Relative humidity",
                                   DESC_D=="Niederschlag"~"Precipitation",
                                   DESC_D=="Windgeschwindigkeit"~"Wind speed",
                                   DESC_D=="Windrichtung"~"Wind direction",
                                   DESC_D=="Windgeschwindigkeit Böe"~"Wind gust",
                                   DESC_D=="Luftdruck"~"Atmospheric pression",
                                   DESC_D=="Globalstrahlung"~"Solar Radiation",
                                   DESC_D=="Sonnenscheindauer"~"Sunshine hours",
                                   DESC_D=="Schneehöhe"~"Snow height",
                                   DESC_D=="Wassertemperatur"~"Water temperature",
                                   DESC_D=="Lufttemperatur"~"Air temperature",
                                   DESC_D=="Durchfluss"~"Water flow",
                                   DESC_D=="Wasserstand"~"Water level",
                                   DESC_D=="Grundwasserstand"~"Groundwater level",
                                   is.na(DESC_D) ~ "unknown")))


        tot_tab<<-legend_tab%>%select(-NAME_L,-DESC_L,-DATE,-LAT,-LONG,-VALUE) %>%
          mutate(NAME = paste(NAME_D,NAME_I,sep=" / ")) %>%
          mutate(DESC_D = paste(DESC_D,UNIT,sep=" - ")) %>%
          mutate(DESC_I = paste(DESC_I,UNIT,sep=" - ")) %>%
          unite(DESC_E,DESC_E,UNIT,sep=" - ")

        # legend_tab<<-legend_tab %>% select(-DATE,-VALUE)

        se_spread <- se  %>%
          select(SCODE,TYPE,UNIT,VALUE,DATE) %>%
          mutate(DATE=paste0("(",as_datetime(DATE),")"))%>%
          unite(VALUE,VALUE,UNIT,sep=" ") %>%
          mutate(VALUE=paste0("<b>",VALUE,"</b>"))%>%
          unite(VALUE,VALUE,DATE,sep=" ") %>%
          spread(TYPE,VALUE)

        sspat<<-getMeteoStat(format = "spatial")
        #tot_tab<<- tot_tab_def
        #sspat<<- sspat_def

        se_spread<<-left_join(sspat,se_spread)

  } else {
    tot_tab<<- tot_tab_def
    #legend_tab<<- legend_tab_def
    sspat<<- sspat_def

    se_spread<<- se_spread_def

    waring_website<-"Province Server not available"

  }
  ##

  # some static variables

  header <<- dashboardHeader(titleWidth = 260)

  anchor <<- tags$a(href='http://www.eurac.edu/',
                    tags$img(style="vertical-align: bottom;width: 250px;",#padding-right: 10px;
                             #src='http://www.eurac.edu/Style%20Library/logoEURAC.jpg'),#, height='60', width='50'
                             src='LogoProvEuracN.PNG'),
                    class='tit')

  header$children[[2]]$children <<- tags$div(anchor,class = 'name')


  refcols <<- c("TimeStamp", "NAME")#,"SCODE"

}
