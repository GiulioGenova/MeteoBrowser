library(dplyr)
library(tidyr)
library(httr)
#library(stringi)
library(MonalisR)
library(MeteoBrowser)

u <- tryCatch({
  GET("http://daten.buergernetz.bz.it/services/meteo/v1/sensors") %>% content
}, error = function(e){NULL})#

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
                                                   ifelse(DESC_D=="Windgeschwindigkeit Böe","Wind gust",
                                                          ifelse(DESC_D=="Luftdruck","Atmospheric pression",
                                                                 ifelse(DESC_D=="Globalstrahlung","Solar Radiation",
                                                                        ifelse(DESC_D=="Sonnenscheindauer","Sunshine hours",
                                                                               ifelse(DESC_D=="Schneehöhe","Snow height",
                                                                                      ifelse(DESC_D=="Wassertemperatur","Water temperature",
                                                                                             ifelse(DESC_D=="Lufttemperatur","Air temperature",
                                                                                                    ifelse(DESC_D=="Durchfluss","Water flow",
                                                                                                           ifelse(DESC_D=="Wasserstand","Water level",
                                                                                                                  ifelse(DESC_D=="Grundwasserstand","Groundwater level","unknown"))))))))))))))))


  tot_tab<-legend_tab%>%select(-NAME_L,-DESC_L,-DATE,-LAT,-LONG,-VALUE) %>%
    mutate(NAME = paste(NAME_D,NAME_I,sep=" / ")) %>%
    mutate(DESC_D = paste(DESC_D,UNIT,sep=" - ")) %>%
    mutate(DESC_I = paste(DESC_I,UNIT,sep=" - ")) %>%
    unite(DESC_E,DESC_E,UNIT,sep=" - ")


  # library(stringi)
  # tot_tab[] <- lapply(tot_tab, function(x) stri_encode(x, "", "UTF-8"))

  tot_tab_def <- tot_tab

  save(tot_tab_def,file = "tot_tab_def.rda")
