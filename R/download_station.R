#' internal function Get data form one sation and multiple sensors from province of Bozen monitoring stations.
#'
#' @param sensor Sensor code
#' @param station SCODE of the station
#' @param datestart starting date of the timeseries.
#' @param dateend ending date of the timeseries.
#' @param spread whether to spread the table or leave it in long format
#' @param round the timestamp of the resample. defalutl is "hour" . write "raw" for no resample
#' @param notScode if TRUE SCODE column is deleted
#'
#' @importFrom lubridate as_date  as_datetime floor_date ceiling_date with_tz
#' @importFrom tidyr gather unite spread
#' @importFrom dplyr bind_rows bind_cols mutate select summarise group_by ungroup filter full_join
#' @importFrom magrittr %>%
#' @importFrom MonalisR downloadMeteo
#' @importFrom pbapply pblapply
#' @importFrom shiny incProgress



download_station<-function(station,station_sensor,datestart,dateend,
                           sensors=unique(get_provBz_sensors()$Sensor),
                           round="hour",
                           notScode=FALSE,
                           inshiny=FALSE,
                           nstations=NULL,
                           spread=FALSE,
                           sort=TRUE){
  tryCatch({
  sensors=station_sensor[which(station_sensor$SCODE==station),]$Sensor

  name_tab=MonalisR::getMeteoStat() %>% dplyr::filter(SCODE==station)
  name= paste(as.character(name_tab$NAME_D),as.character(name_tab$NAME_I),sep="/")

  db<-lapply(sensors,
             download_sensor,
             station = station,datestart = datestart,
             dateend = dateend,round=round,
             notScode=notScode,inshiny=inshiny,nstations=nstations)

  db<-bind_rows(db)
  if(notScode){
    db<- db %>% select(-SCODE)
  }else{
    db
  }

  if(spread){

    db<-db %>%
      spread(Sensor, Value)

  }


  db["NAME"]=name

  if(sort){
    db <- db %>% dplyr::arrange(NAME)

  }

  return(db)
  }, error = function(e){NULL})
}
