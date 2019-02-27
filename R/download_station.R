#' internal function Get data form one sation and multiple sensors from province of Bozen monitoring stations.
#'
#' @param sensor Sensor code
#' @param station SCODE of the station
#' @param datestart starting date of the timeseries.
#' @param dateend ending date of the timeseries.
#' @param spread whether to spread the table or leave it in long format
#' @param round the timestamp of the resample. defalutl is "hour" . write "raw" for no resample
#' @param notScode if TRUE SCODE column is deleted
#' @export
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
                           nstations=NULL){
  tryCatch({
    sensors=station_sensor[which(station_sensor$SCODE==station),]$Sensor

    db<-lapply(sensors,
               download_sensor,
               station = station,datestart = datestart,
               dateend = dateend,round=round,
               notScode=notScode,inshiny=inshiny,nstations=nstations)

    db_all<-bind_rows(db)
    if(notScode){
      db_all<- db_all %>% select(-SCODE)
    }else{
      db_all
    }

  }, error = function(e){NULL})
}