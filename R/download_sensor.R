#' internal function Get data one sensor-sation at a time from province of Bozen monitoring stations.
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




download_sensor<-function(sensor,station,datestart,dateend,
                          round="hour",
                          notScode=FALSE,
                          inshiny=FALSE,nstations=NULL){
  tryCatch({

    if(inshiny){

      incProgress(amount = 1/nstations,message = "Downloading... (SCODE-Sensor):",
                  detail = paste(station,sensor,sep=" - ") )

    }
    data<-downloadMeteo(station_code = station,sensor_code = sensor,
                        datestart = datestart,dateend = dateend)

    data<-as.data.frame(data)

    colnames(data)[colnames(data)=="Station"] <- "SCODE"


    data<-resample_provBz_data(df=data,round=round)


  }, error = function(e){NULL})
}
