#' Get data from province of Bozen monitoring stations.
#'
#' @param station_sensor a table with SCODE and Sensor columns. You can check SCODES station names and sensors typing MeteoBrowser::tot_tab_def
#' @param datestart starting date of the timeseries. format "YYYY-MM-DD"
#' @param dateend ending date of the timeseries. format "YYYY-MM-DD"
#' @param spread whether to spread the table or leave it in long format. Default is FALSE
#' @param round the timestamp of the resample. defalutl is "hour" . write "raw" for no resample
#' @export
#' @importFrom lubridate as_date  as_datetime floor_date ceiling_date with_tz
#' @importFrom tidyr gather unite spread
#' @importFrom dplyr bind_rows bind_cols mutate select summarise group_by ungroup filter full_join
#' @importFrom magrittr %>%
#' @importFrom MonalisR downloadMeteo
#' @importFrom pbapply pblapply
#' @importFrom shiny incProgress

#remotes::install_gitlab(repo = "earth_observation_public/MonalisR",host = "gitlab.inf.unibz.it")

get_provBz_data<-function(station_sensor,
                          datestart=Sys.Date()-1,
                          dateend=Sys.Date()+1,
                          #sensors=unique(get_provBz_sensors()$Sensor),
                          round="hour",spread=FALSE,
                          nstations=NULL,
                          notScode=FALSE,
                          inshiny=FALSE){#

  dateend=as_date(dateend)+1

  #tryCatch({
    #datestart <- as_date(datestart)
    #dateend <- as_date(dateend)


    db<-pblapply(unique(station_sensor$SCODE),
                 download_station,
                 station_sensor=station_sensor,
                 datestart = datestart,dateend = dateend,
                 sensors=sensors,round=round,
                 notScode=notScode,inshiny=inshiny,nstations=nstations)

    db_all<-bind_rows(db)

    db_all <- db_all %>%
      filter(TimeStamp < dateend)

    #db_all$TimeStamp<-as_datetime(db_all$TimeStamp,tz="Europe/Berlin")
    #db_all$TimeStamp <- with_tz(db_all$TimeStamp,tzone = "Europe/Berlin")
    if(spread){

      db_all<-db_all %>%
        spread(Sensor, Value)

    }

    db_all

  #}, error = function(e){NULL})#
}
