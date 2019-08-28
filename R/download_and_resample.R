#' Get data from province of Bozen monitoring stations. applies a resample with "resample_provBz_data"
#'
#' @param station Station "SCODE" to download. You can check SCODES station names and sensors typing MeteoBrowser::tot_tab_def
#' @param sensor Sensor to download. You can check SCODES station names and sensors typing MeteoBrowser::tot_tab_def
#' @param datestart starting date of the timeseries. format "YYYY-MM-DD"
#' @param dateend ending date of the timeseries. format "YYYY-MM-DD"
#' @param spread whether to spread the table or leave it in long format. Default is FALSE
#' @param round the timestamp of the resample. defalutl is "hour" . write "raw" for no resample
#' @param sort column names on which to sort the table. available options: c("NAME","TimeStamp","Sensor","Value")
#' @param inshiny if the function is called in the shiny app. default=FALSE
#' @param nstations if the function is called in the shiny app: number of stations * sensors. default=NULL
#' @export
#' @importFrom lubridate as_date  as_datetime floor_date ceiling_date with_tz
#' @importFrom tidyr gather unite spread
#' @importFrom dplyr bind_rows bind_cols mutate select summarise group_by ungroup filter full_join distinct syms
#' @importFrom magrittr %>%
#' @importFrom MonalisR downloadMeteo
#' @importFrom shiny incProgress


download_and_resample <- function(sensor,station,datestart,dateend,
                                  round = "hour",
                                  spread = FALSE,
                                  notScode = FALSE,
                                  inshiny = FALSE,
                                  nstations = NULL,
                                  sort = NULL,
                                  filter_edges=TRUE){
  datestart_filt=as_datetime(paste(datestart,"00:00:00"),tz="Etc/GMT-1")
  dateend_filt=as_datetime(paste(dateend,"23:59:59"),tz="Etc/GMT-1")

  dateend=lubridate::as_date(dateend)+2
  datestart=as_date(datestart)-1

  tryCatch({

    name_tab=MonalisR::getMeteoStat() %>% dplyr::filter(SCODE==station) %>% dplyr::distinct()
    name= paste(as.character(name_tab$NAME_D),as.character(name_tab$NAME_I),sep="/")

    if(inshiny){

      shiny::incProgress(amount = 1/nstations,message = "Downloading... (SCODE-Sensor):",
                         detail = paste(station,sensor,sep=" - ") )

    }

    db<-MonalisR::downloadMeteo(station_code = station,sensor_code = sensor,
                                datestart = datestart,dateend = dateend)


    colnames(db)[colnames(db)=="Station"] <- "SCODE"


    db<-MeteoBrowser::resample_provBz_data(df=db,round=round)

    if(notScode){

      db<- db %>% dplyr::select(-SCODE)

    }


    db["NAME"] = name

    if(!is.null(sort)){

      db <- db %>% dplyr::arrange(!!!dplyr::syms(sort))

    }

    if(spread){

      db<-db %>%
        tidyr::spread(Sensor, Value)

    }

    if(filter_edges) {
      db <- db %>%
        filter(TimeStamp <= dateend_filt,TimeStamp >= datestart_filt)
    }
    return(db)

  }, error = function(e){NULL})
}
