#' Get data from province of Bozen monitoring stations. applies a resample with "resample_provBz_data2"
#'
#' @param station Station "SCODE" to download. You can check SCODES station names and sensors typing MeteoBrowser::tot_tab_def
#' @param sensor Sensor to download. You can check SCODES station names and sensors typing MeteoBrowser::tot_tab_def
#' @param datestart starting date of the timeseries. format "YYYY-MM-DD"
#' @param dateend ending date of the timeseries. format "YYYY-MM-DD"
#' @param spread whether to spread the table or leave it in long format. Default is FALSE
#' @param sort column names on which to sort the table. available options: "NAME","TimeStamp","Sensor","Value"
#' @param inshiny if the function is called in the shiny app. default=FALSE
#' @param nstations if the function is called in the shiny app: number of stations * sensors. default=NULL
#' @param round the resampling period. Possible options "raw" "hour" "day" "week" "month". Passed to lubridate::round_date
#' @param roundFUN the resampling function. Possible options lubridate::ceiling_date,lubridate::floor_date,lubridate::round_date
#' @param statsFUN the "statistical" function. any function that takes a vector in inpunt and a single value in output
#' @param name a name to give to a column "NAME". default is the german/italian name of the station
#' @param ... additional arguments passed to statsFUN
#' @export
#' @importFrom lubridate as_date  as_datetime floor_date ceiling_date with_tz
#' @importFrom tidyr gather unite spread
#' @importFrom dplyr bind_rows bind_cols mutate select summarise group_by ungroup filter full_join distinct syms
#' @importFrom magrittr %>%
#' @importFrom MonalisR downloadMeteo
#' @importFrom shiny incProgress
#' @import data.table

download_and_resample2 <- function(sensor,station,datestart,dateend,
                                   roundFUN,round="hour",
                                   statsFUN,#statsFUN,
                                   spread = FALSE,
                                   notScode = FALSE,
                                   inshiny = FALSE,
                                   nstations = NULL,
                                   sort = NULL,
                                   name = NULL,
                                   ...){

  dateend=lubridate::as_date(dateend)+1

  #tryCatch({
  if(is.null(name)){
    name_tab=MonalisR::getMeteoStat() %>% dplyr::filter(SCODE==station) %>% dplyr::distinct()
    name= paste(as.character(name_tab$NAME_D),as.character(name_tab$NAME_I),sep="/")
  }

  if(inshiny){

    shiny::incProgress(amount = 1/nstations,message = "Downloading... (SCODE-Sensor):",
                       detail = paste(station,sensor,sep=" - ") )

  }

  db<-MonalisR::downloadMeteo(station_code = station,sensor_code = sensor,
                              datestart = datestart,dateend = dateend)

  db<-MeteoBrowser::resample_provBz_data2(df=db,roundFUN=roundFUN,round="hour",
                                          statsFUN,...)#statsFUN=statsFUN,


  if(spread){

    db <- dcast(db, TimeStamp + Station ~ Sensor,value.var = "Value")

  }


  if(notScode){

    db[, Station := NULL]

  }

  db[, NAME := name]
  #db["NAME"] = name

  if(!is.null(sort)){

    db[order(!!!dplyr::syms(sort))]

  }



  return(db=db)

  #}, error = function(e){NULL})
}
