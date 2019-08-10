#' resamples data from the province of Bozen monitoring stations. uses data.table
#'
#' @param df The data.table downloaded. Has to have colnames = "TimeStamp", "Station", "Sensor", "Value"
#' @param round the resampling period. Possible options "raw" "hour" "day" "week" "month". Passed to lubridate::round_date
#' @param roundFUN the resampling function. Possible options lubridate::ceiling_date,lubridate::floor_date,lubridate::round_date
#' @param statsFUN the "statistical" function. any function that takes a vector in inpunt and a single value in output
#' @param ... additional arguments passed to statsFUN
#' @export
#' @importFrom lubridate as_date  as_datetime floor_date ceiling_date with_tz tz
#' @importFrom tidyr gather unite spread
#' @import data.table
#' @importFrom magrittr %>%
#' @importFrom glue glue
#'

resample_provBz_data2<-function(df,roundFUN,round="hour",
                                statsFUN,...){#statsFUN,



  #if(is.null(statsName)) {statsName = deparse(substitute(statsFUN))}

  df<-as.data.table(df)

  tz(df$TimeStamp)<-"Europe/Berlin"

  df$TimeStamp <- with_tz(df$TimeStamp,tzone = "Etc/GMT-1")

  df$TimeStamp[duplicated(df$TimeStamp,fromLast = F)]<-df$TimeStamp[duplicated(df$TimeStamp,fromLast = F)]-3600


  if(round!="raw"){

    statsName=statsFUN
    statsFUN=match.fun(statsFUN)
    roundFUN=match.fun(roundFUN)

    Sensor_stats=glue("{unique(df$Sensor)}_{statsName}")

    if(unique(df$Sensor)=="WR"){
      df[, Value := reclass_wind(Value)]
    }

    df <- df[, .( Value = statsFUN(Value,...)),
             by = .(TimeStamp=roundFUN(TimeStamp,unit = round),Station,Sensor)]

    df[, Sensor := Sensor_stats]

  }

  return(df=df)

}
