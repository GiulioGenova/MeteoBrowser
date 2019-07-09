#' resamples data from the province of Bozen monitoring stations. uses data.table
#'
#' @param df The dataframe downloaded
#' @param round the resampling period. Possible options "raw" "hour" "day" "week" "month"
#' @export
#' @importFrom lubridate as_date  as_datetime floor_date ceiling_date with_tz tz
#' @importFrom tidyr gather unite spread
#' @import data.table
#' @importFrom magrittr %>%
#' @importFrom glue glue
#'

resample_provBz_data2<-function(df,roundFUN,round="hour",
                                statsFUN,statsName=NULL,rm.na=T){

  if(is.null(statsName)) {statsName = deparse(substitute(statsFUN))}

  df<-as.data.table(df)

  tz(df$TimeStamp)<-"Europe/Berlin"

  df$TimeStamp <- with_tz(df$TimeStamp,tzone = "Etc/GMT-1")

  df$TimeStamp[duplicated(df$TimeStamp,fromLast = F)]<-df$TimeStamp[duplicated(df$TimeStamp,fromLast = F)]-3600


  if(round!="raw"){
    Sensor_stats=glue("{unique(df$Sensor)}_{statsName}")


    df<-df[, by = .(TimeStamp=roundFUN(TimeStamp,unit = round),Station,Sensor),
           .( Value = statsFUN(Value,rm.na=rm.na))] %>%
    .[, Sensor := Sensor_stats]

  }

  return(df)

}
