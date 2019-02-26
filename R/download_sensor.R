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
                          round="hour",spread=FALSE,
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

    resample_provBz_data<-function(df,round="hour",spread=FALSE){

      df$TimeStamp<-as_datetime(df$TimeStamp,tz="Europe/Berlin")

      df$TimeStamp <- with_tz(df$TimeStamp,tzone = "Etc/GMT-1")

      df$TimeStamp[duplicated(df$TimeStamp,fromLast = T)]<-df$TimeStamp[duplicated(df$TimeStamp,fromLast = T)]-3600

      if(round=="raw"){
        db_final<-df

      }else{

        sumList = c("N","SD")

        notmeanList = c("N","WR","SD","WG.BOE")

        if(round!="hour"){

          minmaxList = c("LT","LF","LD.RED")

        }else{

          minmaxList = c()

        }

        windList = c("WR")

        db_sum<-df%>%filter(Sensor%in%sumList) %>% #"LT",
          group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
          summarise(sum=round(sum(Value,na.rm = T),2)) %>%
          gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
          unite(Sensor, Sensor, Variable,sep="_") %>%
          ungroup


        db_mean<-df%>%filter(!Sensor%in%notmeanList) %>%
          group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
          summarise(mean=round(mean(Value,na.rm = T),2)) %>%
          gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
          unite(Sensor, Sensor, Variable,sep="_") %>%
          ungroup

        db_min_max<-df%>%filter(Sensor%in%minmaxList) %>%
          group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
          summarise(min=round(min(Value,na.rm = T),2),
                    max=round(max(Value,na.rm = T),2)) %>%
          gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
          unite(Sensor, Sensor, Variable,sep="_") %>%
          ungroup


        db_wind<-df%>%filter(Sensor%in%windList) %>%
          mutate(Value=ifelse(Value>0 & Value<=22.5,1,
                              ifelse(Value>22.5 & Value<=67.5,2,
                                     ifelse(Value>67.5 & Value<=112.5,3,
                                            ifelse(Value>112.5 & Value<=157.5,4,
                                                   ifelse(Value>157.5 & Value<=202.5,5,
                                                          ifelse(Value>202.5 & Value<=247.5,6,
                                                                 ifelse(Value>247.5 & Value<=292.5,7,
                                                                        ifelse(Value>292.5 & Value<=337.5,8,
                                                                               ifelse(Value>292.5 & Value<=360,1,NA))))))))))%>%

          group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%

          summarise(Dir=as.numeric(names(which.max(table(Value,useNA = "no"))))) %>%
          gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
          unite(Sensor, Sensor, Variable,sep="_") %>%
          ungroup


        #           db_na<-df%>%
        #             group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
        #             summarise(na=sum(is.na(Value))) %>%
        #             gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
        #             unite(Sensor, Sensor, Variable,sep="_") %>%
        #             ungroup

        db_final<-bind_rows(db_sum,db_mean,db_min_max,db_wind) %>%
          mutate(Value=ifelse(Value%in%c(-Inf,Inf,NaN),NA,Value))#,db_na

      }

      df<-db_final %>% as.data.frame
    }

    data<-resample_provBz_data(df=data,round=round,spread=spread)


  }, error = function(e){NULL})
}
