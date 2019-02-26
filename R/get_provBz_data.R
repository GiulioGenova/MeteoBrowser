#' Get data from province of Bozen monitoring stations.
#'
#' @param station_sensor a table with SCODE and Sensor columns.
#' @param datestart starting date of the timeseries.
#' @param dateend ending date of the timeseries.
#' @param spread whether to spread the table or leave it in long format
#' @param round the timestamp of the resample. defalutl is "hour" . write "raw" for no resample
#' @export
#' @importFrom lubridate as_date  as_datetime floor_date
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
                          notScode=FALSE){#

  dateend=as_date(dateend)+1

  tryCatch({
  n<-nstations
  #datestart <- as_date(datestart)
  #dateend <- as_date(dateend)
  download<-function(station,datestart,dateend,
                     sensors=unique(get_provBz_sensors()$Sensor),
                     round="hour",spread=FALSE,
                     notScode=FALSE){
    tryCatch({

    download_sensor<-function(sensor,station,datestart,dateend,
                              round="hour",spread=FALSE,
                              notScode=FALSE){
       tryCatch({

      incProgress(amount = 1/n,message = "Downloading... (SCODE-Sensor):",
                  detail = paste(station,sensor,sep=" - ") )

      data<-downloadMeteo(station_code = station,sensor_code = sensor,
                          datestart = datestart,dateend = dateend)

      data<-as.data.frame(data)

      colnames(data)[colnames(data)=="Station"] <- "SCODE"

      resample_provBz_data<-function(df,round="hour",spread=FALSE){

        df$TimeStamp<-as_datetime(df$TimeStamp,tz="Europe/Berlin")

        if(round=="raw"){
          db_final<-df

        }else{


          db_sum<-df%>%filter(Sensor%in%c("N","SD")) %>% #"LT",
            group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
            summarise(sum=round(sum(Value,na.rm = T),2)) %>%
            gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
            unite(Sensor, Sensor, Variable,sep="_") %>%
            ungroup


          db_mean<-df%>%filter(!Sensor%in%c("N","WR","SD","WG.BOE")) %>%
            group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
            summarise(mean=round(mean(Value,na.rm = T),2)) %>%
            gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
            unite(Sensor, Sensor, Variable,sep="_") %>%
            ungroup

          db_min_max<-df%>%filter(Sensor%in%c("LT","LF")) %>%
            group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
            summarise(min=round(min(Value,na.rm = T),2),
                      max=round(max(Value,na.rm = T),2)) %>%
            gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
            unite(Sensor, Sensor, Variable,sep="_") %>%
            ungroup


          db_wind<-df%>%filter(Sensor%in%c("WR")) %>%
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

    db<-lapply(station_sensor[which(station_sensor$SCODE==station),]$Sensor, download_sensor,station = station,datestart = datestart,
               dateend = dateend,round=round,spread=spread,
               notScode=notScode)

    db_all<-bind_rows(db)
    if(notScode){
    db_all<- db_all %>% select(-SCODE)
    }else{
      db_all
    }

     }, error = function(e){NULL})
  }

  db<-pblapply(unique(station_sensor$SCODE), download,datestart = datestart,dateend = dateend,
               sensors=sensors,round=round,spread=spread,
               notScode=notScode)

  db_all<-bind_rows(db)

  db_all <- db_all %>%
    filter(TimeStamp < dateend)

  db_all$TimeStamp<-as_datetime(db_all$TimeStamp,tz="Europe/Berlin")

  if(spread){

    if(round=="raw"){

      splitted<-split(db_all,db_all$Sensor)

      splitted_rowid<-lapply(splitted, function(x) {
        x %>% mutate(idrow = row_number()) %>%
          spread(key = Sensor,value = Value) %>%
          select(-idrow)
      })

      merged<-Reduce(function(...) merge(..., all = TRUE),
                     splitted_rowid)
      db_all<-merged

    }else{

      db_all<-db_all %>%
        spread(Sensor, Value)

    }
  }

  db_all
  }, error = function(e){NULL})#
}
