#' resamples data from the province of Bozen monitoring stations.
#'
#' @param df The dataframe downloaded
#' @param round the resampling period. Possible options "raw" "hour" "day" "week" "month"
#' @export
#' @importFrom lubridate as_date  as_datetime floor_date ceiling_date with_tz tz
#' @importFrom tidyr gather unite spread
#' @importFrom dplyr bind_rows bind_cols mutate select summarise group_by ungroup filter full_join
#' @importFrom magrittr %>%
#'

resample_provBz_data<-function(df,round="hour"){

  tz(df$TimeStamp)<-"Europe/Berlin"

  df$TimeStamp <- with_tz(df$TimeStamp,tzone = "Etc/GMT-1")

  df$TimeStamp[duplicated(df$TimeStamp,fromLast = F)]<-df$TimeStamp[duplicated(df$TimeStamp,fromLast = F)]-3600

  if(round=="raw"){
    #db_final<-df

  }else{

    sumList = c("N","SD")

    notmeanList = c("N","WR","SD","WG.BOE")

    if(round!="hour"){

      minList = c("LT","LF","LD.RED")
      maxList = c("LT","LF","LD.RED","WG.BOE")

    }else{

      minList = c()
      maxList = c("WG.BOE")
    }

    windList = c("WR")

    if(round=="hour"){

      db_sum<-df%>%filter(Sensor%in%sumList) %>% #"LT",
        group_by(TimeStamp=ceiling_date(TimeStamp,unit = round),SCODE,Sensor)%>%
        summarise(sum=round(sum(Value,na.rm = T),2)) %>%
        gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
        unite(Sensor, Sensor, Variable,sep="_") %>%
        ungroup


      db_mean<-df%>%filter(!Sensor%in%notmeanList) %>%
        group_by(TimeStamp=ceiling_date(TimeStamp,unit = round),SCODE,Sensor)%>%
        summarise(mean=round(mean(Value,na.rm = T),2)) %>%
        gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
        unite(Sensor, Sensor, Variable,sep="_") %>%
        ungroup

      db_min<-df%>%filter(Sensor%in%minList) %>%
        group_by(TimeStamp=ceiling_date(TimeStamp,unit = round),SCODE,Sensor)%>%
        summarise(min=round(min(Value,na.rm = T),2)) %>%
        gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
        unite(Sensor, Sensor, Variable,sep="_") %>%
        ungroup

      db_max<-df%>%filter(Sensor%in%maxList) %>%
        group_by(TimeStamp=ceiling_date(TimeStamp,unit = round),SCODE,Sensor)%>%
        summarise(max=round(max(Value,na.rm = T),2)) %>%
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

        group_by(TimeStamp=ceiling_date(TimeStamp,unit = round),SCODE,Sensor)%>%

        summarise(Dir=as.numeric(names(which.max(table(Value,useNA = "no"))))) %>%
        gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
        unite(Sensor, Sensor, Variable,sep="_") %>%
        ungroup


    }else{


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

      db_min<-df%>%filter(Sensor%in%minList) %>%
        group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
        summarise(min=round(min(Value,na.rm = T),2)) %>%
        gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
        unite(Sensor, Sensor, Variable,sep="_") %>%
        ungroup

      db_max<-df%>%filter(Sensor%in%maxList) %>%
        group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
        summarise(max=round(max(Value,na.rm = T),2)) %>%
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


    }

    #           db_na<-df%>%
    #             group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
    #             summarise(na=sum(is.na(Value))) %>%
    #             gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
    #             unite(Sensor, Sensor, Variable,sep="_") %>%
    #             ungroup

    df<-bind_rows(db_sum,db_mean,db_min,db_max,db_wind) %>%
      mutate(Value=ifelse(Value%in%c(-Inf,Inf,NaN),NA,Value))#,db_na
    remove(db_sum,db_mean,db_min,db_max,db_wind)
  }

  return(df)
}
