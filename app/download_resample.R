dwnld<-function(station,datestart,dateend,sensors=unique(getMeteoSensor()$Sensor),nstations){
  tryCatch({
  n<-nstations
  
download<-function(station,datestart,dateend,sensors=unique(getMeteoSensor()$Sensor)){
  tryCatch({
  ## Debugging variables
  #station = "37100MS"
  #datestart = "2012-01-01";dateend = "2014-01-01"
  #sensor<-"LT"
  #sensors<-unique(getMeteoSensor()$Sensor)
  #sensors<-getMeteoSensor()%>%distinct(Sensor)
  ##
  download_sensor<-function(sensor,station,datestart,dateend){
  tryCatch({  
    
    incProgress(amount = 1/n,message = "Downloading... (SCODE-Sensor):",detail = paste(station,sensor,sep=" - ") )
    data<-downloadMeteo(station_code = station,sensor_code = sensor,datestart = datestart,dateend = dateend)
    data<-as.data.frame(data)

  }, error = function(e){NULL})#    
    }
    
  db<-lapply(sensors, download_sensor,station = station,datestart = datestart,dateend = dateend)
  db_all<-bind_rows(db)
  
  }, error = function(e){NULL})#
}

db<-pblapply(station, download,datestart = datestart,dateend = dateend,sensors=sensors)
db_all<-bind_rows(db)

db_all$TimeStamp<-as_datetime(db_all$TimeStamp)
db_all<-left_join(db_all,getMeteoStat(),"SCODE")#,by="SCODE"
}, error = function(e){NULL})#
}



resample_provBz_data<-function(df,round="hour",spread=FALSE){
  #df<-db_prov
  if(round=="raw"){
    db_final<-df
    if(spread){
      #db_final<-tibble::rowid_to_column(db_final,var = "rowid")
      #db_final<-db_final %>% group_by(TimeStamp,SCODE,year=year(TimeStamp)) %>% filter(!(duplicated(TimeStamp)))%>% as.data.frame
      #seq<-seq.POSIXt(from = range(db_final$TimeStamp)[1],to = range(db_final$TimeStamp)[2],by = "5 min",tz="Europe/Berlin")
      #seq<-floor_date(seq,"5 min")
      #seq<-data.frame(TimeStamp=seq)
      #db_final<-left_join(seq,db_final)
      
      splitted<-split(db_final,df$Sensor)
      
      splitted_rowid<-base::lapply(splitted, function(x) {
        x %>% dplyr::mutate(idrow = row_number()) %>%
          tidyr::spread(key = Sensor,value = Value) %>%
          dplyr::select(-idrow)
      })
      
      merged<-base::Reduce(function(...) base::merge(..., all = TRUE),
                     splitted_rowid)
      db_final<-merged
      #db_final<-db_final %>% select(-year)
    }
    
  }else{
    
      
      db_sum<-df%>%filter(Sensor%in%c("N","LT")) %>% #
        group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor,NAME_D,NAME_I,NAME_L,NAME_E,ALT,LONG,LAT)%>%
        summarise(sum=sum(Value,na.rm = T)) %>% 
        gather(Variable, Value, -Sensor,-TimeStamp,-SCODE,-NAME_D,-NAME_I,-NAME_L,-NAME_E,-ALT,-LONG,-LAT) %>%
        unite(Sensor, Sensor, Variable,sep="") %>% 
        ungroup
      
      
      db_mean<-df%>%filter(!Sensor%in%c("N")) %>% 
        group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor,NAME_D,NAME_I,NAME_L,NAME_E,ALT,LONG,LAT)%>%
        summarise(mean=mean(Value,na.rm = T)) %>% 
        gather(Variable, Value, -Sensor,-TimeStamp,-SCODE,-NAME_D,-NAME_I,-NAME_L,-NAME_E,-ALT,-LONG,-LAT) %>%
        unite(Sensor, Sensor, Variable,sep="") %>% 
        ungroup
      
      db_min_max<-df%>%filter(Sensor%in%c("LT","LF")) %>% 
        group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor,NAME_D,NAME_I,NAME_L,NAME_E,ALT,LONG,LAT)%>%
        summarise(min=min(Value,na.rm = T),max=max(Value,na.rm = T)) %>% 
        gather(Variable, Value, -Sensor,-TimeStamp,-SCODE,-NAME_D,-NAME_I,-NAME_L,-NAME_E,-ALT,-LONG,-LAT) %>%
        unite(Sensor, Sensor, Variable,sep="") %>% 
        ungroup
    
      #db_wind<-df%>%filter(Sensor%in%c("WG")) %>% 
       # dplyr::mutate(Value=ifelse(Value>0 & Value<=22.5,"N",
                          # ifelse(Value>22.5 & Value<=67.5,"NE",
                                  #ifelse(Value>67.5 & Value<=112.5,"E",
                                        # ifelse(Value>112.5 & Value<=157.5,"SE",
                                              #  ifelse(Value>157.5 & Value<=202.5,"S",
                                                     #  ifelse(Value>202.5 & Value<=247.5,"SW",
                                                             # ifelse(Value>247.5 & Value<=292.5,"W",
                                                                   #  ifelse(Value>292.5 & Value<=337.5,"NW",
                                                                         #   ifelse(Value>292.5 & Value<=360,"N",NA))))))))))%>%
    
    #group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor,NAME_D,NAME_I,NAME_L,NAME_E,ALT,LONG,LAT)%>%
        
        #summarise(prevalentDir=as.character(sort(Value, decreasing=T)[1])) %>% 
        #gather(Variable, Value, -Sensor,-TimeStamp,-SCODE,-NAME_D,-NAME_I,-NAME_L,-NAME_E,-ALT,-LONG,-LAT) %>%
        #unite(Sensor, Sensor, Variable,sep="") %>% 
        #ungroup
      
    db_wind<-df%>%filter(Sensor%in%c("WG")) %>% 
        dplyr::mutate(Value=ifelse(Value>0 & Value<=22.5,1,
                           ifelse(Value>22.5 & Value<=67.5,2,
                                  ifelse(Value>67.5 & Value<=112.5,3,
                                         ifelse(Value>112.5 & Value<=157.5,4,
                                                ifelse(Value>157.5 & Value<=202.5,5,
                                                       ifelse(Value>202.5 & Value<=247.5,6,
                                                             ifelse(Value>247.5 & Value<=292.5,7,
                                                                     ifelse(Value>292.5 & Value<=337.5,8,
                                                                            ifelse(Value>292.5 & Value<=360,1,NA))))))))))%>%
    
    group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor,NAME_D,NAME_I,NAME_L,NAME_E,ALT,LONG,LAT)%>%
        
        summarise(Dir=median(Value,na.rm=T)) %>% 
        gather(Variable, Value, -Sensor,-TimeStamp,-SCODE,-NAME_D,-NAME_I,-NAME_L,-NAME_E,-ALT,-LONG,-LAT) %>%
        unite(Sensor, Sensor, Variable,sep="") %>% 
        ungroup
      db_final<-bind_rows(db_sum,db_mean,db_min_max,db_wind) %>% mutate(Value=ifelse(Value%in%c(-Inf,Inf,NaN),NA,Value))#

    
    #db_final<-df%>%
      #group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor,NAME_D,NAME_I,NAME_L,NAME_E,ALT,LONG,LAT)%>%
      #summarise(mean=mean(Value,na.rm = T),sum=sum(Value),max=max(Value,na.rm = T),min=min(Value,na.rm = T) ) %>% 
      #gather(Variable, Value, -Sensor,-TimeStamp,-SCODE,-NAME_D,-NAME_I,-NAME_L,-NAME_E,-ALT,-LONG,-LAT) %>%
      #unite(Sensor, Sensor, Variable,sep="")
    
    if(spread){
      db_final<-db_final %>% 
        #gather(Variable, Value, -Sensor,-TimeStamp,-SCODE,-NAME_D,-NAME_I,-NAME_L,-NAME_E,-ALT,-LONG,-LAT) %>%#
        #unite(temp, Sensor, Variable,sep="") %>%
        spread(Sensor, Value)
    }
    #}
  }
  
  #df_with_names<-left_join(db_final,getMeteoStat(),by="SCODE")
  df_with_names<-db_final %>% as.data.frame
}
