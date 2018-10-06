dwnld<-function(station,datestart,dateend,sensors=unique(getMeteoSensor()$Sensor),
                nstations,round="hour",spread=FALSE){
  tryCatch({
    n<-nstations
    
    download<-function(station,datestart,dateend,
                       sensors=unique(getMeteoSensor()$Sensor),round="hour",spread=FALSE){
      tryCatch({
        ## Debugging variables
        #station = "37100MS"
        #datestart = "2012-01-01";dateend = "2014-01-01"
        #sensor<-"LT"
        #sensors<-unique(getMeteoSensor()$Sensor)
        #sensors<-getMeteoSensor()%>%distinct(Sensor)
        ##
        download_sensor<-function(sensor,station,datestart,dateend,
                                  round="hour",spread=FALSE){
          tryCatch({  
            
            incProgress(amount = 1/n,message = "Downloading... (SCODE-Sensor):",
                        detail = paste(station,sensor,sep=" - ") )
            
            data<-downloadMeteo(station_code = station,sensor_code = sensor,
                                datestart = datestart,dateend = dateend)
            
            data<-as.data.frame(data)
            
            resample_provBz_data<-function(df,round="hour",spread=FALSE){
              #df<-db_prov
              if(round=="raw"){
                db_final<-df
                
              }else{
                
                
                db_sum<-df%>%filter(Sensor%in%c("N","LT")) %>% #
                  group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
                  summarise(sum=sum(Value,na.rm = T)) %>% 
                  gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
                  unite(Sensor, Sensor, Variable,sep="") %>% 
                  ungroup
                
                
                db_mean<-df%>%filter(!Sensor%in%c("N","WR")) %>% 
                  group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
                  summarise(mean=mean(Value,na.rm = T)) %>% 
                  gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
                  unite(Sensor, Sensor, Variable,sep="") %>% 
                  ungroup
                
                db_min_max<-df%>%filter(Sensor%in%c("LT","LF")) %>% 
                  group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
                  summarise(min=min(Value,na.rm = T),max=max(Value,na.rm = T)) %>% 
                  gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
                  unite(Sensor, Sensor, Variable,sep="") %>% 
                  ungroup
                
                
                db_wind<-df%>%filter(Sensor%in%c("WR")) %>% 
                  dplyr::mutate(Value=ifelse(Value>0 & Value<=22.5,1,
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
                  unite(Sensor, Sensor, Variable,sep="") %>% 
                  ungroup
                
                
                db_na<-df%>%
                  group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor)%>%
                  summarise(na=sum(is.na(Value))) %>% 
                  gather(Variable, Value, -Sensor,-TimeStamp,-SCODE) %>%
                  unite(Sensor, Sensor, Variable,sep="_") %>% 
                  ungroup
                
                db_final<-bind_rows(db_sum,db_mean,db_min_max,db_wind,db_na) %>% mutate(Value=ifelse(Value%in%c(-Inf,Inf,NaN),NA,Value))#
                
              }
              
              #df_with_names<-left_join(db_final,getMeteoStat(),by="SCODE")
              #df_with_names<-db_final %>% dplyr::arrange(TimeStamp) %>% as.data.frame
              df<-db_final %>% as.data.frame
            }
            
            data<-resample_provBz_data(df=data,round=round,spread=spread)
            
            
          }, error = function(e){NULL})#    
        }
        
        db<-lapply(sensors, download_sensor,station = station,datestart = datestart,
                   dateend = dateend,round=round,spread=spread)
        
        db_all<-bind_rows(db)
        
      }, error = function(e){NULL})#
    }
    
    db<-pblapply(station, download,datestart = datestart,dateend = dateend,
                 sensors=sensors,round=round,spread=spread)
    
    db_all<-bind_rows(db)
    
    db_all$TimeStamp<-as_datetime(db_all$TimeStamp)
    
    if(spread){
      
      if(round=="raw"){
        
        splitted<-split(db_all,db_all$Sensor)
        
        splitted_rowid<-base::lapply(splitted, function(x) {
          x %>% dplyr::mutate(idrow = row_number()) %>%
            tidyr::spread(key = Sensor,value = Value) %>%
            dplyr::select(-idrow)
        })
        
        merged<-base::Reduce(function(...) base::merge(..., all = TRUE),
                             splitted_rowid)
        db_all<-merged
        #db_final<-db_final %>% select(-year)
      }else{
        
        db_all<-db_all %>% 
          #gather(Variable, Value, -Sensor,-TimeStamp,-SCODE,-NAME_D,-NAME_I,-NAME_L,-NAME_E,-ALT,-LONG,-LAT) %>%#
          #unite(temp, Sensor, Variable,sep="") %>%
          spread(Sensor, Value)
        
      }
    }
    
    db_all#<-left_join(db_all,getMeteoStat(),"SCODE")#,by="SCODE"
  }, error = function(e){NULL})#
}




