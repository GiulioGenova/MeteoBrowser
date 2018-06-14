dwnld<-function(station,datestart,dateend,sensors=unique(getMeteoSensor()$Sensor),nstations){#,round=NULL,server=shiny::getDefaultReactiveDomain()
  tryCatch({
  n<-nstations
  
download<-function(station,datestart,dateend,sensors=unique(getMeteoSensor()$Sensor)){
  tryCatch({
  #station = "37100MS"
  #datestart = "2012-01-01";dateend = "2014-01-01"
  #sensor<-"LT"
  #sensors<-unique(getMeteoSensor()$Sensor)
  #sensors<-getMeteoSensor()%>%distinct(Sensor)
  #initial.stop = 0
  download_sensor<-function(sensor,station,datestart,dateend){
  tryCatch({  
    #if(continue){
    #if (initial.stop < getDefaultReactiveDomain()$input$stop %>% as.numeric) {
     # initial.stop <<- initial.stop + 1
      #stop()
    #}else{
    
    incProgress(amount = 1/n,message = "Downloading... (SCODE-Sensor):",detail = paste(station,sensor,sep=" - ") )
    data<-downloadMeteo(station_code = station,sensor_code = sensor,datestart = datestart,dateend = dateend)
    data<-as.data.frame(data)
  #httpuv:::service()
 # continue <<- !isTRUE(shiny::getDefaultReactiveDomain()$input$stopThis)
  #}
  }, error = function(e){NULL})#    
    }
  
  
  db<-lapply(sensors, download_sensor,station = station,datestart = datestart,dateend = dateend)
  db_all<-bind_rows(db)
  
  }, error = function(e){NULL})#
}

db<-pblapply(station, download,datestart = datestart,dateend = dateend,sensors=sensors)
db_all<-bind_rows(db)

#round="hour"

db_all$TimeStamp<-as_datetime(db_all$TimeStamp)
db_all<-left_join(db_all,getMeteoStat(),"SCODE")#,by="SCODE"
}, error = function(e){NULL})#
}



resample_provBz_data<-function(df,round="hour",spread=FALSE){
  tryCatch({
  #df<-db_prov
  if(round=="raw"){
    db_final<-df
    if(spread){
      #db_final<-db_final %>% group_by(TimeStamp,SCODE,year=year(TimeStamp)) %>% filter(!(duplicated(TimeStamp)))%>% as.data.frame
      #db_final<-db_final %>% spread(key = Sensor,value = Value) %>% as.data.frame
      #db_final<-db_final %>% select(-year)
      splitted<-split(db_final,df$Sensor)
      
      splitted_rowid<-base::lapply(splitted, function(x) {
        x %>% mutate(idrow = row_number()) %>% spread(key = Sensor,value = Value) %>% select(-idrow)
      })
      
      merged<-Reduce(function(...) merge(..., all = TRUE),
                     splitted_rowid)
      db_final<-merged
      
    }
    
  }else{
  
      db_final<-df%>%
        group_by(TimeStamp=floor_date(TimeStamp,unit = round),SCODE,Sensor,NAME_D,NAME_I,NAME_L,NAME_E,ALT,LONG,LAT)%>%
        summarise(mean=mean(Value,na.rm = T),sum=sum(Value),max=max(Value,na.rm = T),min=min(Value,na.rm = T) ) %>% 
        gather(Variable, Value, -Sensor,-TimeStamp,-SCODE,-NAME_D,-NAME_I,-NAME_L,-NAME_E,-ALT,-LONG,-LAT) %>%
        unite(Sensor, Sensor, Variable,sep="")
      
      if(spread){
        db_final<-db_final %>% 
        gather(Variable, Value, -Sensor,-TimeStamp,-SCODE,-NAME_D,-NAME_I,-NAME_L,-NAME_E,-ALT,-LONG,-LAT) %>%
        unite(temp, Sensor, Variable,sep="") %>%
        spread(temp, Value)
      }
  }
  
  #df_with_names<-left_join(db_final,getMeteoStat(),by="SCODE")
  df_with_names<-db_final %>% as.data.frame
    }, error = function(e){df_with_names<-"Something went wrong"})#
}
#db=resample(df=df)
