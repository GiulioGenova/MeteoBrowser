dwnld<-function(station,datestart,dateend,sensors=unique(getMeteoSensor()$Sensor),nstations){#,round=NULL,server=shiny::getDefaultReactiveDomain()
  
  n<-nstations
  
download<-function(station,datestart,dateend,sensors=unique(getMeteoSensor()$Sensor)){
  
  #station = "37100MS"
  #datestart = "2012-01-01";dateend = "2014-01-01"
  #sensor<-"LT"
  #sensors<-unique(getMeteoSensor()$Sensor)
  #sensors<-getMeteoSensor()%>%distinct(Sensor)
  #initial.stop = 0
  download_sensor<-function(sensor,station,datestart,dateend){
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
    }
  
  
  db<-lapply(sensors, download_sensor,station = station,datestart = datestart,dateend = dateend)
  db_all<-bind_rows(db)
  
  
}

db<-pblapply(station, download,datestart = datestart,dateend = dateend,sensors=sensors)
db_all<-bind_rows(db)

#round="hour"

db_all$TimeStamp<-as_datetime(db_all$TimeStamp)
db_all

}

#df="C:/Users/GGenova/Downloads/data_5 mins_2018-05-11_2018-05-15.csv"
#df=read_csv(df)

resample<-function(df,round="hour"){
  
  if(round=="raw"){
    db_final<-df
  }else{
  if(any(c("N") %in% df$Sensor)){
    df_sum<-df%>%spread(Sensor,Value) %>% group_by(TimeStamp=floor_date(TimeStamp,unit=round),Station) %>%
      summarise_at(vars(N),funs(sum(.,na.rm=T))) %>% gather(Sensor,Value,-TimeStamp,-Station)
    
    df_mean<-df%>%spread(Sensor,Value) %>% group_by(TimeStamp=floor_date(TimeStamp,unit=round),Station) %>%
      summarise_at(vars(-N),funs(mean(.,na.rm=T))) %>% gather(Sensor,Value,-TimeStamp,-Station)
    
    db_final<-full_join(df_sum,df_mean)
  }else{
    db_final<-df%>%group_by(TimeStamp=floor_date(TimeStamp,unit = round),Station,Sensor)%>%
      summarise(Value=mean(Value,na.rm = T) )
  }
}

  
db_with_names<-left_join(db_final,getMeteoStat(),c("Station"="SCODE"))
}
#db=resample(df=df)
