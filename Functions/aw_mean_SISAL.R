awmean_SISAL<-function(entity_list , lats = seq(from = -90, to = 90, length.out = 73)){
  
  fld <- array(dim = c(96,73), NA)
  #create field (here as HadCM3)
  for(lon in 1:96){
    lon.start = ((lon-1)*3.75)-180
    lon.stop = (lon*3.75)-180
    for(lat in 1:73){
      lat.start = -90+(lat-1)*2.5
      lat.stop = -90+lat*2.5
      
      NoE = 0
      value_list = c()
      for(entity in entity_list$entity_id){
        lon.entity = entity_list$lon[entity_list$entity_id == entity]
        lat.entity = entity_list$lat[entity_list$entity_id == entity]
        if(lon.entity>lon.start & lon.entity<lon.stop & lat.entity>lat.start & lat.entity<lat.stop){
          value_list = c(value_list, entity_list$value)
        }
      }
      
      if(length(value_list)>0){
        fld[lon,lat] = mean(value_list, na.rm = T)
      }
    }
  }
  
  
  zonmean<-apply(fld,2,mean,na.rm=TRUE)
  w.lats<-cos(lats*pi/180)/sum(cos(lats*pi/180))
  #if ((sum(is.na(zonmean))) > 2) {return(NA)}
  tavg<-sum(w.lats*zonmean,na.rm=TRUE)
  return(tavg)
}