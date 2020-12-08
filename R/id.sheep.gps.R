#' @title id.sheep.gps
#' @description Merge Raw GPS Data with AnimalID, Sex, and Eartag ID
#' @param gpspath path to gps data 
#' @param sheepdb path to sheep capture database
#' @param tzone desired time zone of gps data: "MST" or "US/Pacific"
#' @param capcol name of column where capture/start date exists
#' @param dateformat character string of the format that date columns are in
#' @param mortcol name of column where mortality date or end date exists
#' @param extracols vector of extra columns that should be appended to GPS data. Names in vector MUST match names in lookup table
#' @return Returns a data.frame with all gps data and any extra columns desired
#' @keywords capture, animal ID, gps, append
#' @export
#' @examples
#'

id.sheep.gps<-function(gpspath, sheepdb, tzone, capcol, dateformat, mortcol, extracols){
  sheep.dat<-read.csv(gpspath, stringsAsFactors = F)
  sheep.db<-read.csv(sheepdb, stringsAsFactors = F)
  #newdb<-sheep.db
  
  
  
  # transform dates
  sheep.db$CapDate<-as.Date(sheep.db[, capcol], format = dateformat)
  
  sheep.db$MortDate<-as.Date(sheep.db[, mortcol], format = dateformat)
  sheep.db$MortDate<-ifelse(is.na(sheep.db$MortDate), as.character(Sys.Date()), as.character(sheep.db$MortDate))
  sheep.db$MortDate<-as.Date(sheep.db$MortDate, tryFormats = c('%Y-%m-%d', "%m/%d/%Y"))
  
  
  sheep.dat$GMT<-as.POSIXct(sheep.dat$GMT, format = "%m/%d/%Y %H:%M", tz = "GMT")
  sheep.dat$GMT<-format(sheep.dat$GMT, tz = "MST", usetz = FALSE)
  sheep.dat$GMT<-as.POSIXct(sheep.dat$GMT, format = "%Y-%m-%d %H:%M:%S", tz = "MST")
  
  
  sheep.dat$Date<-as.character(strptime(sheep.dat$GMT, format = "%Y-%m-%d"))
  sheep.dat$Date<-as.Date(sheep.dat$Date, tryFormats = c('%Y-%m-%d', "%m/%d/%Y"))
  
  #make a dataframe of gps data
  sheep.dat<-data.frame(sheep.dat)

  

  
  
  col.hist<-Ovis::collar.history(data = newdb, idcol = "AID", study = "idaho", capdate = "Date", mortdate = "MortDate")
  
  uni<-unique(col.hist$AnimalID)
  
  #final.sheep<-data.frame()
  
  for(k in 1:nrow(col.hist)){
    xx<-col.hist[k, ]
    print(k)
    
    for(l in 1:2){
      if(l == 1){
        xxx<-xx[, c('AnimalID','GMU','PMU', 'Serial1', 'Serial1Start', 'Serial1End')]
      }
      
      if(l == 2){
        xxx<-xx[, c('AnimalID', 'GMU','PMU', 'Serial2', 'Serial2Start', 'Serial2End')]
      }
      
      if(is.na(xxx[1,4])){next}
      ss<-sheep.dat[sheep.dat$Collar_Serial_No==xxx[1,4],]
      ss<-ss[(ss$Date> xxx[,5])&ss$Date < (xxx[,6]),]
      ss<-ss[complete.cases(ss$LAT),]
      
      if(nrow(ss)==0){next}
      if(nrow(ss)>0){
        ss$AID<-xxx[1,1]
        ss$PMU<-xxx[1,3]
      }
      
      if(l == 1){
        ald<-ss
      }
      if(l>1){
        ald<-rbind(ald,ss)
      }
      
    }
    if(k == 1){
      outsp<-ald
    }
    if(k>1){
      outsp<-rbind(outsp,ald)
    }
  }
  
  outsp<-data.frame(outsp)
  outsp2<-outsp[!duplicated(outsp[,c(28,29)]),]
  outsp2<-outsp2[complete.cases(outsp2$Collar_Serial_No),]
  
  
  
  
  if(!is.na(extracols)){
    uni<-unique(outsp2$AID)
    final<-data.frame()
    for(i in 1:length(uni)){
      sub.sheep<-outsp2[outsp2$AID == uni[i],]
      sub.db<-sheep.db[sheep.db$AID == uni[i],]
      sub.db<-sub.db[nrow(sub.db),]
      
      for(l in 1:length(extracols)){
        sub.sheep[,extracols[l]]<-sub.db[,extracols[l]][1]
        
      }
      
      final<-rbind(sub.sheep, final)
    }
    
    return(final)
    
  }
  
  if(is.na(extracols)){
    return(outsp2)
  }
  
}



