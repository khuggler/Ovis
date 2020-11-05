#' @title sheep.gps
#' @description Merge Raw GPS Data with AnimalID, Sex, and Eartag ID
#' @param vecpath path where vec data is located
#' @param sheepdb path to sheep capture database
#' @param tzone desired time zone of gps data: "MST" or "US/Pacific"
#' @param serialcol name of column in capture datebase/lookup table where Serial Number exists
#' @param capcol name of column where capture/start date exists
#' @param dateformat character string of the format that date columns are in
#' @param mortcol name of column where mortality date or end date exists
#' @param extracols vector of extra columns that should be appended to GPS data. Names in vector MUST match names in lookup table
#' @return Returns a data.frame with all gps data and any extra columns desired
#' @keywords capture, animal ID, gps, append
#' @export
#' @examples
#'

sheep.gps<-function(vecpath, sheepdb, tzone, serialcol, capcol, dateformat, mortcol, extracols){
  sheep.dat<-Ovis::getVec(vecpath, tzone)
  sheep.db<-read.csv(sheepdb, stringsAsFactors = F)
  newdb<-sheep.db



  # transform dates
  sheep.db$CapDate<-as.Date(sheep.db[, capcol], format = dateformat)

  sheep.db$MortDate<-as.Date(sheep.db[, mortcol], format = dateformat)
  sheep.db$MortDate<-ifelse(is.na(sheep.db$MortDate), as.character(Sys.Date()), as.character(sheep.db$MortDate))
  sheep.db$MortDate<-as.Date(sheep.db$MortDate, tryFormats = c('%Y-%m-%d', "%m/%d/%Y"))

  sheep.dat$Date<-as.character(strptime(sheep.dat$TelemDate, format = "%Y-%m-%d"))
  sheep.dat$Date<-as.Date(sheep.dat$Date, tryFormats = c('%Y-%m-%d', "%m/%d/%Y"))

  #make a dataframe of gps data
  sheep.dat<-data.frame(sheep.dat)
  sheep.dat$Date<-strftime(sheep.dat$TelemDate, format = "%Y-%m-%d")




  col.hist<-Ovis::collar.history(data = newdb, idcol = "AID", study = "washington", capdate = "CapDate", mortdate = "MortDate")

  uni<-unique(col.hist$AnimalID)

  #final.sheep<-data.frame()

  for(k in 1:nrow(col.hist)){
    xx<-col.hist[k, ]
    print(k)

    for(l in 1:2){
      if(l == 1){
        xxx<-xx[, c('AnimalID', 'Serial1', 'Serial1Start', 'Serial1End')]
      }

      if(l == 2){
        xxx<-xx[, c('AnimalID', 'Serial2', 'Serial2Start', 'Serial2End')]
      }

      if(is.na(xxx[1,2])){next}
      ss<-sheep.dat[sheep.dat$CollarSerialNumber==xxx[1,2],]
      ss<-ss[(ss$Date> xxx[,3])&ss$Date < (xxx[,4]),]
      ss<-ss[complete.cases(ss$Lat),]

      if(nrow(ss)==0){next}
      if(nrow(ss)>0){
        ss$AID<-xxx[1,1]
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
  outsp2<-outsp[!duplicated(outsp[,1:4]),]
  outsp2<-outsp2[complete.cases(outsp2$CollarSerialNumber),]


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



