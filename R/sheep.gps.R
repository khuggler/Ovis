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

  final.sheep<-data.frame()

  for(i in 1:length(uni)){
    sub<-col.hist[col.hist$AnimalID == uni[i],]
    sercols<-sub[, c(2,3)]
    sercols<-c(sercols[1,1], sercols[1,2])
    len<-as.numeric(sum(is.na(sercols)))


    if(len == 0){

      for(l in 1:length(sercols)){

        if(l == 1){
      subsub1<-sub[, c('AnimalID', 'Serial1', 'Serial1Start', 'Serial1End')]
      sub.gps<-sheep.dat[sheep.dat$CollarSerialNumber == subsub1$Serial1, ]


        sub.gps1<-sub.gps[sub.gps$Date > subsub1$Serial1Start & sub.gps$Date < subsub1$Serial1End, ]


               }

        if(l == 2){
          subsub2<-sub[, c('AnimalID', 'Serial2', 'Serial2Start', 'Serial2End')]
          sub.gps<-sheep.dat[sheep.dat$CollarSerialNumber == subsub2$Serial2, ]


            sub.gps2<-sub.gps[sub.gps$Date > subsub2$Serial2Start & sub.gps$Date < subsub2$Serial2End, ]


        }

        sheepsheep<-rbind(sub.gps1, sub.gps2)
        sheepsheep$AID<-sub$AnimalID
      }

    }

      if(len == 1){
        subsub1<-sub[, c('AnimalID', 'Serial1', 'Serial1Start', 'Serial1End')]
        sub.gps<-sheep.dat[sheep.dat$CollarSerialNumber == subsub1$Serial1, ]


          sub.gps1<-sub.gps[sub.gps$Date > subsub1$Serial1Start & sub.gps$Date < subsub1$Serial1End, ]


        sheepsheep<-sub.gps1
        sheepsheep$AID<-sub$AnimalID
      }


      final.sheep<-rbind(sheepsheep, final.sheep)
    }


if(!is.na(extracols)){
  uni<-unique(final.sheep$AID)
  final<-data.frame()
  for(i in 1:length(uni)){
    sub.sheep<-final.sheep[final.sheep$AID == uni[i],]
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
    return(final.sheep)
  }

}


