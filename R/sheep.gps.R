#' @title sheep.gps
#' @description Merge Raw GPS Data with AnimalID, Sex, and Eartag ID
#' @param vecpath path where vec data is located
#' @param sheepdb path to sheep capture database
#' @param tzone desired time zone of gps data: "MST" or "US/Pacific"
#' @param serialcol name of column in capture datebase/lookup table where Serial Number exists
#' @param capcol name of column where capture/start date exists
#' @param dateformat character string of the format that date columns are in
#' @param mortcol name of column where mortality date or end date exists
#' @param extracols vector of extracolumns that should be appended to GPS data. Names in vector MUCH match named in lookup table
#' @return Returns a data.frame with all gps data and any extra columns desired
#' @keywords capture, animal ID, gps, append
#' @export
#' @examples
#'

sheep.gps<-function(vecpath, sheepdb, tzone, serialcol, capcol, dateformat, mortcol, extracols){
  sheep.dat<-Ovis::getVec(vecpath, tzone)
  sheep.db<-read.csv(sheepdb, stringsAsFactors = F)

  #sheep.dat<-data.frame(sheep.dat)
  #sheep2<-sheep.dat
  sheep.dat<-sheep.dat[sheep.dat$CollarSerialNumber %in% sheep.db[, serialcol],]

  sheep.db$CapDate<-as.Date(sheep.db[, capcol], format = dateformat)

  sheep.db$MortDate<-as.Date(sheep.db[, mortcol], format = dateformat)
  sheep.db$MortDate<-ifelse(is.na(sheep.db$MortDate), as.character(Sys.Date()), as.character(sheep.db$MortDate))
  sheep.db$MortDate<-as.Date(sheep.db$MortDate, tryFormats = c('%Y-%m-%d', "%m/%d/%Y"))

  sheep.dat$Date<-as.character(strptime(sheep.dat$TelemDate, format = "%Y-%m-%d"))
  sheep.dat$Date<-as.Date(sheep.dat$Date, tryFormats = c('%Y-%m-%d', "%m/%d/%Y"))

  sheep.dat<-data.frame(sheep.dat)

  uni<-unique(sheep.db[, serialcol])

  final.sheep<-data.frame()
  for(i in 1:length(uni)){
    sub.sheep<-sheep.dat[sheep.dat$CollarSerialNumber == uni[i],]
    sub.dat<-sheep.db[sheep.db[, serialcol] == uni[i],]

    new.sub<-sub.sheep[sub.sheep$Date > sub.dat$CapDate & sub.sheep$Date < sub.dat$MortDate, ]

    if(length(extracols) > 0){
      for(l in 1:length(extracols)){
       new.sub[,extracols[l]]<-sub.dat[,extracols[l]][1]

      }
    }

        final.sheep<-rbind(new.sub, final.sheep)
  }


  return(final.sheep)
}
