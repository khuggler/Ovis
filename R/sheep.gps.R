#' @title sheep.gps
#' @description Merge Raw GPS Data with AnimalID, Sex, and Eartag ID
#' @param vecpath path where vec data is located
#' @param sheepdb path to sheep capture database
#' @return Returns a data.frame with all gps data, AnimalID, Sex, and Species
#' @keywords capture, animal ID, gps, append
#' @export
#' @examples
#'

sheep.gps<-function(vecpath, sheepdb){
  sheep.dat<-Part::getVec(vecpath)
  sheep.db<-read.csv(sheepdb, stringsAsFactors = F)
  sheep.dat<-sheep.dat[sheep.dat$CollarSerialNumber %in% sheep.db$SerialNumber,]

  sheep.db$CapDate<-as.Date(sheep.db$CapDate, format = '%m/%d/%Y')
  sheep.db$MortDate<-ifelse(is.na(sheep.db$MortDate), as.character(Sys.Date()), as.character(sheep.db$MortDate))
  sheep.db$MortDate<-as.Date(sheep.db$MortDate, format = '%Y-%m-%d')

  sheep.dat$Date<-as.character(strptime(sheep.dat$TelemDate, format = "%Y-%m-%d"))
  sheep.dat$Date<-as.Date(sheep.dat$Date, format = '%Y-%m-%d')

  sheep.dat<-data.frame(sheep.dat)

  uni<-unique(sheep.db$SerialNumber)

  final.sheep<-data.frame()
  for(i in 1:length(uni)){
    sub.sheep<-sheep.dat[sheep.dat$CollarSerialNumber == uni[i],]
    sub.dat<-sheep.db[sheep.db$SerialNumber == uni[i],]

    new.sub<-sub.sheep[sub.sheep$Date > sub.dat$CapDate & sub.sheep$Date <= sub.dat$MortDate, ]
    new.sub$Sex<-'F'
    new.sub$AID<-sub.dat$AID[1]
    new.sub$Freq<-sub.dat$CollarFreq[1]
    new.sub$Eartag<-paste0(sub.dat$EarTagColor, " ", sub.dat$EarTagNumber, " ", sub.dat$EarTagSide)
    new.sub$CollarTag<-sub.dat$CollarTagColor

    final.sheep<-rbind(new.sub, final.sheep)

    print(i)
  }
  return(final.sheep)
}
