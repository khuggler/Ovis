#' @title sheep.gps
#' @description Scrape GPS data  from online server, and merge with downloaded data from collars
#' @param keys full path to one or more key files
#' @param sheepdb path to sheep capture database
#' @param collarhistory path to collar history file
#' @param tzone desired time zone of gps data: "MST" or "US/Pacific"
#' @param capcol name of column where capture/start date exists
#' @param dateformat character string of the format that date columns are in
#' @param mortcol name of column where mortality date or end date exists
#' @param dnld.data Logical. TRUE/FALSE whether to include downloaded data from collars
#' @param dnld.fold path to root folder where downloaded data exists
#' @param extracols vector of extra columns that should be appended to GPS data. Names in vector MUST match names in lookup table
#' @return Returns a data.frame with all gps data and any extra columns desired
#' @keywords capture, animal ID, gps, append
#' @export
#' @examples
#'


sheep.gps<-function(keys, sheepdb, collarhistory, tzone, capcol, dateformat, mortcol, dnld.data, dnld.fold, extracols){

  if(!'collar' %in% installed.packages()){
  devtools::install_github("Huh/collar", force = T)
  }

  if('collar' %in% installed.packages()){
    require(collar)
  }





  key_path <- collar::get_paths(keys)
  sheep.dat<-collar::fetch_vectronics(key_path, type = "gps")
  sheep.dat<-data.frame(sheep.dat)
  sheep.dat$acquisitiontime<-as.POSIXct(sheep.dat$acquisitiontime, format = paste0("%Y-%m-%d", "T", "%H:%M:%S"), tz = "UTC", origin = sheep.dat$acquisitiontime)
  attr(sheep.dat$acquisitiontime, "tzone")<-tzone

  sheep.db<-read.csv(sheepdb, stringsAsFactors = F)
  newdb<-read.csv(collarhistory, stringsAsFactors = F)



  # transform dates
  sheep.db$CapDate<-as.Date(sheep.db[, capcol], format = "%m/%d/%Y")

  sheep.db$MortDate<-as.Date(sheep.db[, mortcol], format = "%m'%d/%Y")
  sheep.db$MortDate<-ifelse(is.na(sheep.db$MortDate), as.character(Sys.Date()), as.character(sheep.db$MortDate))
  sheep.db$MortDate<-as.Date(sheep.db$MortDate, tryFormats = c('%Y-%m-%d', "%m/%d/%Y"))

  sheep.dat$Date<-as.character(strptime(sheep.dat$acquisitiontime, format = "%Y-%m-%d"))
  sheep.dat$Date<-as.Date(sheep.dat$Date, tryFormats = c('%Y-%m-%d', "%m/%d/%Y"))


  #sheep.dat$Date<-strftime(sheep.dat$TelemDate, format = "%Y-%m-%d")

  sheep.dat<-sheep.dat[, c(1:4, 6:13,41:44,47)]
  sheep.dat$type<-'Iridium'

  if(dnld.data == TRUE){
    files<-list.files(dnld.fold, full.names = T)

    all.dnld<-data.frame()

    for(i in 1:length(files)){
      sub<-read.csv(files[[i]], stringsAsFactors = F, fileEncoding = 'latin1')
      #aid<-sapply(strsplit(as.character(files[[i]]), "\\.csv"), "[", 1)
      #aid<-sapply(strsplit(as.character(aid), "_GPS"), "[", 1)
      #aid<-sapply(strsplit(as.character(aid), "/"), "[", 7)
      #sub$AID<-aid

      sub$TelemDate<-paste(sub$UTC_Date, sub$UTC_Time, sep = " ")

      names<-c('CollarID', 'TelemDate', 'DOP', 'Temp...C.', 'Latitude....', 'Longitude....', 'UTC_Date')
      sub<-sub[,names]

      # fix order of columns

      sheep.dat<-sheep.dat[, c('idcollar', 'acquisitiontime', 'dop', 'temperature', 'latitude', 'longitude', 'Date', 'type')]

      names(sub)<-names(sheep.dat[1:ncol(sheep.dat)-1])


      sub$acquisitiontime<-as.POSIXct(sub$acquisitiontime, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
      sub$acquisitiontime<-format(sub$acquisitiontime, tz = "US/Pacific", usetz = FALSE)
      sub$acquisitiontime<-as.POSIXct(sub$acquisitiontime, format = "%Y-%m-%d %H:%M:%S")

      sub<-sub[with(sub, order(-idcollar, acquisitiontime)),]

      sub$type<-'Download'

      all.dnld<-rbind(sub, all.dnld)



    }

    #sheep.dat$X2D.3D<-rep(all.dnld$X2D.3D, length.out = nrow(sheep.dat))

    all.dnld$Date<-as.Date(all.dnld$Date, format = "%m/%d/%Y")
    sheep.dat$Date<-as.Date(sheep.dat$Date, format = "%Y-%m-%d")
    sheep.dat<-sheep.dat[, names(sheep.dat) %in% names(all.dnld)]

    sheep.dat<-rbind(sheep.dat, all.dnld)
    sheep.dat<-sheep.dat[with(sheep.dat, order(-idcollar, acquisitiontime)),]


    sheep.dat$hr<-strftime(sheep.dat$acquisitiontime, format = "%H")
    sheep.dat$id<-paste(sheep.dat$Date, sheep.dat$hr, sep = "_")


    sheep.dat<-sheep.dat[!duplicated(sheep.dat[, c('idcollar', 'id')]),]




  }

  decision <- readline(prompt = "Have you updated collar history ? y/(n)")

  if(decision == "y"){
  col.hist<-Ovis::collar.history(data = newdb, idcol = "AnimalID", study = "washington", capdate = "CapDate", mortdate = "MortDate")
  }

  if(decision == "n"){stopifnot('You have updated collar history')}

  uni<-unique(col.hist$AnimalID)

  col.hist$Serial1Start <-as.character(as.Date(col.hist$Serial1Start, format = "%Y-%m-%d"))
  col.hist$Serial1End <-as.character(as.Date(col.hist$Serial1End, format = "%Y-%m-%d"))
  col.hist$Serial2Start <-as.character(as.Date(col.hist$Serial2Start, format = "%Y-%m-%d"))
  col.hist$Serial2End <-as.character(as.Date(col.hist$Serial2End, format = "%Y-%m-%d"))
  col.hist$Serial3Start <-as.character(as.Date(col.hist$Serial3Start, format = "%Y-%m-%d"))
  col.hist$Serial3End <-as.character(as.Date(col.hist$Serial3End, format = "%Y-%m-%d"))

  for(k in 1:nrow(col.hist)){
    xx<-col.hist[k, ]
    print(k)

    for(l in 1:3){
      if(l == 1){
        xxx<-xx[, c('AnimalID', 'Serial1', 'Serial1Start', 'Serial1End')]
      }

      if(l == 2){
        xxx<-xx[, c('AnimalID', 'Serial2', 'Serial2Start', 'Serial2End')]
      }

      if(l==3){
        xxx<-xx[, c('AnimalID', 'Serial3', 'Serial3Start', 'Serial3End')]
      }

      if(is.na(xxx[1,2])){next}
      ss<-sheep.dat[sheep.dat$idcollar==xxx[1,2],]
      ss<-ss[(ss$Date > xxx[,3])&ss$Date < (xxx[,4]),]
      ss<-ss[complete.cases(ss$latitude),]

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
  outsp2<-outsp2[complete.cases(outsp2$idcollar),]




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



