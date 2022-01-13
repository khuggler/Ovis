#' @title sheep.activity
#' @description Merge Animal IDs to activity data
#' @param activity.fold path to activity data
#' @param collarhistory path to collar history file
#' @return Returns a data.frame with all activity data and appended animal ID
#' @keywords capture, animal ID, activity, append
#' @export
#' @examples
#'

sheep.activity<-function(activity.fold, collarhistory){
  files<-list.files(activity.fold, pattern = '.csv$', full.names = T)

  act<-lapply(files, read.csv)
  act<-data.table::rbindlist(act)

  act$dt<-as.POSIXct(paste0(act$UTC_Date, " ", act$UTC_Time), format = "%m/%d/%Y %H:%M:%S %p")
  act$date<-as.Date(act$UTC_Date, format = "%m/%d/%Y")




  col.hist<-read.csv(collarhistory)

  col.hist<-Ovis::collar.history(data = col.hist, idcol = "AnimalID", study = "washington", capdate = "CapDate", mortdate = "MortDate")



  uni<-unique(col.hist$AnimalID)

  col.hist$Serial1Start <-as.character(as.Date(col.hist$Serial1Start, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")))
  col.hist$Serial1End <-as.character(as.Date(col.hist$Serial1End, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")))
  col.hist$Serial2Start <-as.character(as.Date(col.hist$Serial2Start, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")))
  col.hist$Serial2End <-as.character(as.Date(col.hist$Serial2End, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")))
  col.hist$Serial3Start <-as.character(as.Date(col.hist$Serial3Start, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")))
  col.hist$Serial3End <-as.character(as.Date(col.hist$Serial3End, tryFormats = c("%Y-%m-%d", "%m/%d/%Y")))



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
      ss<-act[act$CollarID==xxx[1,2],]
      ss<-ss[(ss$date > xxx[,3])&ss$date < (xxx[,4]),]
      #ss<-ss[complete.cases(ss$latitude),]

      if(nrow(ss)==0){next}

      if(nrow(ss)>0){
        ss$AID<-xxx[1,1]
      }

      if(l == 1){
        ald<-ss
      }
      if(l>1){
        ald<-rbind.fill(ald,ss)
      }

    }
    if(k == 1){
      outsp<-ald
    }
    if(k>1){
      outsp<-rbind.fill(outsp,ald)
    }
  }


  return(outsp)

}
