#' @title collar.history
#' @description Transform capture data to collar history
#' @param data name of data.frame for capture data
#' @param idcol name of column where animal id is stored
#' @param study name of study area to choose for collar history table ("idaho", or "washington")
#' @param capdate name of column where capture date is stored
#' @param mortdate name of column where mortality date is stored
#' @return Returns a date.frame of history of collars for each animal
#' @keywords collar history, data, capture
#' @export
#' @examples
#'

collar.history<-function(data, idcol, study, capdate, mortdate){


  if(study == "idaho"){
    uni<-unique(data[,idcol])

  colhist<-data.frame(AnimalID = rep(NA, length(uni)), GMU = rep(NA, length(uni)), PMU = rep(NA, length(uni)), Serial1 = rep(NA, length(uni)), Serial2 = rep(NA, length(uni)), Serial1Start = rep(NA, length(uni)), Serial1End = rep(NA, length(uni)), Serial2Start = rep(NA, length(uni)), Serial2End = rep(NA, length(uni)))
  colhist$Serial1Start<-as.Date(colhist$Serial1Start, format = '%Y-%m-%d')
  colhist$Serial1End<-as.Date(colhist$Serial1End, format = '%Y-%m-%d')
  colhist$Serial2Start<-as.Date(colhist$Serial1Start, format = '%Y-%m-%d')
  colhist$Serial2End<-as.Date(colhist$Serial1End, format = '%Y-%m-%d')

  for(k in 1:length(uni)){
    sub<-data[data[,idcol] == uni[k],]
    sub<-sub[complete.cases(sub[,idcol]),]

    sub[,capdate]<-as.Date(sub[,capdate], tryFormats = c('%m/%d/%Y', '%Y-%m-%d'))

    sub[,mortdate]<-ifelse(sub[,mortdate] == "", NA, sub[,mortdate])
    sub[,mortdate]<-as.Date(sub[,mortdate], tryFormats = c('%m/%d/%Y', '%Y-%m-%d'))
    sub[,mortdate]<-ifelse(is.na(sub[,mortdate]), as.character(Sys.Date()), as.character(sub[,mortdate]))



    if(nrow(sub) == 1){
      colhist$AnimalID[k]<-sub[,idcol]
      colhist$GMU[k]<-sub$GMU
      colhist$PMU[k]<-sub$PMU
      colhist$Serial1[k]<-sub$NewCollarSerial
      colhist$Serial1Start[k]<-sub[,capdate]
      colhist$Serial1End[k]<-sub[,mortdate]
    }



    if(nrow(sub)>2){
      colhist$AnimalID[k]<-sub[,idcol][1]
      colhist$GMU[k]<-sub$GMU[1]
      colhist$PMU[k]<-sub$PMU[1]
      colhist$Serial1[k]<-sub$NewCollarSerial[1]
      colhist$Serial1Start[k]<-sub[,capdate][1]
      colhist$Serial1End[k]<-sub[,capdate][2]

      colhist$Serial2[k]<-sub$NewCollarSerial[2]
      colhist$Serial2Start[k]<-colhist$Serial1End[k]
      colhist$Serial2End[k]<-max(sub[,mortdate])

    }
  }
  colhist<-colhist[complete.cases(colhist[,"AnimalID"]),]
  }

  if(study == "washington"){

    uni<-unique(data[,idcol])

    data$Serial1Start <-as.character(as.Date(data$Serial1Start, format = "%m/%d/%Y"))
    data$Serial1End <-as.character(as.Date(data$Serial1End, format = "%m/%d/%Y"))
    data$Serial2Start <-as.character(as.Date(data$Serial2Start, format = "%m/%d/%Y"))
    data$Serial2End <-as.character(as.Date(data$Serial2End, format = "%m/%d/%Y"))
    data$Serial3Start <-as.character(as.Date(data$Serial3Start, format = "%m/%d/%Y"))
    data$Serial3End <-as.character(as.Date(data$Serial3End, format = "%m/%d/%Y"))

    colhist<-data.frame()
   for(i in 1:length(uni)){
     sub<-data[data$AnimalID == uni[i],]

     if(is.na(sub$Serial3) & is.na(sub$Serial2)){
       sub$Serial1End<-ifelse(is.na(sub$Serial1End), as.character(Sys.Date()), sub$Serial1End)

     }
     if(!is.na(sub$Serial1) & !is.na(sub$Serial2)){
       sub$Serial2End<-ifelse(is.na(sub$Serial2End), as.character(Sys.Date()), sub$Serial2End)
     }


     if(!is.na(sub$Serial3)){
       sub$Serial3End<-ifelse(is.na(sub$Serial3End), as.character(Sys.Date()), sub$Serial3End)
     }

    colhist<-rbind(sub, colhist)
   }


    colhist<-colhist[complete.cases(colhist[,"AnimalID"]),]
  }



  return(colhist)
}
