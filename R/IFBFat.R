#' @title IFBFat
#' @description calculate IFBFat for bighorn sheep
#' @param dbpath path to location of capture database or where fat measurements are stored
#' @param rumpfat name of column where rump fat measurement exists
#' @param units character. whether rump fat measurement is in "cm" or "mm"
#' @param bcs name of column where body condition score exists
#' @return Returns original capture database with IFBFat column appended
#' @keywords nutritional condition, IFBFat
#' @export
#' @examples
#' \donttest{fat<-IFBFat(dbpath = 'C:/Desktop/yourdb', rumpfat = 'MaxFat', units = "mm", bcs = 'BCS')}



IFBFat<-function(dbpath, rumpfat, units, bcs){
  db<-read.csv(dbpath, stringsAsFactors = F)

  if(units == "mm"){
    db[, rumpfat]<-db[,rumpfat]/10
  }

  db$IFBFat<-NA

  new.db<-data.frame()
    for(i in 1:nrow(db)){

  if(db[i, rumpfat] > 0){
    db$IFBFat<-13.28*db[,rumpfat] + 7.78
  }

    if(db[i, rumpfat] == 0){
      db$IFBFat<-3.92*db[,bcs] -1.48
    }

  }

  return(db)
}
