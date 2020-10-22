#' @title IFBFat
#' @description calculate IFBFat for bighorn sheep
#' @param db dataframe of capture database
#' @param maxfat name of column where max fat measurement exists
#' @param units character. whether rump fat measurement is in "cm" or "mm"
#' @param bcs name of column where body condition score exists
#' @param append Logical. TRUE/FALSE. TRUE if IFBFat should be appended to original dataframe. Default is TRUE.
#' @param summary Logical. TRUE/FALSE. TRUE if a summary should be returned. Summary included mean, SE, and upper and lower 95 percent CIs calculated by categories of choice.
#' @param sumcols vector of column names in database that should be aggregated on. Max categories == 3. Example: calculate summary of IFBFat by Sex and PMU
#' @return Returns original capture database with IFBFat column appended
#' @keywords nutritional condition, IFBFat, summary
#' @export
#' @examples
#' \donttest{fat<-IFBFat(dbpath = 'C:/Desktop/yourdb', rumpfat = 'MaxFat', units = "mm", bcs = 'BCS')}



IFBFat<-function(db, maxfat, units, bcs, append, summary, sumcols){

  if(units == "mm"){
    db[, maxfat]<-db[,maxfat]/10
  }
  db$IFBFat<-NA


  new.db<-data.frame()
    for(i in 1:nrow(db)){

  if(db[i, maxfat] > 0){
    db$IFBFat<-13.28*db[,maxfat] + 7.78
  }

    if(db[i, maxfat] == 0){
      db$IFBFat<-3.92*db[,bcs] -1.48
    }

    }



  if(summary == TRUE){


    st.err <- function(x) {
      sd(x, na.rm = T)/sqrt(length(na.omit(x)))
    }

    if(length(sumcols)==1){
      meanagg<-aggregate(db$IFBFat, by = list(db[,sumcols[1]]), data = db,FUN = mean, na.rm = T)
      se<-aggregate(db$IFBFat, by = list(db[,sumcols[1]]), FUN = st.err)[,2]
      sd<-aggregate(db$IFBFat, by = list(db[,sumcols[1]]), FUN = sd, na.rm = T)[,2]

      agg<-cbind(meanagg, se, sd)

      agg$lower.ci<-agg$x - 1.96*agg$se
      agg$upper.ci<-agg$x + 1.96*agg$se

      names(agg)[1]<-sumcols[1]
    }

    if(length(sumcols)==2){
   meanagg<-aggregate(db$IFBFat, by = list(db[,sumcols[1]], db[,sumcols[2]]), data = db,FUN = mean, na.rm = T)
   se<-aggregate(db$IFBFat, by = list(db[,sumcols[1]], db[,sumcols[2]]), FUN = st.err)[,3]
   sd<-aggregate(db$IFBFat, by = list(db[,sumcols[1]], db[,sumcols[[2]]]), FUN = sd, na.rm = T)[,3]
   agg<-cbind(meanagg, se, sd)

   agg$lower.ci<-agg$x - 1.96*agg$se
   agg$upper.ci<-agg$x + 1.96*agg$

     names(agg)[1:2]<-sumcols[1:2]
    }

    if(length(sumcols)==3){
      meanagg<-aggregate(db$IFBFat, by = list(db[,sumcols[1]], db[,sumcols[2]], db[,sumcols[3]]), data = db,FUN = mean, na.rm = T)
      se<-aggregate(db$IFBFat, by = list(db[,sumcols[1]], db[,sumcols[2]], db[,sumcols[3]]), FUN = st.err)[,4]
      sd<-aggregate(db$IFBFat, by = list(db[,sumcols[1]], db[,sumcols[[2]]], db[,sumcols[[3]]]), FUN = sd, na.rm = T)[,4]

      agg<-cbind(meanagg, se,sd)

      agg$lower.ci<-agg$x - 1.96*agg$se
      agg$upper.ci<-agg$x + 1.96*agg$se

      names(agg)[1:3]<-sumcols[1:3]
    }
  }

  if(append == TRUE & summary == TRUE){
  return(list(db, agg))
  }

  if(append == TRUE & summary == FALSE){
    return(db)
  }

  if(append == FALSE & summary == TRUE){
    return(agg)
  }
}
