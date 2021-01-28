#' @title Prepare variables for Machine Learning Parturition prediction
#
#' @description Use Machine Learning to predict parturition
#' @param jk output of BGBFun
#' @return data.frame of probability predictions for 0, 1, 2 (pre birth, neonate <12hrs old, neonate>12hrs&neonate<24hrs old)
#' @keywords prediction
#' @export
#' @examples
#'
MLVarPrep<-function(jk){
  uni<-unique(jk$IdCol)
  jk<-jk[complete.cases(jk$paraSd),]
  akl<-data.frame()
  for(i in 1:length(uni)){
    sm<-jk[jk$IdCol==uni[i],]
    
    sm<-sm[order(sm$TelemDate),]
    
    sm$rlm6<-NA
    sm$rlm6[6:nrow(sm)]<-zoo::rollmean(sm$dist,align=c('right'),k=6)
    
    sm$psl6<-NA
    sm$psl6[6:nrow(sm)]<-zoo::rollmean(sm$paraSd,align=c('right'),k=6)
    
    sm$osl6<-NA
    sm$osl6[6:nrow(sm)]<-zoo::rollmean(sm$orthSd,align=c('right'),k=6)
    
    sm$rlm12<-NA
    sm$rlm12[12:nrow(sm)]<-zoo::rollmean(sm$dist,align=c('right'),k=12)
    
    sm$psl12<-NA
    sm$psl12[12:nrow(sm)]<-zoo::rollmean(sm$paraSd,align=c('right'),k=12)
    
    sm$osl12<-NA
    sm$osl12[12:nrow(sm)]<-zoo::rollmean(sm$orthSd,align=c('right'),k=12)
    
    sm$rlm18<-NA
    sm$rlm18[18:nrow(sm)]<-zoo::rollmean(sm$dist,align=c('right'),k=18)
    
    sm$psl18<-NA
    sm$psl18[18:nrow(sm)]<-zoo::rollmean(sm$paraSd,align=c('right'),k=18)
    
    sm$osl18<-NA
    sm$osl18[18:nrow(sm)]<-zoo::rollmean(sm$orthSd,align=c('right'),k=18)
    
    #sm$NC<-c(0,cumsum(sm$Used[-1L] != sm$Used[-length(sm$Used)]))
    
    
    
    #smbb<-sm[sm$NC==1,]
    
    # tab<-as.data.frame(table(sm$NC))
    # 
    # tab<-tab[tab$Freq>1,]
    
    akl<-rbind(akl,sm)
  }
  
  
  akl<-akl[complete.cases(akl$psl18),]
  
  
  
  
  uni<-unique(akl$IdCol)
  ald<-data.frame()
  for(i in 1:length(uni)){
    ss<-akl[akl$IdCol==uni[i],]
    ss$FPT50<-adehabitatLT::fpt(adehabitatLT::as.ltraj(ss[,c('Easting','Northing')],ss$TelemDate,ss$IdCol),50,units='hours')[[1]]$r1
    ss$FPT150<-adehabitatLT::fpt(adehabitatLT::as.ltraj(ss[,c('Easting','Northing')],ss$TelemDate,ss$IdCol),150,units='hours')[[1]]$r1
    ss$FPT300<-adehabitatLT::fpt(adehabitatLT::as.ltraj(ss[,c('Easting','Northing')],ss$TelemDate,ss$IdCol),300,units='hours')[[1]]$r1
    
    
    ald<-rbind(ald,ss)
    
  }
  
  
  akl<-ald[complete.cases(ald$FPT300),]
  
  
  return(akl)
  
}