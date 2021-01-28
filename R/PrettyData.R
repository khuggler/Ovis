#' @title PrettyData
#
#' @description Create tables of most recent locations to include in report
#' @param ti data.frame output from adehabitatLT as.ltraj function. Also the output from trajfun
#' @param data data.frame used to make ti
#' @keywords bind
#' @export

PrettyData<-function(dat,idl,filen){
  
  dat$IdCol<-as.character(dat$IdCol)
  
  idl$IdCol<-as.character(idl$IdCol)
  
  dat<-merge(dat,idl,by.x='IdCol',by.y='IdCol')
  
  uni<-unique(dat$IdCol)
  
  #dat<-as.data.frame(dat)
  
  outs<-data.frame()
  for(i in 1:length(uni)){
    s<-dat[which(dat$IdCol==uni[i]),]
    
    s<-s[order(s$TelemDate,decreasing=T),]
    
    s<-s[c(1:12),]
    
  nms<-names(idl)  
  nms<-nms[!nms %in% c('TelemDate', 'Easting', 'Northing')]
  
  
  s<-s[,c(nms, 'Easting.x','Northing.x')]
  names(s)[names(s) %in% c("Easting.x", 'Northing.x')] <-c('Easting', 'Northing')
   
    
  
    outs<-rbind(outs,s)
    
  }
  
  outs$Easting<-floor(outs$Easting)
  outs$Northing<-floor(outs$Northing)
  
  outs$MatchFreq <- gsub('\\.','',as.character(outs$`Frequency`))
  
  
  saveRDS(outs,file=filen)
  
  
}
