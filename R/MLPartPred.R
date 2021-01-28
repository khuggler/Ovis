#' @title Parturition Prediction with Machine Learning
#
#' @description Use Machine Learning to predict parturition
#' @param jk output of BGBFun
#' @param spp species for model. Choices are "Elk", "FMD" (Female mule deer), or "BHS" (bighorn sheep)
#' @return data.frame of probability predictions for 0, 1, 2 (pre birth, neonate <48hrs old, neonate>48hrs old)
#' @keywords prediction
#' @export
#' @examples
#' \donttest{hg<-MLPartPred(mdat3, spp = "BHS")}
#'
MLPartPred<-function(jk,spp){
  
  akl<-Ovis::MLVarPrep(jk)
  akl<-akl[complete.cases(akl),]
  
  
  if(spp=='Elk'){
    data("ElkRealTimeRF",package='Part')
    
    
    akl$Pred0<-as.numeric(randomForest:::predict.randomForest(rf,akl,type='prob')[,1])
    akl$Pred1<-as.numeric(randomForest:::predict.randomForest(rf,akl,type='prob')[,2])
    akl$Pred2<-as.numeric(randomForest:::predict.randomForest(rf,akl,type='prob')[,3])
  }
  if(spp %in% c('FMD', 'BHS')){
    data("DeerRealTimeRF",package='Part')
    
    
    akl$Pred0<-as.numeric(randomForest:::predict.randomForest(deerRF,akl,type='prob')[,1])
    akl$Pred1<-as.numeric(randomForest:::predict.randomForest(deerRF,akl,type='prob')[,2])
    akl$Pred2<-as.numeric(randomForest:::predict.randomForest(deerRF,akl,type='prob')[,3])
  }
  #rm(rf)
  return(akl)
 
}