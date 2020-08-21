#' @title merge.disease
#' @description merge lab results with capture data. This will likely only be used for Idaho and Asotin data (Wyoming is already merged)
#' @param disease path to disease excel workbook
#' @param sheets sheets within disease data.frame to be merged with capture data
#' @param cap path to capture database
#' @param aidcol name of column where animal ID exists in capture data.frame
#' @param merge Logical. If TRUE (default), function will merge desired component of lab results to capture data
#' @param mergecol name of sheet for desired merge
#' @return Returns a list (if merge = TRUE) with list of sheets with lab results and merged data.frame of selected lab results to capture data. If merge = FALSE, a list of sheets for all lab results #' will be returned
#' @keywords disease, lab results, test, movi, pcr, elisa
#' @export

merge.disease<-function(disease, sheets, cap, aidcol, merge, mergecol){
  dis.list<-list()
  nam.list<-list()

  for(k in 1:length(sheets)){
    dis.list[[k]]<-data.frame(readxl::read_excel(disease, sheets[[k]]))
    name<-sheets[[k]]

    nam.list[[k]]<-names(dis.list[[k]])
    names(dis.list)[k]<-name
  }

  if(merge == TRUE){

   sub<-data.frame(dis.list[mergecol])
    names(sub)<-nam.list[[k]]

    cap.dat<-read.csv(cap, stringsAsFactors = F)

    if(mergecol %in% c('movi PCR','Parasite','Preg', 'movi ELISA','Bovis','pasteurella', 'movi culture')){
      sub.cap<-merge(cap.dat, sub, by.x = aidcol, by.y = 'EARTAG', all.x = TRUE)
      col.index<-grep('RESULT', colnames(sub.cap))
      names(sub.cap)[col.index]<-name
    }

    if(merge.col %in% c('minerals','sero')){
      sub.cap<-merge(cap.dat, sub, by.x = aidcol, by.y = 'EARTAG', all.x = TRUE)
    }


    return(list(sub.cap, dis.list))
  }

  if(merge == FALSE){
    return(dis.list)
  }

}






