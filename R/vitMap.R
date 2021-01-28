#' @title Create Plots for Parturition Markdown
#
#' @description Use movement data to create plots for Parturition Markdown Document
#' @param locdat movement data
#' @param vhist Lookup table 
#' @param labels vector of labels to include in plots. Options include: "Frequency", TagNumber", and "VitFreq". If these labels are desired in plots, they MUST be included in your lookup table
#' @param fold Folder to save plots in
#' @param plotdataPath path ('C:/Users/mhayes1/Desktop/PlotData') to save plot data
#' @return Returns plots for markdown in specified folder
#' @keywords VIT, parturition, markdown
#' @export
#' @examples
#' \donttest{system.time({ vitMap(locdat=mdat2,vidat=vi,vhist=vhist,fold='/home/mhayes1/Desktop/DEERPTesting/plots',spp='deer') })
#' )}
#'

vitMap<-function(locdat,vhist,labels,fold,plotdataPath,hg=NULL){
 
    locdat$IdCol <- as.character(locdat$IdCol)
   
    vhist$IdCol <- as.character(vhist$IdCol)
    
    if(TRUE %in% (nchar(vhist$BirthDate)>0)){
      vhist$BirthDate<-as.POSIXct(paste0(vhist$BirthDate,' 5'),'%m/%d/%Y %H',tz='MST')
    }
    
    
    uni <- unique(locdat$IdCol)
    sub <- locdat
    sub <- sub[with(sub, order(sub[, "TelemDate"])), ]
    newdata <- adehabitatLT::as.ltraj(sub[, c("Easting", "Northing")], 
                                      sub[, "TelemDate"], id = sub[, "IdCol"])
    fpt <- adehabitatLT::fpt(newdata, c(50, 100, 150, 200), "hours")
    out <- data.frame()
    for (k in 1:length(uni)) {
      sloc <- locdat[which(locdat$IdCol == 
                             uni[k]), ]
      fptsub <- fpt[[k]]
      sloc$FPT50 <- fptsub$r1
      sloc$FPT100 <- fptsub$r2
      sloc$FPT150 <- fptsub$r3
      sloc$FPT200 <- fptsub$r4
      out <- rbind(out, sloc)
    }
    locdat <- out
    
    tim<-paste(strftime(Sys.time(),format='%Y'),'-', subsetmonth, '-01',sep='')
  



    uni <- unique(locdat$IdCol)
    uni <- uni[which(uni %in% as.character(vhist$IdCol))]
    
    allcks <- data.frame()
    for (l in 1:length(uni)) {
      sub <- locdat[which(locdat$IdCol == 
                            uni[l]), ]
      
      sub<-sub[order(sub$TelemDate),]
      if(nrow(sub)<5){next}
      #subvidat <- vidat[which(vidat$CollarSerialNumber == 
       #                         uni[l]), ]
      vhsub <- vhist[which(vhist$IdCol == uni[l]), 
                     ]
      #subvidat <- subvidat[order(subvidat$Date, decreasing = T), 
       #                    ]
      #tim<-paste(strftime(Sys.time(),format='%Y'),'-03-01 00:00:00',sep='')
      #subvidat<-subvidat[which(subvidat$Date>=as.POSIXct(tim,format='%Y-%m-%d %H:%M:%S')),]
      
      sub$FPT50 <- ifelse(is.na(sub$FPT50),0, sub$FPT50)
      sub$FPT100 <- ifelse(is.na(sub$FPT100), 
                           0, sub$FPT100)
      sub$FPT150 <- ifelse(is.na(sub$FPT150), 
                           0, sub$FPT150)
      sub$FPT200 <- ifelse(is.na(sub$FPT200), 
                           0, sub$FPT200)
      
      #' calculate movement rate
      sub$MR <- sub$dist/sub$dt
      
      #' bring in frequency for report
      fn <- vhsub$Frequency[1]
      fn <- gsub(".", "", fn, fixed = T)
      if(nchar(fn)<6){
        fn<-paste0(fn,paste0(rep('0',6-nchar(fn)),collapse=''))
      }
      fn <- paste(fold, fn, sep = "/")
      fn <- paste(fn, "png", sep = ".")
      png(filename = fn, height = 1400, width = 1500, res = 75)
      par(mfrow = c(3, 2))
      
    
      
      # MOVEMENT RATE PLOTS

      plot(sub$TelemDate, sub$MR, type = "l", ylab = "Movement Rate", 
           xlab = "Date", main = "Movement Rate", cex = 1.25)
      
      #abline(v=vhsub$ActBD[1],col='green',lty=2)
      sub$MRM <- NA
      
      #' rolling mean 
      sub$MRM[12:nrow(sub)] <- zoo::rollmean(sub$MR, k = 12)
      
      #' 2-day quantile
      if(nrow(sub)>48){
        mm <- quantile(sub$MR[(nrow(sub) - 48):(nrow(sub))], 
                       probs = 0.75)
        
      }else{ #'total quantile
        mm <- quantile(sub$MR, 
                       probs = 0.75)
      }
      mc <- as.data.frame(table(sub$MR[(nrow(sub) - 12):(nrow(sub))] < 
                                  mm))
      mc <- mc[which(mc$Var1 == "TRUE"), ]
      mc <- ifelse(nrow(mc) == 0, 0, mc$Freq[1])
      
      lines(sub$TelemDate, sub$MRM, col = "red") # rolling quantile
      abline(h = mm, col = "blue", lty = 2)
      
      #' GAUSSIAN BRIDGE METRICS
    
      plot(sub$TelemDate, sub$orthSd, type = "l", ylim = c(0, 
                                                           50), ylab = "Metric", xlab = "Date", main = "dBGB Metrics", 
           cex = 1.25)
    
      lines(sub$TelemDate, sub$paraSd, col = "blue")
      
    
      
      # FPT METRICS
      
      plot(sub$TelemDate, sub$FPT50, type = "l", ylab = "FPT (hours)", 
           xlab = "Date", main = "FPT 50m radius", cex = 1.25)
      
    
      abline(h = quantile(sub$FPT50, na.rm = T)[4], col = "red")
      #abline(v=vhsub$ActBD[1],col='green',lty=2)
      
      
      plot(sub$TelemDate, sub$FPT100, type = "l", ylab = "FPT (hours)", 
           xlab = "Date", main = "FPT 100m radius", cex = 1.25)
   
      
      abline(h = quantile(sub$FPT100, na.rm = T)[4], col = "red")
     
     plot(sub$TelemDate, sub$FPT150, type = "l", ylab = "FPT (hours)", 
       xlab = "Date", main = "FPT 150m radius", cex = 1.25)
       abline(h = quantile(sub$FPT150, na.rm = T)[4], col = "red")
      
       
       
       
       
       
      predsub<-hg[hg$IdCol==uni[l],]
      
      
      tim<-paste(strftime(Sys.time(),format='%Y'),'-' ,subsetmonth, '-01 00:00:00',sep='')
      predsub<-predsub[which(predsub$TelemDate>=as.POSIXct(tim,format='%Y-%m-%d %H:%M:%S')),]
      #predsub<-predsub[predsub>-]
      
      if(nrow(predsub)>0){
        plot(predsub$TelemDate, predsub$Pred0, type = "l", ylab = "Probability", 
             xlab = "Date", main = "ML Predictions", cex = 1.25,ylim=c(0,1),lwd=1)
        lines(predsub$TelemDate,predsub$Pred2,col='red',lwd=1)
        lines(predsub$TelemDate,predsub$Pred1,col='blue',lwd=2)
        #abline(v=vhsub$ActBD[1],col='green',lty=2)
      }
      
      
      
      if('Frequency' %in% labels){
        mf <- paste(paste("Mom Freq: ", vhsub$Frequency[1], 
                          sep = " "),paste('AID: ',vhsub$IdCol[1],sep=''),sep=' ')
      }else{
        mf <-paste('AID: ',vhsub$IdCol[1],sep='')
      }
      
      mtext(mf, font = 2, side = 3, line = -2.25, outer = T, 
            cex = 2)
      

      if('TagNumber' %in% labels){
    
     tn<- paste("Tag Number:", vhsub$TagNumber[1], sep = " ")
     mtext(tn, font = 2, side = 3, line = -50, outer = T, 
           cex = 2)
      }
        
      if('VitFreq' %in% labels){
        vf <- paste("VIT Freq:", vhsub$VitFreq[1], sep = " ")
        mtext(vf, font = 2, side = 3, line = -25, outer = T, 
              cex = 2)
        
      }
        
      

      
      dev.off()
      
      cks <- data.frame(Pred0Check=predsub$Pred0[nrow(predsub)],
                        Pred1Check=predsub$Pred1[nrow(predsub)],
                        Pred2Check=predsub$Pred2[nrow(predsub)],
                        stringsAsFactors = F)
      if(nrow(cks)==0){next}
      cks$ID <- sub$IdCol[1]
      allcks <- rbind(allcks, cks)
    }
    #allcks$RMean <- rowMeans(allcks[, 1:4])
    
    ppp<-paste0(plotdataPath,'_Plots.RDS')
    saveRDS(allcks, ppp)
  }
  
 

