#' @title neonate.survival
#' @description Convert capture data of neonates into the proper format for conducting survival analysis. This function will return 2 columns of data: survival time of each in
#' whatever units desired (days, weeks, months, etc.) and an indicator column of whether the event (i.e., mortality) occurred in binary format. Further, this function allows the option to
#' use a cutoff age for modeling survival. For instance, in many cases survival of neonatal ungulates is modeled to 140 days.
#' @param dbpath path to neonate database
#' @param startdate name of column where birth date or capture date is stored
#' @param enddate name of column where mortality date is stored
#' @param units Units desired for modeling survival (e.g. days, weeks, etc)
#' @param cuts Logical. TRUE/FALSE. Whether you desire to use a cutoff date for survival
#' @param cutoff days from birth to use as cutoff (e.g. 140 days, indicate 140)
#' @param fatecol name of column where fate/cause of death is stored
#' @param censors Vector of causes in your data.frame in which events should be censored (i.e. CollarFailure)
#' @param model Logical. TRUE/FALSE. Whether model of survival (stratified by year) should be done
#' @return Returns a list that includes original dataframe with time alive, and indicator of event as well as the model of survival (if model == TRUE)
#' @keywords neonate, survival, kaplan meier
#' @export
#' @examples
#' \donttest{neo.survival<-neonate.survival(dbpath = yourpath, units = "days", cuts = TRUE, cutoff = 140, censors = "collar_failure", model = TRUE)}
#'
neonate.survival<-function(dbpath, startdate, enddate, units, cuts, cutoff, fatecol, censors, model){
  
  neo<-read.csv(dbpath, stringsAsFactors = F)

  neo[, startdate]<-as.Date(neo[,startdate], tryFormats = c('%m/%d/%Y', '%Y-%m-%d'))
  neo[, enddate]<-ifelse(neo[, enddate] == "", NA, neo[, enddate])
  
  neo$mort<-ifelse(is.na(neo[, enddate]), 0, 1)
  
  neo[, enddate]<-as.Date(neo[, enddate], tryFormats = c("%m/%d/%Y", '%Y-%m-%d'))
  neo[, enddate]<-ifelse(is.na(neo[, enddate]), as.character(Sys.Date()), as.character(neo[, enddate]))
  neo[, enddate]<-as.Date(neo[, enddate], format = "%Y-%m-%d")
 

  neo$difftime<-as.numeric(difftime(neo[, enddate], neo[, startdate], units = units))
  neo$Year<-strftime(neo[, startdate], format = "%Y")
  
  
  
  if(cuts == TRUE){
    uni<-unique(neo$Year)
    x<-data.frame()
    for(l in 1:length(uni)){
      sub<-neo[neo$Year == uni[l],]
      sub$Cut<-ifelse(sub$difftime > cutoff, 0, 1)

      x<-rbind(sub, x)
    }
    x$Event<-ifelse(x[, fatecol] %in% censors, 0, 1)
    x$Indicator<-ifelse(x$Cut == 1 & x$Event == 1, 1, 0)
    
    neo<-x
  
  }

  if(cuts == FALSE){
    
    neo$Event<-ifelse(neo[, fatecol] %in% censors, 0, 1)
    neo$Indicator<-ifelse(neo$mort == 1 & neo$Event == 1, 1, 0)

  }
  
  if(model == TRUE){
    fit<-Surv(neo$difftime, neo$Indicator, type = "right")
    fit<-survfit(formula = fit ~ Year , data = neo)
    
    plot(fit, col = 'black', xlim = c(0, cutoff),lwd = 2, bty = "l", main = "Daily Survival of Bighorn Sheep Lambs", 
         xlab = "Days From Birth", ylab = "Probability of Survival", conf.int = F)
  }
  
  if(model == FALSE){
    fit<-NA
  }
  
  return(list(neo, fit))
}
