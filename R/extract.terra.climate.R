#' @title extract.terra.climate
#' @description extract locations of composition plots to terraclimate data
#' @param root.dir path of root directory where TerraClimate data are stored
#' @param cropshape path to shapefile to crop TerraClimate data
#' @param months numeric vector of months needed from TerraClimate data ex) c(6,7,8,9)
#' @param comp.data path to composition database
#' @param sub.dir directory name of where intermediate .RDS files should be saved. Should be directory that is different from root.dir
#' @param years vectory of years YYYY for extraction
#' @param outdir path to directory of where extracted data should be saved
#' @return Returns a csv of extracted data for all TerraClimater layers
#' @keywords terraclimate, climate, extract, composition, biomass
#' @export

extract.terra.climate<-function(root.dir, cropshape, months, comp.data, sub.dir, years, out.dir){


  tc<-Ovis::terra.climate(root.dir, cropshape, months)

  BHS_Full <- read.csv(comp.data, stringsAsFactors = FALSE)

  #Create an ID column that is the same as what's created in the raster
  BHS_Full$MONTH <- ifelse(BHS_Full$MN == 5,"May", ifelse(BHS_Full$MN == 6,"June",
                                                          ifelse(BHS_Full$MN == 7,"July", ifelse(BHS_Full$MN == 8,"August",
                                                                                                 ifelse(BHS_Full$MN == 9,"Sept", NA)))))
  BHS_Full$MNYEAR <-paste(BHS_Full$MONTH, BHS_Full$YEAR, sep = "_")

  #' make locations spatial
  sp::coordinates(BHS_Full) <- c("MID_LONG", "MID_LAT")
  sp::proj4string(BHS_Full) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  BHS_Full<-sp::spTransform(BHS_Full, sp::proj4string(tc[[1]]))



  for (i in 1:length(tc)) {
    sub <- tc[[i]]
    rasName <- names(sub)
    year <- strsplit(rasName,"_")[[1]][3]

    alldata <- data.frame()
    for (k in 1:raster::nlayers(sub)) {
      sub2 <- sub[[k]]
      rasName2 <- names(sub2)
      month <- strsplit(rasName2,"_")[[1]][2]
      monthyear <- paste(month,year, sep = "_")

      subplot <- BHS_Full[BHS_Full$MNYEAR == monthyear,]

      if (nrow(subplot) == 0){next}

      subplot <- sp::spTransform(subplot,sp::proj4string(sub2))

      varname <- strsplit(rasName2,"_")[[1]][1]
      subplot@data[,varname] <- raster::extract(sub2, subplot)
      df <- data.frame(subplot)
      alldata <- rbind(alldata,df)



      saveRDS(alldata, paste0(sub.dir,varname,"_",year, ".RDS"))

    }

  }


  #Now, read back in RDS files, and assign to new data.frame with correct data
  #

  #make vector of years that you are interested in

  final.list<-list()
  for(i in 1:length(years)){
    files<-list.files(sub.dir,
                      pattern = paste0(years[i], ".RDS"), full.names = T) #' this path needs to be where you saved the .RDS file from the previous loop
    files2<-list.files(sub.dir,
                       pattern = paste0(years[i], ".RDS"), full.names = F)

    df<-list()
    for(k in 1:length(files)){

      dat<-readRDS(files[k])
      x<-names(dat)
      x<-x[35]
      dat2<-data.frame(dat[,35])
      names(dat2)<-x


      df[k]<-dat2

    }

    name<-unlist(strsplit(files2, ".RDS", fixed = TRUE))
    new.name<-strsplit(name, "_")
    vars<-unlist(new.name)[2*(1:length(name))-1]

    all.df<-data.frame(df)
    names(all.df)<-vars
    bindcols<-dat[, c(1:34, 36,37)]

    final<-cbind(bindcols, all.df)

    final.list[[i]]<-final

  }

  all<-data.table::rbindlist(final.list, fill = T)

  #Save final.list to a csv
  write.csv(all, outdir, row.names = FALSE)


}
