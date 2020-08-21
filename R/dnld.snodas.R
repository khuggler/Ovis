#' @title dnld.snodas
#' @description download raw SNODAS data from server, and transform to SWE and Snow Depth rasters
#' @param root.dir path to root directory where subdirectories of .gz files, and SWE, and depth files will be stored
#' @param cropshape path to shapefile to crop SNODAS data to
#' @param startdate character in "YYYY-MM-DD" format of the beginning date to download SNODAS data
#' @param enddate character in "YYYY-MM-DD" format of the beginning date to download SNODAS data
#' @return Returns rasters for each day of SWE and Snow Depth saved in sub directories of root.dir
#' @keywords SNODAS, snowpack, SWE, download
#' @export
#'


dnld.snodas<-function(root.dir, cropshape, startdate, enddate){

setwd(root.dir)

require(tidyr)

  if(!dir.exists("SNODAS")){
  dir.create("SNODAS")
}

if(!dir.exists("./SNODAS/RawSNODAS")){
  dir.create("./SNODAS/RawSNODAS")
}
if(!dir.exists("./SNODAS/gzSNODAS")){
  dir.create("./SNODAS/gzSNODAS")
}

if(!dir.exists("./SNODAS/SNODAS_Depth")){
  dir.create("./SNODAS/SNODAS_Depth")
}

if(!dir.exists("./SNODAS/SNODAS_SWE")){
  dir.create("./SNODAS/SNODAS_SWE")
}


study<-rgdal::readOGR(cropshape)
study<-sp::spTransform(study, sp::CRS("+init=epsg:4326"))


dts<- seq(as.Date(startdate), as.Date(enddate), by = "day")


url <- "ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/"


outpath<- paste(getwd(), "SNODAS/RawSNODAS", sep = "/")



for(i in 1:length(dts)){

  yr<- lubridate::year(dts[i])


  mc<- as.character(lubridate::month(dts[i], label = TRUE, abbr = TRUE))


  mn<- stringr::str_pad(lubridate::month(dts[i]), width = 2, side = "left", pad = "0")


  d<- stringr::str_pad(lubridate::day(dts[i]), width = 2, side = "left", pad = "0")


  tst<- try(download.file(paste(paste(paste(url, yr, sep = ""), paste(mn, mc, sep = "_"), sep = "/"), paste("/SNODAS_", yr, mn, d, sep = ""), ".tar", sep = ""), paste(outpath, paste("/SNODAS_", yr, mn, d, ".tar", sep = ""), sep = ""), quiet = TRUE, mode = "wb"), silent = TRUE)


  if(!inherits(tst, "try-error")){

    untar(paste(outpath, paste("/SNODAS_", yr, mn, d, ".tar", sep = ""), sep = ""), list = FALSE, exdir = "./SNODAS/gzSNODAS", verbose=FALSE)


    sfiles<- data.frame(Files = dir("./SNODAS/gzSNODAS", pattern = ".gz$", full.names = TRUE), stringsAsFactors = FALSE) %>%
      separate(Files, into = c(NA, NA, "Base", NA), sep = "\\.", remove = FALSE) %>%
      dplyr::filter(Base == "dat") # %>%

    fnd<- integer()
    for(j in c("1034", "1036")){
      fnd<- c(fnd, grep(pattern = j, sfiles$Files))
    }


    sfiles<- sfiles[fnd,]


    for(j in 1:nrow(sfiles)){
      R.utils::gunzip(sfiles$Files[j])
    }


    gfile<- dir("./SNODAS/gzSNODAS", pattern = ".dat$")


    for(j in 1:length(gfile)){


      r <- readBin(paste("./SNODAS/gzSNODAS", gfile[j], sep = "/"), integer(), n = 6935 * 3351, size = 2, signed = TRUE, endian = "big") / 1000


      r <- raster::raster(matrix(r, nrow = 3351, ncol = 6935, byrow = TRUE), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", xmn = -124.7337, xmx = -66.9421, ymn = 24.9504, ymx = 52.8754)


      r<- raster::crop(r, study)


      r[r[] < 0] <- NA



      if(j == 1){
        raster::writeRaster(r, paste("./SNODAS/SNODAS_SWE", paste("/SNODAS_", yr, mn, d, "_SWE.tif", sep = ""), sep = ""), format = "GTiff", overwrite = TRUE)
      } else
      {
        raster::writeRaster(r, paste("./SNODAS/SNODAS_Depth", paste("/SNODAS_", yr, mn, d, "_Depth.tif", sep = ""), sep = ""), format = "GTiff", overwrite = TRUE)
      }
    }

    file.remove(dir("./SNODAS/gzSNODAS", pattern = ".gz$", full.names = TRUE))
    file.remove(dir("./SNODAS/gzSNODAS", pattern = ".dat$", full.names = TRUE))
  }
  print(dts[[i]])
}

}
