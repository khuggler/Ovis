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

# Create a list of dates that we want
dts<- seq(as.Date(startdate), as.Date(enddate), by = "day")

# ftp site of snodas data
url <- "ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/masked/"

# Set a variable that states where we save the downloaded SNODAS files
outpath<- paste(getwd(), "SNODAS/RawSNODAS", sep = "/")


#### Step 2c: Loop through each date and Process SNODAS ####

# Loop through the dates and download the SNODAS data
for(i in 1:length(dts)){
  # Get the year
  yr<- lubridate::year(dts[i])

  # Get the month as character
  mc<- as.character(lubridate::month(dts[i], label = TRUE, abbr = TRUE))

  # Get month as number with a leading 0 if necessary
  mn<- stringr::str_pad(lubridate::month(dts[i]), width = 2, side = "left", pad = "0")

  # Get the day with a leading 0
  d<- stringr::str_pad(lubridate::day(dts[i]), width = 2, side = "left", pad = "0")

  # Now get the .tar file from server, sometimes they are missing, so use a try
  tst<- try(download.file(paste(paste(paste(url, yr, sep = ""), paste(mn, mc, sep = "_"), sep = "/"), paste("/SNODAS_", yr, mn, d, sep = ""), ".tar", sep = ""), paste(outpath, paste("/SNODAS_", yr, mn, d, ".tar", sep = ""), sep = ""), quiet = TRUE, mode = "wb"), silent = TRUE)

  # If download was successful
  if(!inherits(tst, "try-error")){
    # Get out of tar format
    untar(paste(outpath, paste("/SNODAS_", yr, mn, d, ".tar", sep = ""), sep = ""), list = FALSE, exdir = "./SNODAS/gzSNODAS", verbose=FALSE)

    # Get a list of dat files. Should only be two given our query
    sfiles<- data.frame(Files = dir("./SNODAS/gzSNODAS", pattern = ".gz$", full.names = TRUE), stringsAsFactors = FALSE) %>%
      separate(Files, into = c(NA, NA, "Base", NA), sep = "\\.", remove = FALSE) %>%
      dplyr::filter(Base == "dat") # %>%
    # filter(str_detect(Files, pattern = c("1034", "1036")))
    # For some reason, str_detect is failing to always find the pattern when it exists...to create a work-around for this, loop through the two patterns, use grep and fix it this way. Lame but I can't explain it
    fnd<- integer()
    for(j in c("1034", "1036")){
      fnd<- c(fnd, grep(pattern = j, sfiles$Files))
    }

    # Now reduce the rows to match the found string
    sfiles<- sfiles[fnd,]

    # Now unzip the files
    for(j in 1:nrow(sfiles)){
      R.utils::gunzip(sfiles$Files[j])
    }

    # Get list of the unzipped files
    gfile<- dir("./SNODAS/gzSNODAS", pattern = ".dat$")

    # Loop through the list and convert into a cropped raster of the study area
    for(j in 1:length(gfile)){

      # Must read in the data from a binary format. Rescale by dividing by 1000
      r <- readBin(paste("./SNODAS/gzSNODAS", gfile[j], sep = "/"), integer(), n = 6935 * 3351, size = 2, signed = TRUE, endian = "big") / 1000

      # Coerce the vector to a matrix, then to a raster, this will be a 1km cell in DecDeg
      r <- raster::raster(matrix(r, nrow = 3351, ncol = 6935, byrow = TRUE), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", xmn = -124.7337, xmx = -66.9421, ymn = 24.9504, ymx = 52.8754)

      # now crop to our study area
      r<- raster::crop(r, study)

      # Reproject raster to the same CRS as the original points. Change CRS to the original pts
      #r<- projectRaster(r, res = c(1000, 1000), crs = proj4string(study))

      # Replace values less than 1, have to do this after reprojection due to bilinear interpolation
      r[r[] < 0] <- NA


      # Save to the correct directory. Index 1 should be SWE because of alphabetical file listing
      if(j == 1){
        raster::writeRaster(r, paste("./SNODAS/SNODAS_SWE", paste("/SNODAS_", yr, mn, d, "_SWE.tif", sep = ""), sep = ""), format = "GTiff", overwrite = TRUE)
      } else
      {
        raster::writeRaster(r, paste("./SNODAS/SNODAS_Depth", paste("/SNODAS_", yr, mn, d, "_Depth.tif", sep = ""), sep = ""), format = "GTiff", overwrite = TRUE)
      }
    }
    # Remove our mess from the file directories
    # file.remove(paste(outpath, paste("/SNODAS_", yr, mn, d, ".tar", sep = ""), sep = ""))
    file.remove(dir("./SNODAS/gzSNODAS", pattern = ".gz$", full.names = TRUE))
    file.remove(dir("./SNODAS/gzSNODAS", pattern = ".dat$", full.names = TRUE))
  }
  print(dts[[i]])
}

}
