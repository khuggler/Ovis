#' @title terra.climate
#' @description Read in netCDF files and manipulate to fit study area
#' @param root.dir path of root directory where spatial data is located
#' @param cropshape path to shapefile to crop rasters to
#' @param months vector of months to subset rasters to. Must be numeric (e.g. c(4,5,6,7,8))
#' @return Returns a list of raster bricks with all climate data
#' @keywords climate, pdsi, temperature, swe
#' @export
#' @examples
#'


terra.climate<-function(root.dir, cropshape, months){

monthlist<-c('Jan', 'Feb', 'March', 'April', 'May', 'June', 'July', 'August', 'Sept', 'Oct', 'Nov', 'Dec')

  new.stack<-list()
  files<-list.files(root.dir, pattern = ".nc", full.names = T)

  study<-rgdal::readOGR(cropshape)

  for(i in 1:length(files)){

    name<-unlist(strsplit(files[i], "_")[[1]])
    x<-length(name)

    yr<-name[x]
    name<-name[x-1]

    yr<-unlist(strsplit(yr, ".nc")[[1]])[1]


    temp<-raster::stack(files[i])
    temp<-temp[[months]]

    monthnames<-monthlist[months]

    names(temp)<-paste0(name, "_", monthnames,yr)

    study<-sp::spTransform(study, sp::proj4string(temp))

    temp<-raster::crop(temp, study)

    new.stack[[i]]<-temp


}
 return(new.stack)
}
