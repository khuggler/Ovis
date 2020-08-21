#' @title create.bbox
#' @description create a SpatialPolygonsDataFrame from the bounding box coordinates of a shapefile
#' @param inshape input shapefile to create bounding box around
#' @param id character of id for bounding box
#' @param buffer Logical. If TRUE (default), create new shapefile with a specified buffer around it
#' @param buffsize numeric vector of number of kilometers to buffer around bounding box
#' @param native  Logical. If TRUE (default), return shapefile in the native projection, if FALSE, return shapefile in the Albers Equal Area (used to create buffer)
#' @return Returns a SpatialPolygonsDataFrame with bounding box with buffer if buffer = TRUE
#' @keywords bounding box, spdf, shapefile
#' @export

create.bbox<-function(inshape, id, buffer = TRUE, buffsize = 1, native = TRUE){

  bb<-inshape@bbox

coords = matrix(c(bb[1,1], bb[2,1],
                  bb[1,2], bb[2,1],
                  bb[1,2],bb[2,2],
                  bb[1,1], bb[2,2],
                  bb[1,1], bb[2,1]), ncol = 2, byrow = T)
p<-Polygon(coords)
proj<-proj4string(inshape)
p.poly<-SpatialPolygons(list(Polygons(list(p), ID = id)), proj4string=CRS(proj))

p.poly.trans<-sp::spTransform(p.poly, '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')

if(buffer == TRUE){
  p.poly<-sp::spTransform(p.poly, '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  new.poly<-rgeos::gBuffer(p.poly, width = buffsize*1000, capStyle = 'SQUARE', joinStyle = 'MITRE', mitreLimit = 10)

  new.poly.trans<-sp::spTransform(new.poly, sp::proj4string(inshape))
}

if(buffer == TRUE & native == TRUE){
  return(new.poly.trans)
}

if(buffer == TRUE & native == FALSE){
  return(new.poly)
}

if(buffer == FALSE & native == TRUE){
  return(p.poly)
}

if(buffer == TRUE & native == FALSE){
  return(p.poly.trans)
}


}



