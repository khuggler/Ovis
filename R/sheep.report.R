#' @title sheep.report
#' @description generate report with most recent location of desired animal IDs
#' @param keys full path to one or more key files
#' @param sheepdb path to sheep capture database
#' @param tzone desired time zone of gps data: "MST" or "US/Pacific"
#' @param serialcol name of column in capture datebase/lookup table where Serial Number exists
#' @param capcol name of column where capture/start date exists
#' @param dateformat character string of the format that date columns are in
#' @param mortcol name of column where mortality date or end date exists
#' @param extracols vector of extra columns that should be appended to GPS data. Names in vector MUST match names in lookup table
#' @param keep.aid vector of animal IDs to include in report
#' @param out.dir path to folder where kml and html map should be saved
#' @return Returns a kml and html map of most recent location for all desired animals to out.dir
#' @keywords capture, kml, html, map
#' @export


sheep.report<-function(keys, sheepdb, tzone, serialcol, capcol, dateformat, mortcol, extracols, keep.aid, out.dir){

  # get sheep data

  sheep.dat<-Ovis::sheep.gps(keys, sheepdb, tzone, serialcol, capcol, dateformat, mortcol, extracols = 'AID')
  sheep.dat<-sheep.dat[sheep.dat$AID %in% keep.aid, ]


  uni<-unique(sheep.dat$AID)
  most.recent<-data.frame()

  for(i in 1:length(uni)){
    sub<-sheep.dat[sheep.dat$AID == uni[i],]
    sub<-sub[nrow(sub),]

    most.recent<-rbind(sub, most.recent)
  }

  sp::coordinates(most.recent)<-c('Long', 'Lat')
  sp::proj4string(most.recent)<-'+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'


  # create mapview
  x<-mapview::mapview(most.recent, zcol = "AID", map.types = 'Esri.WorldImagery')
  mapview::mapshot(x, url = paste0(out.dir, "map.html"))

  #save kml
  most.recent$name<-most.recent$AID
  rgdal::writeOGR(most.recent['name'], paste0(out.dir, "LatestLocs.kml"),  driver = 'KML', layer = 'sheep.locs', overwrite = T)

}
