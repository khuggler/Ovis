#' @title sample.transects
#' @description sample random distance and azimuths for vegetation transects
#' @param ngroups number of sheep groups to sample within
#' @param nsamps number of sample distance/azimuth combinations to create for each group
#' @param maxdist maximum distance (in meters) to sample from central group location
#' @return Returns data.frame with sampled distance and azimuths for specified number of groups
#' @keywords sample, transects, veg
#' @export


sample.transects<-function(ngroups, nsamps, maxdist){

  samples<-data.frame(Group = NA, Distance = NA, Azimuth = NA )

  final.sample<-data.frame()
  for(k in 1:ngroups){
    samps<-round(runif(nsamps, 0, maxdist))
    azimuth<-round(runif(nsamps, 0, 359))
    samples<-data.frame(Group = k, Distance = samps, Azimuth = azimuth)

    final.sample<-rbind(samples, final.sample)

  }

  final.sample<-final.sample[order(final.sample$Group),]
  return(final.sample)
}
