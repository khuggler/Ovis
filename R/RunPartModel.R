#' @title RunPartModel
#
#' @description Calculate movement statistics including movement rate, first passage time, bivariate gaussian bridge, and machine learning predictions to identify parturition events of ungulates.
#' @param gpsdat data.frame of gps data to run model that includes the following columns (in this order): 'IdCol', 'SerialNumber', 'TelemDate', 'x', 'y'. Optional columns include: 'dop', 'X2D.3D', but recommended if available to help improve data cleaning process. IdCol = column that represents unique identifier, TelemDate = as.POSIXct format. Fix time and date, x = x coordinate, y = y coordinate. See data(sheep.gps) for example of expected format.
#' @param lookup data.frame of lookup table. This table should include the following columns: 'Frequency', 'Serial', 'IdCol'. Frequency = collar frequency in XXX.XXX format, Serial = Serial number of collar, IdCol = Unique identifier column. MUST match the IdCol in the gpsdat, BirthDate (optional), used to fill in birth dates as they occur and add to plots. Other columns such as VitFrequency, TagNumber are allowed. See data(lookup) for example of expected format.
#' @param gpsproj proj4 of gps data
#' @param projectedproj proj4 of projected coordinate system to use for data cleaning (UTM or Albers Equal Area recommended)
#' @param subsetmonth character object indicating which month to use as the start date for analysis. For example, for a start date of March 1 of current year, use "03"
#' @param tempdir path of directory for products to be saved
#' @param ncpus number of CPUs for parallel processing. Recommend 1-2 less than max.
#' @param markdownpath complete path to location of markdown file for parturition report
#' @param from character vector of email sender
#' @param to character vector of email recipients
#' @return Resulting object is a pdf with movement metrics for each unique animal, and a data.frame with machine learning predictions
#' @keywords parturition, movement rate, first passage time, gaussian bridge, machine learning
#' @export
#' @examples
#' \donttest{RunPartModel(data=gps, lookup = lookup, gpsproj = +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", projectedproj = +proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs", subsetmonth = "01", tempdir = "C:/Users/khuggler/Desktop/", ncpus = 5, markdownpath = "C:/Users/Desktop/PartPlots2.Rmd")}

RunPartModel<-function(gpsdat, lookup, gpsproj, projectedproj, subsetmonth,tempdir,
                   ncpus, markdownpath, from=NA,to=NA,subject=NA,SP=NA,
                   progpath=NA,username=NA,password=NA,email='no',ST=TRUE,
                   STUser=NULL,STPass=NULL,systype='Win'){

  # create temporrary directories for products to be stored
  options(warn=-1)
  if(dir.exists(tempdir) == TRUE){
    llist<-list.files(tempdir,all.files=T,full.names=T)
    for(i in 3:length(llist)){
      file.remove(llist[i])
    }
    CleanRep<-paste0(tempdir,'CleaningReport.txt')
    datastore<-paste0(tempdir,'datastore.RDS')
    PrettyDataStore<-paste0(tempdir,'PrettyData.RDS')
    plotfolder<-paste0(tempdir,'plots')
    dir.create(plotfolder)
    pdffolder<-paste0(tempdir,'PDFs')
    dir.create(pdffolder)
    plotdatapath<-paste0(tempdir,'PlotData')
    #dir.create(plotdatapath)

  }
  if(dir.exists(tempdir) == FALSE){
    dir.create(tempdir)
    CleanRep<-paste0(tempdir,'CleaningReport.txt')
    datastore<-paste0(tempdir,'datastore.RDS')
    PrettyDataStore<-paste0(tempdir,'PrettyData.RDS')
    plotfolder<-paste0(tempdir,'plots')
    dir.create(plotfolder)
    pdffolder<-paste0(tempdir,'PDFs')
    dir.create(pdffolder)
    plotdatapath<-paste0(tempdir,"plots/",'PlotData')
    #dir.create(plotdatapath)
  }
  options(warn=0)


  llist<-list.files(plotfolder,full.names=T)
  if(length(llist)>0){
    for(i in 1:length(llist)){
      file.remove(llist[i])
    }
  }
  llist<-list.files(pdffolder,full.names=T)
  if(length(llist)>0){
    for(i in 1:length(llist)){
      file.remove(llist[i])
    }
  }

  # calculate run time
  begtime<-Sys.time()

  # bring in gps data

  dat2<-gpsdat
  names(dat2)[1:5]<-c('IdCol', 'CollarSerialNumber', 'TelemDate', 'x', 'y')

  assertthat::assert_that(class(dat2$TelemDate)[1] == "POSIXct", msg = "TelemDate column must be in POSIXct format")

  # bring in lookup table
  vhist<-lookup
  vhist<-vhist[complete.cases(vhist$Serial),]


  assertthat::assert_that('Frequency' %in% unique(names(vhist)), msg = "Lookup data must include Frequency")
  assertthat::assert_that('Serial' %in% unique(names(vhist)), msg = "Lookup data must include Serial")
  assertthat::assert_that('IdCol' %in% unique(names(vhist)), msg = "Lookup data must include IdCol")







  dat2<-dat2[dat2$IdCol %in% vhist$IdCol,]

  uni<-unique(dat2$IdCol)

  lastpoint<-data.frame()
  for(i in 1:length(uni)){
    sub<-dat2[dat2$IdCol==uni[i],]
    sub<-sub[order(sub$TelemDate,decreasing = T),]
    sub<-as.data.frame(sub)

    lastpoint<-rbind(lastpoint,sub[1,])
  }
    lastpoint$IdCol<-as.character(lastpoint$IdCol)



  vhist$IdCol<-as.character(vhist$IdCol)


  lastpoint<-merge(lastpoint,vhist,by.x='IdCol',by.y='IdCol',all.x=T)
  lastpoint<-lastpoint[complete.cases(lastpoint$Frequency),]

  sp::coordinates(lastpoint)<-~x+y
  sp::proj4string(lastpoint)<-gpsproj

  names(lastpoint)[names(lastpoint) == 'IdCol']<-'name'
  rgdal::writeOGR(lastpoint["name"], paste(tempdir,'LatestLocs.kml',sep=''), layer = 'BHS', driver = "KML",
                  overwrite = T)

  #lastpoint<-sp::spTransform(lastpoint,'+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
  lastpoint<-lastpoint[,names(lastpoint) == 'name']
  #names(lastpoint)[1]<-'Frequency'
  rgdal::writeOGR(lastpoint,paste(tempdir,'LatestLocs.gpx',sep=''),layer='locs',driver='GPX',overwrite_layer=T)

  #=============================================================
  #create map with last 12 hours and points connecting lines
  #=============================================================

  lasttwelve<-data.frame()
  for(i in 1:length(uni)){
    sub<-dat2[dat2$IdCol==uni[i],]
    sub<-sub[order(sub$TelemDate,decreasing = T),]
    sub<-as.data.frame(sub[1:12,])
    sub$cat<-NA
    sub$cat[1]<-'end'
    sub$cat[2:11]<-'mid'
    sub$cat[12]<-'start'
    lasttwelve<-rbind(lasttwelve,sub)
  }

  lasttwelve$IdCol<-as.character(lasttwelve$IdCol)

  vhist$IdCol<-as.character(vhist$IdCol)


  lasttwelve<-merge(lasttwelve,vhist,by.x='IdCol',by.y='IdCol',all.x=T)
  lasttwelve<-lasttwelve[complete.cases(lasttwelve$Frequency),]

  sp::coordinates(lasttwelve)<-~x+y
  sp::proj4string(lasttwelve)<-gpsproj

  rgdal::writeOGR(lasttwelve, paste(tempdir,'Last12pts.shp',sep=''), layer = 'BHS', driver = "ESRI Shapefile",
                  overwrite = T)




  ids=unique(lasttwelve$IdCol)
  trajectory <- list()

  for (i in ids){
    spdf4bird<-subset(lasttwelve, IdCol==i)
    birdtrajectory<-sp::SpatialLines(list(sp::Lines(list(sp::Line(spdf4bird)), "id")))
    trajectory[[i]]<-sp::Lines(list(sp::Line(spdf4bird)), ID=paste(i))
    #trajectory[[i]]<-birdtrajectory
    print(i)
  }

  traj.sp<-sp::SpatialLines(trajectory)

  trajectory.sp.data <- sp::SpatialLinesDataFrame(traj.sp,
                                                  data = data.frame(ID = ids), match.ID = FALSE)


  sp::proj4string(trajectory.sp.data)<-gpsproj
  names(trajectory.sp.data)[1]<-'name'
  rgdal::writeOGR(trajectory.sp.data, paste(tempdir,'Last12.shp',sep=''), layer = 'BHS', driver = "ESRI Shapefile",
                  overwrite = T)

  x<-mapview::mapview(trajectory.sp.data, map.types = 'Esri.WorldImagery') + mapview::mapview(lasttwelve, zcol = "cat")
  mapview::mapshot(x, url = paste0(tempdir, "map.html"))









  # make spatial here to clean data
  sp::coordinates(dat2)<-c('x', 'y')
  sp::proj4string(dat2)<-gpsproj



  system.time({Cdat<-Ovis::cleanFun(dat2, projectedproj, hdopC = "dop", filename = CleanRep)})


  mdat<-as.data.frame(Cdat[[1]])
  mdat$IdCol<-as.character(mdat$IdCol)







  tim<-paste(strftime(Sys.time(),format='%Y'),'-', subsetmonth, '-01 00:00:00',sep='')
  mdat<-mdat[which(mdat$TelemDate>=as.POSIXct(tim,'%Y-%m-%d %H:%M:%S',tz='')),]




  atab<-as.data.frame(table(mdat$IdCol))
  atab<-atab[which(atab$Freq>50),]


  mdat<-mdat[which(mdat$IdCol %in% atab$Var1),]
  mdat<-mdat[as.character(mdat$IdCol) %in% vhist$IdCol,]



  system.time({mdat2<-Ovis::BGBFun(data=mdat,xname='Easting',yname='Northing',timename='TelemDate',
                                   idname='IdCol',projstring=sp::proj4string(Cdat[[1]]),ncpus=ncpus)})


  mdat3<-mdat2
  system.time({hg<-Ovis::MLPartPred(mdat3,spp='BHS')})




  Ovis::vitMap(locdat=mdat3,vhist=vhist,fold=plotfolder,labels = "Frequency",
        plotdataPath=plotdatapath,hg=hg)



  all.locs<-data.frame()
  recent.locs<-function(data, unicol,lookup){
    uni<-unique(data[,unicol])


    for(k in 1:length(uni)){
      sub<-data[data[,unicol] == uni[k],]
      sub.look<-lookup[lookup$IdCol == uni[k],]

      sub<-sub[order(sub$TelemDate, decreasing = T),]
      sub<-sub[1,]

      all.locs<-rbind(sub, all.locs)
    }

    new.locs<-merge(lookup, all.locs, by.x = 'IdCol', by.y = 'IdCol')

    new.locs<-new.locs[, c(names(lookup), 'Easting', 'Northing')]
    if("BirthDate" %in% names(new.locs)){
      new.locs<-new.locs[,!names(new.locs) %in% "BirthDate"]
    }

    return(new.locs)
  }

  tabby<-recent.locs(data = mdat3, unicol = 'IdCol', lookup = lookup)
  saveRDS(tabby, file = datastore)




  Ovis::PrettyData(dat=mdat3,idl=tabby,filen=PrettyDataStore)
  hj<-readRDS(PrettyDataStore)


  #hj$MatchFreq<-gsub('.','',as.character(hj$`Mom Freq`),fixed=T)
  hj$MatchFreq<-ifelse(nchar(hj$MatchFreq)<6,paste0(hj$MatchFreq,'0'),hj$MatchFreq)






  fn<-data.frame(datastore=datastore,prettydatastore=PrettyDataStore,
                 pathloc=paste0(tempdir,'path.RDS'),stringsAsFactors=F)
  saveRDS(fn,paste0(tempdir,'AllPaths.RDS'))




  path<-plotfolder

  llist<-list.files(path,full.names=T)

  finlist<-vector()
  for(i in 1:length(llist)){
    for(k in 1:length(hj$MatchFreq)){
      tes = grepl(hj$MatchFreq[k], llist[i])
      tes = TRUE %in% tes
      if(tes == TRUE){
        finlist<-c(finlist,llist[i])
      }
    }
  }
  finlist<-finlist[!duplicated(finlist)]
  llist<-finlist

  for(i in 1:length(llist)){



    fn<-data.frame(datastore=datastore,prettydatastore=PrettyDataStore,
                   pathloc=paste0(tempdir,'path.RDS'),plotpath=llist[i],
                   vhist=lookup,stringsAsFactors=F)


    sb<-gsub(plotfolder,'',llist[i])
    sb<-gsub('.png','',sb)

    if((i+1)<10){
      of<-paste0(paste0(pdffolder,'/100'),i+1,'.pdf')
    }
    if((i+1)>=10){
      of<-paste0(paste0(pdffolder,'/10'),i+1,'.pdf')
    }
    if((i+1)>=100){
      of<-paste0(paste0(pdffolder,'/1'),i+1,'.pdf')
    }

    #will need to change path to PartPlot RMD file
    rmarkdown::render(input=markdownpath,
                      output_format = 'pdf_document',
                      output_file=of,
                      params=list(tabby=fn[1,1],
                                  ll=fn[1,2],
                                  plotlink=fn[1,4],
                                  basepath=paste0(plotfolder,'/')),quiet=F)

  }

  endtime<-Sys.time()

  c<-list.files(pdffolder,full.names=T)
  c<-c('pdftk',c,'output',paste0(tempdir,'ParturitionMetrics.pdf'))

  c<-paste(c,collapse=' ')
  system(c)

  mailR::send.mail(from = from,
            to = to,
            subject = paste0("Parturition Model Updated ", Sys.time()),
            body = "This email contains the latest parturition model run.",
            authenticate = TRUE,
            smtp = list(host.name = "smtp.office365.com", port = 587, user.name = "khuggler@uidaho.edu", passwd = "Gustobailey1957!", tls = TRUE), attach.files = c("C:/Users/khuggler/Desktop/Last3Days_AsotinSheep.html", 'C:/Users/khuggler/Desktop/LambMark/ParturitionMetrics.pdf'))

}








