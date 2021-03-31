#' @title GPS Data Cleaning
#
#' @description Cleans output from GStar or Irid. Cleaning is user-defined and
#' based on either hdop or 2d/3d fix status. Outputs three results in list format: Cleaned Data,
#' "Bad" data and a report. Data is also cleaned if the 'dist' (from as.ltraj) is greater than the 98 percentile.
#' @param data SpatialPointsDataFrame as outputed from GStar or Irid.
#' @param cval Cutoff for 2d/3d fix status. Valid values are 2 or 3. 3 will only use 3d fixes while 2 uses 2 and 3.
#' @param hval Cutoff for hdop quantile cutoff. Default is 10 which is equalt to 90th quantile.
#' @param filename Full path to store cleaning report file.
#' @param spp Character vector of species name (eg. 'Deer')
#' @param fixstat name of 2d/3d fix column. Defaults to 'X2D.3D'
#' @param hdopC name of HDOP column. Defaults to 'HDOP'
#'
#' @return Resulting object is a list of three elements. First element is a SpatialPointsDataFrame of
#' all the GPS data which met the cleaning critera, the second element is all the "bad" data cleaned by
#' set parameters and a report of what data was cleaned and for what reason.
#'
#' Accessing the "good" spatial data is done by cleanFun()[[1]]
#' Accessing the "bad" spatial data is done by cleanFun()[[1]]
#' Accessing the cleaning report is done by cleanFun()[[1]]
#' @keywords clean, data cleaning
#' @export
#' @examples
#' \donttest{cleanFun(data, projectproj = "+proj=utm +zone=11 +ellps=GRS80 +datum=NAD83 +units=m +no_defs", filename='CleanRep',cval=3,hval=10)}
#'


cleanFun<-function (data, projectedproj, fixstat = "X2D.3D", hdopC = "HDOP", cval = 3,
                    hval = 10, filename){


    rawDat <- data
    #rawDat<-rawDat[rawDat@coords[,1]<(-50),]
    data<-rawDat
    id <- as.data.frame(sp::spTransform(data, projectedproj))
    colnames(id)[(ncol(id)-1):ncol(id)] <- c("Easting", "Northing")
    id$chk<-paste(id$IdCol,id$TelemDate,sep='_')
    id<-id[!duplicated(id$chk),]
    id<-id[,-(ncol(id))]



    t <- Ovis::trajfun(id, "TelemDate", "Easting", "Northing",
                       "IdCol")


    id <- Ovis::bindfun(t, id)

    stepquants <- as.numeric(quantile(id$dist, na.rm = T,
                                      seq(0, 1, 0.01))[100])

    ddr <- id[which(id$dist > stepquants),
                ]

    id <- id[which(id[, "dist"] <= stepquants), ]
    data <- id


    sp::coordinates(data) <- ~Easting + Northing
    sp::proj4string(data) <- projectedproj

    if('HDOP' %in% names(data)){
    val <- as.numeric(quantile(data@data$HDOP, na.rm = T, probs = seq(0,
                                                                      1, 0.1))[hval])
    hdr <- data[which(data@data[, hdopC] > val), ]
    }
    if(!'HDOP' %in% names(data)){
      hdr<-data.frame()
    }


    if(fixstat %in% names(data)){
    fxr <- data[which(data@data[, fixstat] < cval), ]
    }
    if(!fixstat %in% names(data)){
      fxr <- data.frame()
    }


    bdata<-nrow(hdr) + nrow(fxr) + nrow(ddr)

    report <- list(c("Number Bad Locs", "HDOP Clean Val",
                     "Fix Status Clean Val", "Points Removed VIA HDOP",
                     "Points Removed VIA FixStatus", "Points Removed Step Length (greater than 99% quantile)"),
                   c(bdata, hval, cval, nrow(hdr), nrow(fxr),
                     nrow(ddr)))
    report <- as.data.frame(do.call("rbind", report), stringsAsFactors = F)
    names(report) <- as.character(report[1, ])
    report <- report[2, ]

    write.table(report, filename, sep = ",", row.names = F)
    return(list(data, report))


 }
