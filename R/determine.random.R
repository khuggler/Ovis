#' @title determine.random
#' @description determine number of random locations needed to represent true mean of landscape variables
#' @param rasterstack name of raster or raster stack object to evaluate
#' @param poly spatialpolygonsdataframe to sample random locations from
#' @param pts spatialpointsdataframe of used locations
#' @param increase.type whether to increase proportionally (i.e., percentage) or numerically (i.e., integers)
#' @param increment if increase.type = "prop" a proportion in decimal format (e.g., for 10 percent 0.10) if increase type = "number" indicate number of points to increase at each iteration
#' @param number.iterations number of incremental steps to run through
#' @param seqmin if increase.type = "number", indicate the minimum number of random locations to test
#' @param seqmax if increase.type = "number", indicate the maximum number of random locations to test
#' @param inc.ratio Logical. TRUE If you desire the number of random points to be greater than the total number of used points, and equal a 1:1 or greater ratio, otherwise FALSE
#' @param out.dir path to folder where plot should be saved
#' @return Returns a data.frame with the summarized values, and saves a pdf of plots to out.dir named RandomPoints
#' @keywords random, available, sample, raster
#' @export


determine.random<-function(rasterstack, poly, pts, increase.type, increment, number.iterations, seqmin, seqmax, inc.ratio, out.dir){
  means<-data.frame(var = names(rasterstack), mean = raster::cellStats(rasterstack, "mean"))
  means$upper<-means$mean + (0.05*means$mean)
  means$lower<-means$mean - (0.05*means$mean)

  pts<-sp::spTransform(pts, sp::proj4string(rasterstack))
  poly<-sp::spTransform(poly, sp::proj4string(rasterstack))

  iters<-seq(1,number.iterations, 1)

  if(increase.type = "prop"){

    all.rands<-data.frame()
    for(i in 1:length(iters)){
      print(paste0("Working on increment", " ", 100*(increment*iters[[i]]), "%"))

      if(inc.ratio == TRUE){
      rands<-spsample(poly, n= (nrow(pts) + round(nrow(pts)*(increment*iters[[i]]))), type ="random")
      }

      if(inc.ratio == FALSE){
        rands<-spsample(poly, n= round(nrow(pts)*(increment*iters[[i]])), type ="random")
      }
      rand.ex<-data.frame(extract(rasterstack, rands))
      rand.ex$increment<-increment*iters[[i]]
      rand.ex$n<-nrow(rand.ex)

      all.rands<-rbind(all.rands, rand.ex)

    }

    agg<-aggregate(. ~ increment, data = all.rands, FUN = mean, na.rm = T)
    test<-data.table::melt(data.table::setDT(agg), id.vars = c(1, 39), variable.name = "Var")

    new.means<-do.call("rbind", replicate(number.iterations, means, simplify = FALSE))

    props<-increment*iters
    new.means$prop<-rep(props, length.out = nrow(new.means))
    new.means<-new.means[order(new.means$var),]

    vars<-unique(test$Var)
    plotdata<-data.frame()
    for(k in 2:length(vars))  {
      sub<-test[test$Var == vars[[k]],]
      sub2<-new.means[new.means$var == vars[[k]],]
      sub2<-sub2[order(sub2$prop),]

      sub<-sub[, c(1,3,4)]
      sub2<-sub2[, c(2,3,4)]

      all<-cbind(sub, sub2)
      names(all)<-c('Increment', 'Variable', 'RandomMean', 'RasterMean', 'Upper5', 'Lower5')

      plotdata<-rbind(all, plotdata)
    }
  }

  if(increase.type = "number"){

    all.rands<-data.frame()
    seqs<-seq(seqmin, seqmax, increment)

    for(i in 1:length(seqs)){
      print(paste0("Working on iteration", " ", seqs[i]))

      if(inc.ratio == TRUE){
      rands<-spsample(poly, n= nrow(pts) + seqs[i], type ="random")
      }

      if(inc.ratio == FALSE){
        rands<-spsample(poly, n= seqs[i], type ="random")
      }

      rand.ex<-data.frame(extract(rasterstack, rands))
      rand.ex$increment<-seqs[i]
      rand.ex$n<-nrow(rand.ex)

      all.rands<-rbind(all.rands, rand.ex)

    }

    agg<-aggregate(. ~ increment, data = all.rands, FUN = mean, na.rm = T)
    test<-data.table::melt(data.table::setDT(agg), id.vars = c(1,39), variable.name = "Var")
    rownames(test)<-NULL


    new.means<-do.call("rbind", replicate(length(seqs), means, simplify = FALSE))

    rownames(new.means)<-NULL
    new.means$prop<-rep(seqs, length.out = nrow(new.means))
    new.means<-new.means[order(new.means$var),]
    new.means$var<-as.character(new.means$var)



    vars<-unique(test$Var)
    plotdata<-data.frame()
    for(k in 2:length(vars))  {
      sub<-test[test$Var == vars[[k]],]
      sub2<-new.means[new.means$var == vars[[k]],]
      sub2<-sub2[order(sub2$prop),]

      sub<-sub[, c(1,3,4)]
      sub2<-sub2[, c(2,3,4)]

      all<-cbind(sub, sub2)
      names(all)<-c('Increment', 'Variable', 'RandomMean', 'RasterMean', 'Upper5', 'Lower5')

      plotdata<-rbind(all, plotdata)
    }
     }





  ncol<-3
  nrow<-ceiling(length(unique(plotdata$Variable))/ncol)
  n_panels<-nrow*ncol

  vars<-unique(plotdata$Variable)


  pdf(paste0(out.dir,"/", "RandomPoints.pdf"), height = 6, width = 12)
  par(mar = c(3,3,0,0),mfrow = c(nrow, ncol))

  for(l in 1:length(vars)){
      sub<-plotdata[plotdata$Variable == vars[l],]

      ymax<-max(with(sub, pmax(RandomMean, RasterMean, Upper5, Lower5)))
      ymin<-min(with(sub, pmin(RandomMean, RasterMean, Upper5, Lower5)))


      plot<-ggplot2::ggplot(sub, ggplot2::aes(x = Increment, y = RandomMean))+
        ggplot2::geom_point(size = 2)+
        ggplot2::geom_line(sub, mapping = ggplot2::aes(x = Increment, y = RasterMean), col = "red")+
        ggplot2::geom_line(sub, mapping = ggplot2::aes(x = Increment, y = Upper5), linetype = "dashed", col = "red")+
        ggplot2::geom_line(sub, mapping = ggplot2::aes(x = Increment, y = Lower5), linetype = "dashed", col = "red")+
        ggplot2::ylab('Mean')+
        ggplot2::xlab('Increment')+
        ggplot2::ggtitle(vars[l])+
        ggplot2::scale_y_continuous(limits = c(ymin - (0.50*ymin), ymax + (0.50*ymax)))

      print(plot)

  }

  dev.off()

return(plotdata)
}
