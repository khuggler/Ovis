% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.terra.climate.R
\name{extract.terra.climate}
\alias{extract.terra.climate}
\title{extract.terra.climate}
\usage{
extract.terra.climate(root.dir, cropshape, months, comp.data, sub.dir,
  years, out.dir)
}
\arguments{
\item{root.dir}{path of root directory where TerraClimate data are stored}

\item{cropshape}{path to shapefile to crop TerraClimate data}

\item{months}{numeric vector of months needed from TerraClimate data ex) c(6,7,8,9)}

\item{comp.data}{path to composition database}

\item{sub.dir}{directory name of where intermediate .RDS files should be saved. Should be directory that is different from root.dir}

\item{years}{vectory of years YYYY for extraction}

\item{out.dir}{path to directory of where extracted data should be saved}
}
\value{
Returns a csv of extracted data for all TerraClimater layers
}
\description{
extract locations of composition plots to terraclimate data
}
\keyword{biomass}
\keyword{climate,}
\keyword{composition,}
\keyword{extract,}
\keyword{terraclimate,}
