% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bindfun.R
\name{bindfun}
\alias{bindfun}
\title{Bind Movement Trajectory and GPS Data}
\usage{
bindfun(ti, data)
}
\arguments{
\item{ti}{data.frame output from adehabitatLT as.ltraj function. Also the output from trajfun}

\item{data}{data.frame used to make ti}
}
\description{
Combines output of movement trajectory creation and GPS data into a single
data.frame. Internally called function; should not be used on it's own.
}
\keyword{bind}
