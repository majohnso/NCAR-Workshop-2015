\name{seaice}
\alias{seaice}
\docType{data}
\title{
North Pole Monthly Sea Ice Concentration
}
\description{
seaice is a dataset
}
\usage{data("seaice")}
\format{
  The format is a list containing four components:
 $ lon  : two-dimensional longitudes
 $ lat  : two-dimensional latitudes 
 $ conc : sea ice concentration
 $ dates: vector of month and year for each timepoint
}
\details{
Sea ice concentration can range from 0 to 1. However, negative values indicate flags for lakes (-0.04), coasts (-0.03), land (-0.02), and missing values (-0.01).
}
\source{
ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G02202_v2/north/monthly/
}
\references{
Documentation available at http://nsidc.org/data/docs/noaa/g02202_ice_conc_cdr/index.html
}
\keyword{datasets}
