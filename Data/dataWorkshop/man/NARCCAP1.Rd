\name{NARCCAP1}
\alias{narccap1}
\docType{data}
\title{
Seasonal Temperatures from NARCCAP Phase I
%%   ~~ data name/kind ... ~~
}
\description{
Average summer and winter temperatures (degrees Celcius) from NARCCAP Phase I. Included are temperatures simulations from all six of the regional models (CRCM, ECP2, HRM3, MM5I, RCM3, WRFG) from 1980 through 2004, temperatures from the NCEP reanalysis dataset from which boundary conditions are derived from 1980 through 2004, and two observational datasets. The UDEL dataset is from 1980 through 2000 and the CRU dataset is from 1980 through 2004. All models and datasets have been interpolated to a common grid.
NOTE: these data sets are not included by default in the package and before you use them they need to be loaded from an external web page. 
To remotely load these datasets from the web use: \code{dataRemote{narccap1}}.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}

\format{
\strong{narccap1}:  A data set with two lists (summer.temp and winter.temp) of arrays. Each of these arrays is identified by model/dataset acronym and have three dimensions (longitude, latitude, year). Also included are two-dimension arrays (lon, lat) that give the longitude and latitude coordinates.


}
\source{
Background can be found at \url{http://www.narccap.ucar.edu}.  
}
\examples{
# NOTE: you must have a web connection to load the data.
# but once loaded you can always save it to a local directory or workspace. 
# narccap1 is about 45Mb
\dontrun{
dataRemote(narccap1)

#NOTE: to download "by hand"
# webLink<- paste0(URLdataWorkshopDownload,"narccap1",".rda")
# load(url(webLink))

# 20-year average for summer.temp from the CRCM model
  mn <- apply(summer.temp$CRCM[,,1:20],c(1,2),mean)

# heat map of 20-year average
  image.plot(summer.temp$lon,summer.temp$lat,mn)
}
}
\keyword{datasets}
