\name{narccap2}
\alias{narccap2}
\docType{data}
\title{
Seasonal Temperatures from NARCCAP Phase II
%%   ~~ data name/kind ... ~~
}
\description{
Average summer and winter temperatures (degrees Celcius) from NARCCAP Phase II. Included are temperatures simulations from a subset of the Phase II model runs. These include the CRCM-ccsm, CRCM-cgcm3, WRFG-ccsm, and WRFG-cgcm3 model runs for both the current and future periods. The current period is 1970-2000 and the future period is 2040-2070. All models and datasets have been interpolated to a common grid.

To remotely load these datasets from the web use: \code{dataRemote{narccap2}}.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}

\format{
  Four lists (current.summer.temp, current.winter.temp, future.summer.temp, and current.winter.temp) of arrays. Each of these arrays is identified by model acronym (RCMgcm) and have three dimensions (longitude, latitude, year). Also included are two-dimension arrays (lon, lat) that give the longitude and latitude coordinates.  
}
\source{
Background can be found at \url{http://www.narccap.ucar.edu}.  
}
\examples{
\dontrun{	
dataRemote(narccap2)
#
# 30-year average for current.summer.temp from the CRCM-ccsm model
current.mn <- apply(current.summer.temp$CRCMccsm,c(1,2),mean)
#
# 30-year average for future.summer.temp from the CRCM-ccsm model
future.mn <- apply(future.summer.temp$CRCMccs,c(1,2),mean)

# heat map of climate change projection
image.plot(current.summer.temp$lon,current.summer.temp$lat,future.mn-current.mn)
}
}
\keyword{datasets}
