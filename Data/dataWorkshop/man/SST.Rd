\name{SST}
\alias{SST}
\docType{data}
\title{Monthly sea surface temperatures for the Pacific Ocean}
\description{
Monthly sea surface temperatures from NCEP March 1970 - 2003
}
\usage{data(VictoriaT)}
\format{
A \code{data(SST)} loads four data objects
\describe{
\item{SST}{ An  \code{c(84,30,399)} array with dimensions  longitude, latitude, and time 
(in months).The values are the temperature in degrees C for a specific pixel and specific 
month. Note that land grid cells are indicated by  NA.}
\item{lon}{Longitudes (84) for the temperature array. Longitudes are  in [0,360]. To add a 
Mercator world map use the shifted version of the world map data base, \code{world2hires}, from the
mapdata package.}
\item{lat}{Latitudes (30) for the temperature array}
\item{tm}{A vector of 203 times in years  corresponding to the last dimension of \code{z}. }
} 
}
\details{
These data proicded courtesy of Chris Wikle. 
}
\examples{
data(SST)
# times series plot of a central grid box

plot( tm, SST[40,15,], type="h")

# image for August 1971
ind<- 20
print( tm[20])
# image of May 2007
image( lon, lat, SST[,,ind], col=terrain.colors(256))



}
\keyword{datasets}
