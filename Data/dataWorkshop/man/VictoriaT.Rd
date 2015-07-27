\name{VictoriaT}
\alias{VictoriaT}
\docType{data}
\title{Temperatures for Lake Victoria}

\description{
Monthly surface temperatures remotely sensed for Lake Victoria, May 2005 - March, 2012.
}
\usage{data(VictoriaT)}
\format{
\describe{
\item{z}{ An  \code{c(65,66, 203)} array with dimensions  longitude, latitude, and time 
(in months). The values are the temperature in degrees C for a specific pixel and specific 
month. Note that there many missing values indicated by NA.}
\item{x}{Longitudes for the temperature array}
\item{y}{Latitudes for the temperature array}
\item{time}{A vector of 203 times in years  corresponding to the last dimension of \code{z}. }
\item{loc}{ A  4290 X 2 matrix with the longitude and latitude locations for all the 
pixels in the image.} 
}
}
\details{
The measurements are monthly, remotely sensed surface temperatures for
Lake Victoria, Africa's largest lake by area, and it is the largest
tropical lake in the world. These data are a small part of the
globolakes program (\url{http://www.globolakes.ac.uk}) designed to assemble information on the physical
and chemical properties of the Earth's lakes. Overall the goal is "to
develop a global observatory for the ecological condition of lakes."
}
\examples{
data(VictoriaT)

# image of May 2007:  time[144]
image( VictoriaT$x, VictoriaT$y, VictoriaT$z[,,144], col=rainbow(128))

# time series of a middle pixel: location (30,30)

plot( VictoriaT$time, VictoriaT$z[30,30,], type="b")


}
\keyword{datasets}
