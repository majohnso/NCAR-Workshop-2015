\name{UStornado}
\alias{UStornado}
\docType{data}
\title{
US Tornado record 1950- 2012.
}
\description{
Reported tornado for the conterminous US for 1950 -2012 including severity scale. 
}
\usage{data(UStornado)}
\format{
  A data frame with 31588 observations on the following 5 variables.
  \describe{
    \item{\code{year}}{Year of report}
    \item{\code{month}}{Month of report}
    \item{\code{lat}}{Latitude of the average of the beginning  and ending location.}
    \item{\code{lon}}{Longitude of the average of the beginning  and ending location}
    \item{\code{Fscale}}{The Fujita integers from 0 to 5 with 5 being the severest.}
  }
}
\details{
This is a subset of the more complete data base supplied by the NOAA Storm Prediction Center.
}
\source{
\url{http://www.spc.noaa.gov/wcm}
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}

\examples{
data(UStornado)

plot( UStornado$lon, UStornado$lat) 
US( add=TRUE)

table( UStornado$month, UStornado$Fscale)

## maybe str(UStornado) ; plot(UStornado) ...
}
\keyword{datasets}
