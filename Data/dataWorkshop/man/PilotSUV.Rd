\name{PilotSUV}
\alias{PilotSUV}
\docType{data}
\title{
Search results from cars.com to price a Honda Pilot
%%   ~~ data name/kind ... ~~
}
\description{
A sample of used Honda Pilot SUV listed on the cars.com web site. The goal was
to determine how the price is affected by mileage, model and year. This would help in 
deciding when a particular car was priced high or low.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(PilotSUV)}
\format{
  A data frame with 194 observations on the following 7 variables.
  \describe{
    \item{\code{year}}{A factor with levels \code{ 2003 } \code{ 2004 } \code{ 2005 } \code{ 2006 } \code{2005 }}
    \item{\code{model}}{A factor with levels \code{H  } \code{HEX } \code{HLX } indicating the }
    \item{\code{price}}{Asking prices in dollars}
    \item{\code{mileage}}{Reported vehicle mileage.}
    \item{\code{SUV}}{a factor with levels \code{SUV}}
    \item{\code{color}}{Body color.}
    \item{\code{distance}}{Distance of the carfrom Boulder in miles.}
  }
}
\details{
A search was done on the car.com web site  January, 2006 out to approcximately 2000 miles from Boulder. 
 All results that had asking prices are listed. The data was captured by downloading the web page results in 
source format and then editting the text file to be in a regular format. 
}
\examples{
data(PilotSUV)
plot( PilotSUV$mileage, PilotSUV$cost)

## maybe str(PilotSUV) ; plot(PilotSUV) ...
}
\keyword{datasets}
