\name{Boulder Weather}
\alias{BoulderDaily}
\alias{BoulderJuneTemperature}
\alias{BoulderTemperature}

\docType{data}
\title{
Weather data for 
 Boulder,  Colorado  1897 -  March 31,2014.

}
\description{
Daily temperatures and rainfall for Boulder Colorado and monthly averages for June.
\strong{BoulderDaily}: 
Minimum and maximum daily temperatures in degrees F and  daily precipitation in 
inches. Temperatures are accurate to 1 degree and precipitation to 1/100 of an inch. 
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
 


\strong{data(BoulderJuneTemperature)}
  A data frame with 30 observations on the following 2 variables.
  \describe{
    \item{\code{Year}}{ year (1985:2014)}
    \item{\code{Temp}}{Average monthly  June temperature  degrees F}
  }
\strong{data(BoulderTemperature)}
  A data frame of monthly means with 117 rows (years 1897:2014) on the 12 months
Months have the names:
\code{ "jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"}. 
\cr
See the example below for constructing this data frame from the daily data set. 

\strong{data(BoulderDaily)}

  A data frame with 42913 observations on the following 6 variables.
  \describe{
    \item{\code{year}}{ year (1897:2014)}
    \item{\code{month}}{months 1:12}
    \item{\code{day}}{ day of month}
    \item{\code{time}}{Time in years with the fractional part being the position within the seasonal cycle.}
    \item{\code{tmin}}{daily minimum temperature}
    \item{\code{tmax}}{daily maximum temperature}
    \item{\code{precip}}{daily total precipitation}
  }


}

\source{
See 
\url{http://www.cdc.noaa.gov/Boulder}
The file boulderdaily.complete.txt was downloaded and parsed to create the data frame. 
}
\examples{
data(BoulderDaily)

#
# monthly mean for tmin
tapply( BoulderDaily$tmin,  BoulderDaily$month, "mean", na.rm=TRUE)

# time series plot of precip starting from 2012
ind<- BoulderDaily$time >= 2012
plot( BoulderDaily$time[ind],BoulderDaily$precip[ind], type="h")  

# creating monthly mean temperatures
tmean<-(BoulderDaily$tmin + BoulderDaily$tmax)/2 
 
 months<- c( "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
 INDEX<- list(BoulderDaily$year,BoulderDaily$month )
 BoulderTemperature<- tapply( tmean,  INDEX,  FUN= "mean", na.rm=TRUE)
 
 BoulderTemperature<- data.frame( BoulderTemperature)
 names( BoulderTemperature) <- months

}
\keyword{datasets}
