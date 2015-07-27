\name{Denver0Z}
\alias{Denver0Z}
\docType{data}
\title{
Radiosonde temperature measurements for the Denver upper air station made at "0Z GMT"
Greenwhich mean time ( early evening in Denver).
}
\description{
As part of the world wide network of balloon measurements of the atmosphere, upper air
measurements are made at Denver everyday at 0Z and 12 Z Greenwhich mean time. As the 
balloon ascends it measures several variables at specific pressure levels. This data sets
is a subset of the record that only includes the temperature (in degrees C) and the values at 
9 standard pressure levels ( 850 700 500 400 300 250 200 100 70 in hP).
Here 850 is a reading close to the surface and 70 hP is into the stratosphere.
The height of the balloon is measured in pressure because it was easier for early instruments
to register pressure instead of altitude.  
}
\usage{data(Denver0Z)}
\format{
  The format is:
List of 3 components

  Temp: A dataframe of 13648 X 9 variables with rows being different soundings and the columns indexing the temperatures at the different pressure levels.

time: A dataframe of 13648 X 4 variables giving the day of the sounding. Columns index year,
 month, day of month, and day of year.  (Day of year is useful for looking at seasonality in the
record.)
 
 alt : The 9 pressure levels where measurements are recorded.  850 700 500 400 300 250 200 100 70
}
\details{
Note there are many missing observations. 
}

\examples{
data(Denver0Z)
bplot(Denver0Z$Temp, labels=  Denver0Z$alt,xlab="pressure level hP", 
           ylab="Degrees C", pos= Denver0Z$alt)
}
\keyword{datasets}
