\name{dataRemote}
\alias{dataRemote}
\alias{URLdataWorkshopDownload}

\title{
Loads a data set from a remote location using the
dataWorkshop URL
}
\description{
	This is designed to work the same as the \code{data} function 
	but uses the dataWorkshop web site as to download the 
	data. This is used to avoid including large datasets with the 
	standard package distribution as CRAN has limits on the
	size of contributed packages. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dataRemote(dname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dname}{ The name of a data set that is part of the 
  	dataWorkshop package but not distributed with the installation.
%%     ~~Describe \code{dname} here~~
}
}
\details{
	The default dataWorkshop URL is given in the object 
	URLdataWorkshopDownload and currently is 
	\url{http://www.image.ucar.edu/~nychka/dataWorkshop/dataDownload}
	This is just a browsable directory of R binary data sets. 
    For completeness all the dataWorkshop data sets are included here but of course many can be loaded using the normal data function. 
%%  ~~ If necessary, more details than the description above ~~
}


\author{
Doug Nychka
%%  ~~who you are~~
}

\examples{
	dataRemote(AudiA4)
     plot( AudiA4$mileage, AudiA4$price )
     table( AudiA4$color)
}
\keyword{data}

