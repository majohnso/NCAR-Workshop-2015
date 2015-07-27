\name{DC3}
\alias{DC3}
\alias{DC8}
\alias{GulfStream}
\docType{data}
\title{ DC3 NCAR/NOAA field experiment studying severe weather using reseach aircraft}
%%   ~~ data name/kind ... ~~
\description{Two sets of aircraft measurements from the 
Deep Convective Clouds and Chemistry (DC3) experiment over the Central United States. 
The data sets here are for 2012-05-29 and for the NOAA research aircraft (DC8)  and the
NCAR aircraft (GulfStream). The data frames consist of differement measurements of chemical 
species in the atmosphere during the flight of the jets.  
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(DC3)}
\format{
  Two data frames \code{DC8} and \code{GulfStream} with  observations on the following 9 variables.
  \describe{
    \item{\code{longitude}}{ location of measurement in degrees}
    \item{\code{latitude}}{location of measurement in degrees}
    \item{\code{altitude}}{height of aircraft in km}
    \item{\code{NO}}{Concentration of NO pptv}
    \item{\code{NO2}}{Concentration of NO2 pptv}
    \item{\code{NOTotal}}{Total amount of NO }
    \item{\code{CO}}{Concentration of carbon monoxide}
    \item{\code{O3}}{Total column ozone DU}
    \item{\code{Time}}{time in seconds since take off.}
  }
}
\details{
The DC3 field campaign investigated the impact of deep, midlatitude
continental convective clouds, including their dynamical, physical,
and lightning processes, on upper tropospheric (UT) composition and
chemistry. The campaign made use of extensively instrumented aircraft
platforms and ground-based observations.

Salina, Kansas was the base location for the three research aircraft
and the project operations center at the Salina Airport, while a
network of ground-based radar, lightning antenna stations and
instrumentation in Colorado, Oklahoma and Alabama is used to support
DC3 during the project from 15 May - 30 June 2012. Click for more
information about Strategy and Experimental Design.

DC3 experiments were motivated by determining the role of
thunderstorms on the concentration of ozone in the upper troposphere,
where ozone acts as a greenhouse gas.

The NSF/NCAR Gulfstream-V (GV) aircraft was the primary platform to
study the high altitude outflow of the storms, and was instrumented to
measure a variety of gas-phase species, radiation, and cloud particle
characteristics. The DLR Falcon 20 aircraft that was part of DC3 is
not included as part of this data sets.  The NASA DC-8 aircraft
complemented the GV and Falcon with in situ observations to
characterize the convective storm inflow and provided remote sensing
to aid in flight planning and column characterization.

These data are for just May 29, 2012 of the field experiment.
Time recorded for the DC8 flight is seconds since 2012-05-29 19:52:56 +0000
and time for the GulfStream is seconds since 2012-05-29 20:02:01 +0000.

%%  ~~ If necessary, more details than the __description__ above ~~
}

\examples{
data(DC3)
plot( DC8[,1:2], type="l", col= "red4", lwd=2)
lines( GulfStream[,1:2], col="grey", lwd=2)

plot( DC8[,"CO"], DC8[,"altitude"], type="l")


par( oma=c( 10,0,0,0))
par( mar=c( 1,1,0,0))
set.panel( 2,1)
plot( rbind(GulfStream[,1:2], DC8[,1:2]), type="n", axes=FALSE, xlab="", ylab="")
box()
#add times 
ribbon.plot(  DC8[,1], DC8[,2] , DC8[,"Time"], zlim=c(0,2e4), lwd=4, col=tim.colors(256))

plot( rbind( GulfStream[,1:2],DC8[,1:2]), type="n", axes=FALSE, xlab="", ylab="")
box()
ribbon.plot(  GulfStream[,1], GulfStream[,2] , GulfStream[,"Time"], zlim=c(0,2e4), lwd=4, col=tim.colors(256))
par( oma= c( 5,1,1,1))
image.plot( legend.only=TRUE, zlim =c(0,2e4), col=tim.colors(256), horizontal=TRUE)

# using rgl 
# cscale<- color.scale( GulfStream[,"CO"], col=tim.colors(256))
# plot3d(GulfStream[,1], GulfStream[,2], GulfStream[,3], col= cscale)

}
\keyword{datasets}
