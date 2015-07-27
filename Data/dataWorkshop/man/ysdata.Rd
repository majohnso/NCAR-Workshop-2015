\name{ysdata}
\alias{ysdata}
\docType{data}
\title{
Six months of daily users statistics from the Yellostone supercomputer.
}
\description{
This dataframe is composed of 33 columns of data collected daily, from 12/1/2014 through 5/31/2015. 
for the Yellowstone supercomputer. 
}
\usage{data("ysdata")}
\format{
  A data frame with 182 observations on t 33 variables.
  The first four columns are the "Day" (as a Data object), "CPU Hours: Total", "Number of Jobs Submitted", and "Wait Hours: Total". 
  Columns 5-18 are wait hours per job by job size, and columns 19-26 are wait hours per job by requested wall time. Columns 27-33 are total CPU hours by job wall time. Here are some pertinent definitions:
\itemize{  
\item{wall time:} The total linear time each job took to execute.

\item{wait hours:} The total time, in hours, jobs waited before execution on Yellowstone.

\item{CPU hours:} The number of CPU cores multiplied by wall time hours used by  all jobs. 
}


  \describe{
    \item{\code{Date}}{A Date object}
 
    \item{\samp{CPU Hours}}{Amount hours used in computing/}
    \item{\samp{Submitted Jobs}}{Number of submitted jobs to Yellowstone.}
    \item{\samp{Wait Hours}}{Hours submitted jobs waited in queuw before running.}
    \item{\samp{1n}}{Hours waited for jobs requesting 1 node }
    \item{\samp{2n}}{Hours waited for  2 nodes.}
    \item{\samp{3-4n}}{Hours waited for  3 or 4 nodes.}
    \item{\samp{5-8n}}{Hours waited  5 through 8 nodes.}
    \item{\samp{9-16n}}{Hours waited  5 through 8 nodes.}
    \item{\samp{17-32n}}{Hours waited  17 through 32 nodes.}
    \item{\samp{33-64n}}{Hours waited  33 through 64 nodes.}
    \item{\samp{65-128n}}{Hours waited  65 through 128 nodes.}
    \item{\samp{129-256n}}{Hours waited  129 through 256 nodes.}
    \item{\samp{257-512n}}{Hours waited  257 through 512 nodes.}
    \item{\samp{513-1024n}}{Hours waited  513 through 1024 nodes}
    \item{\samp{1kn-2kn}}{a numeric vector}
    \item{\samp{2kn-4kn}}{a numeric vector}
    \item{\samp{>4kn}}{a numeric vector}
    \item{\samp{0-1s}}{a numeric vector}
    \item{\samp{1-30s}}{a numeric vector}
    \item{\samp{30s-30min}}{a numeric vector}
    \item{\samp{30-60min}}{a numeric vector}
    \item{\samp{1-5hr}}{a numeric vector}
    \item{\samp{5-10hr}}{a numeric vector}
    \item{\samp{10-18hr}}{a numeric vector}
    \item{\samp{18+hr}}{a numeric vector}
    \item{\samp{1-30s}}{a numeric vector}
    \item{\samp{30s-30min}}{a numeric vector}
    \item{\samp{30-60min}}{a numeric vector}
    \item{\samp{1-5hr}}{a numeric vector}
    \item{\samp{5-10hr}}{a numeric vector}
    \item{\samp{10-18hr}}{a numeric vector}
    \item{\samp{18+hr}}{a numeric vector}
   
  }
}

\examples{
data(ysdata)
# number of jobs for each day 
plot( ysdata[,1], ysdata[,2], xlab="day", ylab="number of submitted jobs", type="h", log="y")
day<- weekdays(ysdata[,1])
# order 
lev<- c("Monday","Tuesday", "Wednesday", "Thursday",  "Friday", "Saturday",  "Sunday")
day<- factor( day, levels=lev)
boxplot( ysdata[,2] ~ day, ylim=c(1.5e6, 1.75E6), las=3)
}
\keyword{datasets}
