\name{YSUsers}
\alias{YSUsers}
\docType{data}
\title{User  activity on the Yellowstone supercomputer}
\description{
One week ( March 19-25, 2104) of user job information collected from the Yellowstone supercomputer.
}
\usage{data(YSUsers)}
\format{
  A data frame with 66189 observations on the following 12 variables.
  \describe{
    \item{\code{username}}{a character vector giving the user}
    \item{\code{projcode}}{a character vector giving the project -- a group of users}
    \item{\code{start_time}}{Job start in seconds}
    \item{\code{end_time}}{Job end in seconds}
    \item{\code{submit_time}}{Time job was submitted}
    \item{\code{unix_user_time}}{Time in seconds.}
    \item{\code{unix_system_time}}{Time in seconds.}
    \item{\code{queue_wait_time}}{Time in seconds job remained in queue}
    \item{\code{num_nodes_used}}{Number of nodes used.}
    \item{\code{interactive}}{ 0 if batch 1 if interactive.}
    \item{\code{num_cores_used}}{Number of cores used by Job}
    \item{\code{external_charge}}{}
\item{\code{activity_date}}{The date and time as a more friendly string.}

  }
}
\details{
For more details on the Yellowstone computer see
\url{http://www2.cisl.ucar.edu/resources/yellowstone} Briefly this
system consists of approximately 4500 computational nodes, with each node
has 16 separate processors.  This gives a total of more than 72,000
processors although most jobs do not use that much of this resource at
once. Typically the connection of processors within a node is more efficient 
than between nodes although Yellowstone was designed to have state-of-art
speed of connectivity across the machine. It was purchased along with the disk and tape systems for $30M
in summer 2012 and sits in a special purpose data center in Cheyenne,
WY. The primary scientific work on Yellowstone is the simulation of
climate using large and complex climate models and the forecasting of
weather at fine scales. Members of the NSF geosciences university
community have access to this computer along with the scientific
staff and visitors at the National Center for Atmospheric Research
(NCAR).

Timing on a supercomputer has several different perspective.

For the "Unix time" values: Those are "seconds since Jan 01 1970
UTC". See \url{http://www.unixtimestamp.com/index.php} Job duration is
simple (end_time - start_time = # of seconds wallclock time).  Most
jobs that are not "interactive" sit in a queue and have some waiting
period before there is room on the system for them to run.  Wait_time
(in seconds) = start_time - submit_time.

The unix_user_time and unix_system_time reflect actual CPU time used
-- across all the CPUs on a node. More or less,  the following
should true for a given job

unix_user_time + unix_system_time <= num_nodes_used * 16 *
wallclock_time (in seconds)

Only a job that is 100% efficient at using the nodes would hit "=".
%%  ~~ If necessary, more details than the __description__ above ~~
}

\examples{
data(YSUsers)
# number of jobs run by distinct users
out<-table(YSUsers[,"username"])
hist( log10( out))

# wall clock time of jobs
wallTime<- YSUsers[,"end_time"] - YSUsers[,"start_time"]
cores<- YSUsers[,"num_nodes_used"] * 16
plot( wallTime, cores, log="xy", xlab="Time running", ylab="Number of cores")


## maybe str(YSUsers) ; plot(YSUsers) ...
}
\keyword{datasets}
