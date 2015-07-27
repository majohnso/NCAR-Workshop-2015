\name{nba}
\alias{nba}
\docType{data}
\title{

    NBA Basketball Reference Dataset.
}
\description{
Annual statistics on all players in the NBA over the years 1950-2015. 
}
\usage{data(nba)}
\format{
    A data frame containing every NBA player's total season statistics from 1950-2015. There are 23449 observations which represents every unique player/team/year combination, and 50 variables recorded for each one of these combinations.
  \describe{
    \item{\code{Year}}{Year of the season. a numeric vector}
    \item{\code{Player}}{NBA Player. character vector}
    \item{\code{Pos}}{Which position the Player plats. a character vector}
    \item{\code{Age}}{Age of Player. a numeric vector}
    \item{\code{Team}}{Team of player. a character vector}
    \item{\code{G}}{Number of games the player played. a numeric vector}
    \item{\code{GS}}{Number of games a player started. a numeric vector}
    \item{\code{MP}}{Amount of minutes played. a numeric vector}
    \item{\code{FG}}{Number of shots made. a numeric vector}
    \item{\code{FGA}}{Number of shots attempted. a numeric vector}
    \item{\code{FG.}}{Field Goal percentages. a numeric vector}
    \item{\code{X3P}}{Three point shots made. a numeric vector}
    \item{\code{X3PA}}{Three point shots attempted. a numeric vector}
    \item{\code{X3P.}}{Three point shooting percentage. a numeric vector}
    \item{\code{X2P}}{Two point shots made. a numeric vector}
    \item{\code{X2PA}}{Two point shots attempted. a numeric vector}
    \item{\code{X2P.}}{Two point shooting percentages. a numeric vector}
    \item{\code{eFG.}}{Effective field goal percentage. a numeric vector}
    \item{\code{FT}}{Free throws Made. a numeric vector}
    \item{\code{FTA}}{Free throws attempted. a numeric vector}
    \item{\code{FT.}}{Free throw percentage. a numeric vector}
    \item{\code{ORB}}{Total offensive rebounds. a numeric vector}
    \item{\code{DRB}}{Total defensive rebounds. a numeric vector}
    \item{\code{TRB}}{Total recounds. a numeric vector}
    \item{\code{AST}}{Total assists. a numeric vector}
    \item{\code{STL}}{Total steals. a numeric vector}
    \item{\code{BLK}}{Total blocks. a numeric vector}
    \item{\code{TOV}}{Total turnovers. a numeric vector}
    \item{\code{PF}}{Total personal fouls. a numeric vector}
    \item{\code{PTS}}{Total points Scored. a numeric vector}
    \item{\code{PER}}{Player efficiency rating. a numeric vector}
    \item{\code{TS.}}{True shooting percentage. numeric vector}
    \item{\code{X3PAr}}{Percentage of FG attempts that are threes. a numeric vector}
    \item{\code{FTr}}{Free throw attempts per Field goal. a numeric vector}
    \item{\code{ORB.}}{Offensive rebound percentage. a numeric vector}
    \item{\code{DRB.}}{Defensive rebound percentage. a numeric vector}
    \item{\code{TRB.}}{Total rebound percentage. a numeric vector}
    \item{\code{AST.}}{Assist percentage. a numeric vector}
    \item{\code{STL.}}{Steal percentage. a numeric vector}
    \item{\code{BLK.}}{Block percentage. a numeric vector}
    \item{\code{TOV.}}{Turnover percentage. a numeric vector}
    \item{\code{USG.}}{Usage rate. a numeric vector}
    \item{\code{OWS}}{Offensive win shares. a numeric vector}
    \item{\code{DWS}}{Defensive win shares. a numeric vector}
    \item{\code{WS}}{Win Shares. a numeric vector}
    \item{\code{WS.48}}{Win shares per 48 minutes. a numeric vector}
    \item{\code{Var.56}}{a logical vector}
    \item{\code{OBPM}}{Offensive box plus minus. a numeric vector}
    \item{\code{DBPM}}{Defensive box plus minus. a numeric vector}
    \item{\code{BPM}}{Box plus minues. a numeric vector}
    \item{\code{VORP}}{Value over replacement player. a numeric vector}
  }
}

\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
http://www.basketball-reference.com/
}

\examples{
data(nba)
str(nba)

# Load in LeBron's career Statistics
lebron <- nba[nba$Player == "LeBron James",]

# Look into LeBron's statistics in 2015
lebron_2015 <- lebron[lebron$Year == 2015,]

# Look into LeBron's scoring average over time 
lebron_ppg <- lebron$PTS/lebron$G
plot(lebron$Age, lebron_ppg,
 		main = "LeBron scoring over time", type = "l",
 		xlab = "Age", ylab = "Points per Game")

# Plot points per game as a function of age
boxplot(nba$PTS/nba$G ~ nba$Age, ylim = c(0, 32))

## maybe str(nba) ; plot(nba) ...
}
\keyword{datasets}
