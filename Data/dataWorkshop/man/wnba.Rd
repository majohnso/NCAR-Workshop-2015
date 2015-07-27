\name{wnba}
\alias{wnba}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
WNBA 2014 Player stats by game.
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
This dataframe contains individual player statistics for every game in the 2014 WNBA season.}
\usage{data(wnba)}
\format{
    Data frame with 4032 observations which corresponds to all unqiue player/game combinations in the 2014 season. There are 26 variables recorded for each one of these games.
  \describe{
    \item{\code{Player}}{Name of player. a character vector}
    \item{\code{player_id}}{Unique ID of the player. a numeric vector}
    \item{\code{team}}{Name of player's team. a character vector}
    \item{\code{date}}{Day of game. a numeric vector}
    \item{\code{home}}{Indicator specifying if the player is on the home team. a numeric vector}
    \item{\code{opponent}}{Name of opposing team. a character vector}
    \item{\code{win}}{Indicator if the player's team won. a numeric vector}
    \item{\code{team_pts}}{Number of points team scored. a numeric vector}
    \item{\code{opp_pts}}{Number of points opposing team scored. a numeric vector}
    \item{\code{minutes}}{Minutes played. a numeric vector}
    \item{\code{fgmade}}{Field goals made. a numeric vector}
    \item{\code{fgatt}}{Field goals attempted. a numeric vector}
    \item{\code{made3}}{Three point shots made. a numeric vector}
    \item{\code{att3}}{Three point shots attempted. a numeric vector}
    \item{\code{made1}}{Free throws made. a numeric vector}
    \item{\code{att1}}{Free throws attempted. a numeric vector}
    \item{\code{offrb}}{Offensive rebounds. a numeric vector}
    \item{\code{defrb}}{Defensive rebounds. a numeric vector}
    \item{\code{totrb}}{Total rebounds. a numeric vector}
    \item{\code{assist}}{Assists. a numeric vector}
    \item{\code{steal}}{Steals. a numeric vector}
    \item{\code{block}}{Blocks. a numeric vector}
    \item{\code{turnover}}{Turnovers. a numeric vector}
    \item{\code{fouls}}{Fouls. a numeric vector}
    \item{\code{points}}{Point scored. a numeric vector}
    \item{\code{efficiency}}{Player efficiency. a numeric vector}
  }
}

\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
http://www.stat.ufl.edu/~winner/datasets.html
}

\examples{
data(wnba)

# Look at all of the players in the WNBA
# and how many are there?
table(wnba$Player)
num_players <- unique(wnba$Player)
length(num_players)

# Look at the distribution of points scored
# for WNBA players.
hist(wnba$points, breaks = 100)

# Observe the relationship between points
# scored and minutes played.
plot(wnba$points, wnba$minutes, main = "Points Scored vs. Minutes Played",
    xlab = "Points", ylab = "Minutes")

## maybe str(wnba) ; plot(wnba) ...
}
\keyword{datasets}
