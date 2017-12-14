\name{GetLastMatchups}
\alias{GetLastMatchups}

\title{
This function gets the last regular season matchups for two teams.
}
\description{
This function gets the last regular season matchups for two teams.
}
\usage{
GetLastMatchups(team, Team2, n = 5)
}
\arguments{
  \item{team}{Enter a string for the three letter abbreviation of a franchise or the entire franchise name of the first team in the matchup you want to compare.}
  \item{Team2}{Do the same foe the second team}
  \item{n}{Enter the number of most recent matchups you want to return, default is 5, if less than the number you entered exist, the most will be returned.}
}
\details{
}
\value{
\item{df}{A data frame of the games and results for this matchup.}
}
\examples{
GetLastMatchups("BOS", "CLE", 5)
GetTeamSchedule("Boston Celtics", "CLE", 10)
}
\keyword{Matchups}
\keyword{Team}
\keywork{Results}
\keyword{Seasons}

