\name{GetTeamSchedule}
\alias{GetTeamSchedule}

\title{
This function gets the team schedule and results for a given year. The default year is the current season.
}
\description{
This function gets the team schedule and results for a given year. The default year is the current season.
}
\usage{
GetTeamSchedule(team, Year = "2018")
}
\arguments{
  \item{team}{Enter a string for the three letter abbreviation of a franchise or the entire franchise name.}
  \item{season}{Enter a string for the year end of which season you want. Default is the current season.}
}
\details{
}
\value{
\item{df}{A data frame of the games and results for a given season.}
}
\examples{
GetTeamSchedule("BOS", "2018")
GetTeamSchedule("Boston Celtics", "2015")
}

\keyword{Schedule}
\keyword{Team}
\keyword{Results}
\keyword{Seasons}
