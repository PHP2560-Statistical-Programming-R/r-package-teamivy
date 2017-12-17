\name{Seasons Plot}
\alias{Seasons Plot}
\title{
Creates a plot that visualizes a single statistic of a particular season across all NBA teams.
}
\description{
This function creates a plot that shows the average of a single statistic of every team within the NBA for the past 10 years.
}
\usage{
Seasons Plot(year, col_name)
}

\arguments{
  \item{year}{a character value of the year that will be analyzed.}
  \item{col)name}{a character value of the statistic to be analyzed.}
  
}

\examples{
season_plot("2015", "PTS")
}

\keyword{season}% use one of  RShowDoc("KEYWORDS")
\keyword{plot}% __ONLY ONE__ keyword per line
