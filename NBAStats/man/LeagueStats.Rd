\name{LeagueStats}
\alias{LeagueStats}
\title{
Creates a tibble of the average of any single stat of the entire NBA league for the past 10 years
}
\description{
This function allows the user to view the average of any single stat of the entire NBA league within the past 10 years. It helps to view any trends that the NBA has gone through relative to changes it may have experienced during that time. 
}
\usage{
LeagueStats(variable)
}

\arguments{
  \item{variable}{a character value of the statistic that will be analyzed.}
}
\details{
for this function, do not include quotations when entering the variable. see example for details.
}

\examples{
LeagueStats(PTS)
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
