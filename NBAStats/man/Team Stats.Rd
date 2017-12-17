\name{Team Stats}
\alias{Team Stats}

\title{
Get any stat for any team of any year
}
\description{
Creates a table with desired regular season NBA statistic from the past 10 years.
}
\usage{
Team Stats(team, year, table_code)
}
\arguments{
  \item{team}{a character value. Three letter abbreviation of the team name.}
  \item{year}{a character value. The year of the of the data table.}
  \item{table_code}{a numeric value. Reference code to the content of the data table that will be extracted.}
}
\details{
Table codes for this function should be between the values 4 to 11. Refer to glossary for more details.
}
\examples{
get.team("SAS", "2015", 5)
}
