\name{Playoffs Stats}
\alias{Playoffs Stats}

\title{
Get the playoff stats
}
\description{
Creates a table with desired NBA playoffs statistic from the past 10 years.
}
\usage{
Playoffs Stats(year, table_code, type)
}

\arguments{
  \item{year}{a character value. The year of the of the data table.}
  \item{table_code}{a numeric value. The code of the table that is being scraped. Code reference located in glossary}
  \item{type}{a character value. The content of the data table. default is set to blank.}
}
\details{
  The table code values for this function should be between 12 and 18. Refer to the glossary for more details.
  Table codes 12,14, and 16 require a type to be entered.
}
\examples{

#The table code value for playoffs must be between 12 to 18 as shown in the glossary
get.playoffs("2017", 15, "schedule")
}
