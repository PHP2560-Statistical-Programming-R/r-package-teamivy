\name{TableCode}
\alias{TableCode}

\title{
Translates table code into description of table for scraping in other functions
}
\description{
This function translates the table code entered by the user according to our glossary into a character string that allows some of our scraping functions to scrape the correct table. 
}
\usage{
TableCode(element)
}
\arguments{
  \item{element}{a numeric value with a range of [1,18] as indicated by the glossary. This value relates to the table that will be scraped.}
}
\details{
  Please refer to the glossary for more details.
}
\examples{
TableCode(1)
}

\keyword{Table}% use one of  RShowDoc("KEYWORDS")
\keyword{Code}% __ONLY ONE__ keyword per line
