\name{CalculateWPs}
\alias{CalculateWPs}
\title{Get our calculated win probabilities for a given date}
\description{THIS FUNCTION IS COMING SOON! This function webscrapes the NBA betting lines from three different markets. It also calculates a win probability based on our model for a given date.}
\usage{CalculateWPs(x)}
\arguments{
  \item{x}{The date entered as "YYYYMMDD"}
}
\value{
  \item{df}{A data frame with the win probability is returned.}
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
CalculateWPs("20171206")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Odds}% use one of  RShowDoc("KEYWORDS")
\keyword{Betting}% __ONLY ONE__ keyword per line
\keyword{Spread}
\keyword{Lines}
\keyword{OverUnder}
\keyword{winprobability}
