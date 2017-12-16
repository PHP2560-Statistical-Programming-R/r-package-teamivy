\name{DailyOdds}
\alias{DailyOdds}
\title{Get the NBA betting lines for a given date}
\description{This function webscrapes the NBA betting lines from three different markets. It also calculates a win probability based on the Pinnacle lines.}
\usage{DailyOdds(x)}
\arguments{
  \item{x}{The date entered as "YYYY-MM-DD"}
}
\value{
  \item{df}{A data frame with the games and odds for that day is returned.}
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
DailyOdds("2017-12-06")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Odds}% use one of  RShowDoc("KEYWORDS")
\keyword{Betting}% __ONLY ONE__ keyword per line
\keyword{Spread}
\keyword{Lines}
\keyword{OverUnder}
