\name{Team Stats}
\alias{Team Stats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
Creates a table with desired regular season NBA statistic from the past 10 years.%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Team Stats(team, year, element)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{team}{a character value. Three letter abbreviation of the team name.}
  \item{year}{a character value. The year of the of the data table.}
  \item{element}{a character value. The content of the data table.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
get.team("CLE", "2015", "contracts")
