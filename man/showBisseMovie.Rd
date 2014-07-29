\name{showBisseMovie}
\alias{showBisseMovie}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
showBisseMovie(q0 = 0, q1 = 0.555, b0 = 5.199, b1 = 2.933, d0 = 5.433, d1 = 2.319)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{q0}{
%%     ~~Describe \code{q0} here~~
}
  \item{q1}{
%%     ~~Describe \code{q1} here~~
}
  \item{b0}{
%%     ~~Describe \code{b0} here~~
}
  \item{b1}{
%%     ~~Describe \code{b1} here~~
}
  \item{d0}{
%%     ~~Describe \code{d0} here~~
}
  \item{d1}{
%%     ~~Describe \code{d1} here~~
}
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (q0 = 0, q1 = 0.555, b0 = 5.199, b1 = 2.933, d0 = 5.433, 
    d1 = 2.319) 
{
    quartz()
    ani.start(nmax = 1000, interval = 1, title = "Diversification and Character Evolution")
    bisse.ani(q0 = q0, q1 = q1, b0 = b0, b1 = b1, d0 = d0, d1 = d1)
    ani.stop()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
