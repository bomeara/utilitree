\name{heritableTransform}
\alias{heritableTransform}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
heritableTransform(phy, rate = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phy}{
%%     ~~Describe \code{phy} here~~
}
  \item{rate}{
%%     ~~Describe \code{rate} here~~
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
function (phy, rate = 1) 
{
    tips <- abs(1 + sim.char(phy, matrix(rate), model = "brownian", 
        nsims = 1))
    ancstates <- (ace(tips, phy))$ace
    ancstates <- ancstates[-1]
    matches <- match(as.numeric(names(ancstates)), phy$edge[, 
        2])
    phy$edge.length[matches] <- phy$edge.length[matches] * ancstates
    tip.id <- match(row.names(tips), phy$tip.label)
    matches <- match(tip.id, phy$edge[, 2])
    phy$edge.length[matches] <- phy$edge.length[matches] * tips
    return(phy)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
