\name{find.cherries}
\alias{find.cherries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
find.cherries(phy)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phy}{
%%     ~~Describe \code{phy} here~~
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
function (phy) 
{
    focalNodes <- which(tabulate(phy$edge[which(phy$edge[, 2] <= 
        Ntip(phy)), 1]) == 2)
    returnTaxa <- function(focalNode, phy) {
        return(c(phy$edge[which(phy$edge[, 1] == focalNode), 
            2]))
    }
    returnBrlen <- function(focalTaxon, phy) {
        return(phy$edge.length[which(phy$edge[, 2] == focalTaxon)])
    }
    focalTaxa <- sapply(focalNodes, returnTaxa, phy)
    focalBrlen <- matrix(sapply(focalTaxa, returnBrlen, phy), 
        ncol = 2, byrow = TRUE)
    focalTaxa <- matrix(as.character(phy$tip.label[c(focalTaxa)]), 
        ncol = 2, byrow = TRUE)
    result <- data.frame(focalTaxa, focalBrlen, stringsAsFactors = FALSE)
    names(result) <- c("taxon1", "taxon2", "brlen1", "brlen2")
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
