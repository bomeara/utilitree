\name{summarizeNode}
\alias{summarizeNode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summarizeNode(nodeId, focalTree, sourceTreeList, print.progress)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nodeId}{
%%     ~~Describe \code{nodeId} here~~
}
  \item{focalTree}{
%%     ~~Describe \code{focalTree} here~~
}
  \item{sourceTreeList}{
%%     ~~Describe \code{sourceTreeList} here~~
}
  \item{print.progress}{
%%     ~~Describe \code{print.progress} here~~
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
function (nodeId, focalTree, sourceTreeList, print.progress) 
{
    matchingVector <- unlist(lapply(sourceTreeList, isClade, 
        descendants(focalTree, nodeId, type = "tips")))
    proportion <- sum(matchingVector)/length(matchingVector)
    lengths <- unlist(lapply(sourceTreeList[matchingVector], 
        edgeLengthTaxset, descendants(focalTree, nodeId, type = "tips")))
    result <- c(nodeId, NA, NA, NA, NA)
    if (sum(is.na(lengths)) < length(lengths)) {
        result <- c(nodeId, proportion, mean(lengths, na.rm = TRUE), 
            median(lengths, na.rm = TRUE), sd(lengths, na.rm = TRUE))
    }
    if (print.progress) {
        print(result)
    }
    names(result) <- c("nodeId", "proportion", "mean_brlen", 
        "median_brlen", "sd_brlen")
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
