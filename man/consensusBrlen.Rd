\name{consensusBrlen}
\alias{consensusBrlen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
consensusBrlen(focalTree, sourceTreeList, type = c("proportion", "mean_brlen", "median_brlen", "sd_brlen"), print.progress = TRUE, return.val = "tree")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{focalTree}{
%%     ~~Describe \code{focalTree} here~~
}
  \item{sourceTreeList}{
%%     ~~Describe \code{sourceTreeList} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{print.progress}{
%%     ~~Describe \code{print.progress} here~~
}
  \item{return.val}{
%%     ~~Describe \code{return.val} here~~
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
function (focalTree, sourceTreeList, type = c("proportion", "mean_brlen", 
    "median_brlen", "sd_brlen"), print.progress = TRUE, return.val = "tree") 
{
    type <- match.arg(type)
    if (class(focalTree) != "phylo4") {
        focalTree <- as(focalTree, "phylo4")
    }
    if (class(sourceTreeList[[1]]) != "phylo4") {
        sourceTreeList <- lapply(sourceTreeList, as, "phylo4")
    }
    allNodes <- nodeId(focalTree, "all")
    allNodes <- allNodes[which(allNodes != nodeId(focalTree, 
        "root"))]
    if (print.progress) {
        print(c("nodeId", "proportion", "mean_brlen", "median_brlen", 
            "sd_brlen"))
    }
    allResults <- sapply(allNodes, summarizeNode, focalTree, 
        sourceTreeList, print.progress)
    if (return.val == "tree") {
        newEdgeLengths <- edgeLength(focalTree)
        newNodeLabels <- nodeLabels(focalTree)
        for (nodeIndex in 1:length(allNodes)) {
            newLength <- allResults[which(row.names(allResults) == 
                type), nodeIndex]
            if (is.na(newLength)) {
                newLength = 0
            }
            newEdgeLengths[which(names(newEdgeLengths) == getEdge(focalTree, 
                allNodes[nodeIndex]))] <- newLength
            newNodeLabels[which(names(newNodeLabels) == allNodes[nodeIndex])] <- round(allResults[which(row.names(allResults) == 
                "proportion"), nodeIndex], 2)
        }
        edgeLength(focalTree) <- newEdgeLengths
        nodeLabels(focalTree) <- newNodeLabels
        return(focalTree)
    }
    else {
        return(allResults)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
