\name{resolveNames}
\alias{resolveNames}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
resolveNames(names, maxPerCall = Inf, verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{names}{
%%     ~~Describe \code{names} here~~
}
  \item{maxPerCall}{
%%     ~~Describe \code{maxPerCall} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (names, maxPerCall = Inf, verbose = TRUE) 
{
    names <- sapply(names, sub, pattern = "_", replacement = " ")
    names <- sapply(names, URLencode)
    callBase <- "http://tnrs.iplantc.org/tnrsm-svc/matchNames?retrieve=best&names="
    newNames <- rep(NA, length(names))
    namesInCall <- 0
    callActual <- callBase
    startingPosition <- 1
    for (nameIndex in sequence(length(names))) {
        namesInCall <- namesInCall + 1
        callActual <- paste(callActual, names[nameIndex], ",", 
            sep = "")
        if (namesInCall == maxPerCall || nameIndex == length(names)) {
            returnedValues <- fromJSON(file = callActual)$items
            for (returnIndex in sequence(length(returnedValues))) {
                newNames[startingPosition + returnIndex - 1] <- returnedValues[[returnIndex]]$nameScientific
            }
            if (verbose) {
                print(paste("finished ", nameIndex, "of ", length(names), 
                  "names"))
            }
            startingPosition <- nameIndex + 1
            namesInCall <- 0
            callActual <- callBase
        }
    }
    print("Ignore a warning message about incomplete final line")
    if (length(newNames) != length(names)) {
        warning(paste("the input name list was", length(names), 
            "long but the new one is ", length(newNames), "long"))
    }
    return(newNames)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
