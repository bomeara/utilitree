\name{bisse.ani}
\alias{bisse.ani}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bisse.ani(q0 = 0, q1 = 0.555, b0 = 5.199, b1 = 2.933, d0 = 5.433, d1 = 2.319, starting0 = 0, starting1 = 1, colorVector = c(rgb(0, 1, 0.2, 0.9), rgb(0.3, 0, 1, 0.7), rgb(0, 1, 0.2, 0.2), rgb(0.3, 0, 1, 0.2)), radius = 0.1, drawtomutatecount = 2, allowExtinction = FALSE, subsampleAt = 100)
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
  \item{starting0}{
%%     ~~Describe \code{starting0} here~~
}
  \item{starting1}{
%%     ~~Describe \code{starting1} here~~
}
  \item{colorVector}{
%%     ~~Describe \code{colorVector} here~~
}
  \item{radius}{
%%     ~~Describe \code{radius} here~~
}
  \item{drawtomutatecount}{
%%     ~~Describe \code{drawtomutatecount} here~~
}
  \item{allowExtinction}{
%%     ~~Describe \code{allowExtinction} here~~
}
  \item{subsampleAt}{
%%     ~~Describe \code{subsampleAt} here~~
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
    d1 = 2.319, starting0 = 0, starting1 = 1, colorVector = c(rgb(0, 
        1, 0.2, 0.9), rgb(0.3, 0, 1, 0.7), rgb(0, 1, 0.2, 0.2), 
        rgb(0.3, 0, 1, 0.2)), radius = 0.1, drawtomutatecount = 2, 
    allowExtinction = FALSE, subsampleAt = 100) 
{
    colorMatrix <- matrix(nrow = 2, ncol = 2)
    colorMatrix[1, ] = colorVector[1:2]
    colorMatrix[2, ] = colorVector[3:4]
    qrate <- c(q0, q1)/10
    qcounts <- c(0, 0)
    brate <- c(b0, b1)/10
    bcounts <- c(0, 0)
    drate <- c(d0, d1)/10
    dcounts <- c(0, 0)
    labelText <- c("c", "i")
    drawtomutatecount = round(drawtomutatecount)
    makePoint <- function(x = 0, y = 0, state = 0, pointAge = 0, 
        stateAge = 0, extant = 1) {
        pointVector <- c(x, y, x, y, state, pointAge, stateAge, 
            extant)
        return(pointVector)
    }
    movePoint <- function(point, pointsMatrix = pointsMatrix, 
        flee.scaling = 1) {
        x.velocity = 0
        y.velocity = 0
        x.me <- pointsMatrix[point, 1]
        y.me <- pointsMatrix[point, 2]
        for (otherpoint in 1:dim(pointsMatrix)[1]) {
            if (point != otherpoint) {
                x.other <- pointsMatrix[otherpoint, 1]
                y.other <- pointsMatrix[otherpoint, 2]
                distance <- min(1000, sqrt(((x.other - x.me)^2) + 
                  ((y.other - y.me)^2)), na.rm = TRUE)
                if (distance <= radius * 2) {
                  x.velocity <- x.velocity + (x.me - x.other)/distance
                  y.velocity <- y.velocity + (y.me - y.other)/distance
                }
            }
        }
        if (pointsMatrix[point, 8] == 1) {
            x.me <- pointsMatrix[point, 1]
            y.me <- pointsMatrix[point, 2]
            x.goal <- -2
            if (pointsMatrix[point, 5] == 1) {
                x.goal <- 2
            }
            y.goal = 0
            distance <- sqrt(((x.me - x.goal)^2) + ((y.me - y.goal)^2))
            x.velocity <- x.velocity - (x.me - x.goal) * distance
            y.velocity <- y.velocity - (y.me - y.goal) * distance
        }
        else {
            y.me <- max(-5, pointsMatrix[point, 2], na.rm = TRUE)
            y.goal <- -4
            if (y.me > y.goal) {
                y.velocity <- y.velocity - 1
            }
        }
        pointsMatrix[point, 3] <- x.velocity * flee.scaling
        pointsMatrix[point, 4] <- y.velocity * flee.scaling
        return(pointsMatrix)
    }
    moveAllPoints <- function(pointsMatrix = pointsMatrix, velocity.max = 0.2) {
        for (point in 1:dim(pointsMatrix)[1]) {
            pointsMatrix <- movePoint(point, pointsMatrix)
        }
        for (point in 1:dim(pointsMatrix)[1]) {
            pointsMatrix[point, 3] <- sign(pointsMatrix[point, 
                3]) * min(abs(velocity.max), abs(pointsMatrix[point, 
                3]))
            pointsMatrix[point, 4] <- sign(pointsMatrix[point, 
                4]) * min(abs(velocity.max), abs(pointsMatrix[point, 
                4]))
            pointsMatrix[point, 1] <- pointsMatrix[point, 3] + 
                pointsMatrix[point, 1]
            pointsMatrix[point, 2] <- pointsMatrix[point, 4] + 
                pointsMatrix[point, 2]
        }
        for (point in 1:dim(pointsMatrix)[1]) {
            pointsMatrix[point, 2] <- max(-4, pointsMatrix[point, 
                2])
            pointsMatrix[point, 1] <- max(-4, pointsMatrix[point, 
                1])
            pointsMatrix[point, 1] <- min(4, pointsMatrix[point, 
                1])
        }
        return(pointsMatrix)
    }
    plotPoints <- function(pointsMatrix = pointsMatrix) {
        plot(x = c(-4, 4), y = c(-4, 4), bty = "n", xlab = "", 
            ylab = "", type = "n", xlim = c(-4, 4), ylim = c(-4, 
                4), xaxt = "n", yaxt = "n")
        for (point in 1:dim(pointsMatrix)[1]) {
            symbols(x = pointsMatrix[point, 1], y = pointsMatrix[point, 
                2], fg = "black", bg = colorMatrix[abs(2 - pointsMatrix[point, 
                8]), 1 + pointsMatrix[point, 5]], circles = radius, 
                add = TRUE, inches = FALSE)
            text(x = pointsMatrix[point, 1], y = pointsMatrix[point, 
                2], labels = labelText[1 + pointsMatrix[point, 
                5]], col = "white")
        }
        text(x = -2, y = 4, labels = c(paste("0=SC\nq01=", q0, 
            "\nb0=", b0, "\nd0=", d0, sep = "")), adj = c(0.5, 
            1))
        text(x = 2, y = 4, labels = c(paste("1=SI\nq10=", q1, 
            "\nb1=", b1, "\nd1=", d1, sep = "")), adj = c(0.5, 
            1))
        if (max(pointsMatrix[, 8]) == 0) {
            text(x = 0, y = 0, labels = c("Everything\nExtinct!"), 
                adj = c(0.5, 0.5), cex = 2)
        }
    }
    mutatePoints <- function(pointsMatrix = pointsMatrix) {
        for (point in 1:dim(pointsMatrix)[1]) {
            if (pointsMatrix[point, 8] == 1) {
                point.state <- pointsMatrix[point, 5]
                point.reversestate <- 0
                if (point.state == 0) {
                  point.reversestate <- 1
                }
                if (runif(1, 0, 1) < qrate[(point.state + 1)]) {
                  pointsMatrix[point, 5] <- point.reversestate
                  pointsMatrix[point, 7] = 0
                  qcounts[(point.state + 1)] <- qcounts[(point.state + 
                    1)] + 1
                }
            }
        }
        return(pointsMatrix)
    }
    birthdeathPoints <- function(pointsMatrix = pointsMatrix) {
        for (point in 1:dim(pointsMatrix)[1]) {
            if (pointsMatrix[point, 8] == 1) {
                point.state <- pointsMatrix[point, 5]
                randomdraw <- runif(1, 0, 1)
                if (randomdraw < brate[(point.state + 1)]) {
                  pointsMatrix[point, 6] = 0
                  pointsMatrix <- rbind(pointsMatrix, makePoint(x = (pointsMatrix[point, 
                    1] + runif(1, -0.1, 0.1)), y = (pointsMatrix[point, 
                    2] + runif(1, -0.1, 0.1)), state = point.state))
                  bcounts[(point.state + 1)] <- bcounts[(point.state + 
                    1)] + 1
                }
                else {
                  if (randomdraw < (brate[(point.state + 1)] + 
                    drate[(point.state + 1)])) {
                    if (sum(pointsMatrix[, 8]) > 1 || allowExtinction) {
                      pointsMatrix[point, 8] = 0
                      dcounts[(point.state + 1)] <- dcounts[(point.state + 
                        1)] + 1
                    }
                  }
                }
            }
        }
        return(pointsMatrix)
    }
    i <- 1
    timesteps <- c()
    pointsMatrix <- matrix(nrow = starting0 + starting1, ncol = 8)
    colnames(pointsMatrix) <- c("x_now", "y_now", "x_velocity", 
        "y_velocity", "state", "pointAge", "stateAge", "extant")
    for (point in 1:starting0) {
        pointsMatrix[point, ] <- makePoint(x = runif(1, -2, 0), 
            y = runif(1, -1, 1), state = 0)
    }
    for (point in (starting0 + 1):(starting0 + starting1)) {
        pointsMatrix[point, ] <- makePoint(x = runif(1, 0, 2), 
            y = runif(1, -1, 1), state = 1)
    }
    for (initialmove in 1:10) {
        pointsMatrix <- moveAllPoints(pointsMatrix)
    }
    while (i <= ani.options("nmax")) {
        if (floor(i/drawtomutatecount) == ceiling(i/drawtomutatecount)) {
            pointsMatrix <- mutatePoints(pointsMatrix)
            pointsMatrix <- birthdeathPoints(pointsMatrix)
        }
        pointsMatrix <- moveAllPoints(pointsMatrix)
        if (dim(pointsMatrix)[1] > subsampleAt) {
            pointsMatrix <- pointsMatrix[-1 * seq(1, dim(pointsMatrix)[1], 
                2), ]
        }
        print(i)
        plotPoints(pointsMatrix)
        Sys.sleep(ani.options("interval"))
        i <- i + 1
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
