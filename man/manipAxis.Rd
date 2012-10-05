\name{manipAxis}
\alias{manipAxis}
\title{
Modify the display of axes and nodes in a hive plot
}
\description{
This function modifies various aspects of a \code{HivePlotData} object.  A typical use is to convert the radii from the native/absolute values in the original object to either a normalized value (0\ldots1) or to a ranked value.  The order of nodes on an axis can also be inverted, and an axis can be pruned (removed) from the \code{HivePlotData} object.
}
\usage{
manipAxis(HPD, method, action = NULL, ...)
}
\arguments{
  \item{HPD}{
An object of S3 class \code{HivePlotData}. }
  \item{method}{
One of \code{c("rank", "norm", "scale", "invert", "ranknorm", "prune")} giving the type of modification to be made.}
  \item{action}{
For \code{method = c("scale", "invert")}, a numeric vector of the same length as the number of axes.  For \code{"scale"} node radii will be multiplied by the corresponding value in this argument.  For \code{"invert"} a value of -1 will cause the corresponding axis to be inverted.  For \code{"prune"}, a single value specifying the axis to be pruned should be given.
}
  \item{...}{
Arguments to be passed downstream.  Needed in this case for when \code{plotHive} has arguments for \code{grid} that get laundered through \code{manipAxis} }
}
\details{
The rank method uses \code{ties.method = "first"} so that each node gets a unique radius.  For pruning, the nodes and edges are removed and then the remaining axes are renumbered to start from one.  Exercise caution!
}
\value{
A modified \code{HivePlotData} object.
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
set.seed(55)
test3 <- ranHiveData(nx = 3)
plotHive(test3)
plotHive(test3, method = "rank")
plotHive(test3, method = "norm")
plotHive(test3, method = "invert", action = c(-1, 1, 1))
#plotHive(test3, method = "prune", action = 1)
}
\keyword{ utilities }
