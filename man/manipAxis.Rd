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
One of \code{c("rank", "norm", "scale", "invert", "ranknorm", "prune", "offset", "stretch")} giving the type of modification to be made.}
  \item{action}{
For \code{method = c("scale", "invert", "offset", "stretch")}, a numeric vector of the same length as the number of axes.
}
  \item{...}{
Arguments to be passed downstream.  Needed in this case for when \code{plotHive} has arguments for \code{grid} that get laundered through \code{manipAxis} }
}
\details{
The rank method uses \code{ties.method = "first"} so that each node gets a unique radius.  For pruning, the nodes and edges are removed and then the remaining axes are renumbered to start from one.  Exercise caution!

For \code{"scale"} node radii will be multiplied by the corresponding value in this argument.  For \code{"invert"} a value of -1 will cause the corresponding axis to be inverted.  For \code{"prune"}, a single value specifying the axis to be pruned should be given.  For \code{"offset"} the values in \code{"action"} will be subtracted from the node radii.  For \code{"stretch"}, node radii will first be offset so that the minimum value is zero, then multiplied by the values in \code{"action"} to stretch the axis.  Depending upon the desired effect, one might use \code{"stretch"} followed by \code{"offset"} or perhaps other combinations.  


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
data(HEC)
# The first 3 examples take advantage of the argument '...'
# in plotHive, which passes action through to manipAxis on the fly.
# For this particular data, norm and absolute scaling appear the same.

plotHive(HEC, bkgnd = "white") # default is absolute positioning of nodes
plotHive(HEC, method = "rank", bkgnd = "white")
plotHive(HEC, method = "norm", bkgnd = "white")

# In these examples, we'll explicitly use manipAxis and then plot
# in a separate step.  This is because trying to plot on the fly in
# these cases will result in absolute scaling (which we do use here,
# but one might not want to be forced to do so).

HEC2 <- manipAxis(HEC, method = "invert", action = c(-1, 1))
plotHive(HEC2, bkgnd = "white")
HEC3 <- manipAxis(HEC, method = "stretch", action = c(2,3))
plotHive(HEC3, bkgnd = "white")
HEC4 <- manipAxis(HEC, method = "offset", action = c(0, 1.5))
plotHive(HEC4, bkgnd = "white")
}
\keyword{ utilities }
