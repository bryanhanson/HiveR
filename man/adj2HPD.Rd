\name{adj2HPD}
\alias{adj2HPD}
\title{
Process an adjacency graph into a HivePlotData object
}
\description{
This function will take an adjacency graph and convert it into a basic \code{\link{HivePlotData}} object.  Further manipulation will almost certainly be required before the data can be plotted.
}
\usage{
adj2HPD(M = NULL, axis.cols = NULL, type = "2D", desc = NULL, ...)
}
\arguments{
  \item{M}{
A matrix with named dimensions.  The names should be the node names.
}
  \item{axis.cols}{
A character vector giving the colors desired for the axes.
}
  \item{type}{
One of \code{c("2D", "3D")}.  If \code{2D}, a \code{HivePlotData} object suitable for use with \code{\link{plotHive}} will be created and the eventual hive plot will be static and 2D.  If \code{3D}, the \code{HivePlotData} object will be suitable for a 3D interactive plot using \code{\link{plot3dHive}}.
}
  \item{desc}{
Character.  A description of the data set.
}
  \item{\dots}{
Other parameters to be passed downstream.
}
}
\details{
This function produces a "bare bones" \code{HivePlotData} object.  The names of the dimensions of \code{M} are used as the node names.  All nodes are given size 1, an id number (\code{1:number of nodes}), are colored black and are assigned to axis 1.  The edges are all gray, and the weight is M[i,j].  The user will likely have to manually make some changes to the resulting \code{HivePlotData} object before plotting.  Alternatively, \code{\link{mineHPD}} may be able to extract some information buried in the data, but even then, the user will probably need to make some adjustments.  See the examples. 
}
\seealso{\code{\link{dot2HPD}}
}
\value{
A \code{
\link{HivePlotData}} object which is capable of more sophisticated and flexible processing of a .dot file if the file contains tags.
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}
\examples{
### Note: this example has questionable scientific value!
### The purpose is to show how to troubleshoot and
### manipulate a HivePlotData object.

set.seed(31)
require(bipartite)
data(Safariland)

# You may wish to do ?Safariland or ?Safari for background

hive1 <- adj2HPD(Safariland, desc = "Safariland data set from bipartite")
sumHPD(hive1)

# Note that all nodes are one axis with radius 1. Process further:

hive2 <- mineHPD(hive1, option = "rad <- tot.edge.count")
sumHPD(hive2)

# All nodes still on 1 axis but degree has been used to set radius

# Process further:

hive3 <- mineHPD(hive2, option = "axis <- source.man.sink")
sumHPD(hive3, chk.all = TRUE)

# Note that mineHPD is generating some warnings, telling us
# that the first 9 nodes were not assigned to an axis.  Direct
# inspection of the data shows that these nodes are insects
# that did not visit any of the flowers in this particular study.

# Pretty up a few things, then plot:

hive3$edges$weight <- sqrt(hive3$edges$weight)*0.5
hive3$nodes$size <- 0.5
plotHive(hive3)

# This is a one-sided hive plot of 2 axes, which results
# from the curvature of the splines.  We can manually fix
# this by reversing the ends of edges as follows:

for (n in seq(1, length(hive3$edges$id1), by = 2)) {
	a <- hive3$edges$id1[n]
	b <- hive3$edges$id2[n]
	hive3$edges$id1[n] <- b
	hive3$edges$id2[n] <- a
	}

plotHive(hive3)
}
\keyword{ utilities }