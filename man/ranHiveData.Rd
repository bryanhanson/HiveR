\name{ranHiveData}
\alias{ranHiveData}
\title{
Generate random hive plot data
}
\description{
This function generates random data sets which can be used to make a hive plot.
}
\usage{
ranHiveData(type = "2D", nx = 4, nn = nx * 15, ne = nx * 15, rad = 1:100,
	ns = c(0.5, 1.0, 1.5), ew = 1:3, nc = brewer.pal(5, "Set1"),
	ec = brewer.pal(5, "Set1"), axis.cols = brewer.pal(nx, "Set1"),
	desc = NULL, allow.same = FALSE, verbose = FALSE)
}
\arguments{
  \item{type}{
The type of hive plot to be generated.  One of \code{c("2D", "3D")}.
}
  \item{nx}{
An integer giving the number of axes to be created (\code{2 =< nx =< 6}).
}
  \item{nn}{
An integer giving the number of nodes to be created.  This is an initial number which may be reduced during clean up.  See Details.
}
  \item{ne}{
An integer giving the number of edges to be created.  This is an initial number which may be reduced during clean up.  See Details.
}
  \item{rad}{
Numeric; a range of values that will be used as node radius values (the position of the node along the axis).
}
  \item{ns}{
Numeric; a range of values that will be used as the node sizes.
}
  \item{ew}{
Numeric; a range of values that will be used as the edge weights.
}
  \item{nc}{
A vector of valid color names giving the node colors.
}
  \item{ec}{
A vector of valid color names giving the edge colors.
}
  \item{axis.cols}{
A vector of valid color names to be used to color the axes; \code{length(axis.cols) must = nx}.
}
  \item{desc}{
Character; a description of the data set.
}
  \item{allow.same}{
Logical; indicates if edges may begin and end on the same axis. Only applies to \code{type = 2D}.
}

  \item{verbose}{
Logical; If \code{TRUE}, the generation, processing and final result is reported to the console.
}
}
\details{
For \code{type = "2D"}, after the function creates an initial set of random nodes, these are randomly chosen and connected between adjacent axes, so that no edge crosses an axis.
\cr
\cr
For \code{type = "3D"}, after the function creates an initial set of random nodes and edges, these are cleaned up by removing the following cases (which the rest of \code{HiveR} is not intended to handle at this time): duplicated nodes, nodes that are not part of any edge, edges that begin and end on the same point, edges that begin and end on the same axis, and finally, for \code{nx = 5 or 6}, edges that begin and end on colinear axes.  Most of these don't cause an error, but produce some ugly results.
\cr
\cr
For the arguments \code{rad, ns, ew, nc} and \code{ec}, the values given are sampled randomly (with replacement) and assigned to particular nodes or edges.
}
\section{Warning}{
If you create a very small data set with few nodes, there may be no nodes assigned to some axes which will give an error when you try to plot the data.  It's up to the user to check for this possibility (you can use \code{sumHPD}).
}
\value{
An object of S3 class \code{\link{HivePlotData}}.
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}

\examples{
test4 <- ranHiveData(nx = 4)
str(test4)
sumHPD(test4)
}
\keyword{ datagen }
