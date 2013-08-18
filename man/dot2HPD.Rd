\name{dot2HPD}
\alias{dot2HPD}
\title{
Process a .dot graph file into a HivePlotData object
}
\description{
This function will read a .dot file containing a graph specification in the DOT language, and using two other files, convert the information into a \code{\link{HivePlotData}} object.
}
\usage{
dot2HPD(file = NULL, node.inst = NULL, edge.inst = NULL, axis.cols = NULL,
	type = "2D", desc = NULL, ...)
}
\arguments{
  \item{file}{
The path to the .dot file to be processed.
}
  \item{node.inst}{
The path to a .csv file containing instructions about how to map node tags in the .dot file to parameters in the \code{HivePlotData} object. May be NULL.
}
  \item{edge.inst}{
The path to a .csv file containing instructions about how to map edge tags in the .dot file to parameters in the \code{HivePlotData} object.  May be NULL.
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
This function is currently agnostic with respect to whether or not the .dot graph is directed or not.  Either type will be processed, but if the graph is directed, this will only be indirectly stored in the \code{HivePlotData} object (in that the first node of an edge in the .dot file will be in \code{HPD$nodes$id1} and the second node of an edge will be in \code{HPD$nodes$id2}.  This fact can be used; see the vignette and \code{\link{mineHPD}}.  Keep in mind the .dot standard is fairly loose.  This function has been tested to work with several .dot files, include those with multiple tag=value attributes.  If you have trouble, please file a issue at Github so I can track it down.
}
\value{
A \code{\link{HivePlotData}} object.
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}
\seealso{
See the vignette for an example of using this function.  Use \code{> vignette("HiveR")} to produce the vignette.
\cr
\cr
\code{\link{adj2HPD}} for a means of importing adjacency matrices.
}
\keyword{ utilities }