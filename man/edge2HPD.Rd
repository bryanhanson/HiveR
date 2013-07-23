\name{edge2HPD}
\alias{edge2HPD}
\title{
Process an edge list into a HivePlotData object
}
\description{
This function will take an edge list and convert it into a basic \code{\link{HivePlotData}} object.  Further manipulation by \code{\link{mineHPD}} will almost certainly be required before the data can be plotted.
}
\usage{
edge2HPD(edge_df = NULL, axis.cols = NULL, type = "2D", desc = NULL, ...)
}
\arguments{
  \item{edge_df}{
A data frame containing edge list information. Columns should be node1, node2, edge weight (column names are arbitrary).  Edge weight information is optional. If missing, edge weights will be set to 1.
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
This function produces a "bare bones" \code{HivePlotData} object.  The user will likely have to make some changes manually to the resulting \code{HivePlotData} object before plotting.  Alternatively, \code{\link{mineHPD}} may be able to extract some information buried in the data, but even then, the user might need to make some adjustments.  See the examples. 
}
\seealso{ \code{\link{dot2HPD}} and \code{link{adj2HPD}}
}
\value{
A \code{\link{HivePlotData}} object.
}
\author{
Jonathan H. Chung, with minor changes for consistency by Bryan A. Hanson.
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}
\examples{
# Create a simple edge list & process it
set.seed(31)
ne <- 20 # no. of edges
el <- data.frame(node1 = sample(c(letters, LETTERS), ne, replace = FALSE),
  node2 = sample(c(letters, LETTERS), ne, replace = TRUE),
  stringsAsFactors = FALSE)
el <- unique(el)
el$weight = sample(1:5, nrow(el), replace = TRUE)
hive1 <- edge2HPD(el)
sumHPD(hive1)
}
\keyword{ utilities }