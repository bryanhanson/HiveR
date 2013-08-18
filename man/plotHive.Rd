\name{plotHive}
\alias{plotHive}
\alias{plot3dHive}
\title{
Create a hive plot (2D or 3D)
}
\description{
These functions plot a \code{HivePlotData} object in either 2D or 3D, depending upon which function is called.
}
\usage{
plotHive(HPD, ch = 1, method = "abs", dr.nodes = TRUE, bkgnd = "black",
	axLabs = NULL, axLab.pos = NULL, axLab.gpar = NULL,
	anNodes = NULL, anNode.gpar = NULL,
	arrow = NULL, np = TRUE, ...)


plot3dHive(HPD, ch = 1, dr.nodes = TRUE, method = "abs",
	axLabs = NULL, axLab.pos = NULL, LA = FALSE, ...)
}
\arguments{
  \item{HPD}{
An object of S3 class \code{\link{HivePlotData}}.
}
  \item{ch}{
Numeric; the size of the central hole in the hive plot.
}
  \item{method}{
Character.  Passed to \code{\link{manipAxis}} (see there for allowed values).
}
\item{dr.nodes}{
Logical; if \code{TRUE} nodes will be drawn.
}
\item{bkgnd}{
Any valid color specification.  Used for the background color for \code{plotHive}.
}
\item{axLabs}{
A vector of character strings for the axis labels.
}
\item{axLab.pos}{
Numeric; An offset from the end of the axis for label placement.  Either a single value or a vector of values.  If a single value, all labels are offset the same amount.  If a vector of values, there should be a value for each axis.  This allows flexibility with long axis names.  The units depend upon the \code{method} employed.
}
\item{axLab.gpar}{
(Applies to \code{plotHive} only) A list of name - value pairs acceptable to \code{\link{gpar}}.  These control the label and arrow displays.  See the examples.  Note that if you use this argument the default color for text and arrows becomes black, so if \code{bkgnd = "black"} be sure to specify \code{col = "white"} (or some other non-black color) or you won't be able to see your labels.
}
\item{anNodes}{
(Applies to \code{plotHive} only) The path to a csv file containing information for labeling nodes.  If present, a line segment will be drawn from the node to the specified text.  The text is positioned near the end of a radial vector.  The columns in the csv file must be named as follows (description and use in parentheses): node.lab (node label from HPD$nodes$lab), node.text (the text to be drawn on the plot), angle (polar coordinates: angle at which to draw the text), radius (polar coordinates: radius at which to draw the text), offset (additional distance along radius vector to offset text), hjust, vjust (horizontal and vertical justification; nominally 0 or 1 but fractional and negative values also work).
}
\item{anNode.gpar}{
(Applies to \code{plotHive} only) A list of name - value pairs acceptable to \code{\link{gpar}} for use with \code{plotHive} only.  These control both the text used to annotate the nodes and the line segments connecting that text to the node.  See the examples.  Note that if you use this argument the default color for text and arrows becomes black, so if \code{bkgnd = "black"} be sure to specify \code{col = "white"} (or some other non-black color) or you won't be able to see your labels.
}

\item{np}{
(Applies to \code{plotHive} only) Logical; should a new device (page) be opened when drawing the hive plot?  If you are making multiple plots within some sort of \code{grid} manipulations (e.g. a hive plot per the vignette) then this should be set to \code{FALSE}.
}
\item{arrow}{
(Applies to \code{plotHive} only) A vector of 5 or 6 values: a character string to label the arrow, and 4 numeric values giving the angle of the arrow, the radius at which to start the arrow, the radius at which to end the arrow, and a value to offset the arrow label from the end of the arrow.  A 5th numeric value (the 6th argument overall) can specify an offset in the y direction for the arrow when \code{nx = 2}.  See the examples.
}

  \item{LA}{
Logical: should splines be drawn with \code{line_antialias = TRUE}?  Only applies to \code{plot3dHive}.
}

  \item{\dots}{
Additional parameters to be passed downstream.
}
}
\details{
\code{plotHive} uses \code{grid} graphics to produce a 2D hive plot in a style similar to the original concept.  For a 2D plot, axis number 1 is vertical except in the case of 2 axes.  \code{plot3dHive} produces a 3D hive plot using \code{rgl} graphics.
\cr
\cr
For most work with \code{plot3dHive}, use \code{LA = FALSE} for speed of drawing.  \code{LA = TRUE} is over 20 times slower, and is more appropriate for high quality hive plots.  These are probably better made with \code{R CMD BATCH script.R} rather than interactive use.
\cr
\cr
Using \code{anNodes} gives a lot of flexibility to positioning annotation text, but it may take quite a few iterations if you are perfectionistics.  Prepare to be patient if you plan to label quite a few nodes.  I suggest beginning with \code{offset, hjust and vjust} set to 0.  There may be some changes to this in the near future.
}
\value{
None.  Side effect is a plot.
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
test2 <- ranHiveData(nx = 2)
test3 <- ranHiveData(nx = 3)
test4 <- ranHiveData(nx = 4, type = "3D")
#
# plot one with defaults:
plotHive(test3)
#
# Add axis labels & options to nx = 3 example.  Note that rot is not part of gpar
require("grid")
plotHive(test3, ch = 5, labs = c("axis 1", "axis 2", "axis 3"),
lab.pos = c(10, 15, 15), lab.gpar = gpar(col = "orange", fontsize = 14),
rot = c(0, 30, -30))
#
# Now the nx = 2 case.
# Note that gpar contains parameters that apply to both the
# axis labels and arrow. A 6th value in arrow offsets the arrow vertically:
plotHive(test2, ch = 5, labs = c("axis 1", "axis 2"), rot = c(-90, 90),
lab.pos = c(20, 20), lab.gpar = gpar(col = "pink", fontsize = 14, lwd = 2),
arrow = c("radius units", 0, 20, 60, 25, 40))

# The following must be run interactively:
\dontrun{
plot3dHive(test4)
}
}
\keyword{ plot }
\keyword{ hplot }
\keyword{ interactive }
