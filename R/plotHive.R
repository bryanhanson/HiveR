#' Create a hive plot (2D or 3D)
#' 
#' These functions plot a \code{HivePlotData} object in either 2D or 3D,
#' depending upon which function is called.
#' 
#' General.  \code{plotHive} uses \code{grid} graphics to produce a 2D hive
#' plot in a style similar to the original concept.  For a 2D plot, axis number
#' 1 is vertical except in the case of 2 axes in which case it is to the right.
#' \code{plot3dHive} produces a 3D hive plot using \code{rgl} graphics.
#' Functions from either package can be used to make additional modifications
#' after the hive plot is drawn, either via the \ldots{} argument or by
#' subsequent function calls.  See the examples. \cr \cr Units and Annotations.
#' If you add node labels, arrows or graphic decorations, the units that you
#' must specify are those intrinsic to the data itself, modified by your
#' setting of \code{ch} and \code{method}.  These generally cannot be known
#' precisely ahead of time, so some experimentation will be necessary to polish
#' the plots.  For instance, if you have data with node radii that run from
#' 4-23 then you have an idea of how to position your annotations if using
#' \code{method = "abs"}.  But the same data plotted with \code{method =
#' "norm"} or \code{method = "rank"} will require that you move your annotation
#' positions accordingly.  In the first case no radius is larger than 23, but
#' the maximum radius is 1 when the data is normed and when it is ranked, the
#' maximum value will depend upon which axis has the most nodes on it, and the
#' number of unique radii values. \cr \cr Positioning Node Labels and Graphics.
#' In addition to the nuances just above, there are two ways to specify the
#' location of node labels and graphic decorations.  Polar coordinates are used
#' in both cases.  If \code{annCoord = "local"} then the angle, radius and
#' offset arguments are relative to the node to be annotated.  An angle of 0
#' positions the label horizontally to the right of the node. Thus the label
#' can be placed within a circular area around the node.  If \code{annCoord =
#' "global"} then the specifications are relative to dead center on the plot.
#' These two methods give one lots of flexibility in lining up labels in
#' different ways.  See the examples. \cr \cr Size of Graphics.  The size of
#' graphic decorations is controlled by the column 'width' in \code{grInfo}.
#' The ultimate call to display the graphic is done with \code{as.raster}.
#' Specifying only the width preserves the aspect ratio of the graphic.  See
#' \code{?as.raster} for further discussion. \cr \cr Colors.  For any of the
#' \code{gpar} arguments, watch out: In grid graphics the default color for
#' text and arrows is black, so if are using the default \code{bkgnd = "black"}
#' in the hive plot be sure to specify \code{col = "white"} (or some other
#' non-black color) for the labels and arrows or you won't see them. \cr \cr
#' Speed and 3D Hive Plots.  For most work with \code{plot3dHive}, use \code{LA
#' = FALSE} for speed of drawing.  \code{LA = TRUE} is over 20 times slower,
#' and is more appropriate for high quality hive plots.  These are probably
#' better made with \code{R CMD BATCH script.R} rather than interactive use.
#' 
#' @aliases plotHive plot3dHive
#' @param HPD An object of S3 class \code{\link{HivePlotData}}.
#' @param ch Numeric; the size of the central hole in the hive plot.
#' @param method Character.  Passed to \code{\link{manipAxis}} (see there for
#' allowed values - the default given above plots using the native or absolute
#' coordinates of the data).
#' @param dr.nodes Logical; if \code{TRUE} nodes will be drawn.
#' @param bkgnd Any valid color specification.  Used for the background color
#' for \code{plotHive}.
#' @param axLabs A vector of character strings for the axis labels.
#' @param axLab.pos Numeric; An offset from the end of the axis for label
#' placement.  Either a single value or a vector of values.  If a single value,
#' all labels are offset the same amount.  If a vector of values, there should
#' be a value for each axis.  This allows flexibility with long axis names.
#' The units depend upon the \code{method} employed (see Details).
#' @param axLab.gpar (Applies to \code{plotHive} only) A list of name - value
#' pairs acceptable to \code{\link{gpar}}.  These control the label and arrow
#' displays.  See the examples.
#' @param anNodes (Applies to \code{plotHive} only) The path to a csv file
#' containing information for labeling nodes.  If present, a line segment will
#' be drawn from the node to the specified text.  The text is positioned near
#' the end of the line segment.  The columns in the csv file must be named as
#' follows (description and use in parentheses): node.lab (node label from
#' HPD$nodes$lab), node.text (the text to be drawn on the plot), angle (polar
#' coordinates: angle at which to draw the segment), radius (polar coordinates:
#' radius at which to draw the text), offset (additional distance along the
#' radius vector to offset text), hjust, vjust (horizontal and vertical
#' justification; nominally in [0\ldots{}1] but fractional and negative values
#' also work).  The first two values will be treated as type \code{character},
#' the others as \code{numeric}.
#' @param anNode.gpar (Applies to \code{plotHive} only) A list of name - value
#' pairs acceptable to \code{\link{gpar}}.  These control both the text used to
#' annotate the nodes and the line segments connecting that text to the node.
#' See the examples.
#' @param grInfo (Applies to \code{plotHive} only) The path to a csv file
#' containing information for adding graphic decorations to the plot.  If
#' present, a line segment will be drawn from the node to the specified
#' location and the graphic is positioned near the end the line segment.  The
#' columns in the csv file must be named as follows (description and use in
#' parentheses): node.lab (node label from HPD$nodes$lab), angle (polar
#' coordinates: angle at which to position the graphic), radius (polar
#' coordinates: radius at which to position the graphic), offset (additional
#' distance along radius vector to offset the graphic), width (the width of the
#' graphic), path (a valid path to the graphics in jpg or png format).  The
#' path should include the extension is it is autodetected.  Valid extensions
#' are jpg, JPG, jpeg, JPEG, png, or PNG.  All image files must be of the same
#' type (all jpg, or all png).
#' @param arrow (Applies to \code{plotHive} only) A vector of 5 or 6 values: a
#' character string to label the arrow, and 4 numeric values giving the angle
#' of the arrow, the radius at which to start the arrow, the radius at which to
#' end the arrow, and a value to offset the arrow label from the end of the
#' arrow.  A 5th numeric value (the 6th argument overall) can specify an offset
#' in the y direction for the arrow useful when \code{nx = 2}.  See the
#' examples.
#' @param np (Applies to \code{plotHive} only) Logical; should a new device
#' (page) be opened when drawing the hive plot?  If you are making multiple
#' plots within some sort of \code{grid} scheme then this should be set to
#' \code{FALSE}.
#' @param anCoord (Applies to \code{plotHive} only) One of \code{c("local",
#' "global")}.  Controls how the position of node labels and graphic
#' decorations are specified.  See Details.
#' @param LA Applies to \code{plot3dHive} only) Logical: should splines be
#' drawn with \code{line_antialias = TRUE}? See Details.
#' @param \dots Additional parameters to be passed downstream.
#' @return None.  Side effect is a plot.
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#' @references \url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
#' @keywords plot hplot interactive
#' @importFrom grid grid.lines grid.text grid.segments grid.raster grid.newpage
#' @importFrom grid grid.rect grid.curve grid.points gpar unit viewport
#' @importFrom grid pushViewport
#' @importFrom stats na.omit
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom utils read.csv
#' @examples
#' 
#' ### 2D Hive Plots
#' require("grid")
#' # Generate some random data
#' test2 <- ranHiveData(nx = 2)
#' test3 <- ranHiveData(nx = 3)
#' 
#' # First the nx = 2 case.
#' # Note that gpar contains parameters that apply to both the
#' # axis labels and arrow. A 6th value in arrow offsets the arrow vertically:
#' plotHive(test2, ch = 5, axLabs = c("axis 1", "axis 2"), rot = c(-90, 90),
#' 	axLab.pos = c(20, 20), axLab.gpar = gpar(col = "pink", fontsize = 14, lwd = 2),
#' 	arrow = c("radius units", 0, 20, 60, 25, 40))
#' 
#' # Now nx = 3:
#' plotHive(test3) # default plot
#' 
#' # Add axis labels & options to nx = 3 example.  Note that rot is not part of gpar
#' plotHive(test3, ch = 5, axLabs = c("axis 1", "axis 2", "axis 3"),
#' 	axLab.pos = c(10, 15, 15), rot = c(0, 30, -30),
#' 	axLab.gpar = gpar(col = "orange", fontsize = 14))
#' 
#' # Call up a built-in data set to illustrate some plotting tricks
#' data(HEC)
#' require("grid") # for text additions outside of HiveR (grid.text)
#' 
#' plotHive(HEC, ch = 0.1, bkgnd = "white",
#' 	axLabs = c("hair\ncolor", "eye\ncolor"),
#' 	axLab.pos = c(1, 1),
#' 	axLab.gpar = gpar(fontsize = 14))
#' grid.text("males", x = 0, y = 2.3, default.units = "native")
#' grid.text("females", x = 0, y = -2.3, default.units = "native")
#' grid.text("Pairing of Eye Color with Hair Color", x = 0, y = 4,
#' 	default.units = "native", gp = gpar(fontsize = 18))
#' 
#' # Add node labels and graphic decorations
#' # The working directory has to include
#' # not only the grInfo and anNodes files but also the jpgs.
#' # So, we are going to move to such a directory and return you home afterwards.
#' 
#' currDir <- getwd()
#' setwd(system.file("extdata", "Misc", package = "HiveR"))
#' plotHive(HEC, ch = 0.1, bkgnd = "white",
#' 	axLabs = c("hair\ncolor", "eye\ncolor"),
#' 	axLab.pos = c(1, 1),
#' 	axLab.gpar = gpar(fontsize = 14),
#' 	anNodes = "HECnodes.txt",
#' 	anNode.gpar = gpar(col = "black"),
#' 	grInfo = "HECgraphics.txt",
#' 	arrow = c("more\ncommon", 0.0, 2, 4, 1, -2))
#' 
#' grid.text("males", x = 0, y = 2.3, default.units = "native")
#' grid.text("females", x = 0, y = -2.3, default.units = "native")
#' grid.text("Pairing of Eye Color with Hair Color", x = 0, y = 3.75,
#' 	default.units = "native", gp = gpar(fontsize = 18))
#' grid.text("A test of plotHive annotation options", x = 0, y = 3.25,
#' 	default.units = "native", gp = gpar(fontsize = 12))
#' grid.text("Images from Wikipedia Commons", x = 0, y = -3.5,
#' 	default.units = "native", gp = gpar(fontsize = 9))
#' setwd(currDir)
#' 
#' # Use the node label concept to create tick marks
#' 
#' currDir <- getwd()
#' setwd(system.file("extdata", "Misc", package = "HiveR"))
#' plotHive(HEC, ch = 0.1, bkgnd = "white",
#' 	axLabs = c("hair\ncolor", "eye\ncolor"),
#' 	axLab.pos = c(1, 1),
#' 	axLab.gpar = gpar(fontsize = 14),
#' 	anNodes = "HECticks.txt",
#' 	anNode.gpar = gpar(col = "black"),
#' 	arrow = c("more\ncommon", 0.0, 2, 4, 1, -2),
#' 	dr.nodes = FALSE)
#' 
#' grid.text("males", x = 0, y = 2.3, default.units = "native")
#' grid.text("females", x = 0, y = -2.3, default.units = "native")
#' grid.text("Pairing of Eye Color with Hair Color", x = 0, y = 3.75,
#' 	default.units = "native", gp = gpar(fontsize = 18))
#' grid.text("Adding tick marks to the nodes", x = 0, y = 3.25,
#' 	default.units = "native", gp = gpar(fontsize = 12))
#' setwd(currDir)
#' 
#' 
#' ### 3D Hive Plots. The following must be run interactively.
#' \dontrun{
#' require("rgl")
#' test4 <- ranHiveData(nx = 4, type = "3D")
#' plot3dHive(test4)
#' }
#' 
#' @export plotHive
plotHive <- function(HPD, ch = 1, method = "abs",
	dr.nodes = TRUE, bkgnd = "black",
	axLabs = NULL, axLab.pos = NULL, axLab.gpar = NULL,
	anNodes = NULL, anNode.gpar = NULL, grInfo = NULL,
	arrow = NULL, np = TRUE, anCoord = "local", ...) {
	
	# Function to plot hive plots using grid graphics
	# Inspired by the work of Martin Kryzwinski
	# Bryan Hanson, DePauw Univ, Feb 2011 onward
	
	# This function is intended to draw in 2D for nx from 2 to 6
	# The results will be similar to the original hive plot concept

##### Set up some common parameters

	if (!HPD$type == "2D") stop("This is not a 2D hive data set: use plot3dHive instead")		
	chkHPD(HPD)
	nx <- length(unique(HPD$nodes$axis))

	if (nx == 1) stop("Something is wrong: only one axis seems to be present")

	# Send out for ranking/norming/pruning/inverting if requested
	
	if (!method == "abs") HPD <- manipAxis(HPD, method, ...)

	nodes <- HPD$nodes
	edges <- HPD$edges
	axis.cols <- HPD$axis.cols

	# Fix up center hole
	
	nodes$radius <- nodes$radius + ch
	HPD$nodes$radius <- nodes$radius

##### Some convenience functions, only defined in this function environ.
##### The two long functions need to stay here for simplicity, since
##### all of the radius checking etc is here and if moved elsewhere,
##### these calculations would have to be redone or results passed.

	p2cX <- function(r, theta) { x <- r*cos(theta*2*pi/360) }
	p2cY <- function(r, theta) { y <- r*sin(theta*2*pi/360) }

	addArrow <- function(arrow, nx) {
		if (!length(arrow) >= 5) stop("Too few arrow components")
		if (is.null(axLab.gpar)) {
			if (bkgnd == "black") axLab.gpar <- gpar(fontsize = 12, col = "white", lwd = 2)
			if (!bkgnd == "black") axLab.gpar <- gpar(fontsize = 12, col = "black", lwd = 2)
			}
		a <- as.numeric(arrow[2])
		rs <- as.numeric(arrow[3])
		re <- as.numeric(arrow[4])
		b <- as.numeric(arrow[5]) # label offset from end of arrow
		
		x.st <- p2cX(rs, a)
		y.st <- p2cY(rs, a)
		x.end <- p2cX(re, a)
		y.end <- p2cY(re, a)
					
		x.lab <- p2cX(re + b, a) # figure arrow label position
		y.lab <- p2cY(re + b, a)
		al <- 0.2*(re-rs) # arrow head length
		
		# for nx = 2 only, offset the arrow
		# in the y direction to save space overall
		
		if (nx == 2) {
			if (is.na(arrow[6])) {
				arrow[6] <- 0
				cat("\tThe arrow can be offset vertically; see ?plotHive\n")
				}
			y.st <- y.st + as.numeric(arrow[6])
			y.end <- y.end + as.numeric(arrow[6])
			y.lab <- y.lab + as.numeric(arrow[6])		
			}

		grid.lines(x = c(x.st, x.end), y = c(y.st, y.end),
			arrow = arrow(length = unit(al, "native")),
			default.units = "native", gp = axLab.gpar)
		grid.text(arrow[1], x.lab, y.lab, default.units = "native", gp = axLab.gpar)
		}

	annotateNodes <- function(anNodes, nodes, nx, anCoord) {

		if (is.null(anNode.gpar)) {
			if (bkgnd == "black") anNode.gpar <- gpar(fontsize = 10, col = "white", lwd = 0.5)
			if (!bkgnd == "black") anNode.gpar <- gpar(fontsize = 10, col = "black", lwd = 0.5)
			}
			
		ann <- utils::read.csv(anNodes, header = TRUE, colClasses = c(rep("character", 2), rep("numeric", 5)))
		cds <- getCoords(anNodes, anCoord, nodes)
		
		grid.segments(x0 = cds$x.st, x1 = cds$x.end, y0 = cds$y.st, y1 = cds$y.end,
			default.units = "native", gp = anNode.gpar)
		grid.text(ann$node.text, cds$x.lab, cds$y.lab, hjust = ann$hjust, vjust = ann$vjust,
			default.units = "native", gp = anNode.gpar, ...)
		}

	addGraphic <- function(grInfo, nodes, nx, anCoord) {

		gr <- utils::read.csv(grInfo, header = TRUE, stringsAsFactors = FALSE)
		cds <- getCoords(grInfo, anCoord, nodes)
				
		grid.segments(x0 = cds$x.st, x1 = cds$x.end, y0 = cds$y.st, y1 = cds$y.end,
			default.units = "native", gp = anNode.gpar)
			
		# readJPEG and readPNG are not vectorized, grab each graphic in turn
		# Figure out if we are using jpg or png files
		
		ext <- substr(gr$path[1], nchar(gr$path[1])-2, nchar(gr$path[1]))
		if ((ext == "png") | (ext == "PNG")) ext <- "png"
		if ((ext == "jpg") | (ext == "JPG") | (ext == "peg") | (ext =="PEG")) ext <- "jpg"

		# Now draw the images
		
		if (ext == "jpg") {
			for (n in 1:nrow(gr)) {
				grid.raster(readJPEG(gr$path[n]),
					x = cds$x.lab[n], y = cds$y.lab[n], default.units = "native", width = gr$width[n])			
				}
			}

		if (ext == "png") {
			for (n in 1:nrow(gr)) {
				grid.raster(readPNG(gr$path[n]),
					x = cds$x.lab[n], y = cds$y.lab[n], default.units = "native", width = gr$width[n])			
				}
			}

		}

	getCoords <- function(file, anCoord, nodes) {
		
		# Figure out the coordinates of the line segments and labels/graphics
		# anNodes and grInfo both contains certain columns which are used here
		
		df <- utils::read.csv(file, header = TRUE)
		
		id <- rep(NA, nrow(df))	
		for (n in 1:nrow(df)) {			
			pat <- paste("\\b", df$node.lab[n], "\\b", sep = "") 
			id[n] <- grep(pat, nodes$lab)
			}
		
		N <- matrix(data = c(
		0, 180, NA, NA, NA, NA,
		90, 210, 330, NA, NA, NA,
		90, 180, 270, 0, NA, NA,
		90, 162, 234, 306, 18, NA,
		90, 150, 210, 270, 330, 390),
		byrow = TRUE, nrow = 5)
			
		ax <- nodes$axis[id] # axis number
		for (n in 1:length(ax)) {
			ax[n] <- N[nx-1,ax[n]]			
			}

		# Figure coords in requested reference frame

		x.st <- p2cX(nodes$radius[id], ax)
		y.st <- p2cY(nodes$radius[id], ax)
		
		if (anCoord == "local") {
			x.end <- x.st + p2cX(df$radius, df$angle)
			y.end <- y.st + p2cY(df$radius, df$angle)
						
			x.lab <- x.st + p2cX(df$radius + df$offset, df$angle)
			y.lab <- y.st + p2cY(df$radius + df$offset, df$angle)		
			}

		if (anCoord == "global") {					
			x.end <- p2cX(df$radius, df$angle)
			y.end <- p2cY(df$radius, df$angle)
						
			x.lab <- p2cX(df$radius + df$offset, df$angle)
			y.lab <- p2cY(df$radius + df$offset, df$angle)		
			}
		
		retval <- data.frame(x.st, y.st, x.end, y.end, x.lab, y.lab)
		retval
		}

###############

	# Figure out which nodes to draw for each edge
	# Since they are in random order
	# Do this once/early to save time
	
	id1 <- id2 <- c()
	
	for (n in 1:nrow(edges)) {
		
		pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
		pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
		id1 <- c(id1, grep(pat1, nodes$id))
		id2 <- c(id2, grep(pat2, nodes$id))
		
		}

##### Two dimensional case  (using grid graphics)

	# Prep axes first
	
	if (nx == 2) {
		
		# n1 <- subset(nodes, axis == 1)
		# n2 <- subset(nodes, axis == 2)

		n1 <- nodes[nodes[,"axis"] == 1,]
		n2 <- nodes[nodes[,"axis"] == 2,]

		max1 <- max(n1$radius)
		max2 <- max(n2$radius)
		min1 <- min(n1$radius)
		min2 <- min(n2$radius)
	
		r.st <- c(min1, min2) # in polar coordinates
		axst <- c(0, 180)
		x0a = p2cX(r.st, axst)
		y0a = p2cY(r.st, axst)

		r.end <- c(max1, max2)
		axend <- c(0, 180)
		x1a = p2cX(r.end, axend)
		y1a = p2cY(r.end, axend)
	
	# Set up grid graphics viewport
		
		md <- max(abs(c(x0a, y0a, x1a, y1a)))*1.5 # max dimension
		# 1.5 is used in case of labels
		
		if (np) grid.newpage()
		grid.rect(gp = gpar(col = NA, fill = bkgnd))
		vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1,
			xscale = c(-md, md), yscale = c(-md, md),
			name = "3DHivePlot")

		pushViewport(vp)
	
	# Now draw edges
		
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if (nodes$axis[id1[n]] == 1) { # set up edge start params 1st
				th.st <- c(th.st, 0)
				r.st <- c(r.st, nodes$radius[id1[n]])
				}
			if (nodes$axis[id1[n]] == 2) {
				th.st <- c(th.st, 180)
				r.st <- c(r.st, nodes$radius[id1[n]])
				}

			if (nodes$axis[id2[n]] == 1) { # now edge end params
				th.end <- c(th.end, 0)
				r.end <- c(r.end, nodes$radius[id2[n]])
				}
			if (nodes$axis[id2[n]] == 2) {
				th.end <- c(th.end, 180)
				r.end <- c(r.end, nodes$radius[id2[n]])
				}

			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
			}
				
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Draw axes
	
		grid.segments(x0a, y0a, x1a, y1a,
			gp = gpar(col = HPD$axis.cols, lwd = 8),
			default.units = "native")
	
	# Now add nodes
	
		if (dr.nodes) {
			r <- c(n1$radius, n2$radius) 
			theta <- c(rep(0, length(n1$radius)),
				rep(180, length(n2$radius)))
			x = p2cX(r, theta)
			y = p2cY(r, theta)
			grid.points(x, y, pch = 20, gp = gpar(cex = c(n1$size, n2$size), col = c(n1$color, n2$color)))
			}

	# Now label axes
		
		if (!is.null(axLabs)) {
			if (!length(axLabs) == nx) stop("Incorrect number of axis labels")
			if (is.null(axLab.gpar)) axLab.gpar <- gpar(fontsize = 12, col = "white")
			r <- c(max1, max2)
			if (is.null(axLab.pos)) axLab.pos <- r*0.1
			r <- r + axLab.pos
			th <- c(0, 180)
			x <- p2cX(r, th)
			y <- p2cY(r, th)
			grid.text(axLabs, x, y, gp = axLab.gpar,  default.units = "native", ...)
			}

	# Add a legend arrow & any annotations
		
		if (!is.null(arrow)) addArrow(arrow, nx)
		if (!is.null(anNodes)) annotateNodes(anNodes, nodes, nx, anCoord)
		if (!is.null(grInfo)) addGraphic(grInfo, nodes, nx, anCoord)

		} # end of 2D
	
##### Three dimensional case (using grid graphics)

	# Prep axes first
	
	if (nx == 3) {
		
		# n1 <- subset(nodes, axis == 1)
		# n2 <- subset(nodes, axis == 2)
		# n3 <- subset(nodes, axis == 3)

		n1 <- nodes[nodes[,"axis"] == 1,]
		n2 <- nodes[nodes[,"axis"] == 2,]
		n3 <- nodes[nodes[,"axis"] == 3,]

		max1 <- max(n1$radius)
		max2 <- max(n2$radius)
		max3 <- max(n3$radius)
		min1 <- min(n1$radius)
		min2 <- min(n2$radius)
		min3 <- min(n3$radius)

		r.st <- c(min1, min2, min3) # in polar coordinates
		axst <- c(90, 210, 330)
		x0a = p2cX(r.st, axst)
		y0a = p2cY(r.st, axst)

		r.end <- c(max1, max2, max3)
		axend <- c(90, 210, 330)
		x1a = p2cX(r.end, axend)
		y1a = p2cY(r.end, axend)
	
	# Set up grid graphics viewport
	
		md <- max(abs(c(x0a, y0a, x1a, y1a)))*1.3 # max dimension
		if (np) grid.newpage()
		grid.rect(gp = gpar(col = NA, fill = bkgnd))
		vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1,
			xscale = c(-md, md), yscale = c(-md, md), name = "3DHivePlot")
		pushViewport(vp)

	# Now draw edges (must do in sets as curvature is not vectorized)
		
	# Axis 1 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 1) & (nodes$axis[id2[n]] == 2)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 210)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
				
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 2 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 2) & (nodes$axis[id2[n]] == 3)) {
				th.st <- c(th.st, 210)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 330)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 3 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 3) & (nodes$axis[id2[n]] == 1)) {
				th.st <- c(th.st, 330)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 1 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 1) & (nodes$axis[id2[n]] == 3)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 330)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 3 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 3) & (nodes$axis[id2[n]] == 2)) {
				th.st <- c(th.st, 330)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 210)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 2 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 2) & (nodes$axis[id2[n]] == 1)) {
				th.st <- c(th.st, 210)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
				
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 1 -> 1, 2 -> 2 etc (can be done as a group since curvature can be fixed)
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 1) & (nodes$axis[id2[n]] == 1)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			if ((nodes$axis[id1[n]] == 2) & (nodes$axis[id2[n]] == 2)) {
				th.st <- c(th.st, 210)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 210)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			if ((nodes$axis[id1[n]] == 3) & (nodes$axis[id2[n]] == 3)) {
				th.st <- c(th.st, 330)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 330)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
				
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Draw axes
	
		grid.segments(x0a, y0a, x1a, y1a,
			gp = gpar(col = HPD$axis.cols, lwd = 3),
			default.units = "native")
	
	# Now add nodes
	
		if (dr.nodes) {
			r <- c(n1$radius, n2$radius, n3$radius) 
			theta <- c(rep(90, length(n1$radius)),
				rep(210, length(n2$radius)),
				rep(330, length(n3$radius)))
			x = p2cX(r, theta)
			y = p2cY(r, theta)
			grid.points(x, y, pch = 20, gp = gpar(cex = c(n1$size, n2$size, n3$size),
			col = c(n1$color, n2$color, n3$color)))
			}

	# Now label axes
		
		if (!is.null(axLabs)) {
			if (!length(axLabs) == nx) stop("Incorrect number of axis labels")
			if (is.null(axLab.gpar)) axLab.gpar <- gpar(fontsize = 12, col = "white")
			r <- c(max1, max2, max3)
			if (is.null(axLab.pos)) axLab.pos <- r*0.1
			r <- r + axLab.pos
			th <- c(90, 210, 330)
			x <- p2cX(r, th)
			y <- p2cY(r, th)
			grid.text(axLabs, x, y, gp = axLab.gpar,  default.units = "native", ...)
			}

	# Add a legend arrow & any annotations
		
		if (!is.null(arrow)) addArrow(arrow, nx)
		if (!is.null(anNodes)) annotateNodes(anNodes, nodes, nx, anCoord)
		if (!is.null(grInfo)) addGraphic(grInfo, nodes, nx, anCoord)

		} # end of 3D
	

##### Four dimensional case (using grid graphics)

	# Prep axes first
	
	if (nx == 4) {
		
		# n1 <- subset(nodes, axis == 1)
		# n2 <- subset(nodes, axis == 2)
		# n3 <- subset(nodes, axis == 3)
		# n4 <- subset(nodes, axis == 4)

		n1 <- nodes[nodes[,"axis"] == 1,]
		n2 <- nodes[nodes[,"axis"] == 2,]
		n3 <- nodes[nodes[,"axis"] == 3,]
		n4 <- nodes[nodes[,"axis"] == 4,]
		
		max1 <- max(n1$radius)
		max2 <- max(n2$radius)
		max3 <- max(n3$radius)
		max4 <- max(n4$radius)
		min1 <- min(n1$radius)
		min2 <- min(n2$radius)
		min3 <- min(n3$radius)
		min4 <- min(n4$radius)

		r.st <- c(min1, min2, min3, min4) # in polar coordinates
		axst <- c(90, 180, 270, 0)
		x0a = p2cX(r.st, axst)
		y0a = p2cY(r.st, axst)

		r.end <- c(max1, max2, max3, max4)
		axend <- c(90, 180, 270, 0)
		x1a = p2cX(r.end, axend)
		y1a = p2cY(r.end, axend)
	
	# Set up grid graphics viewport
	
		md <- max(abs(c(x0a, y0a, x1a, y1a)))*1.5 # max dimension
		if (np) grid.newpage()
		grid.rect(gp = gpar(col = NA, fill = bkgnd))
		vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1,
			xscale = c(-md, md), yscale = c(-md, md), name = "3DHivePlot")
		pushViewport(vp)

	# Now draw edges (must do in sets as curvature is not vectorized)
		
	# Axis 1 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 1) & (nodes$axis[id2[n]] == 2)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 180)
				r.end <- c(r.end, nodes$radius[id2[n]])
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 2 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 2) & (nodes$axis[id2[n]] == 3)) {
				th.st <- c(th.st, 180)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 270)
				r.end <- c(r.end, nodes$radius[id2[n]])
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 3 -> 4
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 3) & (nodes$axis[id2[n]] == 4)) {
				th.st <- c(th.st, 270)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 0)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 4 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 4) & (nodes$axis[id2[n]] == 1)) {
				th.st <- c(th.st, 0)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2[n]])
			ecol <- c(ecol, edges$color[n])
			ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 1 -> 4
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 1) & (nodes$axis[id2[n]] == 4)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 0)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 4 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 4) & (nodes$axis[id2[n]] == 3)) {
				th.st <- c(th.st, 0)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 270)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 3 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 3) & (nodes$axis[id2[n]] == 2)) {
				th.st <- c(th.st, 270)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 180)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}				
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 2 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 2) & (nodes$axis[id2[n]] == 1)) {
				th.st <- c(th.st, 180)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}				
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 1 -> 1, 2 -> 2 etc (can be done as a group since curvature can be fixed)
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 1) & (nodes$axis[id2[n]] == 1)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			if ((nodes$axis[id1[n]] == 2) & (nodes$axis[id2[n]] == 2)) {
				th.st <- c(th.st, 180)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 180)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			if ((nodes$axis[id1[n]] == 3) & (nodes$axis[id2[n]] == 3)) {
				th.st <- c(th.st, 270)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 270)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
				
			if ((nodes$axis[id1[n]] == 4) & (nodes$axis[id2[n]] == 4)) {
				th.st <- c(th.st, 0)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 0)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Draw axes
	
		grid.segments(x0a, y0a, x1a, y1a,
			gp = gpar(col = HPD$axis.cols, lwd = 3),
			default.units = "native")
	
	# Now add nodes
	
		if (dr.nodes) {
			r <- c(n1$radius, n2$radius, n3$radius, n4$radius) 
			theta <- c(rep(90, length(n1$radius)),
				rep(180, length(n2$radius)),
				rep(270, length(n3$radius)),
				rep(0, length(n4$radius)))
			x = p2cX(r, theta)
			y = p2cY(r, theta)
			grid.points(x, y, pch = 20, gp = gpar(cex = c(n1$size, n2$size, n3$size, n4$size),
			col = c(n1$color, n2$color, n3$color, n4$color)))
			}

	# Now label axes
		
		if (!is.null(axLabs)) {
			if (!length(axLabs) == nx) stop("Incorrect number of axis labels")
			if (is.null(axLab.gpar)) axLab.gpar <- gpar(fontsize = 12, col = "white")
			r <- c(max1, max2, max3, max4)
			if (is.null(axLab.pos)) axLab.pos <- r*0.1
			r <- r + axLab.pos
			th <- c(90, 180, 270, 0)
			x <- p2cX(r, th)
			y <- p2cY(r, th)
			grid.text(axLabs, x, y, gp = axLab.gpar,  default.units = "native", ...)
			}

	# Add a legend arrow & any annotations
		
		if (!is.null(arrow)) addArrow(arrow, nx)
		if (!is.null(anNodes)) annotateNodes(anNodes, nodes, nx, anCoord)
		if (!is.null(grInfo)) addGraphic(grInfo, nodes, nx, anCoord)

		} # end of 4D
	
##### Five dimensional case (using grid graphics)

	# Prep axes first
	
	if (nx == 5) {
		
		# n1 <- subset(nodes, axis == 1)
		# n2 <- subset(nodes, axis == 2)
		# n3 <- subset(nodes, axis == 3)
		# n4 <- subset(nodes, axis == 4)
		# n5 <- subset(nodes, axis == 5)

		n1 <- nodes[nodes[,"axis"] == 1,]
		n2 <- nodes[nodes[,"axis"] == 2,]
		n3 <- nodes[nodes[,"axis"] == 3,]
		n4 <- nodes[nodes[,"axis"] == 4,]
		n5 <- nodes[nodes[,"axis"] == 5,]

		max1 <- max(n1$radius)
		max2 <- max(n2$radius)
		max3 <- max(n3$radius)
		max4 <- max(n4$radius)
		max5 <- max(n5$radius)
		min1 <- min(n1$radius)
		min2 <- min(n2$radius)
		min3 <- min(n3$radius)
		min4 <- min(n4$radius)
		min5 <- min(n5$radius)

		r.st <- c(min1, min2, min3, min4, min5) # in polar coordinates
		axst <- c(90, 162, 234, 306, 18)
		x0a = p2cX(r.st, axst)
		y0a = p2cY(r.st, axst)

		r.end <- c(max1, max2, max3, max4, max5)
		axend <- c(90, 162, 234, 306, 18)
		x1a = p2cX(r.end, axend)
		y1a = p2cY(r.end, axend)
	
	# Set up grid graphics viewport
	
		md <- max(abs(c(x0a, y0a, x1a, y1a)))*1.3 # max dimension
		if (np) grid.newpage()
		grid.rect(gp = gpar(col = NA, fill = bkgnd))
		vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1,
			xscale = c(-md, md), yscale = c(-md, md), name = "3DHivePlot")
		pushViewport(vp)

	# Now draw edges (must do in sets as curvature is not vectorized)
		
	# Axis 1 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 1) & (nodes$axis[id2[n]] == 2)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 162)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}			
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 2 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 2) & (nodes$axis[id2[n]] == 3)) {
				th.st <- c(th.st, 162)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 234)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 3 -> 4
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 3) & (nodes$axis[id2[n]] == 4)) {
				th.st <- c(th.st, 234)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 306)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 4 -> 5
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 4) & (nodes$axis[id2[n]] == 5)) {
				th.st <- c(th.st, 306)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 18)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 5 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 5) & (nodes$axis[id2[n]] == 1)) {
				th.st <- c(th.st, 18)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}


	# Axis 1 -> 5
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 1) & (nodes$axis[id2[n]] == 5)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 18)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 5 -> 4
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 5) & (nodes$axis[id2[n]] == 4)) {
				th.st <- c(th.st, 18)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 306)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 4 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 4) & (nodes$axis[id2[n]] == 3)) {
				th.st <- c(th.st, 306)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 234)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 3 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 3) & (nodes$axis[id2[n]] == 2)) {
				th.st <- c(th.st, 234)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 162)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 2 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 2) & (nodes$axis[id2[n]] == 1)) {
				th.st <- c(th.st, 162)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 1 -> 1, 2 -> 2 etc (can be done as a group since curvature can be fixed)
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 1) & (nodes$axis[id2[n]] == 1)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			if ((nodes$axis[id1[n]] == 2) & (nodes$axis[id2[n]] == 2)) {
				th.st <- c(th.st, 162)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 162)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			if ((nodes$axis[id1[n]] == 3) & (nodes$axis[id2[n]] == 3)) {
				th.st <- c(th.st, 234)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 234)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
				
			if ((nodes$axis[id1[n]] == 4) & (nodes$axis[id2[n]] == 4)) {
				th.st <- c(th.st, 306)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 306)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			if ((nodes$axis[id1[n]] == 5) & (nodes$axis[id2[n]] == 5)) {
				th.st <- c(th.st, 18)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 18)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Draw axes
	
		grid.segments(x0a, y0a, x1a, y1a,
			gp = gpar(col = HPD$axis.cols, lwd = 3),
			default.units = "native")
	
	# Now add nodes
	
		if (dr.nodes) {
			r <- c(n1$radius, n2$radius, n3$radius, n4$radius, n5$radius) 
			theta <- c(rep(90, length(n1$radius)),
				rep(162, length(n2$radius)),
				rep(234, length(n3$radius)),
				rep(306, length(n4$radius)),
				rep(18, length(n5$radius)))
			x = p2cX(r, theta)
			y = p2cY(r, theta)
			grid.points(x, y, pch = 20, gp = gpar(cex = c(n1$size, n2$size, n3$size, n4$size, n5$size),
			col = c(n1$color, n2$color, n3$color, n4$color, n5$color)))
			}

	# Now label axes
		
		if (!is.null(axLabs)) {
			if (!length(axLabs) == nx) stop("Incorrect number of axis labels")
			if (is.null(axLab.gpar)) axLab.gpar <- gpar(fontsize = 12, col = "white")
			r <- c(max1, max2, max3, max4, max5)
			if (is.null(axLab.pos)) axLab.pos <- r*0.1
			r <- r + axLab.pos
			th <- c(90, 162, 234, 306, 18)
			x <- p2cX(r, th)
			y <- p2cY(r, th)
			grid.text(axLabs, x, y, gp = axLab.gpar,  default.units = "native", ...)
			}

	# Add a legend arrow & any annotations
		
		if (!is.null(arrow)) addArrow(arrow, nx)
		if (!is.null(anNodes)) annotateNodes(anNodes, nodes, nx, anCoord)
		if (!is.null(grInfo)) addGraphic(grInfo, nodes, nx, anCoord)

		} # end of 5D

##### Six dimensional case (using grid graphics)

	# Prep axes first
	
	if (nx == 6) {
		
		# n1 <- subset(nodes, axis == 1)
		# n2 <- subset(nodes, axis == 2)
		# n3 <- subset(nodes, axis == 3)
		# n4 <- subset(nodes, axis == 4)
		# n5 <- subset(nodes, axis == 5)
		# n6 <- subset(nodes, axis == 6)
		
		n1 <- nodes[nodes[,"axis"] == 1,]
		n2 <- nodes[nodes[,"axis"] == 2,]
		n3 <- nodes[nodes[,"axis"] == 3,]
		n4 <- nodes[nodes[,"axis"] == 4,]
		n5 <- nodes[nodes[,"axis"] == 5,]
		n6 <- nodes[nodes[,"axis"] == 6,]
		
		max1 <- max(n1$radius)
		max2 <- max(n2$radius)
		max3 <- max(n3$radius)
		max4 <- max(n4$radius)
		max5 <- max(n5$radius)
		max6 <- max(n6$radius)
		min1 <- min(n1$radius)
		min2 <- min(n2$radius)
		min3 <- min(n3$radius)
		min4 <- min(n4$radius)
		min5 <- min(n5$radius)
		min6 <- min(n6$radius)

		r.st <- c(min1, min2, min3, min4, min5, min6) # in polar coordinates
		axst <- c(90, 150, 210, 270, 330, 390)
		x0a = p2cX(r.st, axst)
		y0a = p2cY(r.st, axst)

		r.end <- c(max1, max2, max3, max4, max5, max6)
		axend <- c(90, 150, 210, 270, 330, 390)
		x1a = p2cX(r.end, axend)
		y1a = p2cY(r.end, axend)
	
	# Set up grid graphics viewport
	
		md <- max(abs(c(x0a, y0a, x1a, y1a)))*1.3 # max dimension
		if (np) grid.newpage()
		grid.rect(gp = gpar(col = NA, fill = bkgnd))
		vp <- viewport(x = 0.5, y = 0.5, width = 1, height = 1,
			xscale = c(-md, md), yscale = c(-md, md), name = "3DHivePlot")
		pushViewport(vp)

	# Now draw edges (must do in sets as curvature is not vectorized)
		
	# Axis 1 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {

			if ((nodes$axis[id1[n]] == 1) & (nodes$axis[id2[n]] == 2)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 150)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 2 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 2) & (nodes$axis[id2[n]] == 3)) {
				th.st <- c(th.st, 150)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 210)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 3 -> 4
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 3) & (nodes$axis[id2[n]] == 4)) {
				th.st <- c(th.st, 210)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 270)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 4 -> 5
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 4) & (nodes$axis[id2[n]] == 5)) {
				th.st <- c(th.st, 270)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 330)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 5 -> 6
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 5) & (nodes$axis[id2[n]] == 6)) {
				th.st <- c(th.st, 330)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 390)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 6 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 6) & (nodes$axis[id2[n]] == 1)) {
				th.st <- c(th.st, 390)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Axis 1 -> 6
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 1) & (nodes$axis[id2[n]] == 6)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 390)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 6 -> 5
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 6) & (nodes$axis[id2[n]] == 5)) {
				th.st <- c(th.st, 390)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 330)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 5 -> 4
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 5) & (nodes$axis[id2[n]] == 4)) {
				th.st <- c(th.st, 330)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 270)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 4 -> 3
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 4) & (nodes$axis[id2[n]] == 3)) {
				th.st <- c(th.st, 270)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 210)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)
		
		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 3 -> 2
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 3) & (nodes$axis[id2[n]] == 2)) {
				th.st <- c(th.st, 210)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 150)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 2 -> 1
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
						
			if ((nodes$axis[id1[n]] == 2) & (nodes$axis[id2[n]] == 1)) {
				th.st <- c(th.st, 150)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])				}
			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = -0.5)
			}

	# Axis 1 -> 1, 2 -> 2 etc (can be done as a group since curvature can be fixed)
	
		r.st <- r.end <- th.st <- th.end <- ecol <- ewt <- c()
			
		for (n in 1:nrow(edges)) {
			
			if ((nodes$axis[id1[n]] == 1) & (nodes$axis[id2[n]] == 1)) {
				th.st <- c(th.st, 90)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 90)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			if ((nodes$axis[id1[n]] == 2) & (nodes$axis[id2[n]] == 2)) {
				th.st <- c(th.st, 150)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 150)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			if ((nodes$axis[id1[n]] == 3) & (nodes$axis[id2[n]] == 3)) {
				th.st <- c(th.st, 210)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 210)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}
				
			if ((nodes$axis[id1[n]] == 4) & (nodes$axis[id2[n]] == 4)) {
				th.st <- c(th.st, 270)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 270)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			if ((nodes$axis[id1[n]] == 5) & (nodes$axis[id2[n]] == 5)) {
				th.st <- c(th.st, 330)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 330)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			if ((nodes$axis[id1[n]] == 6) & (nodes$axis[id2[n]] == 6)) {
				th.st <- c(th.st, 390)
				r.st <- c(r.st, nodes$radius[id1[n]])
				th.end <- c(th.end, 390)
				r.end <- c(r.end, nodes$radius[id2[n]])
				ecol <- c(ecol, edges$color[n])
				ewt <- c(ewt, edges$weight[n])
				}

			}
		
		x0 = p2cX(r.st, th.st)
		y0 = p2cY(r.st, th.st)
		x1 = p2cX(r.end, th.end)
		y1 = p2cY(r.end, th.end)

		if (!length(x0) == 0) {
			grid.curve(x0, y0, x1, y1,
				default.units = "native", ncp = 5, square = FALSE,
				gp = gpar(col = ecol, lwd = ewt), curvature = 0.5)
			}

	# Draw axes
	
		# grid.segments(x0a, y0a, x1a, y1a,
			# gp = gpar(col = "black", lwd = 7),
			# default.units = "native") # more like linnet

		grid.segments(x0a, y0a, x1a, y1a,
			gp = gpar(col = HPD$axis.cols, lwd = 3),
			default.units = "native")
	
	# Now add nodes
	
		if (dr.nodes) {
			r <- c(n1$radius, n2$radius, n3$radius, n4$radius, n5$radius, n6$radius) 
			theta <- c(rep(90, length(n1$radius)),
				rep(150, length(n2$radius)),
				rep(210, length(n3$radius)),
				rep(270, length(n4$radius)),
				rep(330, length(n5$radius)),
				rep(390, length(n6$radius)))
			x = p2cX(r, theta)
			y = p2cY(r, theta)
			grid.points(x, y, pch = 20, gp = gpar(cex = c(n1$size, n2$size, n3$size, n4$size, n5$size, n6$size),
			col = c(n1$color, n2$color, n3$color, n4$color, n5$color, n6$color)))
			}

	# Now label axes
		
		if (!is.null(axLabs)) {
			if (!length(axLabs) == nx) stop("Incorrect number of axis labels")
			if (is.null(axLab.gpar)) axLab.gpar <- gpar(fontsize = 12, col = "white")
			r <- c(max1, max2, max3, max4, max5, max6)
			if (is.null(axLab.pos)) axLab.pos <- r*0.1
			r <- r + axLab.pos
			th <- c(90, 150, 210, 270, 330, 390)
			x <- p2cX(r, th)
			y <- p2cY(r, th)
			grid.text(axLabs, x, y, gp = axLab.gpar,  default.units = "native", ...)
			}

	# Add a legend arrow & any annotations
		
		if (!is.null(arrow)) addArrow(arrow, nx)
		if (!is.null(anNodes)) annotateNodes(anNodes, nodes, nx, anCoord)
		if (!is.null(grInfo)) addGraphic(grInfo, nodes, nx, anCoord)

		} # end of 6D

	
	} # closing brace, this is the end!
