

plotHive <- function(HPD, ch = 1, method = "abs",
	dr.nodes = TRUE, bkgnd = "black",
	axLabs = NULL, axLab.pos = NULL, axLab.gpar = NULL,
	anNodes = NULL, anNode.gpar = NULL, grInfo = NULL,
	arrow = NULL, np = TRUE, ...) {
	
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

	p2cX <- function(r, theta) x <- r*cos(theta*2*pi/360)
	p2cY <- function(r, theta) y <- r*sin(theta*2*pi/360)

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

	annotateNodes <- function(anNodes, nodes, nx) {

		if (is.null(anNode.gpar)) {
			if (bkgnd == "black") anNode.gpar <- gpar(fontsize = 10, col = "white", lwd = 0.5)
			if (!bkgnd == "black") anNode.gpar <- gpar(fontsize = 10, col = "black", lwd = 0.5)
			}
			
		ann <- read.csv(anNodes, header = TRUE)
		print(ann)
		# Columns should be: node.lab, node.text, angle, radius, offset, hjust, vjust
		# Locate the node on an axis at a particular radius
		
		id <- c()	
		for (n in 1:nrow(ann)) {			
			pat <- paste("\\b", ann$node.lab[n], "\\b", sep = "") 
			id <- c(id, grep(pat, nodes$lab))
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

		x.st <- p2cX(nodes$radius[id], ax)
		y.st <- p2cY(nodes$radius[id], ax)
		
		x.end <- p2cX(ann$radius, ann$angle)
		y.end <- p2cY(ann$radius, ann$angle)
					
		x.lab <- p2cX(ann$radius + ann$offset, ann$angle) # figure label position
		y.lab <- p2cY(ann$radius + ann$offset, ann$angle)
		
		grid.segments(x0 = x.st, x1 = x.end, y0 = y.st, y1 = y.end,
			default.units = "native", gp = anNode.gpar)
		grid.text(ann$node.text, x.lab, y.lab, hjust = ann$hjust, vjust = ann$vjust,
			default.units = "native", gp = anNode.gpar)
		}

	addGraphic <- function(grInfo, nodes, nx) {

		# grInfo should be a csv containing:
		# node.lab, angle, radius, offset, path
		
		gr <- read.csv(grInfo, header = TRUE)
		
		id <- c()	
		for (n in 1:nrow(gr)) {			
			pat <- paste("\\b", gr$node.lab[n], "\\b", sep = "") 
			id <- c(id, grep(pat, nodes$lab))
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

		x.st <- p2cX(nodes$radius[id], ax)
		y.st <- p2cY(nodes$radius[id], ax)
		
		x.end <- p2cX(gr$radius, gr$angle)
		y.end <- p2cY(gr$radius, gr$angle)
					
		x.gr <- p2cX(gr$radius + gr$offset, gr$angle) # figure label position
		y.gr <- p2cY(gr$radius + gr$offset, gr$angle)
		
		grid.segments(x0 = x.st, x1 = x.end, y0 = y.st, y1 = y.end,
			default.units = "native", gp = anNode.gpar)
		grid.raster(readJPEG(as.character(gr$path)),
			x.gr, y.gr)
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
		
		n1 <- subset(nodes, axis == 1)
		n2 <- subset(nodes, axis == 2)
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
		grid.rect(gp = gpar(fill = bkgnd))
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
		if (!is.null(anNodes)) annotateNodes(anNodes, nodes, nx)

		} # end of 2D
	
##### Three dimensional case (using grid graphics)

	# Prep axes first
	
	if (nx == 3) {
		
		n1 <- subset(nodes, axis == 1)
		n2 <- subset(nodes, axis == 2)
		n3 <- subset(nodes, axis == 3)
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
		grid.rect(gp = gpar(fill = bkgnd))
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
		if (!is.null(anNodes)) annotateNodes(anNodes, nodes, nx)
#		if (!is.null(grInfo)) addGraphic(grInfo, nodes, nx)

		} # end of 3D
	

##### Four dimensional case (using grid graphics)

	# Prep axes first
	
	if (nx == 4) {
		
		n1 <- subset(nodes, axis == 1)
		n2 <- subset(nodes, axis == 2)
		n3 <- subset(nodes, axis == 3)
		n4 <- subset(nodes, axis == 4)
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
		grid.rect(gp = gpar(fill = bkgnd))
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
		if (!is.null(anNodes)) annotateNodes(anNodes, nodes, nx)

		} # end of 4D
	
##### Five dimensional case (using grid graphics)

	# Prep axes first
	
	if (nx == 5) {
		
		n1 <- subset(nodes, axis == 1)
		n2 <- subset(nodes, axis == 2)
		n3 <- subset(nodes, axis == 3)
		n4 <- subset(nodes, axis == 4)
		n5 <- subset(nodes, axis == 5)
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
		grid.rect(gp = gpar(fill = bkgnd))
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
		if (!is.null(anNodes)) annotateNodes(anNodes, nodes, nx)

		} # end of 5D

##### Six dimensional case (using grid graphics)

	# Prep axes first
	
	if (nx == 6) {
		
		n1 <- subset(nodes, axis == 1)
		n2 <- subset(nodes, axis == 2)
		n3 <- subset(nodes, axis == 3)
		n4 <- subset(nodes, axis == 4)
		n5 <- subset(nodes, axis == 5)
		n6 <- subset(nodes, axis == 6)
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
		grid.rect(gp = gpar(fill = bkgnd))
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
		if (!is.null(anNodes)) annotateNodes(anNodes, nodes, nx)

		} # end of 6D

	
	} # closing brace, this is the end!