

drawHiveSpline <- function(HPD, L_A = FALSE, ...) {
	
	# Function to locate a 3d spline curve in a particular n dimensional
	# system & figure out the control point
	
	# For use with plot3dHive
	# Bryan Hanson, DePauw University, Feb 2011 and onward
	
	# The point pairs to be connected given by df edges
	
	chkHPD(HPD)
	nodes <- HPD$nodes
	edges <- HPD$edges		
	nx <- length(unique(nodes$axis))
	if ((nx == 2) | (nx == 3)) stop("You shouldn't be calling this function w/2 or 3 axes")

##### Get the edges data frame ready

	ax1 <- rad1 <- ax2 <- rad2 <- c()

	for (n in 1:nrow(edges)) {
		pat1 <- paste("\\b", edges$id1[n], "\\b", sep = "") 
		pat2 <- paste("\\b", edges$id2[n], "\\b", sep = "")
		id1 <- grep(pat1, nodes$id)
		id2 <- grep(pat2, nodes$id)

		ax1 = c(ax1, nodes$axis[id1])
		rad1 = c(rad1, nodes$radius[id1])
		ax2 = c(ax2, nodes$axis[id2])
		rad2 = c(rad2, nodes$radius[id2])
		}
		
	ds <- data.frame(ax1, rad1, ax2, rad2)
	ds$phi1 <- ds$phi2 <- ds$th1 <- ds$th2 <- rep(NA, length(ds$ax1))
	
##### 4D, This requires a 3D spline curve to be drawn

	if (nx == 4) {

		for (n in 1:nrow(ds)) {
			if (ds$ax1[n] == 1) {ds$phi1[n] = 54.7; ds$th1[n] = 45}
			if (ds$ax1[n] == 2) {ds$phi1[n] = 125.3; ds$th1[n] = -45}
			if (ds$ax1[n] == 3) {ds$phi1[n] = 125.3; ds$th1[n] = 135}
			if (ds$ax1[n] == 4) {ds$phi1[n] = 54.7; ds$th1[n] = -135}
			
			if (ds$ax2[n] == 1) {ds$phi2[n] = 54.7; ds$th2[n] = 45}
			if (ds$ax2[n] == 2) {ds$phi2[n] = 125.3; ds$th2[n] = -45}
			if (ds$ax2[n] == 3) {ds$phi2[n] = 125.3; ds$th2[n] = 135}
			if (ds$ax2[n] == 4) {ds$phi2[n] = 54.7; ds$th2[n] = -135}
			}
	
	pt1 <- data.frame(radius = ds$rad1, theta = ds$th1, phi = ds$phi1)	
	pt2 <- data.frame(radius = ds$rad2, theta = ds$th2, phi = ds$phi2)
	pt1 <- sph2cart(pt1)
	pt2 <- sph2cart(pt2)
	
	# Compute control point, then create splines
	# Splines must be drawn one at a time (slow!)

	cp <- 0.6*(pt1 + pt2)
	
	pt1 <- as.matrix(pt1)
	cp <- as.matrix(cp)
	pt2 <- as.matrix(pt2)

		for (n in 1:nrow(pt1)) {
			spl <- rcsr(p0 = pt1[n,], cp = cp[n,], p1 = pt2[n,])
			lines3d(x = spl[,1], y = spl[,2], z = spl[,3],
				line_antialias = L_A, col = edges$color[n], lwd = edges$weight[n])		
			}
	
		} # end of nx = 4

##### 5D, This requires a 3D spline curve to be drawn
	
	if (nx == 5) {
	
		for (n in 1:nrow(ds)) {
			if (ds$ax1[n] == 1) {ds$phi1[n] = 90; ds$th1[n] = 0}
			if (ds$ax1[n] == 2) {ds$phi1[n] = 90; ds$th1[n] = 120}
			if (ds$ax1[n] == 3) {ds$phi1[n] = 90; ds$th1[n] = 240}
			if (ds$ax1[n] == 4) {ds$phi1[n] = 0; ds$th1[n] = 0}
			if (ds$ax1[n] == 5) {ds$phi1[n] = 180; ds$th1[n] = 0}
			
			if (ds$ax2[n] == 1) {ds$phi2[n] = 90; ds$th2[n] = 0}
			if (ds$ax2[n] == 2) {ds$phi2[n] = 90; ds$th2[n] = 120}
			if (ds$ax2[n] == 3) {ds$phi2[n] = 90; ds$th2[n] = 240}
			if (ds$ax2[n] == 4) {ds$phi2[n] = 0; ds$th2[n] = 0}
			if (ds$ax2[n] == 5) {ds$phi2[n] = 180; ds$th2[n] = 0}
			}
	
	pt1 <- data.frame(radius = ds$rad1, theta = ds$th1, phi = ds$phi1)	
	pt2 <- data.frame(radius = ds$rad2, theta = ds$th2, phi = ds$phi2)
	pt1 <- sph2cart(pt1)
	pt2 <- sph2cart(pt2)

	# Compute control point, then create splines
	# Splines must be drawn one at a time (slow!)

	cp <- 0.6*(pt1 + pt2)
	
	pt1 <- as.matrix(pt1)
	cp <- as.matrix(cp)
	pt2 <- as.matrix(pt2)
		for (n in 1:nrow(pt1)) {
			spl <- rcsr(p0 = pt1[n,], cp = cp[n,], p1 = pt2[n,])
			lines3d(x = spl[,1], y = spl[,2], z = spl[,3],
				line_antialias = L_A, col = edges$color[n], lwd = edges$weight[n])		
			}
	
		} # end of nx = 5

##### 6D, This requires a 3D spline curve to be drawn
	
	if (nx == 6) {
	
		for (n in 1:nrow(ds)) {
			if (ds$ax1[n] == 1) {ds$phi1[n] = 90; ds$th1[n] = 0}
			if (ds$ax1[n] == 2) {ds$phi1[n] = 90; ds$th1[n] = 90}
			if (ds$ax1[n] == 3) {ds$phi1[n] = 90; ds$th1[n] = 180}
			if (ds$ax1[n] == 4) {ds$phi1[n] = 90; ds$th1[n] = 270}
			if (ds$ax1[n] == 5) {ds$phi1[n] = 0; ds$th1[n] = 0}
			if (ds$ax1[n] == 6) {ds$phi1[n] = 180; ds$th1[n] = 0}
			
			if (ds$ax2[n] == 1) {ds$phi2[n] = 90; ds$th2[n] = 0}
			if (ds$ax2[n] == 2) {ds$phi2[n] = 90; ds$th2[n] = 90}
			if (ds$ax2[n] == 3) {ds$phi2[n] = 90; ds$th2[n] = 180}
			if (ds$ax2[n] == 4) {ds$phi2[n] = 90; ds$th2[n] = 270}
			if (ds$ax2[n] == 5) {ds$phi2[n] = 0; ds$th2[n] = 0}
			if (ds$ax2[n] == 6) {ds$phi2[n] = 180; ds$th2[n] = 0}
			}
	
	pt1 <- data.frame(radius = ds$rad1, theta = ds$th1, phi = ds$phi1)	
	pt2 <- data.frame(radius = ds$rad2, theta = ds$th2, phi = ds$phi2)
	pt1 <- sph2cart(pt1)
	pt2 <- sph2cart(pt2)

	# Compute control point, then create splines
	# Splines must be drawn one at a time (slow!)

	cp <- 0.6*(pt1 + pt2)
	
	pt1 <- as.matrix(pt1)
	cp <- as.matrix(cp)
	pt2 <- as.matrix(pt2)
		for (n in 1:nrow(pt1)) {
			spl <- rcsr(p0 = pt1[n,], cp = cp[n,], p1 = pt2[n,])
			lines3d(x = spl[,1], y = spl[,2], z = spl[,3],
				line_antialias = L_A, col = edges$color[n], lwd = edges$weight[n])		
			}
	
		} # end of nx = 6

	
	} # closing brace, this is the very end!
