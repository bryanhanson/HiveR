#' Compute the details of a 3D spline for a hive plot edge
#' 
#' This is a wild bit of trigonometry!  Three points in 3D space, two ends and
#' an control point, are rotated into 2D space.  Then a spline curve is
#' computed.  This is necessary because spline curves are only defined in
#' \code{R} as 2D objects.  The new collection of points, which is the complete
#' spline curve and when drawn will be the edge of a hive plot, is rotated back
#' into the original 3D space. \code{rcsr} stands for rotate, compute spline,
#' rotate back.
#' 
#' See the code for exactly how the function works.  Based upon the process
#' described at \url{http://www.fundza.com/mel/axis_to_vector/index.html}
#' Timing tests show this function is fast and scales linearly (i.e. 10x more
#' splines to draw takes 10x more time).  Roughly 3 seconds were required to
#' draw 1,000 spline curves in my testing.
#' 
#' @param p0 A triple representing one end of the final curve (x, y, z).
#'
#' @param cp A triple representing the control point used to compute the final
#' curve (x, y, z).
#'
#' @param p1 A triple representing the other end of the final curve (x, y, z).
#'
#' @return A 3 column matrix with the x, y and z coordinates to be plotted to
#' create a hive plot edge.
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#'
#' @references \url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
#'
#' @keywords utilities 3D spline
#'
#' @export rcsr
#'
#' @importFrom stats spline
#'
#' @examples
#' 
#' # This is a lengthy example to prove it works.
#' # Read it and then copy the whole thing to a blank script.
#' # Parts of it require rgl and are interactive.
#' # So none of the below is run during package build/check.
#' 
#' ### First, a helper function
#' \dontrun{
#' 
#' drawUnitCoord <- function() {
#' 	
#' 	# Simple function to draw a unit 3D coordinate system
#' 	
#' 	# Draw a Coordinate System
#' 	
#' 	r <- c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1) # in polar coordinates
#' 	theta <- c(0, 0, 0, 90, 0, 180, 0, 270, 0, 0, 0, 0)  # start, end, start, end
#' 	phi <- c(0, 90, 0, 90, 0, 90, 0, 90, 0, 0, 0, 180)
#' 	cs <- data.frame(radius = r, theta, phi)
#' 	ax.coord <- sph2cart(cs)
#' 	
#' 	segments3d(ax.coord, col = "gray", line_antialias = TRUE)
#' 	points3d(x = 0, y = 0, z = 0, color = "black", size = 4,
#' 		point_antialias = TRUE) # plot origin
#' 	
#' 	# Label the axes
#' 
#' 	r <- c(1.1, 1.1, 1.1, 1.1, 1.1, 1.1) # in polar coordinates
#' 	theta <- c(0, 90, 180, 270, 0, 0)
#' 	phi <- c(90, 90, 90, 90, 0, 180)
#' 	l <- data.frame(radius = r, theta, phi)
#' 	lab.coord <- sph2cart(l)
#' 	text3d(lab.coord, texts = c("+x", "+y", "-x", "-y", "+z", "-z"))
#' 	
#' 	}
#' 
#' ###  Now, draw a reference coordinate system and demo the function in it.
#' 
#' drawUnitCoord()
#' 
#' ### Draw a bounding box
#' 
#' box <- data.frame( 
#' 	x = c(1, -1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, 1, 1, 1, -1, 1, -1, -1, -1, -1, -1, -1, -1),
#' 	y = c(1, 1, 1, 1, 1, -1, 1, -1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 1, -1, 1),
#' 	z = c(1, 1, 1, -1, 1, 1, -1, -1, -1, -1, 1, -1, 1, -1, 1, 1, -1, -1, -1, 1, 1, 1, -1, -1))
#' 
#' segments3d(box$x, box$y, box$z, line_antialias = TRUE, col = "red")
#' 
#' ### Draw the midlines defining planes
#' 
#' mid <- data.frame( 
#' 	x = c(0, 0, 0, 0, 0, 0, 0, 0, 1, -1, -1, -1, -1, 1, 1, 1, 1, -1, -1, -1, -1, 1, 1, 1),
#' 	y = c(-1, -1, -1, 1, 1, 1, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -1, 1, 1, 1, 1, -1),
#' 	z = c(-1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, 1, 1, 1, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0))
#' 
#' segments3d(mid$x, mid$y, mid$z, line_antialias = TRUE, col = "blue")
#' 
#' ### Generate two random points
#' 
#' p <- runif(6, -1, 1)
#' 
#' # Special case where p1 is on z axis
#' # Uncomment line below to demo
#' #p[4:5] <- 0
#' 
#' p0 <- c(p[1], p[2], p[3])
#' p1 <- c(p[4], p[5], p[6])
#' 
#' ### Draw the pts, label them, draw vectors to those pts from origin
#' 
#' segments3d(x = c(0, p[1], 0, p[4]),
#' 	y = c(0, p[2], 0, p[5]),
#' 	z = c(0, p[3], 0, p[6]),
#' 	line_antialias = TRUE, col = "black", lwd = 3)
#' 
#' points3d(x = c(p[1], p[4]),
#' 	y = c(p[2], p[5]),
#' 	z = c(p[3], p[6]),
#' 	point_antialias = TRUE, col = "black", size = 8)
#' 	
#' text3d(x = c(p[1], p[4]),
#' 	y = c(p[2], p[5]),
#' 	z = c(p[3], p[6]),
#' 	col = "black", texts = c("p0", "p1"), adj = c(1,1))
#' 
#' ### Locate control point
#' ### Compute and draw net vector from origin thru cp
#' ### Connect p0 and p1
#' 
#' s <- p0 + p1 
#' segments3d(x = c(0, s[1]), y = c(0, s[2]), z = c(0, s[3]),
#' 	line_antialias = TRUE, col = "grey", lwd = 3)
#' 
#' segments3d(x = c(p[1], p[4]), # connect p0 & p1
#' 	y = c(p[2], p[5]),
#' 	z = c(p[3], p[6]),
#' 	line_antialias = TRUE, col = "grey", lwd = 3)
#' 
#' cp <- 0.6*s # Now for the control point
#' 
#' points3d(x = cp[1], # Plot the control point
#' 	y = cp[2],
#' 	z = cp[3],
#' 	point_antialias = TRUE, col = "black", size = 8)
#' 
#' text3d(x = cp[1], # Label the control point
#' 	y = cp[2],
#' 	z = cp[3],
#' 	texts = c("cp"), col = "black", adj = c(1,1))
#' 
#' ### Now ready to work on the spline curve
#' 
#' n2 <- rcsr(p0, cp, p1) # Compute the spline
#' 
#' lines3d(x = n2[,1], y = n2[,2], z = n2[,3],
#' 	line_antialias = TRUE, col = "blue", lwd = 3)
#' 
#' ### Ta-Da!!!!!
#' }
#' 
rcsr <- function(p0, cp, p1) {
	
	# Bryan Hanson, DePauw Univ, April 2011
	
	# Function to take 3 points in 3d and compute a spline curve
	# using them.  The strategy is to rotate into a 2d system,
	# figure the spline, and then rotate back to the orig 3d system.
	
	# p0 and p1 are end points, cp is the control point
	
	# rcsr = rotate, compute spline, rotate back
	
	# Local copies of functions borrowed from RFOC
	
	rotx3 <- function (deg) {
	    rad1 = deg * 0.0174532925199
	    r = diag(3)
	    r[2, 2] = cos(rad1)
	    r[2, 3] = sin(rad1)
	    r[3, 3] = r[2, 2]
	    r[3, 2] = -r[2, 3]
	    return(r)
		}

	roty3 <- function (deg) {
	    rad1 = deg * 0.0174532925199
	    r = diag(3)
	    r[1, 1] = cos(rad1)
	    r[3, 1] = sin(rad1)
	    r[3, 3] = r[1, 1]
	    r[1, 3] = -r[3, 1]
	    return(r)
		}

	rotz3 <- function (deg) {
	    rad1 = deg * 0.0174532925199
	    r = diag(3)
	    r[1, 1] = cos(rad1)
	    r[1, 2] = sin(rad1)
	    r[2, 2] = r[1, 1]
	    r[2, 1] = -r[1, 2]
	    return(r)
		}

	m <- matrix(c(0, 0, 0, p0, cp, p1), nrow = 4, byrow = TRUE)

# Align p0 with the +y axis by rotating around x and z axes
# see www.fundza.com/mel/axis_to_vector/align_axis_to_vector.html
# Other points follow using rotation matrix

	xyL <- sqrt(m[2,1]^2 + m[2,2]^2)
	vL <- sqrt(sum((m[2,]^2)))

	if (xyL == 0) { # have to catch when p1 is on z axis
		zA <- ifelse(m[2,1] > 0, 90, -90)
		} else {
		zA <-acos(m[2,2]/xyL)
		zA <- ifelse(m[2,1] > 0, -zA, zA) # adjusts for quadrant
		}
	zA <- zA*180/pi
	zA <- rotz3(zA)

	xA <- acos(xyL/vL)
	xA <- ifelse(m[2,3] > 0, xA, -xA) # adjusts for quadrant
	xA <- xA*180/pi
	xA <- rotx3(xA)

	n1 <- t(zA %*% t(m)) # order of rotations matter!
	n2 <- t(xA %*% t(n1))

# Now rotate p1 to +x+y plane by rotating around y axis

	yzL <- sqrt(n2[4,1]^2 + n2[4,3]^2)

	yA <-acos(n2[4,3]/yzL)
	yA <- ifelse(n2[4,1] > 0, yA, -yA) # adjusts for quadrant
	yA <- 90 + (yA*180/pi)
	yA <- roty3(yA)

	n3 <- t(yA %*% t(n2))

# Compute spline curve
	n3 <- n3[-1,] # remove origin point
	sp <- stats::spline(n3[,1], n3[,2], n = 25)
	sp <- matrix(c(sp$x, sp$y, rep(0, length(sp$x))), ncol = 3) # add back the z coord (all 0's)

# Now reverse the transformations back to the original 3d space

	sp <- t(t(yA) %*% t(sp))
	sp <- t(t(xA) %*% t(sp))
	sp <- t(t(zA) %*% t(sp))
	sp
	}
