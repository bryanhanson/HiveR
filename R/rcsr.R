



rcsr <- function(p0, cp, p1) {
	
	# Bryan Hanson, DePauw Univ, April 2011
	
	# Function to take 3 points in 3d and compute a spline curve
	# using them.  The strategy is to rotate into a 2d system,
	# figure the spline, and then rotate back to the orig 3d system.
	
	# p0 and p1 are end points, cp is the control point
	
	# rcsr = rotate, compute spline, rotate back
		
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
	sp <- spline(n3[,1], n3[,2], n = 25)
	sp <- matrix(c(sp$x, sp$y, rep(0, length(sp$x))), ncol = 3) # add back the z coord (all 0's)

# Now reverse the transformations back to the original 3d space

	sp <- t(t(yA) %*% t(sp))
	sp <- t(t(xA) %*% t(sp))
	sp <- t(t(zA) %*% t(sp))
	sp
	}
