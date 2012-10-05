
cart2sph <- function(df) {
	
	# Bryan Hanson
	# DePauw Univ, Feb 2011
	
	radius <- sqrt(df$x^2 + df$y^2 + df$z^2)
	if (any(radius == 0)) warning("One of these points is the origin")
	phi <- acos(df$z/radius)*180/pi
	theta <- atan2(df$y, df$x)*180/pi
	ans <- data.frame(radius, theta, phi) # answer in degrees
	}