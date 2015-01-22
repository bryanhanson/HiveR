
sph2cart <- function(df) {
	
	# Bryan Hanson
	# DePauw Univ, Feb 2011
	
	theta <- df$theta * 2 * pi/360 # input angles in degrees
	phi <- df$phi * 2 * pi/360
	x <- df$r * cos(theta) * sin(phi)
	y <- df$r * sin(theta) * sin(phi)
	z <- df$r * cos(phi) 
	ans <- data.frame(x, y, z)
	} # answer in degrees
