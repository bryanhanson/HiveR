#' Convert Spherical to Cartesian Coordinates
#' 
#' This function converts spherical to Cartesian coordinates.
#' 
#' 
#' @param df A data frame with columns named r, theta and phi with the radius
#' and angles (in spherical coordinates) to be converted to Cartesian
#' coordinates.
#'
#' @return A data frame with named columns containing the converted
#' coordinates.
#'
#' @note Cobbled together from similar functions in other packages.
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#'
#' @keywords utilities
#'
#' @export sph2cart
#'
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
