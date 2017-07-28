#' Verify the Integrity of a Hive Plot Data Object
#' 
#' This function inspects the classes of each part of a \code{\link{HPD}} as a
#' means of verifying its integrity.  A few other characteristics are checked
#' as well.
#' 
#' 
#' @param HPD An object of S3 class \code{HivePlotData}.
#'
#' @param confirm Logical; if \code{TRUE} then a favorable result is affirmed
#' in the console (problems are always reported).
#'
#' @return A logical value; \code{TRUE} is there is a problem, otherwise
#' \code{FALSE}.
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#'
#' @seealso \code{\link{sumHPD}} which allows inspection (checking) of many
#' properties of your \code{\link{HPD}}.
#'
#' @keywords utilities
#'
#' @export chkHPD
#'
#' @examples
#' 
#' test4 <- ranHiveData(nx = 4)
#' good <- chkHPD(test4, confirm = TRUE)
#' # mess it up and do again
#' # next test is not run as it halts execution
#' \dontrun{
#' test4$nodes$color <- as.factor(test4$nodes$color)
#' bad <- chkHPD(test4)
#' }
#' 
chkHPD <- function(HPD, confirm = FALSE) {

# Function to Check the Integrity of HPD Objects
# Bryan Hanson, DePauw University, Oct 2011
# Part of HiveR package

	if (missing(HPD)) stop("Nothing to check")
	w <- FALSE
	
	if (!class(HPD) == "HivePlotData") { warning("The object provided was not of class HivePlotData"); w <- TRUE }

	if (!class(HPD$nodes) == "data.frame") { warning("The nodes data appear to be corrupt"); w <- TRUE }
	if (!class(HPD$nodes$id) == "integer") { warning("nodes$id appears to be corrupt"); w <- TRUE }
	if (!class(HPD$nodes$radius) == "numeric") { warning("nodes$radius appears to be corrupt"); w <- TRUE }
	if (!class(HPD$nodes$lab) == "character") { warning("nodes$lab appears to be corrupt"); w <- TRUE }
	if (!class(HPD$nodes$axis) == "integer") { warning("nodes$axis appears to be corrupt"); w <- TRUE }
	if (!class(HPD$nodes$color) == "character") { warning("nodes$color appears to be corrupt"); w <- TRUE }
	if (!class(HPD$nodes$size) == "numeric") { warning("nodes$size appears to be corrupt"); w <- TRUE }

	if (!class(HPD$edges) == "data.frame") { warning("The edges data appear to be corrupt"); w <- TRUE }
	if (!class(HPD$edges$id1) == "integer") { warning("edges$id1 appears to be corrupt"); w <- TRUE }
	if (!class(HPD$edges$id2) == "integer") { warning("edges$id2 appears to be corrupt"); w <- TRUE }
	if (!class(HPD$edges$weight) == "numeric") { warning("edges$weight appears to be corrupt"); w <- TRUE }
	if (!class(HPD$edges$color) == "character") { warning("edges$color appears to be corrupt"); w <- TRUE }


	if (!class(HPD$desc) == "character") { warning("The description appears to be corrupt"); w <- TRUE }
	if (!class(HPD$axis.cols) == "character") { warning("axis.cols appears to be corrupt"); w <- TRUE }

	if (!((HPD$type == "2D") | (HPD$type == "3D"))) { warning("Type must be 2D or 3D"); w <- TRUE }

	if (any(HPD$nodes$radius < 0)) warning("Some node radii < 0; the behavior of these is unknown")
	if (any(HPD$nodes$size < 0)) warning("Some node sizes < 0; the behavior of these is unknown")
	if (any(HPD$edges$weight < 0)) warning("Some edge widths (weights) < 0; the behavior of these is unknown")
	
	if ((!w) && (confirm)) cat("You must be awesome: This hive plot data looks dandy!")
	if (w) {
		cat("*** There seem to be one or more problems with this hive plot data!\n")
		stop("Sorry, we can't continue this way: It's not me, it's you!\n")
		}

	return(w)
	}

