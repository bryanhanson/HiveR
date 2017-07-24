#' Summarize a hive plot data object and optionally run some checks
#' 
#' This function summarizes a \code{\link{HivePlotData}} object in a convenient
#' form. Optionally, it can run some checks for certain conditions that may be
#' of interest.  It can also output a summary of edges to be drawn, either as a
#' data frame or in a LaTeX ready form, or a data frame of orphaned nodes.
#' 
#' Argument \code{chk.sm.pt} applies only to hive plots of \code{type = 2D} and
#' only when edges exist that start and end on the same axis.  It checks to see
#' if any of the edges start and end at the same radius on the same axis, which
#' causes an error in \code{plotHive} since you are trying to draw an edge of
#' length zero (the actual error message is \code{Error in calcCurveGrob(x,
#' x$debug) : End points must not be identical}.  Some data sets may have such
#' cases intrinsically or due to data entry error, or the condition may arise
#' during processing.  Either way, one needs to be able to detect such cases
#' for removal or modification. This argument will tell you which nodes cause
#' the problem. \cr \cr Argument \code{chk.ax.jump} applies only to hive plots
#' of \code{type = 2D}.  It checks to see if any of the edges jump an axis,
#' e.g. axis 1 --> axis 3. This argument will tell you which nodes are at
#' either end of the jumping edge.  Jumping should should be avoided in hive
#' plots as it makes the plot aesthetically unpleasing.  However, depending
#' upon how you process the data, this condition may arise and hence it is
#' useful to be able to locate jumps.
#' 
#' @param HPD An object of S3 class \code{HivePlotData}.
#'
#' @param chk.all Logical; should all the checks below be run?  See Details.
#'
#' @param chk.sm.pt Logical; should the edges be checked to see if any of them
#' start and end on the same axis with the same radius?  See Details.
#'
#' @param chk.ax.jump Logical; should the edges be checked to see if any of
#' them start and end on non-adjacent axes, e.g. axis 1 --> axis 3?  See
#' Details.
#"
#' @param chk.sm.ax Logical; should the edges be checked to see if any of them
#' start and end on the same axis?
#'
#' @param chk.orphan.node Logical; should orphan nodes be identifed?  Orphan
#' nodes have degree 0 (no incoming or outgoing edges).
#'
#' @param plot.list Logical; should a data frame of edges to be drawn be
#' returned?
#'
#' @param tex Logical; should the \code{plot.list} be formatted for LaTeX?
#'
#' @param orphan.list Logical; should a data frame of orphaned nodes be
#' returned?
#'
#' @return A summary of the \code{HivePlotData} object's key characteristics is
#' printed at the console, followed by the results of any checks set to
#' \code{TRUE}.  The format of these results is identical to that of
#' \code{plot.list} described just below, except for the orphan node check.
#' This is formatted the same as \code{HPD$nodes}; see \code{?HPD} for details.
#' \cr \cr If \code{plot.list = TRUE}, a data frame containing a list of the
#' edges to be drawn in a format suitable for troubleshooting a plot.  If
#' \code{tex = TRUE} as well, the data frame will be in a format suitable for
#' pasting into a LaTeX document.  The data frame will contain rows describing
#' each edge to be drawn with the following columns: node 1 id, node 1 axis,
#' node 1 label, node 1 radius, then the same info for node 2, then the edge
#' weight and the edge color. \cr \cr If \code{orphan.list = TRUE} a data frame
#' giving the orphan nodes is returned.  If you want both \code{plot.list} and
#' \code{orphan.list} you have to call this function twice.
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#'
#' @references \url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
#'
#' @keywords utilities
#'
#' @export sumHPD
#'
#' @importFrom plyr count
#'
#' @examples
#' 
#' set.seed(55)
#' test <- ranHiveData(nx = 4, ne = 5, desc = "Tiny 4D data set")
#' out <- sumHPD(test, chk.all = TRUE, plot.list = TRUE)
#' print(out)
#' 
sumHPD <- function(HPD, chk.all = FALSE, chk.sm.pt = FALSE, chk.ax.jump = FALSE,
	chk.sm.ax = FALSE, chk.orphan.node = FALSE,
	plot.list = FALSE, tex = FALSE, orphan.list = FALSE){
	
# Function to summarize objects of S3 class 'HivePlotData'
# Part of HiveR package
# Bryan Hanson, DePauw Univ, Oct 2011


	chkHPD(HPD) # verify it's legit
	
	# Overall summary
	na <- length(unique(HPD$nodes$axis))
	
	cat("\t", HPD$desc, "\n", sep = "")
	cat("\tThis hive plot data set contains ",
		length(HPD$nodes$id), " nodes on ",
		na, " axes and ",
		length(HPD$edges$id1), " edges.\n", sep = "")
	cat("\tIt is a  ", HPD$type, " data set.\n\n", sep = "")

	# Now summarize the axes and nodes

	nodes <- HPD$nodes
	
	for (n in sort(unique(nodes$axis))) {
		g <- nodes[nodes[,"axis"] == n,]
#		g <- subset(nodes, axis == n)
		cat("\t\tAxis", n, "has", length(g$id), "nodes spanning radii from",
		min(g$radius), "to", max(g$radius), "\n", sep = " ")		
		}	

	# Create a list of edges to be drawn (used for several chks)
	
	n1.lab <- n1.rad <- n2.lab <- n2.rad <- n1.ax <- n2.ax <- c()

	for (n in 1:(length(HPD$edges$id1))) {
		pat1 <- HPD$edges$id1[n]
		pat2 <- HPD$edges$id2[n]
		pat1 <- paste("\\b", pat1, "\\b", sep = "") # ensures exact match
		pat2 <- paste("\\b", pat2, "\\b", sep = "")
		i1 <- grep(pat1, HPD$nodes$id)
		i2 <- grep(pat2, HPD$nodes$id)
		n1.lab <- c(n1.lab, HPD$nodes$lab[i1])
		n2.lab <- c(n2.lab, HPD$nodes$lab[i2])
		n1.rad <- c(n1.rad, HPD$nodes$radius[i1])
		n2.rad <- c(n2.rad, HPD$nodes$radius[i2])
		n1.ax <- c(n1.ax, HPD$nodes$axis[i1])
		n2.ax <- c(n2.ax, HPD$nodes$axis[i2])
		}

	fd <- data.frame(
		n1.id = HPD$edges$id1,
		n1.ax,
		n1.lab,
		n1.rad,
		n2.id = HPD$edges$id2,
		n2.ax,
		n2.lab,
		n2.rad,
		e.wt = HPD$edges$weight,
		e.col = HPD$edges$color)		

	# Now summarize edges by axis pair

	fd2 <- fd[,c(2,6)]
	fd2 <- plyr::count(fd2, vars = c("n1.ax", "n2.ax"))
	cat("\n")
	for (n in 1:nrow(fd2)) {
	cat("\t\tAxes", fd2$n1.ax[n], "and", fd2$n2.ax[n], "share", fd2$freq[n], "edges\n", sep = " ")		
		}
	cat("\n")

	##### Done with default basic summary #####
	
	# Perform the additional requested checks
	
	if (chk.all) chk.sm.pt <- chk.ax.jump <- chk.sm.ax <- chk.orphan.node <- TRUE

	if (chk.sm.pt) {
		prob <- which((fd$n1.rad == fd$n2.rad) & (fd$n1.ax == fd$n2.ax))
		if (length(prob) == 0) cat("\n\t No edges were found that start and end on the same point\n")
		if (length(prob) > 0) {
			cat("\n\n\tThe following edges start and end at the same point and the\n\tcorresponding nodes should be deleted, offset or\n\tjittered (or the edge deleted) before plotting:\n\n")
			print(fd[prob,], row.names = FALSE)
			}
		}

	if (chk.sm.ax) {
		prob <- which(fd$n1.ax == fd$n2.ax)
		if (length(prob) == 0) cat("\n\t No edges were found that start and end on the same axis\n")
		if (length(prob) > 0) {
			cat("\n\n\tThe following edges start and end on the same axis:\n\n")
			print(fd[prob,], row.names = FALSE)
			}
		}

	if (chk.orphan.node) {
		e.ids <- union(HPD$edges$id1, HPD$edges$id2)
		n.ids <- HPD$nodes$id
		prob <- setdiff(n.ids, e.ids)
		prob <- match(prob, HPD$nodes$id)
		if (length(prob) == 0) cat("\n\t No orphaned nodes were found\n")
		if (length(prob) > 0) {
			cat("\n\n\tThe following", length(prob), "nodes are orphaned (degree = 0):\n\n")
			print(HPD$nodes[prob,], row.names = FALSE)
			orphans <- HPD$nodes[prob,]
			}
		}

	if (chk.ax.jump) {
		prob <- which(
			((fd$n1.ax == 1) & (fd$n2.ax == 3)) &
			((fd$n1.ax == 2) & (fd$n2.ax == 4)) &
			((fd$n1.ax == 3) & (fd$n2.ax == 5)) &
			((fd$n1.ax == 4) & (fd$n2.ax == 6)) &
			((fd$n1.ax == 5) & (fd$n2.ax == 1)) &
			((fd$n1.ax == 6) & (fd$n2.ax == 2)) &
			#
			((fd$n1.ax == 6) & (fd$n2.ax == 4)) &
			((fd$n1.ax == 5) & (fd$n2.ax == 3)) &
			((fd$n1.ax == 4) & (fd$n2.ax == 2)) &
			((fd$n1.ax == 3) & (fd$n2.ax == 1)) &
			((fd$n1.ax == 2) & (fd$n2.ax == 6)) &
			((fd$n1.ax == 1) & (fd$n2.ax == 5)))
			
		if (length(prob) == 0) cat("\n\t No edges that jump axes were found\n")
		if (length(prob) > 0) {
			cat("\n\n\tThe following edges jump over an axis (and won't be drawn):\n\n")
			print(fd[prob,], row.names = FALSE)
			}
		}
		
	if ((tex) & (plot.list)) {
		if (!requireNamespace("xtable", quietly = TRUE)) {
			stop("To use option tex you need to install package xtable")
			}
		fd <- xtable::xtable(fd, hline.after = c(1), include.rownames = FALSE)
		xtable::align(fd) <- "|r|rrlr|rrlr|rl|"
		}	

	if (plot.list) return(fd) # user must not ask for both at the same time!
	if (orphan.list) return(orphans)
	}

