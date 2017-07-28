#' Examine (mine) a Hive Plot Data Object and Extract Information Contained
#' Within It
#' 
#' A \code{HivePlotData object}, especially one created fresh using
#' \code{\link{dot2HPD}}, generally contains a lot of hidden information about
#' the network described. This function can extract this hidden information.
#' This function has \code{option}s which are quite specific as to what they
#' do.  The user can easily write new options and incorporate them.
#' This function can be called multiple times
#' using different options to gradually modify the \code{HivePlotData} object.
#' 
#' \code{option = "rad <- tot.edge.count"} This option looks through the
#' \code{HivePlotData} object and determines how many edges start or end on
#' each node (the "degree").  This value is then assigned to the radius for
#' that node.
#'
#' \code{option = "axis <- source.man.sink"} This option
#' examines the nodes and corresponding edges in a \code{HivePlotData} object
#' to determine if the node is a source, manager or sink.  A source node only
#' has outgoing edges.  A sink node only has incoming edges.  A manager has
#' both.  Hence, this option treats the \code{HivePlotData} object as if it
#' were directed in that the first node of an edge in will be in
#' \code{HPD$nodes$id1} and the second node of an edge will be in
#' \code{HPD$nodes$id2}.  As a result, this option produces a hive plot with 3
#' axes (note: sources are on axis 1, sinks on axis 2, and managers on axis 3).
#' This concept is similar to the idea of \code{\link[FuncMap]{FuncMap}} but
#' the internals are quite different.  See also \code{\link{dot2HPD}} for some
#' details about processing .dot files in an agnostic fashion.
#'
#' \code{option = "remove orphans"} removes nodes that have degree zero (no
#' incoming or outgoing edges).
#'
#' \code{option = "remove zero edge"}
#' removes edges with length zero.  Such edges cause an error because
#' the spline cannot be drawn.  This option combines the next two options.
#'
#' \code{option = "remove self edge"} removes edges that
#' start and end on the same node.
#' 
#' \code{option = "remove virtual edge"} removes virtual edges which are
#' edges which involve different nodes but the nodes happen to be on the
#' the same axis at the same radius.
#' 
#' @param HPD A \code{\link{HivePlotData}} object.
#'
#' @param option A character string giving the option desired.  See Details for
#' current options.
#'
#' @return A modified \code{HivePlotData} object.
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#'
#' @seealso See the vignette for an example of using this function.  Use
#' \code{browseVignettes("HiveR")} to produce the vignette.
#'
#' @keywords utilities
#'
#' @export mineHPD
#'
mineHPD <- function(HPD, option = "rad <- tot.edge.count") {
	
# Function to process HPD objects in various ways
# to dig out additional hidden info.
# dot2HPD can only use the attribute tags in the dot file.
# However, the graph intrinsically contains additional info
# which can be mapped into a Hive Plot
# This function can dig that info out.
# Additional methods are easily added to this function.

# Bryan Hanson, DePauw Univ, July 2011 onward

# Check for valid option

	curopts <- c("rad <- tot.edge.count",
		"axis <- source.man.sink",
		"remove orphans",
		"remove virtual edge",
		"remove self edge",
		"remove zero edge")
		
	if (!option %in% curopts) {
		message("Unrecognized option, select from:")
		print(curopts)
		# msg <- paste("Unrecognized option, select from", paste(curopts, collapse = ", "), sep = " ")
		stop("invalid option")
	}
	edges <- HPD$edges
	nodes <- HPD$nodes
	nn <- length(nodes$id)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

	if (option == "rad <- tot.edge.count") {

# This option assigns a radius value to a node
# based upon the total number of edges in which the node participates.

		for (n in 1:nn) {
			pat <- paste("\\b", nodes$id[n], "\\b", sep = "")
			p <- length(grep(pat, edges$id1))
			q <- length(grep(pat, edges$id2))
			nodes$radius[n] <- p + q			
			}
		
		}  ##### end of option == "rad <- tot.edge.count"

### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

	if (option == "axis <- source.man.sink") {

# This option assigns a node to an axis
# based upon whether it is a source, manager or sink
# by examining the edges.
# Note that this option assumes a directed
# input graph, as a source node only has "outgoing" edges etc.
# Thus all edges start at edges$id1 and end on edges$id2.

# source = axis 1, manager = axis 3, sink = axis  2
# Basic procedure is that a node only listed in
# edges$id2 is a sink etc.
# Do things affirmatively, not assuming a default:
# Ensures that things are done correctly.

		# if (!length(unique(nodes$axis)) == 3) {
			# stop("This option requires 3 unique axes")
			# }
		
		done <- FALSE # a check to make sure all nodes get an axis
		
		for (n in 1:nn) {
			pat <- paste("\\b", nodes$id[n], "\\b", sep = "")
			id1 <- grep(pat, edges$id1)
			id2 <- grep(pat, edges$id2)
			
			if ((length(id1) == 0) & (length(id2) > 0 )) {
				nodes$axis[n] <- 2
				done <- TRUE
				next
				} # these are sinks, as they only receive an edge
			
			# note that set operations below drop duplicate values
				
			if ((length(id1) > 0) & (length(id2) > 0 )) {
				common <- union(id1, id2)
				source <- setdiff(id1, common)
				if (length(source) == 1) {
					nodes$axis[n] <- 1
					done <- TRUE
					next		
					} # these are sources

				if (length(common) >= 1) {
					nodes$axis[n] <- 3
					done <- TRUE
					next		
					} # these are managers
				} 

			if (!done) {
				msg <- paste("node ", nodes$id[n], " was not assigned to an axis", sep = "")
				warning(msg)
				}  # alert the user there was a problem
			
			} # end of loop inspecting nodes

		nodes$axis <- as.integer(nodes$axis)
		
		}  ##### end of option == "axis <- source.man.sink

### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

	if (option == "remove orphans") {

# This option removes orphaned nodes (which have no edges)
# Almost the same code as over in sumHPD

		e.ids <- union(HPD$edges$id1, HPD$edges$id2)
		n.ids <- HPD$nodes$id
		prob <- setdiff(n.ids, e.ids)
		prob <- match(prob, HPD$nodes$id)
		if (length(prob) == 0) cat("\n\t No orphaned nodes were found\n")
		if (length(prob) > 0) {
			cat("\n\t", length(prob), "orphaned nodes (degree = 0) were removed\n\n")
			nodes <- nodes[-prob,]
			}
		
		}  ##### end of option == "remove orphans"

### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

	if (option == "remove self edge") {

# This option removes edges which start and end on the same node
# It re-uses code from sumHPD

	# Create a list of edges to be drawn
	
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
		n1.lab = as.character(n1.lab), # June 2017
		n1.rad,
		n2.id = HPD$edges$id2,
		n2.ax,
		n2.lab = as.character(n2.lab), # June 2017
		n2.rad,
		e.wt = HPD$edges$weight,
		e.col = HPD$edges$color,
		stringsAsFactors = FALSE)		

	bad <- NA_integer_
	
	prob <- which(fd$n1.lab == fd$n2.lab)
	if (length(prob) == 0) cat("\n\t No edges were found that start and end on the same node\n")
	if (length(prob) > 0) {

		for (n in 1:(length(HPD$edges$id1))) {
			pat1 <- HPD$edges$id1[n]
			pat2 <- HPD$edges$id2[n]
			if (pat1 == pat2) bad <- c(bad, n)
			}
			
		cat("\n\t", length(na.omit(bad)), "edge(s) that start and end on the same node were removed\n")
		}
	
	# Now actually remove the edges
		
	bad <- unique(na.omit(bad))
	edges <- edges[-bad,]
	}  ##### end of option == "remove self edge"

### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

	if (option == "remove virtual edge") {

# This option removes edges which start and end on the same
# axis at the same radius.  It reuses code from sumHPD

	# Create a list of edges to be drawn
	
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
		n1.lab = as.character(n1.lab), # June 2017
		n1.rad,
		n2.id = HPD$edges$id2,
		n2.ax,
		n2.lab = as.character(n2.lab), # June 2017
		n2.rad,
		e.wt = HPD$edges$weight,
		e.col = HPD$edges$color,
		stringsAsFactors = FALSE)		

	bad <- NA_integer_ 
	prob1 <- which((fd$n1.rad == fd$n2.rad) & (fd$n1.ax == fd$n2.ax))
	prob2 <- which(fd$n1.id == fd$n2.id) # omit true self edges
	prob <- setdiff(prob1, prob2)
	
	if (length(prob) == 0) cat("\n\t No virtual self-edges were found\n")
	if (length(prob) > 0) {

		virtProb <- data.frame(id1 = fd$n1.id[prob], id2 = fd$n2.id[prob])
		for (i in 1:nrow(virtProb)) {
			for (j in 1:(length(HPD$edges$id1))) {
				if ((HPD$edges$id1[j] == virtProb[i,1]) & (HPD$edges$id2[j] == virtProb[i,2])) bad <- c(bad, j)
				if ((HPD$edges$id1[j] == virtProb[i,2]) & (HPD$edges$id2[j] == virtProb[i,1])) bad <- c(bad, j)
			}
		}
		cat("\n\t", length(na.omit(bad)), "virtual self-edge(s) were removed\n")
		}
	
	# Now actually remove the edges
		
	bad <- unique(na.omit(bad))
	edges <- edges[-bad,]
	}  ##### end of option == "remove virtual edge"

### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

	if (option == "remove zero edge") {

# This option runs the two above
		HPD <- mineHPD(HPD, option = "remove self edge")
		HPD <- mineHPD(HPD, option = "remove virtual edge")
		return(HPD)
		}  ##### end of option == "remove zero edge"

### ++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

	# Final assembly and checking...
	
	HPD$edges <- edges
	HPD$nodes <- nodes
	chkHPD(HPD)
	HPD
	}
