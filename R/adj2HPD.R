#' Process an Adjacency Graph into a HivePlotData Object
#'
#' This function will take an adjacency graph and convert it into a basic
#' \code{\link{HivePlotData}} object.  Further manipulation by
#' \code{\link{mineHPD}} will almost certainly be required before the data can
#' be plotted.
#'
#' This function produces a "bare bones" \code{HivePlotData} object.  The names
#' of the dimensions of \code{M} are used as the node names.  All nodes are
#' given size 1, an id number (\code{1:number of nodes}), are colored black and
#' are assigned to axis 1.  The edges are all gray, and the weight is M[i,j].
#' The user will likely have to manually make some changes to the resulting
#' \code{HivePlotData} object before plotting.  Alternatively,
#' \code{\link{mineHPD}} may be able to extract some information buried in the
#' data, but even then, the user will probably need to make some adjustments.
#' See the examples.
#'
#' @param M A matrix with named dimensions.  The names should be the node
#' names.  Should not be symmetric.  If it is, only the lower triangle is used
#' and a message is given.
#'
#' @param axis.cols A character vector giving the colors desired for the axes.
#'
#' @param type One of \code{c("2D", "3D")}.  If \code{2D}, a
#' \code{HivePlotData} object suitable for use with \code{\link{plotHive}} will
#' be created and the eventual hive plot will be static and 2D.  If \code{3D},
#' the \code{HivePlotData} object will be suitable for a 3D interactive plot
#' using \code{\link{plot3dHive}}.
#'
#' @param desc Character.  A description of the data set.
#'
#' @param \dots Other parameters to be passed downstream.
#'
#' @return A \code{ \link{HivePlotData}} object.
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu} Vesna
#' Memisevic contributed a fix that limited this function to bipartite networks
#' (changed in v. 0.2-12).
#'
#' @seealso \code{\link{dot2HPD}} and \code{\link{adj2HPD}}
#'
#' @keywords utilities
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @export adj2HPD
#'
#' @examples
#'
#' ### Example 1: a bipartite network
#' ### Note: this first example has questionable scientific value!
#' ### The purpose is to show how to troubleshoot and
#' ### manipulate a HivePlotData object.
#'
#' if (require("bipartite")) {
#'   data(Safariland, package = "bipartite") # This is a bipartite network
#'
#'   # You may wish to do ?Safariland or ?Safari for background
#'
#'   hive1 <- adj2HPD(Safariland, desc = "Safariland data set from bipartite")
#'   sumHPD(hive1)
#'
#'   # Note that all nodes are one axis with radius 1. Process further:
#'
#'   hive2 <- mineHPD(hive1, option = "rad <- tot.edge.count")
#'   sumHPD(hive2)
#'
#'   # All nodes still on 1 axis but degree has been used to set radius
#'
#'   # Process further:
#'
#'   hive3 <- mineHPD(hive2, option = "axis <- source.man.sink")
#'   sumHPD(hive3, chk.all = TRUE)
#'
#'   # Note that mineHPD is generating some warnings, telling us
#'   # that the first 9 nodes were not assigned to an axis.  Direct
#'   # inspection of the data shows that these nodes are insects
#'   # that did not visit any of the flowers in this particular study.
#'
#'   # Pretty up a few things, then plot:
#'
#'   hive3$edges$weight <- sqrt(hive3$edges$weight) * 0.5
#'   hive3$nodes$size <- 0.5
#'   plotHive(hive3)
#'
#'   # This is a one-sided hive plot of 2 axes, which results
#'   # from the curvature of the splines.  We can manually fix
#'  # this by reversing the ends of edges as follows:
#'
#'   for (n in seq(1, length(hive3$edges$id1), by = 2)) {
#'     a <- hive3$edges$id1[n]
#'     b <- hive3$edges$id2[n]
#'     hive3$edges$id1[n] <- b
#'     hive3$edges$id2[n] <- a
#'   }
#'
#'   plotHive(hive3)
#'
#'   ### Example 2, a simple random adjacency matrix
#'   set.seed(31)
#'   nr <- 20
#'   nc <- 15
#'   M <- matrix(floor(runif(nc * nr, 0, 10)), ncol = nc)
#'   colnames(M) <- sample(c(letters, LETTERS), nc, replace = FALSE)
#'   rownames(M) <- sample(c(letters, LETTERS), nr, replace = FALSE)
#'   hive4 <- adj2HPD(M)
#'   sumHPD(hive4)
#' }
#'
adj2HPD <- function(M = NULL, axis.cols = NULL, type = "2D", desc = NULL, ...) {

  # Function to read adjacency matrices and convert to HPD
  # Bryan Hanson, DePauw Univ, December 2011
  # Part of HiveR package

  # Assumptions/Caveats:

  # No checking for whether the type (2D/3D) is actually true/relevant
  # Without outside info, many parameters have to be set arbitrarily
  # and perhaps changed later.

  if (is.null(M)) stop("No adjacency matrix provided")
  if (is.null(dimnames(M))) stop("Adjacency matrix must have named dimensions")
  if (isSymmetric(M)) {
    message("Matrix is symmetric, using only the lower triangle")
    M[upper.tri(M)] <- 0
  }

  lab1 <- unlist(dimnames(M)[1])
  lab1 <- as.character(lab1)
  lab2 <- unlist(dimnames(M)[2])
  lab2 <- as.character(lab2)
  d1 <- dim(M)[1]
  d2 <- dim(M)[2]

  nn <- length(unique(c(lab1, lab2)))
  size <- rep(1, nn)
  id <- 1:nn
  axis <- rep(1, nn)
  color <- as.character(rep("black", nn))
  radius <- rep(1, nn)

  # Set up HPD$nodes

  HPD <- list()
  HPD$nodes$id <- id
  labNames <- unique(c(lab1, lab2)) # VM fix v. 0.2-12
  HPD$nodes$lab <- labNames # VM fix v. 0.2-12
  HPD$nodes$axis <- axis
  HPD$nodes$radius <- radius
  HPD$nodes$size <- size
  HPD$nodes$color <- color

  # Set up HPD$edges

  id1 <- id2 <- v <- c() # v = value of M[i, j]
  for (i in 1:d1) {
    for (j in 1:d2) {
      if (!M[i, j] == 0) {
        id1 <- c(id1, which(lab1[i] == labNames)) # VM fix v. 0.2-12
        id2 <- c(id2, which(lab2[j] == labNames)) # VM fix v. 0.2-12
        v <- c(v, M[i, j])
      }
    }
  }

  if (!length(id1) == length(id2)) stop("Something is wrong with the M[i,j] counts")
  ne <- length(id1)

  HPD$edges$id1 <- id1
  HPD$edges$id2 <- id2
  HPD$edges$weight <- v
  HPD$edges$color <- rep("gray", ne)

  # Final clean-up

  HPD$nodes <- as.data.frame(HPD$nodes)
  HPD$edges <- as.data.frame(HPD$edges)

  if (is.null(desc)) desc <- "No description provided"
  HPD$desc <- desc

  if (is.null(axis.cols)) axis.cols <- RColorBrewer::brewer.pal(length(unique(HPD$nodes$axis)), "Set1")
  HPD$axis.cols <- axis.cols

  HPD$nodes$axis <- as.integer(HPD$nodes$axis)
  HPD$nodes$size <- as.numeric(HPD$nodes$size)
  HPD$nodes$color <- as.character(HPD$nodes$color)
  HPD$nodes$lab <- as.character(HPD$nodes$lab)

  HPD$edges$id1 <- as.integer(HPD$edges$id1)
  HPD$edges$id2 <- as.integer(HPD$edges$id2)
  HPD$edges$weight <- as.numeric(HPD$edges$weight)
  HPD$edges$color <- as.character(HPD$edges$color)

  HPD$type <- type

  class(HPD) <- "HivePlotData"

  chkHPD(HPD)

  HPD
} # The very end!
