#' Modify the Display of Axes and Nodes in a Hive Plot
#'
#' This function modifies various aspects of a \code{HivePlotData} object.  A
#' typical use is to convert the radii from the native/absolute values in the
#' original object to either a normalized value (0\ldots{}1) or to a ranked
#' value.  The order of nodes on an axis can also be inverted, and an axis can
#' be pruned (removed) from the \code{HivePlotData} object.
#'
#' The rank method uses \code{ties.method = "first"} so that each node gets a
#' unique radius.  For pruning, the nodes and edges are removed and then the
#' remaining axes are renumbered to start from one.  Exercise caution!
#'
#' For \code{"scale"} node radii will be multiplied by the corresponding value
#' in this argument.  For \code{"invert"} a value of -1 will cause the
#' corresponding axis to be inverted.  For \code{"prune"}, a single value
#' specifying the axis to be pruned should be given.  For \code{"offset"} the
#' values in \code{"action"} will be subtracted from the node radii.  For
#' \code{"stretch"}, node radii will first be offset so that the minimum value
#' is zero, then multiplied by the values in \code{"action"} to stretch the
#' axis.  Depending upon the desired effect, one might use \code{"stretch"}
#' followed by \code{"offset"} or perhaps other combinations.
#'
#' @param HPD An object of S3 class \code{HivePlotData}.
#'
#' @param method One of \code{c("rank", "norm", "scale", "invert", "ranknorm",
#' "prune", "offset", "stretch")} giving the type of modification to be made.
#'
#' @param action For \code{method = c("scale", "invert", "offset", "stretch")},
#' a numeric vector of the same length as the number of axes.
#'
#' @param ...  Arguments to be passed downstream.  Needed in this case for when
#' \code{plotHive} has arguments for \code{grid} that get laundered through
#' \code{manipAxis}
#'
#' @return A modified \code{HivePlotData} object.
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#'
#' @keywords utilities
#'
#' @export manipAxis
#'
#' @examples
#'
#' data(HEC)
#' # The first 3 examples take advantage of the argument '...'
#' # in plotHive, which passes action through to manipAxis on the fly.
#' # For this particular data, norm and absolute scaling appear the same.
#'
#' plotHive(HEC, bkgnd = "white") # default is absolute positioning of nodes
#' plotHive(HEC, method = "rank", bkgnd = "white")
#' plotHive(HEC, method = "norm", bkgnd = "white")
#'
#' # In these examples, we'll explicitly use manipAxis and then plot
#' # in a separate step.  This is because trying to plot on the fly in
#' # these cases will result in absolute scaling (which we do use here,
#' # but one might not want to be forced to do so).
#'
#' HEC2 <- manipAxis(HEC, method = "invert", action = c(-1, 1))
#' plotHive(HEC2, bkgnd = "white")
#' HEC3 <- manipAxis(HEC, method = "stretch", action = c(2, 3))
#' plotHive(HEC3, bkgnd = "white")
#' HEC4 <- manipAxis(HEC, method = "offset", action = c(0, 1.5))
#' plotHive(HEC4, bkgnd = "white")
manipAxis <- function(HPD, method, action = NULL, ...) {

  # Function to rank or norm a Hive Plot Object
  # Part of Hive3dR
  # Bryan Hanson, DePauw Univ, July 2011 onward

  # Check for valid option

  if (!method %in% c(
    "rank", "norm", "invert", "scale", "prune", "ranknorm",
    "offset", "stretch"
  )) {
    stop("Unrecognized method")
  }

  chkHPD(HPD)
  nodes <- HPD[[1]]
  nx <- length(unique(nodes$axis))

  if (nx == 2) {
    n1 <- which(nodes$axis == 1)
    n2 <- which(nodes$axis == 2)
  }

  if (nx == 3) {
    n1 <- which(nodes$axis == 1)
    n2 <- which(nodes$axis == 2)
    n3 <- which(nodes$axis == 3)
  }

  if (nx == 4) {
    n1 <- which(nodes$axis == 1)
    n2 <- which(nodes$axis == 2)
    n3 <- which(nodes$axis == 3)
    n4 <- which(nodes$axis == 4)
  }

  if (nx == 5) {
    n1 <- which(nodes$axis == 1)
    n2 <- which(nodes$axis == 2)
    n3 <- which(nodes$axis == 3)
    n4 <- which(nodes$axis == 4)
    n5 <- which(nodes$axis == 5)
  }

  if (nx == 6) {
    n1 <- which(nodes$axis == 1)
    n2 <- which(nodes$axis == 2)
    n3 <- which(nodes$axis == 3)
    n4 <- which(nodes$axis == 4)
    n5 <- which(nodes$axis == 5)
    n6 <- which(nodes$axis == 6)
  }

  if (method == "rank") {
    if (nx == 2) {
      nodes$radius[n1] <- rank(nodes$radius[n1], ties.method = "first")
      nodes$radius[n2] <- rank(nodes$radius[n2], ties.method = "first")
    }

    if (nx == 3) {
      nodes$radius[n1] <- rank(nodes$radius[n1], ties.method = "first")
      nodes$radius[n2] <- rank(nodes$radius[n2], ties.method = "first")
      nodes$radius[n3] <- rank(nodes$radius[n3], ties.method = "first")
    }

    if (nx == 4) {
      nodes$radius[n1] <- rank(nodes$radius[n1], ties.method = "first")
      nodes$radius[n2] <- rank(nodes$radius[n2], ties.method = "first")
      nodes$radius[n3] <- rank(nodes$radius[n3], ties.method = "first")
      nodes$radius[n4] <- rank(nodes$radius[n4], ties.method = "first")
    }

    if (nx == 5) {
      nodes$radius[n1] <- rank(nodes$radius[n1], ties.method = "first")
      nodes$radius[n2] <- rank(nodes$radius[n2], ties.method = "first")
      nodes$radius[n3] <- rank(nodes$radius[n3], ties.method = "first")
      nodes$radius[n4] <- rank(nodes$radius[n4], ties.method = "first")
      nodes$radius[n5] <- rank(nodes$radius[n5], ties.method = "first")
    }

    if (nx == 6) {
      nodes$radius[n1] <- rank(nodes$radius[n1], ties.method = "first")
      nodes$radius[n2] <- rank(nodes$radius[n2], ties.method = "first")
      nodes$radius[n3] <- rank(nodes$radius[n3], ties.method = "first")
      nodes$radius[n4] <- rank(nodes$radius[n4], ties.method = "first")
      nodes$radius[n5] <- rank(nodes$radius[n5], ties.method = "first")
      nodes$radius[n6] <- rank(nodes$radius[n6], ties.method = "first")
    }
  } # end of method == "rank"

  if (method == "norm") {
    if (nx == 2) {
      min1 <- min(nodes$radius[n1])
      max1 <- max(nodes$radius[n1])
      min2 <- min(nodes$radius[n2])
      max2 <- max(nodes$radius[n2])

      nodes$radius[n1] <- (nodes$radius[n1] - min1) / (max1 - min1)
      nodes$radius[n2] <- (nodes$radius[n2] - min2) / (max2 - min2)
    }

    if (nx == 3) {
      min1 <- min(nodes$radius[n1])
      max1 <- max(nodes$radius[n1])
      min2 <- min(nodes$radius[n2])
      max2 <- max(nodes$radius[n2])
      min3 <- min(nodes$radius[n3])
      max3 <- max(nodes$radius[n3])

      nodes$radius[n1] <- (nodes$radius[n1] - min1) / (max1 - min1)
      nodes$radius[n2] <- (nodes$radius[n2] - min2) / (max2 - min2)
      nodes$radius[n3] <- (nodes$radius[n3] - min3) / (max3 - min3)
    }

    if (nx == 4) {
      min1 <- min(nodes$radius[n1])
      max1 <- max(nodes$radius[n1])
      min2 <- min(nodes$radius[n2])
      max2 <- max(nodes$radius[n2])
      min3 <- min(nodes$radius[n3])
      max3 <- max(nodes$radius[n3])
      min4 <- min(nodes$radius[n4])
      max4 <- max(nodes$radius[n4])

      nodes$radius[n1] <- (nodes$radius[n1] - min1) / (max1 - min1)
      nodes$radius[n2] <- (nodes$radius[n2] - min2) / (max2 - min2)
      nodes$radius[n3] <- (nodes$radius[n3] - min3) / (max3 - min3)
      nodes$radius[n4] <- (nodes$radius[n4] - min4) / (max4 - min4)
    }

    if (nx == 5) {
      min1 <- min(nodes$radius[n1])
      max1 <- max(nodes$radius[n1])
      min2 <- min(nodes$radius[n2])
      max2 <- max(nodes$radius[n2])
      min3 <- min(nodes$radius[n3])
      max3 <- max(nodes$radius[n3])
      min4 <- min(nodes$radius[n4])
      max4 <- max(nodes$radius[n4])
      min5 <- min(nodes$radius[n5])
      max5 <- max(nodes$radius[n5])

      nodes$radius[n1] <- (nodes$radius[n1] - min1) / (max1 - min1)
      nodes$radius[n2] <- (nodes$radius[n2] - min2) / (max2 - min2)
      nodes$radius[n3] <- (nodes$radius[n3] - min3) / (max3 - min3)
      nodes$radius[n4] <- (nodes$radius[n4] - min4) / (max4 - min4)
      nodes$radius[n5] <- (nodes$radius[n5] - min5) / (max5 - min5)
    }

    if (nx == 6) {
      min1 <- min(nodes$radius[n1])
      max1 <- max(nodes$radius[n1])
      min2 <- min(nodes$radius[n2])
      max2 <- max(nodes$radius[n2])
      min3 <- min(nodes$radius[n3])
      max3 <- max(nodes$radius[n3])
      min4 <- min(nodes$radius[n4])
      max4 <- max(nodes$radius[n4])
      min5 <- min(nodes$radius[n5])
      max5 <- max(nodes$radius[n5])
      min6 <- min(nodes$radius[n6])
      max6 <- max(nodes$radius[n6])

      nodes$radius[n1] <- (nodes$radius[n1] - min1) / (max1 - min1)
      nodes$radius[n2] <- (nodes$radius[n2] - min2) / (max2 - min2)
      nodes$radius[n3] <- (nodes$radius[n3] - min3) / (max3 - min3)
      nodes$radius[n4] <- (nodes$radius[n4] - min4) / (max4 - min4)
      nodes$radius[n5] <- (nodes$radius[n5] - min5) / (max5 - min5)
      nodes$radius[n6] <- (nodes$radius[n6] - min6) / (max6 - min6)
    }
  } # end of method == "norm"

  if (method == "scale") {
    if (is.null(action)) stop("You must supply action")
    if (!length(action) == length(unique(nodes$axis))) stop("length(action) did not match no. axes")

    if (nx == 2) {
      nodes$radius[n1] <- nodes$radius[n1] * action[1]
      nodes$radius[n2] <- nodes$radius[n2] * action[2]
    }

    if (nx == 3) {
      nodes$radius[n1] <- nodes$radius[n1] * action[1]
      nodes$radius[n2] <- nodes$radius[n2] * action[2]
      nodes$radius[n3] <- nodes$radius[n3] * action[3]
    }

    if (nx == 4) {
      nodes$radius[n1] <- nodes$radius[n1] * action[1]
      nodes$radius[n2] <- nodes$radius[n2] * action[2]
      nodes$radius[n3] <- nodes$radius[n3] * action[3]
      nodes$radius[n4] <- nodes$radius[n4] * action[4]
    }

    if (nx == 5) {
      nodes$radius[n1] <- nodes$radius[n1] * action[1]
      nodes$radius[n2] <- nodes$radius[n2] * action[2]
      nodes$radius[n3] <- nodes$radius[n3] * action[3]
      nodes$radius[n4] <- nodes$radius[n4] * action[4]
      nodes$radius[n5] <- nodes$radius[n5] * action[5]
    }

    if (nx == 6) {
      nodes$radius[n1] <- nodes$radius[n1] * action[1]
      nodes$radius[n2] <- nodes$radius[n2] * action[2]
      nodes$radius[n3] <- nodes$radius[n3] * action[3]
      nodes$radius[n4] <- nodes$radius[n4] * action[4]
      nodes$radius[n5] <- nodes$radius[n5] * action[5]
      nodes$radius[n6] <- nodes$radius[n6] * action[6]
    }
  } # end of method == "scale"

  if (method == "invert") {
    if (is.null(action)) stop("You must supply action")
    if (!length(action) == length(unique(nodes$axis))) stop("length(action) did not match no. axes")

    for (n in 1:length(action)) {
      if (action[n] == -1) {
        xx <- which(nodes$axis == n)
        nodes$radius[xx] <- nodes$radius[xx] * -1 + min(nodes$radius[xx]) * 2 + diff(range(nodes$radius[xx]))
      }
    }
  } # end of method == "invert"

  if (method == "prune") {
    if (is.null(action)) stop("You must supply action")
    if (!length(action) == 1) stop("Action must give only one axis to prune")
    if ((action > max(nodes$axis)) | (action < 1)) stop("Axis to prune is < 1 or  > no. of axes")

    # rem <- subset(nodes, axis == action)
    # nodes <- subset(nodes, !axis == action)
    rem <- nodes[nodes[, "axis"] == action, ]
    nodes <- nodes[nodes[, "axis"] != action, ]

    if (action == 1) nodes$axis <- nodes$axis - 1L
    if (action == 2) {
      ch <- which(nodes$axis >= 3)
      nodes$axis[ch] <- nodes$axis[ch] - 1L
    }
    if (action == 3) {
      ch <- which(nodes$axis >= 4)
      nodes$axis[ch] <- nodes$axis[ch] - 1L
    }
    if (action == 4) {
      ch <- which(nodes$axis >= 5)
      nodes$axis[ch] <- nodes$axis[ch] - 1L
    }
    if (action == 5) {
      ch <- which(nodes$axis == 6)
      nodes$axis[ch] <- nodes$axis[ch] - 1L
    } # df nodes fixed!

    edges <- HPD$edges
    k1 <- !edges$id1 %in% rem$id
    edges <- edges[k1, ]
    k2 <- !edges$id2 %in% rem$id
    edges <- edges[k2, ]
    HPD[[2]] <- edges # prune is the only place edges are messed with
  } # end of method == "prune"

  if (method == "ranknorm") { # rank first, then norm

    HPD <- manipAxis(HPD, method = "rank")
    HPD <- manipAxis(HPD, method = "norm")
    nodes <- HPD$nodes
  } # end of method == "ranknorm"

  if (method == "offset") {
    if (is.null(action)) stop("You must supply action")
    if (!length(action) == length(unique(nodes$axis))) stop("length(action) did not match no. axes")

    if (nx == 2) {
      nodes$radius[n1] <- nodes$radius[n1] + action[1]
      nodes$radius[n2] <- nodes$radius[n2] + action[2]
    }

    if (nx == 3) {
      nodes$radius[n1] <- nodes$radius[n1] + action[1]
      nodes$radius[n2] <- nodes$radius[n2] + action[2]
      nodes$radius[n3] <- nodes$radius[n3] + action[3]
    }

    if (nx == 4) {
      nodes$radius[n1] <- nodes$radius[n1] + action[1]
      nodes$radius[n2] <- nodes$radius[n2] + action[2]
      nodes$radius[n3] <- nodes$radius[n3] + action[3]
      nodes$radius[n4] <- nodes$radius[n4] + action[4]
    }

    if (nx == 5) {
      nodes$radius[n1] <- nodes$radius[n1] + action[1]
      nodes$radius[n2] <- nodes$radius[n2] + action[2]
      nodes$radius[n3] <- nodes$radius[n3] + action[3]
      nodes$radius[n4] <- nodes$radius[n4] + action[4]
      nodes$radius[n5] <- nodes$radius[n5] + action[5]
    }

    if (nx == 6) {
      nodes$radius[n1] <- nodes$radius[n1] + action[1]
      nodes$radius[n2] <- nodes$radius[n2] + action[2]
      nodes$radius[n3] <- nodes$radius[n3] + action[3]
      nodes$radius[n4] <- nodes$radius[n4] + action[4]
      nodes$radius[n5] <- nodes$radius[n5] + action[5]
      nodes$radius[n6] <- nodes$radius[n6] + action[6]
    }
  } # end of method == "offset"

  if (method == "stretch") {
    if (is.null(action)) stop("You must supply action")
    if (!length(action) == length(unique(nodes$axis))) stop("length(action) did not match no. axes")

    # Get min of each axis & subtract
    if (nx == 2) {
      min1 <- min(nodes$radius[n1])
      min2 <- min(nodes$radius[n2])
      off <- c(min1, min2) * -1
    }

    if (nx == 3) {
      min1 <- min(nodes$radius[n1])
      min2 <- min(nodes$radius[n2])
      min3 <- min(nodes$radius[n3])
      off <- c(min1, min2, min3) * -1
    }

    if (nx == 4) {
      min1 <- min(nodes$radius[n1])
      min2 <- min(nodes$radius[n2])
      min3 <- min(nodes$radius[n3])
      min4 <- min(nodes$radius[n4])
      off <- c(min1, min2, min3, min4) * -1
    }

    if (nx == 5) {
      min1 <- min(nodes$radius[n1])
      min2 <- min(nodes$radius[n2])
      min3 <- min(nodes$radius[n3])
      min4 <- min(nodes$radius[n4])
      min5 <- min(nodes$radius[n5])
      off <- c(min1, min2, min3, min4, min5) * -1
    }

    if (nx == 6) {
      min1 <- min(nodes$radius[n1])
      min2 <- min(nodes$radius[n2])
      min3 <- min(nodes$radius[n3])
      min4 <- min(nodes$radius[n4])
      min5 <- min(nodes$radius[n5])
      min6 <- min(nodes$radius[n6])
      off <- c(min1, min2, min3, min4, min5, min6) * -1
    }

    HPD <- manipAxis(HPD, method = "offset", action = off)
    HPD <- manipAxis(HPD, method = "scale", action = action)
    nodes <- HPD$nodes
  } # end of method == "stretch"

  HPD[[1]] <- nodes
  chkHPD(HPD)
  HPD
}
