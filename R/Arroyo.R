

#' Plant-pollinator data sets in hive plot data format
#' 
#' Plant-pollinator data sets which were derived ultimately from Vasquez and
#' Simberloff, 2003.  These are two-trophic level systems that have almost
#' exactly the same plants and pollinators.  \code{Safari} is from an
#' undisturbed area, while \code{Arroyo} is from a nearby location grazed by
#' cattle.  In the original publication, the data sets are called Safariland
#' and Arroyo Goye.  See Details for how the original data was converted.
#' 
#' These data sets are \code{\link{HivePlotData}} objects.  They were created
#' from the datasets \code{Safariland} and \code{vazarr} in the package
#' \code{bipartite}.  The process was the same for each: 1.  Plants were placed
#' on one axis, pollinators on the other.  2.  A radius was assigned by
#' calculating d' using function \code{dfun} in package \code{bipartite}.  d'
#' is an index of specialization; higher values mean the plant or pollinator is
#' more specialized.  3.  Edge weights were assigned proportional to the square
#' root of the normalized number of visits of a pollinator to a plant.  Thus
#' the width of the edge drawn is an indication of the visitation rate.  4.
#' The number of visits were divided manually into 4 groups and used to assign
#' edge colors ranging from white to red.  The redder colors represent greater
#' numbers of visits, and the color-coding is comparable for each data set.
#' Thus both the edge color and
#' 2D and 3D Hive Plots for R
#' 
#' Creates and plots 2D and 3D hive plots. Hive plots are a unique method of
#' displaying networks of many types in which node properties are mapped to
#' axes using meaningful properties rather than being arbitrarily positioned.
#' The hive plot concept was invented by Martin Krzywinski at the Genome
#' Science Center (www.hiveplot.net/).  Keywords: networks, food webs, linnet,
#' systems biology, bioinformatics.
#' 
#' 
#' @name Arroyo
#' @docType data
#' @aliases Arroyo Safari
#' @author Bryan A. Hanson, DePauw University, Greencastle Indiana USA
#' @references \url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
#' @keywords datasets
NULL



