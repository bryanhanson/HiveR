
#' Hive Plot Data Objects
#' 
#' In package \code{HiveR}, hive plot data sets are stored as an S3 class
#' called \code{HivePlotData}, detailed below.
#' 
#' 
#' @note While \code{$edges$id1} and \code{$edges$id2} are defined as the
#' starting and ending nodes of a particular edge, hive plots as currently
#' implemented are not directed graphs (agnostic might be a better word). \cr
#' \cr \code{HPD$type} indicates the type of hive data: If \code{2D}, then the
#' data is intended to be plotted with \code{hivePlot} which is a 2D plot with
#' axes radially oriented, and (hopefully) no edges that cross axes.  If
#' \code{3D}, then the data is intended to be plotted with \code{plot3dHive}
#' which gives an interactive 3D plot, with axes oriented in 3D.
#'
#' @section Structure: The structure of a \code{HivePlotData} object is a list
#' of 6 elements, some of which are data frames, and an attribute, as follows:
#' 
#' \tabular{llll}{
#'   \emph{element} \tab \emph{(element)} \tab \emph{type} \tab \emph{description}\cr
#'   $nodes \tab \tab data frame \tab Data frame of node properties \cr
#'     \tab $id \tab int \tab Node identifier \cr
#'     \tab $lab \tab chr \tab Node label \cr
#'     \tab $axis \tab int \tab Axis to which node is assigned \cr
#'     \tab $radius \tab num \tab Radius (position) of node along the axis \cr
#'     \tab $size \tab num \tab Node size in pixels \cr
#'     \tab $color \tab chr \tab Node color \cr
#'   $edges \tab \tab data frame \tab Data frame of edge properties \cr
#'     \tab $id1 \tab int \tab Starting node id \cr
#'     \tab $id2 \tab int \tab Ending node id \cr
#'     \tab $weight \tab num \tab Width of edge in pixels \cr
#'     \tab $color \tab chr \tab Edge color \cr
#'   $type \tab \tab chr \tab Type of hive. See Note. \cr
#'   $desc \tab \tab chr \tab Description of data \cr
#'   $axis.cols \tab \tab chr \tab Colors for axes \cr
#'   - attr \tab \tab chr "HivePlotData" \tab The S3 class designation.\cr }
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#'
#' @seealso \code{\link{sumHPD}} to summarize a \code{HivePlotData} object.\cr
#' \code{\link{chkHPD}} to verify the integrity of a \code{HivePlotData}
#' object.\cr \code{\link{ranHiveData}} to generate random \code{HivePlotData}
#' objects for testing and demonstration.
#'
#' @keywords classes
#' @name HivePlotData
#' @aliases HPD HivePlotData

#' @examples
#' 
#' test4 <- ranHiveData(nx = 4)
#' str(test4)
#' sumHPD(test4)
#' plotHive(test4)
#' 
NULL
