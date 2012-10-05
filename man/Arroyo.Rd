\name{Arroyo}
\alias{Arroyo}
\alias{Safari}
\docType{data}
\title{
Plant-pollinator data sets in hive plot data format
}
\description{
Plant-pollinator data sets which were derived ultimately from Vasquez and Simberloff, 2003.  These are two-trophic level systems that have almost exactly the same plants and pollinators.  \code{Safari} is from an undisturbed area, while \code{Arroyo} is from a nearby location grazed by cattle.  In the original publication, the data sets are called Safariland and Arroyo Goye.  See Details for how the original data was converted.
}
\usage{
data(Arroyo)
data(Safari)
}
\format{
  The format is: \cr
List of 7 \cr
 $ nodes      :'data.frame':	39 obs. of  6 variables: \cr
  ..$ id    : int [1:39] 1 2 3 4 5 6 7 8 9 10 ... \cr
  ..$ lab   : chr [1:39] "Aristotelia chilensis" "Alstroemeria aurea" "Schinus patagonicus" "Berberis darwinii" ... \cr
  ..$ radius: num [1:39] 0.404 0.775 0.507 0.79 0.55 ... \cr
  ..$ axis  : int [1:39] 1 1 1 1 1 1 1 1 1 1 ... \cr
  ..$ size  : num [1:39] 1 1 1 1 1 1 1 1 1 1 ... \cr
  ..$ color : chr [1:39] "black" "black" "black" "black" ... \cr
 $ edges      :'data.frame':	43 obs. of  4 variables: \cr
  ..$ id1   : int [1:43] 2 21 2 2 33 2 38 39 16 6 ... \cr
  ..$ id2   : int [1:43] 13 2 27 30 2 37 2 3 6 20 ... \cr
  ..$ weight: num [1:43] 0.985 0.985 0.985 0.985 0.985 ... \cr
  ..$ color : chr [1:43] "white" "white" "white" "white" ... \cr
 $ type       : chr "2D" \cr
 $ desc       : chr "Modified Arroyoland Data Set" \cr
 $ axis.cols  : chr [1:2] "gray" "gray" \cr
 - attr(*, "class")= chr "HivePlotData" \cr
}
\details{
These data sets are \code{\link{HivePlotData}} objects.  They were created from the datasets \code{Safariland} and \code{vazarr} in the package \code{bipartite}.  The process was the same for each:  1.  Plants were placed on one axis, pollinators on the other.  2.  A radius was assigned by calculating d' using function \code{\link{dfun}} in package \code{bipartite}.  d' is an index of specialization; higher values mean the plant or pollinator is more specialized.  3.  Edge weights were assigned proportional to the square root of the normalized number of visits of a pollinator to a plant.  Thus the width of the edge drawn is an indication of the visitation rate.  4.  The number of visits were divided manually into 4 groups and used to assign edge colors ranging from white to red.  The redder colors represent greater numbers of visits, and the color-coding is comparable for each data set.  Thus both the edge color and the edge weight encode the same information.  It would of course be possible to encode an additional variable by changing either edge color or weight.  Please e-mail if you would like the script used to convert the data sets.
}
\source{
D. P. Vasquez and D. Simberloff "Changes in interaction biodiversity induced by an introduced ungulate" Ecology Letters, vol. 6 pgs 1077-1083 (2003).
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}
\examples{
data(Safari)
sumHPD(Safari)
plotHive(Safari)
}
\keyword{datasets}
