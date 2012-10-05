\name{chkHPD}
\alias{chkHPD}
\title{
Verify the integrity of a hive plot data object
}
\description{
This function inspects the classes of each part of a \code{\link{HPD}} as a means of verifying its integrity.  A few other characteristics are checked as well.
}
\usage{
chkHPD(HPD, confirm = FALSE)
}
\arguments{
  \item{HPD}{
An object of S3 class \code{HivePlotData}.
}
  \item{confirm}{
Logical; if \code{TRUE} then a favorable result is affirmed in the console (problems are always reported).
}
}
\value{
A logical value; \code{TRUE} is there is a problem, otherwise \code{FALSE}.
}
\seealso{\code{\link{sumHPD}} which allows inspection (checking) of many properties of your \code{\link{HPD}}.
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
test4 <- ranHiveData(nx = 4)
good <- chkHPD(test4, confirm = TRUE)
# mess it up and do again
# next test is not run as it halts execution
\dontrun{
test4$nodes$color <- as.factor(test4$nodes$color)
bad <- chkHPD(test4)
}
}
\keyword{ utilities }
