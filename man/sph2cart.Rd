\name{sph2cart}
\alias{sph2cart}
\title{
Convert spherical to Cartesian coordinates
}
\description{
This function converts spherical to Cartesian coordinates.
}
\usage{
sph2cart(df)
}

\arguments{
  \item{df}{
A data frame with columns named r, theta and phi with the radius and angles (in spherical coordinates) to be converted to Cartesian coordinates.
}
}
\value{
A data frame with named columns containing the converted coordinates.
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\note{
Cobbled together from similar functions in other packages.
}

\keyword{ utilities }