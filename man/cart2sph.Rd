\name{cart2sph}
\alias{cart2sph}
\alias{sph2cart}
\title{
Convert spherical to Cartesian coordinates or vice versa
}
\description{
These functions interconvert coordinate systems.
}
\usage{
cart2sph(df)
sph2cart(df)
}

\arguments{
  \item{df}{
For \code{cart2sph}, a data frame with columns named x, y and z containing the Cartesian coordinates to be converted.  For \code{sph2cart}, a data frame with columns named r, theta and phi with the radius and angles (in spherical coordinates) to be converted to Cartesian coordinates.
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

\examples{
df.cart1 <- data.frame(x = rnorm(10), y = rnorm(10), z = rnorm(10))
df.sph <- cart2sph(df.cart1)
str(df.sph)
df.cart2 <- sph2cart(df.sph)
all.equal(df.cart1, df.cart2)
}
\keyword{ utilities }