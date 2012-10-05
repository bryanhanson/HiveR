\name{animateHive}
\alias{animateHive}
\title{
Animate one or more 3D hive plots with an handy controller
}
\description{
This function takes a list of \code{HivePlotData} objects of \code{type = "3D"} and plots each in its own \code{rgl} window using its own arguments, then adds a controller which handles rotation and scaling.
}
\usage{
animateHive(hives = list(), cmds = list(), xy = 400, ...)
}
\arguments{
  \item{hives}{
A list of \code{HivePlotData} objects.
}
  \item{cmds}{
A list of arguments corresponding to how you want each hive plotted.
}
  \item{xy}{
An integer giving the size of the \code{rgl} window in pixels.
}
  \item{\dots}{
Other parameters to be passed downstream to \code{rgl}.
}
}
\value{
None.  Side effect is one or more plots
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}

\section{Warning}{If you click the 'continue rotating' box on the controller window, be sure to unclick it and wait for the system to halt before closing any of the windows.  If you close the controller w/o doing this, the remaining open windows with the hive plots will continue rotating endlessly and it seems you can't get their attention to close the windows.}

\examples{
\dontrun{
require("rgl")
require("tkrgl")
# Sillyness: let's draw different hives with different settings
# List of hives 
t4 <- ranHiveData(type = "3D", nx = 4)
t5 <- ranHiveData(type = "3D", nx = 5)
t6 <- ranHiveData(type = "3D", nx = 6)
myhives <- list(t4, t5, t6)
# List of arguments to plot in different coordinate systems
cmd1 <- list(method = "abs", LA = TRUE, dr.nodes = FALSE, ch = 10)
cmd2 <- list(method = "rank", LA = TRUE, dr.nodes = FALSE, ch = 2)
cmd3 <- list(method = "norm", LA = TRUE, dr.nodes = FALSE, ch = 0.1)
mycmds <- list(cmd1, cmd2, cmd3)
#
animateHive(hives = myhives, cmds = mycmds)
}
}
\keyword{ interactive }
