#' Animate One or More 3D Hive Plots with a Handy Controller
#' 
#' This function takes a list of \code{HivePlotData} objects of \code{type =
#' "3D"} and plots each in its own \code{rgl} window using its own arguments,
#' then adds a controller which handles rotation and scaling.
#' 
#' 
#' @param hives A list of \code{HivePlotData} objects.
#'
#' @param cmds A list of arguments corresponding to how you want each hive
#' plotted.
#'
#' @param xy An integer giving the size of the \code{rgl} window in pixels.
#'
#' @param \dots Other parameters to be passed downstream to \code{rgl}.
#'
#' @return None.  Side effect is one or more plots.
#'
#' @section Warning: If you click the 'continue rotating' box on the controller
#' window, be sure to unclick it and wait for the system to halt before closing
#' any of the windows.  If you close the controller w/o doing this, the
#' remaining open windows with the hive plots will continue rotating endlessly
#' and it seems you can't get their attention to close the windows.
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#'
#' @keywords interactive
#'
#' @export animateHive
#'
#' @importFrom rgl open3d rgl.bringtotop rgl.cur
#' @importFrom tkrgl spinControl
#' @importFrom tcltk tktoplevel tkwm.title
#'
#' @examples
#' 
#' \dontrun{
#' require("rgl")
#' require("tkrgl")
#' # Sillyness: let's draw different hives with different settings
#' # List of hives 
#' t4 <- ranHiveData(type = "3D", nx = 4)
#' t5 <- ranHiveData(type = "3D", nx = 5)
#' t6 <- ranHiveData(type = "3D", nx = 6)
#' myhives <- list(t4, t5, t6)
#' # List of arguments to plot in different coordinate systems
#' cmd1 <- list(method = "abs", LA = TRUE, dr.nodes = FALSE, ch = 10)
#' cmd2 <- list(method = "rank", LA = TRUE, dr.nodes = FALSE, ch = 2)
#' cmd3 <- list(method = "norm", LA = TRUE, dr.nodes = FALSE, ch = 0.1)
#' mycmds <- list(cmd1, cmd2, cmd3)
#' #
#' animateHive(hives = myhives, cmds = mycmds)
#' }
#' 
#'
animateHive <- function(hives = list(), cmds = list(), xy = 400, ...) {
	
	# Function to create coordinated rgl animations
	# using different plotting arguments for each hive plot

	if (!requireNamespace("rgl", quietly = TRUE)) {
		stop("You need to install package rgl to use this function")
		}
	if (!requireNamespace("tkrgl", quietly = TRUE)) {
		stop("You need to install package tkrgl to use this function")
		}
	
	nh <- length(hives)
	if (nh == 0) stop("No hives specified")
	
	# Draw each hive in its own window w/its own parameters

	win.list <- c()
	
	for (n in 1:nh) {
		type <- hives[[n]]$type
		if (!type == "3D") {
			msg <- paste("Hive no.", n, "is not 3D", sep = " ")
			warning(msg)
			next
			}
		rgl::open3d(windowRect = c(0, 0, xy, xy))
		win.name <- paste("window", rgl::rgl.cur(), sep = "")
		win.list <- c(win.list, win.name)
		rgl::rgl.bringtotop(TRUE)
		do.call(plot3dHive, args = c(hives[n], cmds[[n]]))
		# Since hives is a list of lists, you must unlist it one level
		}

	# Set up a controller
	base <- tcltk::tktoplevel()
	tcltk::tkwm.title(base, "Master Controls")
	devL <- as.integer(gsub("window", "", win.list))
	con <- tkrgl::spinControl(base, dev = devL)
	}
