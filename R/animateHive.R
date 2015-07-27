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
