animateHive <- function(hives = list(), cmds = list(), xy = 400, ...) {
	
	# Function to create coordinated rgl animations
	# using different plotting arguments for each hive plot
	
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
		open3d(windowRect = c(0, 0, xy, xy))
		win.name <- paste("window", rgl.cur(), sep = "")
		win.list <- c(win.list, win.name)
		rgl.bringtotop(TRUE)
		do.call(plot3dHive, args = c(hives[n], cmds[[n]]))
		# Since hives is a list of lists, you must unlist it one level
		}

	# Set up a controller
	base <- tktoplevel()
	tkwm.title(base, "Master Controls")
	devL <- as.integer(gsub("window", "", win.list))
	con <- spinControl(base, dev = devL)
	}
