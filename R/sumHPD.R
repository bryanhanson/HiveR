sumHPD <-
function(HPD, chk.all = FALSE, chk.sm.pt = FALSE, chk.ax.jump = FALSE,
	chk.sm.ax = FALSE, chk.orphan.node = FALSE,
	plot.list = FALSE, tex = FALSE, orphan.list = FALSE){
	
# Function to summarize objects of S3 class 'HivePlotData'
# Part of HiveR package
# Bryan Hanson, DePauw Univ, Oct 2011


	chkHPD(HPD) # verify it's legit
	
	# Overall summary
	na <- length(unique(HPD$nodes$axis))
	
	cat("\t", HPD$desc, "\n", sep = "")
	cat("\tThis hive plot data set contains ",
		length(HPD$nodes$id), " nodes on ",
		na, " axes and ",
		length(HPD$edges$id1), " edges.\n", sep = "")
	cat("\tIt is a  ", HPD$type, " data set.\n\n", sep = "")

	# Now summarize the axes and nodes

	nodes <- HPD$nodes
	
	for (n in sort(unique(nodes$axis))) {
		g <- subset(nodes, axis == n)
		cat("\t\tAxis", n, "has", length(g$id), "nodes spanning radii from",
		min(g$radius), "to", max(g$radius), "\n", sep = " ")		
		}	

	# Create a list of edges to be drawn (used for several chks)
	
	n1.lab <- n1.rad <- n2.lab <- n2.rad <- n1.ax <- n2.ax <- c()

	for (n in 1:(length(HPD$edges$id1))) {
		pat1 <- HPD$edges$id1[n]
		pat2 <- HPD$edges$id2[n]
		pat1 <- paste("\\b", pat1, "\\b", sep = "") # ensures exact match
		pat2 <- paste("\\b", pat2, "\\b", sep = "")
		i1 <- grep(pat1, HPD$nodes$id)
		i2 <- grep(pat2, HPD$nodes$id)
		n1.lab <- c(n1.lab, HPD$nodes$lab[i1])
		n2.lab <- c(n2.lab, HPD$nodes$lab[i2])
		n1.rad <- c(n1.rad, HPD$nodes$radius[i1])
		n2.rad <- c(n2.rad, HPD$nodes$radius[i2])
		n1.ax <- c(n1.ax, HPD$nodes$axis[i1])
		n2.ax <- c(n2.ax, HPD$nodes$axis[i2])
		}

	fd <- data.frame(
		n1.id = HPD$edges$id1,
		n1.ax,
		n1.lab,
		n1.rad,
		n2.id = HPD$edges$id2,
		n2.ax,
		n2.lab,
		n2.rad,
		e.wt = HPD$edges$weight,
		e.col = HPD$edges$color)		

	# Now summarize edges by axis pair

	fd2 <- fd[,c(2,6)]
	fd2 <- count(fd2, vars = c("n1.ax", "n2.ax"))
	cat("\n")
	for (n in 1:nrow(fd2)) {
	cat("\t\tAxes", fd2$n1.ax[n], "and", fd2$n2.ax[n], "share", fd2$freq[n], "edges\n", sep = " ")		
		}
	cat("\n")

	##### Done with default basic summary #####
	
	# Perform the additional requested checks
	
	if (chk.all) chk.sm.pt <- chk.ax.jump <- chk.sm.ax <- chk.orphan.node <- TRUE

	if (chk.sm.pt) {
		prob <- which((fd$n1.rad == fd$n2.rad) & (fd$n1.ax == fd$n2.ax))
		if (length(prob) == 0) cat("\n\t No edges were found that start and end on the same point\n")
		if (length(prob) > 0) {
			cat("\n\n\tThe following edges start and end at the same point and the\n\tcorresponding nodes should be deleted, offset or\n\tjittered (or the edge deleted) before plotting:\n\n")
			print(fd[prob,], row.names = FALSE)
			}
		}

	if (chk.sm.ax) {
		prob <- which(fd$n1.ax == fd$n2.ax)
		if (length(prob) == 0) cat("\n\t No edges were found that start and end on the same axis\n")
		if (length(prob) > 0) {
			cat("\n\n\tThe following edges start and end on the same axis:\n\n")
			print(fd[prob,], row.names = FALSE)
			}
		}

	if (chk.orphan.node) {
		e.ids <- union(HPD$edges$id1, HPD$edges$id2)
		n.ids <- HPD$nodes$id
		prob <- setdiff(n.ids, e.ids)
		prob <- match(prob, HPD$nodes$id)
		if (length(prob) == 0) cat("\n\t No orphaned nodes were found\n")
		if (length(prob) > 0) {
			cat("\n\n\tThe following", length(prob), "nodes are orphaned (degree = 0):\n\n")
			print(HPD$nodes[prob,], row.names = FALSE)
			orphans <- HPD$nodes[prob,]
			}
		}

	if (chk.ax.jump) {
		prob <- which(
			((fd$n1.ax == 1) & (fd$n2.ax == 3)) &
			((fd$n1.ax == 2) & (fd$n2.ax == 4)) &
			((fd$n1.ax == 3) & (fd$n2.ax == 5)) &
			((fd$n1.ax == 4) & (fd$n2.ax == 6)) &
			((fd$n1.ax == 5) & (fd$n2.ax == 1)) &
			((fd$n1.ax == 6) & (fd$n2.ax == 2)) &
			#
			((fd$n1.ax == 6) & (fd$n2.ax == 4)) &
			((fd$n1.ax == 5) & (fd$n2.ax == 3)) &
			((fd$n1.ax == 4) & (fd$n2.ax == 2)) &
			((fd$n1.ax == 3) & (fd$n2.ax == 1)) &
			((fd$n1.ax == 2) & (fd$n2.ax == 6)) &
			((fd$n1.ax == 1) & (fd$n2.ax == 5)))
			
		if (length(prob) == 0) cat("\n\t No edges that jump axes were found\n")
		if (length(prob) > 0) {
			cat("\n\n\tThe following edges jump over an axis (and won't be drawn):\n\n")
			print(fd[prob,], row.names = FALSE)
			}
		}
		
	if ((tex) & (plot.list)) {
		fd <- xtable(fd, hline.after = c(1), include.rownames = FALSE)
		align(fd) <- "|r|rrlr|rrlr|rl|"
		}	

	if (plot.list) return(fd) # user must not ask for both at the same time!
	if (orphan.list) return(orphans)
	}

