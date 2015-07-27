

ranHiveData <- function(type = "2D", nx = 4,
	nn = nx*15, ne = nx*15,
	rad = 1:100, ns = c(0.5, 1.0, 1.5), ew = 1:3,
	nc = brewer.pal(5, "Set1"),
	ec = brewer.pal(5, "Set1"),
	axis.cols = brewer.pal(nx, "Set1"),
	desc = NULL, allow.same = FALSE,
	verbose = FALSE) {
	
# Function to generate random data for testing/demonstrating HiveR
# Bryan Hanson, DePauw Univ, June 2011 onward

# Defaults make small hives that draw fast and are not too cluttered

# type = whether data is to be plotted 2D or 3D
# nx = no. axes
# nn = no. nodes
# ne = no. edges
# nc = node color
# ec = edge color
# rad = possible values for radii
# ns = node size
# ew = edge weight/width
# desc = description

	if ((nx == 1) | (nx > 6)) stop("nx out of bounds: 2 =< nx =< 6")
	
			
# Create a set of labels/names to choose from

	Labs <-rep(NA, nn)
	for (n in 1:nn) {
		Labs[n] <- rep(paste(letters[stats::runif(1, 1, 26)],
		letters[stats::runif(1, 1, 26)],
		letters[stats::runif(1, 1, 26)],
		letters[stats::runif(1, 1, 26)],
		letters[stats::runif(1, 1, 26)], sep = ""))
		}
		
	# Create nodes df
	
	ndf <- data.frame(
		id = 1:nn,
		lab = as.character(Labs),
		axis = sample(1:nx, nn, replace = TRUE),
		radius = sample(rad, nn, replace = TRUE),
		size = sample(ns, nn, replace = TRUE),
		color = sample(nc, nn, replace = TRUE)
		)
	ndf$color <- as.character(ndf$color)
	
	# Clean up ndf by removing duplicates
	# (do before the creation of edf calls on these points)
	# Important: this means that nodes$id is not continuous!

	dup <- duplicated(ndf[,c(3,4)])
	if (any(dup)) {
		ndf <- ndf[-dup,]
		if (verbose) cat(length(any(!dup)), "duplicate nodes were removed\n\n")
		}

##### Time to create the nodes.
##### Note that nx = 2 or 3 are going to be the same regardless of type
##### and they will be plotted with plotHive not plot3dHive.
##### As a result, there are 3 if statements below.

if  ((nx == 2) | (nx == 3)) {  ###### 2D edges for nx = 2 or 3

	# Create edges df
	
		edf <- data.frame(
			id1 = sample(ndf$id, ne, replace = TRUE),
			id2 = sample(ndf$id, ne, replace = TRUE),
			weight = sample(ew, ne, replace = TRUE),
			color = as.character(sample(ec, ne, replace = TRUE)))
		edf$color <- as.character(edf$color)
	
	# Clean up edf
	
	# remove edges that start & end on the same point
		
		same.pt <- which(edf$id1 == edf$id2)
		if (length(!same.pt == 0)) {
			edf <-edf[-same.pt,]
			if (verbose) cat("Removing an edge (same.pt) = ", same.pt, "\n\n")
			}
		
	# remove edges that start & end on the same axis
	
	if (!allow.same) {
		
		same.axis <- c()
		
		if (nx >= 2) {
			one <- which(ndf$axis == 1) # row indices
			one <- ndf$id[one] # id values
			two <- which(ndf$axis == 2) # row indices
			two <- ndf$id[two] # id values
			for (n in 1:nrow(edf)) {
				if ((edf$id1[n] %in% one) && (edf$id2[n] %in% one)) same.axis <- c(same.axis, n)			
				if ((edf$id1[n] %in% two) && (edf$id2[n] %in% two)) same.axis <- c(same.axis, n)
				}
			}
			
		if (nx == 3) {
			three <- which(ndf$axis == 3) # row indices
			three <- ndf$id[three] # id values
			for (n in 1:nrow(edf)) if ((edf$id1[n] %in% three) && (edf$id2[n] %in% three)) same.axis <- c(same.axis, n)			
			}
				
		if (length(!same.axis == 0)) {
			edf <- edf[-same.axis,]
			if (verbose) cat("Removing an edge (same.axis) = ", same.axis, "\n\n")
			}

		}
		
	} ###### End of 2D edges for nx = 2 or 3

if ((type == "3D") & (nx > 3)) {  ###### 3D edge generation and checking for nx > 3

	# Create edges df
	
		edf <- data.frame(
			id1 = sample(ndf$id, ne, replace = TRUE),
			id2 = sample(ndf$id, ne, replace = TRUE),
			weight = sample(ew, ne, replace = TRUE),
			color = as.character(sample(ec, ne, replace = TRUE)))
		edf$color <- as.character(edf$color)
	
	# Clean up edf
	
		# remove edges that start & end on the same point
		same.pt <- which(edf$id1 == edf$id2)
		if (length(!same.pt == 0)) {
			edf <-edf[-same.pt,]
			if (verbose) cat("Removing an edge (same.pt) = ", same.pt, "\n\n")
			}
		
		# remove edges that start & end on the same axis
			
		same.axis <- c()
		
		if (nx >= 2) { # going to use these values later too when checking colinearity
			one <- which(ndf$axis == 1) # row indices
			one <- ndf$id[one] # id values
			two <- which(ndf$axis == 2) # row indices
			two <- ndf$id[two] # id values
			for (n in 1:nrow(edf)) {
				if ((edf$id1[n] %in% one) && (edf$id2[n] %in% one)) same.axis <- c(same.axis, n)			
				if ((edf$id1[n] %in% two) && (edf$id2[n] %in% two)) same.axis <- c(same.axis, n)
				}
			}
			
		if (nx >= 3) {
			three <- which(ndf$axis == 3) # row indices
			three <- ndf$id[three] # id values
			for (n in 1:nrow(edf)) if ((edf$id1[n] %in% three) && (edf$id2[n] %in% three)) same.axis <- c(same.axis, n)			
			}
			
		if (nx >= 4) {
			four <- which(ndf$axis == 4) # row indices
			four <- ndf$id[four] # id values	
			for (n in 1:nrow(edf)) if ((edf$id1[n] %in% four) && (edf$id2[n] %in% four)) same.axis <- c(same.axis, n)			
			}
			
		if (nx >= 5) {
			five <- which(ndf$axis == 5)
			five <- ndf$id[five]		
			for (n in 1:nrow(edf)) if ((edf$id1[n] %in% five) && (edf$id2[n] %in% five)) same.axis <- c(same.axis, n)			
			}
			
		if (nx == 6) {
			six <- which(ndf$axis == 6) # row indices
			six <- ndf$id[six] # id values
			for (n in 1:nrow(edf)) if ((edf$id1[n] %in% six) && (edf$id2[n] %in% six)) same.axis <- c(same.axis, n)			
			}
	
		if (length(!same.axis == 0)) {
			edf <- edf[-same.axis,]
			if (verbose) cat("Removing an edge (same.axis) = ", same.axis, "\n\n")
			}
				
	# For nx = 5 and 6, we need to remove edges that start and end on colinear axes
	
		colin <- c()
		
		if (nx == 5) {  # axes 4 & 5 are colinear
				
			for (n in 1:nrow(edf)) {
				if ((edf$id1[n] %in% four) && (edf$id2[n] %in% five)) colin <- c(colin, n)			
				if ((edf$id1[n] %in% five) && (edf$id2[n] %in% four)) colin <- c(colin, n)
				}
			
			if (length(!colin == 0)) {
				edf <- edf[-colin,] # remove the colinear edges
				if (verbose) cat("Removing colinear edges (nx = 5): ", colin, "\n\n")
				}
			}
	
		if (nx == 6) {
			
			# axes 1 & 3, 2 & 4, 5 & 6 are colinear
					
			for (n in 1:nrow(edf)) {
				if ((edf$id1[n] %in% one) && (edf$id2[n] %in% three)) colin <- c(colin, n)			
				if ((edf$id1[n] %in% two) && (edf$id2[n] %in% four)) colin <- c(colin, n)
				if ((edf$id1[n] %in% five) && (edf$id2[n] %in% six)) colin <- c(colin, n)
				}
			
			if (length(!colin == 0)) {
				edf <- edf[-colin,] # remove the colinear edges
				if (verbose) cat("Removing colinear edges (nx = 5): ", colin, "\n\n")
				}
			}

	} ##### end of 3D edge generation and checking

if ((type == "2D") & (nx > 3)) { ###### 2D edge generation and checking

	# Create edges df
	# In this case, edges must be 1->2, 2->3... 5->6 but not 3->5
	# i.e. no crossings. Thus they are pretty much done manually 
	
	# Select from possibilites pairwise, roughly equal no. per axis pair
	
		ne <- round(ne/nx) # divide edges among axes
		if (allow.same) ne <- ne/nx # acct for edges st/end on same axis
		
		one <- which(ndf$axis == 1) # row indices
		one <- ndf$id[one] # id values
		two <- which(ndf$axis == 2) # row indices
		two <- ndf$id[two] # id values
		three <- which(ndf$axis == 3) # row indices
		three <- ndf$id[three] # id values
			
		if (nx >= 4) {
			four <- which(ndf$axis == 4) # row indices
			four <- ndf$id[four] # id values	
			}
			
		if (nx >= 5) {
			five <- which(ndf$axis == 5)
			five <- ndf$id[five]		
			}
			
		if (nx == 6) {
			six <- which(ndf$axis == 6) # row indices
			six <- ndf$id[six] # id values
			}

	id1 <- id2 <- c()
	
	if (nx == 4)  {
		id1 <- c(id1, sample(one, ne, replace = TRUE))
		id2 <- c(id2, sample(two, ne, replace = TRUE))
		id1 <- c(id1, sample(two, ne, replace = TRUE))
		id2 <- c(id2, sample(three, ne, replace = TRUE))
		id1 <- c(id1, sample(three, ne, replace = TRUE))
		id2 <- c(id2, sample(four, ne, replace = TRUE))
		id1 <- c(id1, sample(four, ne, replace = TRUE))
		id2 <- c(id2, sample(one, ne, replace = TRUE))
		
		if (allow.same) {
			id1 <- c(id1, sample(one, ne, replace = TRUE))
			id2 <- c(id2, sample(one, ne, replace = TRUE))
			id1 <- c(id1, sample(two, ne, replace = TRUE))
			id2 <- c(id2, sample(two, ne, replace = TRUE))
			id1 <- c(id1, sample(three, ne, replace = TRUE))
			id2 <- c(id2, sample(three, ne, replace = TRUE))
			id1 <- c(id1, sample(four, ne, replace = TRUE))
			id2 <- c(id2, sample(four, ne, replace = TRUE))
			}

		}			

	if (nx == 5)  {
		id1 <- c(id1, sample(one, ne, replace = TRUE))
		id2 <- c(id2, sample(two, ne, replace = TRUE))
		id1 <- c(id1, sample(two, ne, replace = TRUE))
		id2 <- c(id2, sample(three, ne, replace = TRUE))
		id1 <- c(id1, sample(three, ne, replace = TRUE))
		id2 <- c(id2, sample(four, ne, replace = TRUE))
		id1 <- c(id1, sample(four, ne, replace = TRUE))
		id2 <- c(id2, sample(five, ne, replace = TRUE))
		id1 <- c(id1, sample(five, ne, replace = TRUE))
		id2 <- c(id2, sample(one, ne, replace = TRUE))

		if (allow.same) {
			id1 <- c(id1, sample(one, ne, replace = TRUE))
			id2 <- c(id2, sample(one, ne, replace = TRUE))
			id1 <- c(id1, sample(two, ne, replace = TRUE))
			id2 <- c(id2, sample(two, ne, replace = TRUE))
			id1 <- c(id1, sample(three, ne, replace = TRUE))
			id2 <- c(id2, sample(three, ne, replace = TRUE))
			id1 <- c(id1, sample(four, ne, replace = TRUE))
			id2 <- c(id2, sample(four, ne, replace = TRUE))
			id1 <- c(id1, sample(five, ne, replace = TRUE))
			id2 <- c(id2, sample(five, ne, replace = TRUE))
			}

		}			

	if (nx == 6)  {
		id1 <- c(id1, sample(one, ne, replace = TRUE))
		id2 <- c(id2, sample(two, ne, replace = TRUE))
		id1 <- c(id1, sample(two, ne, replace = TRUE))
		id2 <- c(id2, sample(three, ne, replace = TRUE))
		id1 <- c(id1, sample(three, ne, replace = TRUE))
		id2 <- c(id2, sample(four, ne, replace = TRUE))
		id1 <- c(id1, sample(four, ne, replace = TRUE))
		id2 <- c(id2, sample(five, ne, replace = TRUE))
		id1 <- c(id1, sample(five, ne, replace = TRUE))
		id2 <- c(id2, sample(six, ne, replace = TRUE))
		id1 <- c(id1, sample(six, ne, replace = TRUE))
		id2 <- c(id2, sample(one, ne, replace = TRUE))

		if (allow.same) {
			id1 <- c(id1, sample(one, ne, replace = TRUE))
			id2 <- c(id2, sample(one, ne, replace = TRUE))
			id1 <- c(id1, sample(two, ne, replace = TRUE))
			id2 <- c(id2, sample(two, ne, replace = TRUE))
			id1 <- c(id1, sample(three, ne, replace = TRUE))
			id2 <- c(id2, sample(three, ne, replace = TRUE))
			id1 <- c(id1, sample(four, ne, replace = TRUE))
			id2 <- c(id2, sample(four, ne, replace = TRUE))
			id1 <- c(id1, sample(five, ne, replace = TRUE))
			id2 <- c(id2, sample(five, ne, replace = TRUE))
			id1 <- c(id1, sample(six, ne, replace = TRUE))
			id2 <- c(id2, sample(six, ne, replace = TRUE))
			}

		}			

		edf <- data.frame( # clean momentaril
			id1 = id1,
			id2 = id2,
			weight = sample(ew, ne, replace = TRUE),
			color = as.character(sample(ec, ne, replace = TRUE)))
		edf$color <- as.character(edf$color)

	# Remove edges that start & end on the same point
	# (allow.same may have introduced some new cases)
		
		same.pt <- which(edf$id1 == edf$id2)
		if (length(!same.pt == 0)) {
			edf <-edf[-same.pt,]
			if (verbose) cat("Removing an edge (same.pt) = ", same.pt, "\n\n")
			}
	
	} ##### end of 2D edge generation and checking

# The rest of this applies to 2D and 3D
			
	# Finally, remove nodes that are not part of an edge
	# Note: another reason that nodes$id is not continous
		
	draw <- ndf$id %in% unique(c(edf$id1, edf$id2))
	if (any(!draw)) {
		ndf1 <- nrow(ndf)
		ndf <- ndf[draw,]
		ndf2 <- nrow(ndf)
		if (verbose) cat(ndf1 - ndf2, "nodes did not have any edges and have been removed\n\n")
		}

# Report results (also creates desc if needed):

	msg1 <- paste(nx, " axes -- ", dim(ndf)[1], " nodes -- ", dim(edf)[1], " edges", sep = "")
	msg2 <- paste("Data set is", msg1)
	if (verbose) cat(msg2, "\n")
	
	if (!is.null(desc)) desc = paste(desc, " (", msg1, ")", sep = "")
	if (is.null(desc)) desc = msg1
	
# Fix up classes to meet definition

	ndf$lab <- as.character(ndf$lab)
	ndf$radius <- as.numeric(ndf$radius)
	edf$weight <- as.numeric(edf$weight)
	type <- type
	res <- list(nodes = ndf,
		edges = edf,
		type = type, 
		desc = desc,
		axis.cols = axis.cols)
	class(res) <- "HivePlotData"
	chkHPD(res)
	
	res
	}


