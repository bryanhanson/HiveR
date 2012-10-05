

dot2HPD <- function (file = NULL, node.inst = NULL, edge.inst = NULL,
	axis.cols = NULL, type = "2D", desc = NULL, ...) {
	
# Function to read dot files and convert to HPD
# Bryan Hanson, DePauw Univ, July 2011

# Gradually building in compliance with DOT stds...

# Assumptions/Caveats:
	# No distinction between undirected and directed graphs
	# Not sure how A -- B -- C would be handled
	
# No checking for whether the type (2D/3D) is actually true

    lines <- readLines(file, ...)

# Clean off 1st and last lines which contain { and }
# And clean out leading and trailing spaces

 	lines <- lines[-grep("\\{", lines)] # cleans off 1st line
 	lines <- lines[-grep("\\}", lines)] # cleans off last line
 	lines <- gsub("^[[:space:]]|[[:space:]]$", "", lines) # leading spaces + trailing spaces
	lines <- sub(";", "", lines)

# The following will find edges and their attributes

    ed <- lines[grep("--|->", lines)]

# Find nodes and their attributes by inverting the edge pattern

    no <- lines[-grep("--|->", lines)]
    no <- unique(no)

# Initialize HPD$nodes

	HPD <- list()
	HPD$nodes$id <- 1:length(no)
	HPD$nodes$lab <- gsub("\\[.*\\]$", "", no) # strips off any attributes
	HPD$nodes$lab <- gsub("[[:space:]]", "", HPD$nodes$lab) # strips off any spaces
	HPD$nodes$axis <- rep(1, length(no))
	HPD$nodes$radius <- rep(1, length(no))
	HPD$nodes$size <- rep(1, length(no))
	HPD$nodes$color <- rep("transparent", length(no))

# Process node attributes
# Not sure how this will handle multiple tag=value sets
	

	if (!is.null(node.inst)) {

	at <- sub("^.*\\[", "", no)
	at <- sub("\\]$", "", at)
	at <- unlist(strsplit(at, ",", fixed = TRUE))
	at <- strsplit(at, "=", fixed = TRUE)
	dot.tag <- dot.val <- c()
	for (n in 1:length(at)) dot.tag[n] <- at[[n]][1]
	for (n in 1:length(at)) dot.val[n] <- at[[n]][2]
	dot.tag <- gsub("[[:space:]]", "", dot.tag) # remove any whitespace
	dot.val <- gsub("[[:space:]]", "", dot.val)

	ni <- read.csv(node.inst) # read in translation instructions

		for (n in 1:length(at)) { # match up instructions
			for (i in 1:nrow(ni)) {
				# print(dot.tag[n])
				# print(dot.val[n])
				# cat("Node no. = ", n, "Node inst no = ", i, "\n")
				if ((dot.tag[n] == ni$dot.tag[i]) & (dot.val[n] == ni$dot.val[i])) {
					# only certain hive.tag values are valid & will be processed
					# other values are silently ignored
					# more options readily added
					
					if (ni$hive.tag[i] == "axis") {
						HPD$nodes$axis[n] <- as.numeric(as.character(ni$hive.val[i]))
						}
					if (ni$hive.tag[i] == "radius") {
						HPD$nodes$radius[n] <- as.numeric(as.character(ni$hive.val[i]))
						}
					if (ni$hive.tag[i] == "size") {
						HPD$nodes$size[n] <- as.numeric(as.character(ni$hive.val[i]))
						}
					if (ni$hive.tag[i] == "color") {
						HPD$nodes$color[n] <- as.character(ni$hive.val[i])
						}
					}
				}
			}
 
	}

# Set up HPD$edges
	
	HPD$edges$id1 <- rep(1, length(ed))
	HPD$edges$id2 <- rep(1, length(ed))
	HPD$edges$weight <- rep(1, length(ed))
	HPD$edges$color <- rep("gray", length(ed))

# Match up the two node names in the input file
# with the node ids created above and add to HPD$edges

# remove attributes, remove -- or ->, strip white space, keep 2 names together

	ed_prs <- sub("\\[.*$", "", ed) # remove attributes
	ed_prs <- gsub("[[:space:]]", "", ed_prs) # remove any whitespace

	for (n in 1:(length(ed_prs))) {
		pat1 <- sub("(--|->).*$", "", ed_prs[n])
		pat2 <- sub("^.*(--|->)", "", ed_prs[n])
		pat1 <- paste("\\b", pat1, "\\b", sep = "") # need word boundaries
		pat2 <- paste("\\b", pat2, "\\b", sep = "") # to avoid finding fragments
		HPD$edges$id1[n] <- grep(pat1, HPD$nodes$lab)
		HPD$edges$id2[n] <- grep(pat2, HPD$nodes$lab)
		}

# # Process edge attributes
# # Not sure how this will handle multiple tag=value sets

	at <- sub("^.*\\[", "", ed) # cut off at the front
	at <- sub("\\]$", "", at) # cut off at the back
	at <- unlist(strsplit(at, ",", fixed = TRUE))
	at <- strsplit(at, "=", fixed = TRUE)
	dot.tag <- dot.val <- c()
	for (n in 1:length(at)) dot.tag[n] <- at[[n]][1]
	for (n in 1:length(at)) dot.val[n] <- at[[n]][2]
	dot.tag <- gsub("[[:space:]]", "", dot.tag) # remove any whitespace
	dot.val <- gsub("[[:space:]]", "", dot.val)

	ei <- read.csv(edge.inst) # read in translation instructions

	for (n in 1:length(ed)) { # match up instructions
		for (i in 1:nrow(ei)) {
			if ((dot.tag[n] == ei$dot.tag[i]) & (dot.val[n] == ei$dot.val[i])) {
				# only certain hive.tag values are valid & will be processed
				# other values are silently ignored
				# more options readily added
				
				if (ei$hive.tag[i] == "weight") {
					HPD$edges$weight[n] <- as.numeric(as.character(ei$hive.val[i]))
					}
				if (ei$hive.tag[i] == "color") {
					HPD$edges$color[n] <- as.character(ei$hive.val[i])
					}
				}
			}
		}
 
# Final clean-up
	
	HPD$nodes <- as.data.frame(HPD$nodes)
	HPD$edges <- as.data.frame(HPD$edges)
	
	if (is.null(desc)) desc <- "No description provided"
	HPD$desc <- desc
	
	if (is.null(axis.cols)) axis.cols <- brewer.pal(length(unique(HPD$nodes$axis)), "Set1")
	HPD$axis.cols <- axis.cols
	
	HPD$nodes$axis <- as.integer(HPD$nodes$axis)
	HPD$nodes$size <- as.numeric(HPD$nodes$size)
	HPD$nodes$color <- as.character(HPD$nodes$color)
	HPD$nodes$lab <- as.character(HPD$nodes$lab)
	
	HPD$edges$id1 <- as.integer(HPD$edges$id1)
	HPD$edges$id2 <- as.integer(HPD$edges$id2)
	HPD$edges$weight <- as.numeric(HPD$edges$weight)
	HPD$edges$color <- as.character(HPD$edges$color)
	
	HPD$type <- type
	
	class(HPD) <- "HivePlotData"
	
	chkHPD(HPD)
	
    HPD
	} # The very end!
