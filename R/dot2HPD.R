

dot2HPD <- function (file = NULL, node.inst = NULL, edge.inst = NULL,
	axis.cols = NULL, type = "2D", desc = NULL, ...) {
	
# Function to read dot files and convert to HPD
# Bryan Hanson, DePauw Univ, July 2011

# Assumptions/Caveats/Features:
	# No distinction between undirected and directed graphs
	# Not sure how A -- B -- C would be handled
	# Multiple tag=value entries OK
	
# No checking for whether the type (2D/3D) is actually true

	if (is.null(node.inst)) message("No node instructions provided, proceeding without them")
	if (is.null(edge.inst)) message("No edge instructions provided, proceeding without them")

    lines <- readLines(file, ...)

# Clean off 1st and last lines which contain { and }
# And clean out leading and trailing spaces

 	lines <- lines[-grep("\\{", lines)] # cleans off 1st line
 	lines <- lines[-grep("\\}", lines)] # cleans off last line
 	lines <- gsub("^[[:space:]]|[[:space:]]$", "", lines) # leading spaces + trailing spaces
	lines <- sub(";", "", lines)

# The following will find edges and their attributes

    ed <- lines[grep("--|->", lines)]
    ed <- unique(ed) # just in case

# Find nodes and their attributes by inverting the edge pattern

    no <- lines[-grep("--|->", lines)]
    no <- unique(no) # just in case
	
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
# Collect multiple tag=value sets with their node info

	
	if (!is.null(node.inst)) {
		# get the node names (everything not an attribute)
		nn <- sub("\\[.*\\]$", "", no)
		nn <- gsub("[[:space:]]", "", nn)
		# get the entire list of attributes
		nats <- sub("^.*\\[", "", no) # clean off front
		nats <- sub("\\]$", "", nats) # clean off back
		nats <- strsplit(nats, ",", fixed = TRUE) # returns a list of attributes for each node
		# it works even if there is no ',' i.e. only one attribute (very handy)

		# read in translation instructions
		ni <- read.csv(node.inst, stringsAsFactors = FALSE)

		# loop over the list & match up instructions

		for (i in 1:length(nats)) { # match up instructions
			tagval <- unlist(nats[i])
			tagval <- gsub("[[:space:]]", "", tagval)
			for (j in 1:length(tagval)) {
				tv <- unlist(strsplit(tagval[j], "=", fixed = TRUE))
				for (k in 1:nrow(ni)) {
					#cat("Node no. = ", i, "attribute no = ", j, "node inst = ", k, "\n")
					if ((tv[1] == ni$dot.tag[k]) & (tv[2] == ni$dot.val[k])) {
						# only certain hive.tag values are valid & will be processed
						# other values are silently ignored
						# more options readily added
						
						if (ni$hive.tag[k] == "axis") {
							HPD$nodes$axis[i] <- as.numeric(ni$hive.val[k])
							}
						if (ni$hive.tag[k] == "radius") {
							HPD$nodes$radius[i] <- as.numeric(ni$hive.val[k])
							}
						if (ni$hive.tag[k] == "size") {
							HPD$nodes$size[i] <- as.numeric(ni$hive.val[k])
							}
						if (ni$hive.tag[k] == "color") {
							HPD$nodes$color[i] <- ni$hive.val[k]
							}
						}
					}
				}
			}

	} # end of !is.null(node.inst) & node processing

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
#		print(n)
		pat1 <- sub("(--|->).*$", "", ed_prs[n])
		pat2 <- sub("^.*(--|->)", "", ed_prs[n])
		# print(pat1)
#		print(pat2)
		pat1 <- paste("\\b", pat1, "\\b", sep = "") # need word boundaries
		pat2 <- paste("\\b", pat2, "\\b", sep = "") # to avoid finding fragments
		HPD$edges$id1[n] <- grep(pat1, HPD$nodes$lab)
		HPD$edges$id2[n] <- grep(pat2, HPD$nodes$lab)
		}

# # Process edge attributes

	if (!is.null(edge.inst)) {
		# get the entire list of attributes
		eats <- sub("^.*\\[", "", ed) # clean off front
		eats <- sub("\\]$", "", eats) # clean off back
#		print(head(eats))
		eats <- strsplit(eats, ",", fixed = TRUE) # returns a list of attributes for each edge
		# it works even if there is no ',' i.e. only one attribute (very handy)
#		print(head(eats))
		# read in translation instructions
		ei <- read.csv(edge.inst, stringsAsFactors = FALSE)

		# loop over the list & match up instructions

		for (i in 1:length(eats)) { # match up instructions
			tagval <- unlist(eats[i])
			tagval <- gsub("[[:space:]]", "", tagval)
			for (j in 1:length(tagval)) {
				tv <- unlist(strsplit(tagval[j], "=", fixed = TRUE))
				for (k in 1:nrow(ei)) {
					#cat("Edge no. = ", i, "attribute no = ", j, "edge inst = ", k, "\n")
					if ((tv[1] == ei$dot.tag[k]) & (tv[2] == ei$dot.val[k])) {
						# only certain hive.tag values are valid & will be processed
						# other values are silently ignored
						# more options readily added
						
					if (ei$hive.tag[k] == "weight") {
						HPD$edges$weight[i] <- as.numeric(ei$hive.val[k])
						}
					if (ei$hive.tag[k] == "color") {
						HPD$edges$color[i] <- as.character(ei$hive.val[k])
						}
					}
				}
			}
		}

	} # end of !is.null(edge.inst) & edge processing

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
