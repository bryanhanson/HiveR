edge2HPD <- function(edge_df = NULL, axis.cols = NULL, type = "2D", desc = NULL, ...) {
	
	# Authored and contributed to HiveR by Jonathan H. Chung, June 2013.
	# Thanks Jon.  Some changes for consistency by Bryan A. Hanson
	# A few boo-boos caught by Vesna Memisevic
	
  if (is.null(edge_df)){
    stop("No edge data provided")
  	}
  if (!is.data.frame(edge_df)){
    stop("edge_df is not a data frame")
  	}
  
  ### Process nodes
  # Get node labels
  lab1 <- unlist(edge_df[, 1])
  lab1 <- as.character(lab1)
  lab2 <- unlist(edge_df[, 2])
  lab2 <- as.character(lab2)
  
  # Get number of unique nodes
  nn <- length(unique(c(lab1, lab2)))

  # Set default node size to 1
  size <- rep(1, nn)
  # Create a vector for node ID
  id <- 1:nn
  
  # Assign default axis
  axis <- rep(1, nn)
  # Assign node color
  color <- as.character(rep("black", nn))
  # Assign radius
  radius <- rep(1, nn)

  # Create empty HPD object
  HPD <- list()
  
  # Assemble node attributes
  HPD$nodes$id <- id
  HPD$nodes$lab <- unique(c(lab1, lab2))
  HPD$nodes$axis <- axis
  HPD$nodes$radius <- radius
  HPD$nodes$size <- size
  HPD$nodes$color <- color
  
  ### Process edges - a bit tricky to coordinate!
  ne <- nrow(edge_df)
  edge_df[,1] <- as.character(edge_df[,1]) # for use as id
  edge_df[,2] <- as.character(edge_df[,2]) # may read in as integers
  HPD$edges$id1 <- rep(NA, ne)
  HPD$edges$id2 <- rep(NA, ne)

  for (n in 1:ne) { # same logic as over in dot2HPD
	pat1 <- paste("\\b", edge_df[n,1], "\\b", sep = "") # need word boundaries
#	print(pat1)
	pat2 <- paste("\\b", edge_df[n,2], "\\b", sep = "") # to avoid finding fragments
	HPD$edges$id1[n] <- grep(pat1, HPD$nodes$lab)
	HPD$edges$id2[n] <- grep(pat2, HPD$nodes$lab)
	}
  
  # check if edge data has weights in col 3
  if (ncol(edge_df) > 2) {
    if (is.numeric(edge_df[, 3]) | is.integer(edge_df[, 3])){
      edge_weight <- edge_df[, 3]
      } else {
        warning("No edge weight column detected. Setting default edge weight to 1")
        edge_weight <- rep(1, ne)
        }
     }
     
  HPD$edges$weight <- edge_weight
  HPD$edges$color <- rep("gray", ne)
  HPD$nodes <- as.data.frame(HPD$nodes)
  HPD$edges <- as.data.frame(HPD$edges)
  
  # Add description
  if (is.null(desc)) {
    desc <- "No description provided"
    }
  HPD$desc <- desc
  
  # Define axis columns
  if (is.null(axis.cols)){
    axis.cols <- brewer.pal(length(unique(HPD$nodes$axis)), "Set1")
    }
  HPD$axis.cols <- axis.cols
  
  # Clean up HPD object
  HPD$nodes$axis <- as.integer(HPD$nodes$axis)
  HPD$nodes$size <- as.numeric(HPD$nodes$size)
  HPD$nodes$color <- as.character(HPD$nodes$color)
  HPD$nodes$lab <- as.character(HPD$nodes$lab)
  HPD$nodes$id <- as.integer(HPD$nodes$id)
  HPD$edges$id1 <- as.integer(HPD$edges$id1)
  HPD$edges$id2 <- as.integer(HPD$edges$id2)
  HPD$edges$weight <- as.numeric(HPD$edges$weight)
  HPD$edges$color <- as.character(HPD$edges$color)
  HPD$type <- type
  class(HPD) <- "HivePlotData"
  
  # Check HPD object
  chkHPD(HPD)
  return (HPD)
}
