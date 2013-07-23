edge2HPD <- function(edge_df = NULL, axis.cols = NULL, type = "2D", desc = NULL, ...) {
	
	# Authored and contributed to HiveR by Jonathan H. Chung, June 2013.
	# Thanks Jon.  Small changes for consistency by Bryan A. Hanson
	
  if (is.null(edge_df)){
    stop("No edge data provided")
  }
  if (!is.data.frame(edge_df)){
    stop("edge_df is not a data frame")
  }
  
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
  node_attributes <- cbind("id"    = id, 
                           "label" = unique(c(lab1, lab2)))
  node_attributes <- as.data.frame(node_attributes)
  
  # Define default axis
  axis <- rep(1, nn)
  # Define node color
  color <- as.character(rep("black", nn))
  # Define radius
  radius <- rep(1, nn)
  # Create HPD object
  HPD <- list()
  
  # Define node attributes
  HPD$nodes$id <- node_attributes$id
  HPD$nodes$lab <- node_attributes$label
  HPD$nodes$axis <- axis
  HPD$nodes$radius <- radius
  HPD$nodes$size <- size
  HPD$nodes$color <- color
  
  # Get number of edges
  ne <- nrow(edge_df)
  # Set up edge list
  edge1 <- merge(edge_df[, 1], node_attributes, by.x = 1, by.y = "label")
  edge2 <- merge(edge_df[, 2], node_attributes, by.x = 1, by.y = "label")
  
  HPD$edges$id1 <- edge1$id
  HPD$edges$id2 <- edge2$id
  # check if edge data has attributes
  if (ncol(edge_df) > 2) {
    if (is.numeric(edge_df[, 3]) | is.integer(edge_df[, 3])){
      edge_weight <- edge_df[, 3]
    } else{
      stop("Edge weight column is not numeric or integer.")
    }
  } else {
    warning("No edge weight column detected. Setting default edge weight to 1")
    edge_weight <- rep(1, ne)
  }
  print(str(HPD))
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
    axis.cols <- brewer.pal(length(unique(HPD$nodes$axis)),
                            "Set1")
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