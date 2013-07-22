mod.adj2HPD <- function (M = NULL, axis.cols = NULL, type = "2D", desc = NULL) 
{
  # Vesna Memisevic provided the fix to read non-bipartite networks.
  
  if (is.null(M)) 
    stop("No adjacency matrix provided")
  if (is.null(dimnames(M))) 
    stop("Adjacency matrix must have named dimensions")
  lab1 <- unlist(dimnames(M)[1])
  lab1 <- as.character(lab1)
  lab2 <- unlist(dimnames(M)[2])
  lab2 <- as.character(lab2)
  d1 <- dim(M)[1]
  d2 <- dim(M)[2]
  nn <- length(unique(c(lab1, lab2)))
  size <- rep(1, nn)
  id <- 1:nn
  axis <- rep(1, nn)
  color <- as.character(rep("black", nn))
  radius <- rep(1, nn)
  HPD <- list()
  HPD$nodes$id <- id
  HPD$nodes$lab <- unique(c(lab1, lab2)) # VM change
  HPD$nodes$axis <- axis
  HPD$nodes$radius <- radius
  HPD$nodes$size <- size
  HPD$nodes$color <- color
  id1 <- id2 <- v <- c()  
  for (i in 1:d1) {
    for (j in 1:d2) {
      if (!M[i, j] == 0) {
        id1 <- c(id1, which(lab1[i] == labNames)) # VM change
        id2 <- c(id2, which(lab2[j] == labNames)) # VM change
        v <- c(v, M[i, j])
      }
    }
  }
  if (!length(id1) == length(id2)) 
    stop("Something is wrong with the M[i,j] counts")
  ne <- length(id1)
  HPD$edges$id1 <- id1
  HPD$edges$id2 <- id2
  HPD$edges$weight <- v
  HPD$edges$color <- rep("gray", ne)
  HPD$nodes <- as.data.frame(HPD$nodes)
  HPD$edges <- as.data.frame(HPD$edges)
  if (is.null(desc)) 
    desc <- "No description provided"
  HPD$desc <- desc
  if (is.null(axis.cols)) 
    axis.cols <- brewer.pal(length(unique(HPD$nodes$axis)), "Set1")
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
}
