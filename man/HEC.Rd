\name{HEC}
\alias{HEC}
\docType{data}
\title{
A Hive Plot of the Hair Eye Color Data Set
}
\description{
This is an \code{\link{HPD}} (\code{HivePlotData} object) derived from the built-in hair eye color data set (see \code{?HairEyeColor}).  It serves as a test 2D data set, and the example below shows how it was built.  While every data set is different and will require a different approach, the example illustrates the general approach to building a hive plot from scratch, manually.
}
\usage{data(HEC)}
\format{
  The format is:
List of 5
 $ nodes    :'data.frame':	64 obs. of  6 variables:
  ..$ id    : int [1:64] 1 2 3 4 5 6 7 8 9 10 ...
  ..$ lab   : chr [1:64] "hair_1" "hair_2" "hair_3" "hair_4" ...
  ..$ axis  : int [1:64] 1 1 1 1 1 1 1 1 1 1 ...
  ..$ radius: num [1:64] 2 4 1 3 2 4 1 3 2 4 ...
  ..$ size  : num [1:64] 1.52 4.03 1 1.79 1.52 ...
  ..$ color : chr [1:64] "black" "black" "black" "black" ...
 $ edges    :'data.frame':	32 obs. of  4 variables:
  ..$ id1   : int [1:32] 1 2 3 4 5 6 7 8 9 10 ...
  ..$ id2   : int [1:32] 33 34 35 36 37 38 39 40 41 42 ...
  ..$ weight: num [1:32] 4.097 5.563 1.633 0.309 1.789 ...
  ..$ color : chr [1:32] "lightblue" "lightblue" "lightblue" "lightblue" ...
 $ type     : chr "2D"
 $ desc     : chr "HairEyeColor data set"
 $ axis.cols: chr [1:2] "grey" "grey"
 - attr(*, "class")= chr "HivePlotData"
}
\examples{
# An example of building an HPD from scratch
# Get the data
data(HairEyeColor) # see ?HairEyeColor for background
df <- data.frame(HairEyeColor) # str(df) is useful

# Frequencies of the colors can be found with:
eyeF <- aggregate(Freq ~ Eye, data = df, FUN = 'sum')
hairF <- aggregate(Freq ~ Hair, data = df, FUN = 'sum')
es <- eyeF$Freq/eyeF$Freq[4] # node sizes for eye
hs <- hairF$Freq/hairF$Freq[3] # node sizes for hair

### Step 1. Assemble a data frame of the nodes
# There are 32 rows in the data frame, but we are going to
# separate the hair color from the eye color and thus
# double the number of rows in the node data frame

nodes <- data.frame(
	id = 1:64,
	lab = paste(rep(c("hair", "eye"), each = 32), 1:64, sep = "_"),
	axis = rep(1:2, each = 32),
	radius = rep(NA, 64))
	
for (n in 1:32) {
	# assign node radius based most common colors
	if (df$Hair[n] == "Black") nodes$radius[n] <- 2	
	if (df$Hair[n] == "Brown") nodes$radius[n] <- 4	
	if (df$Hair[n] == "Red") nodes$radius[n] <- 1	
	if (df$Hair[n] == "Blond") nodes$radius[n] <- 3	

	if (df$Eye[n] == "Brown") nodes$radius[n + 32] <- 1	
	if (df$Eye[n] == "Blue") nodes$radius[n + 32] <- 2	
	if (df$Eye[n] == "Hazel") nodes$radius[n + 32] <- 3	
	if (df$Eye[n] == "Green") nodes$radius[n + 32] <- 4	

	# now do node sizes
	if (df$Hair[n] == "Black") nodes$size[n] <- hs[1]	
	if (df$Hair[n] == "Brown") nodes$size[n] <- hs[2]	
	if (df$Hair[n] == "Red") nodes$size[n] <- hs[3]	
	if (df$Hair[n] == "Blond") nodes$size[n] <- hs[4]	

	if (df$Eye[n] == "Brown") nodes$size[n + 32] <- es[4]	
	if (df$Eye[n] == "Blue") nodes$size[n + 32] <- es[3]	
	if (df$Eye[n] == "Hazel") nodes$size[n + 32] <- es[2]	
	if (df$Eye[n] == "Green") nodes$size[n + 32] <- es[1]	
	}
		
nodes$color <- rep("black", 64)
nodes$lab <- as.character(nodes$lab) # clean up some data types
nodes$radius <- as.numeric(nodes$radius)	

### Step 2. Assemble a data frame of the edges

edges <- data.frame( # There will be 32 edges, corresponding to the original 32 rows
	id1 = c(1:16,49:64), # This will set up edges between each eye/hair pair
	id2 = c(33:48,17:32), # & put the males above and the females below
	weight = df$Freq,
	color = rep(c("lightblue", "pink"), each = 16))
	
edges$color <- as.character(edges$color)

# Scale the edge weight (trial & error to emphasize differences)
edges$weight <- 0.25*log(edges$weight)^2.25

### Step 3. Now assemble the Hive Plot Data (HPD) object

HEC <- list()
HEC$nodes <- nodes
HEC$edges <- edges
HEC$type <- "2D"
HEC$desc <- "HairEyeColor data set"
HEC$axis.cols <- c("grey", "grey")
class(HEC) <- "HivePlotData"

### Step 4. Check it & summarize

chkHPD(HEC) # answer of FALSE means there are no problems
sumHPD(HEC)

### Step 5.  Some plots.
# A minimal plot
plotHive(HEC, ch = 0.1, bkgnd = "white")

}
\keyword{datasets}
