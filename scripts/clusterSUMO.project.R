# clusterSUMO.project.R
# Author: Quentin Goss
# Last Modified: 10/30/18
#
# Performs clustering on an a SUMO map given a project name.

# Transforms json data from a given project into a matrix usable with the base `kmeans` method`.
#
# @param string project.name <- Name of the project to retreive junctions.matrix from
# (i.e. 3choices)
# @param string jsons.dir <- location of the jsons project directory
# (i.e ../jsons/)
# @return coords.matrix <- A 2 column matrix of junction coordinates.
project.name2cm <- function(project.name,jsons.dir='../jsons/'){
  source(file='../scripts/SUMO.project2plot.R')
  
  # Get a junctions.matrix from the json files for project.name
  junctions.matrix <- project.name2jm(project.name,jsons.dir)
  
  # Extract the data we want from junction.data at column 'True Coords' (default)
  xynames <- jm2xynames(junctions.matrix)
  
  # Unlist and give x, y, and names their own vectors.
  coords.x <- unlist(xynames[1])
  coords.y <- unlist(xynames[2])
  coords.names <- unlist(xynames[3])
  
  # Clean up
  rm(junctions.matrix,xynames)
  
  # Create coords.matrix
  coords.matrix <- cbind(coords.x,coords.y)
  colnames(coords.matrix) <- c('x','y')
  rownames(coords.matrix) <- coords.names
  
  return(coords.matrix)
}

# Performs kmeans to a matrix of xy coordinates and plots tehm to graph
#
# @param matrix x <- Data to fed into the kmeans aglorithm and plotted.
# @param int k <- The amount of centers in our kmeans algorithm
# @param int iter.max <- Maximum number of iterations
# @param int nstart <- # of random sets chosen
# @param string method <- name of k-means method to be used
# @param bool verbose <- print kmeans output
# @param string legend.pos <- legend position
# @param bool return.cc <- Returns kmeans cluster dataframe
plot.kmeans <- function(x,k=3,iter.max=10,nstart=1,algorithm="Hartigan-Wong",verbose=FALSE,legend.pos='bottomleft',return.cc=FALSE){
  coords.clusters <- kmeans(x, centers = k, iter.max, nstart, algorithm)
  if (verbose) print(coords.clusters)
  plot(x, col = coords.clusters$cluster)
  points(coords.clusters$centers, col = 1:k , pch = 8, cex = 2)
  legend(
    legend.pos,
    pch = rep(1,k),
    legend = 1:k,
    col = unique(1:k)
  )
  if (return.cc) return(coords.clusters)
}