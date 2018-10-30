# SUMO.project2plot.R
# Author: Quentin Goss
# Last Modified: 10/26/18
# 
# Provides all of the functions needed to create a plot of junctions in a sumo project.

# Retrieves a junctions.matrix from a given project.
#
# @param string project.name <- Name of the project to retreive junctions.matrix from
# (i.e. 3choices)
# @param string jsons.dir <- location of the jsons project directory
# (i.e ../jsons/)
# @return junctions.matrix <- The junctions data of project project.name in matrix format
project.name2jm <- function(project.name,jsons.dir='../jsons/'){
  source(file="../scripts/sumo_dg_viewer.R")
  
  # Get the junctions.matrix and edges.matrix into a temporary variable jmem
  jmem <- json2matrices(project.name,jsons.dir) # @return list(junctions.matrix,edges.matrix)
  
  # junctions.matrix is at index 1
  junctions.matrix <- jmem[[1]]
  
  # We don't need the temporary variable anymore so clear it from the workspace.
  rm(jmem)

  return(junctions.matrix)
}

# Retrieves the following from a junctions.matrix:
# - Node names
# - True Center Coordinates X and Y
#
# @param jm <- junctions.matrix
# @return List <- Index:
# 1. vector string of node names
# 2. vector numeric of X coordinates
# 3. vector numeric of Y coordinates
jm2xynames <- function(jm){
  coords <- jm[,'true_center_coords']
  coords.names <-  rownames(jm)
  coords.x <- unname(unlist(lapply(coords,'[[',1)))
  coords.y <- unname(unlist(lapply(coords,'[[',2)))
  return(list(coords.x,coords.y,coords.names))
}

# Automates the entire process from a SUMO project's jsons data to a plot of points.
#
# @param string project.name <- Name of the project to retreive junctions.matrix from
# (i.e. 3choices)
# @param string jsons.dir <- location of the jsons project directory
# (i.e ../jsons/)
SUMO.project2plot <- function(project.name,jsons.dir='../jsons/',plot.type=1){
  junctions.matrix <- project.name2jm(project.name,jsons.dir)
  xynames <- jm2xynames(junctions.matrix)
  coords.x <- unlist(xynames[1])
  coords.y <- unlist(xynames[2])
  #coords.names <- unlist(xynames[3])
  rm(xynames)
  plot(coords.x,coords.y)
}