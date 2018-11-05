# SUMO.kmeans.simplify.R
# Author: Quentin Goss
# Last modified: 11/5/18
#
# Simplifys SUMO map data using the kmeans clustering algorithm.
#
# The following inputs are required:
#
#   1. A path to the location containing the project folder.
#      (i.e. ../ or c:/flpoly/jsons/)
#   2. A name the folder containing the `junctions.json` and `edges.json`.
#      (i.e. leopard or 3choices)
#   3. The value of k (the amount of clusters)
#   4. The path where the output of the script will be written.
#   5. Plot height and width (default is 640x480 resolution)
#
# Centers are used as new junctions and edges are determined by the following rule:
#
#   If a edge begins in cluster A and ends in cluster B, then we say that there is a
#   connection A ---> B
#
# The output of the script is as follows:
#
#   1. Three (3) .json files.
#      1.1. junctions.json with cluster centers. The normalized coords have been re-fitted.
#      1.2. edges.json now with edge weights.
#      1.3. clusters.json which contains a list of all of the nodes within a simplified cluster.
#   2. A colored plot of the clusters.
#   3. A colored plot showing only cluster centers.
#   4. A network of the simplified directed graph.
#

library(jsonlite) # JSON read + write
library(igraph) # Directed graph style 1
library(statnet) # Directed graph style 2

# The meat and potatoes of the script
#
# @param string project.name <- Name of the project to retreive junctions.matrix from
# (i.e. 3choices)
# @param string jsons.dir <- location of the jsons project directory
# (i.e ../jsons/)
# @param string output.dir <- Path where teh output will be placed
# @param int plot.width <- width of plot
# @param int plot.height <- height of plot
# @param int k <- The amount of centers in our kmeans algorithm
# @param int iter.max <- Maximum number of iterations
# @param int nstart <- # of random sets chosen
# @param string legend.pos <- legend position
SUMO.kmeans.simplify <- function(project.name,jsons.dir='../jsons/',output.dir='./',plot.width=640,plot.height=480,plot.legend.pos='bottomleft',kmeans.k=3,kmeans.iter.max=10,kmeans.nstart=1){
  
  ######################################
  # Perform kmeans and plot clusters
  ######################################
  png(filename=paste0(output.dir,project.name,'.clusters.png'),width=plot.width,height=plot.height,units='px')
  coords.clusters <- plot.kmeans(project.name2cm(project.name),k=kmeans.k,legend.pos=plot.legend.pos,return.cc=T)
  
  ######################################
  # Plot of centers
  ######################################
  png(filename=paste0(output.dir,project.name,'.centers.png'),width=plot.width,height=plot.height,units='px')
  k <- nrow(coords.clusters$centers)
  plot(coords.clusters$centers,col=1:k)
  legend(plot.legend.pos,pch = rep(1,k),legend = 1:k,col = unique(1:k))
  
  ######################################
  # Load in junction and edge data
  ######################################
  jmem <- json2matrices('leopard')
  junctions.matrix <- jmem[[1]]
  edges.matrix <- jmem[[2]]
  rm(jmem) # Clean up temporary object
  
  # Initialze an empty adjacency matrix
  adjacency.matrix <- matrix(data = rep(0,k * k),nrow = k,ncol = k)
  rownames(adjacency.matrix) <- paste0('cluster_',1:k)
  colnames(adjacency.matrix) <- paste0('cluster_',1:k)
  
  ######################################
  # Fill adjacency matrix
  ######################################
  
  for (i in 1:nrow(edges.matrix)){
    # From
    node.from <- edges.matrix[[i,'from']]
    node.from.cluster <- coords.clusters$cluster[node.from]
    
    # To
    node.to <- edges.matrix[[i,'to']]
    node.to.cluster <- coords.clusters$cluster[node.to]
    
    # Update adjacency table
    if (node.to.cluster != node.from.cluster){
      adjacency.matrix[node.from.cluster,node.to.cluster] <- adjacency.matrix[node.from.cluster,node.to.cluster] + 1
    }
  }
  
  # Plot the directed graph
  png(filename=paste0(output.dir,project.name,'.dg.png'),width=plot.width,height=plot.height,units='px')
  adjacency.network <- network(adjacency.matrix,matrix.type="adjacency",directed=FALSE,cignore.eval=FALSE,names.eval="value")
  gplot(adjacency.network,gmode="digraph",label=adjacency.network%v%'vertex.names',arrowhead.cex=1)
  
  ######################################
  # Prepare clusters.json to be written
  ######################################
  
  # Amount of clusters
  nn <- length(coords.clusters$size)
  
  # Cluster names
  cl.names <- paste0(rep("cluster_", nn), 1:nn)
  
  # Size of each cluster (The amount of junctions in each cluster)
  cl.size <- coords.clusters$size
  
  # Names of all of the nodes within each cluster.
  cl.max <- max(cl.size) 
  cl.nodes <- matrix(data = NA_character_, nrow = nn, ncol = cl.max)
  
  # Populate the list of nodes
  counter <- rep(1L,nn)
  for (i in 1:length(coords.clusters$cluster)){
    cl.index <- coords.clusters$cluster[i]
    cl.nodes[cl.index,counter[cl.index]] <- names(cl.index)
    counter[cl.index] <- counter[cl.index] + 1
  }
  rm(counter)
  
  ######################################
  # Write clusters.json
  ######################################
  
  filepath <- paste0(output.dir,project.name,'.clusters.json')
  
  # Start file
  write('{',file=filepath)
  
  # Add objects
  for (i in 1:nn){
    # Begin cluster object
    line <- paste0('\t"',cl.names[i],'": {')
    write(line, file=filepath, append=T)
    
    # name
    line <- paste0('\t\t"id": "',cl.names[i],'",')
    write(line, file=filepath, append=T)
    
    # size
    line <- paste0('\t\t"size": ',as.integer(cl.size[i]),',')
    write(line, file=filepath, append=T)
    
    # nodes
    line <- paste0('\t\t"nodes": [')
    for (j in 1:cl.max){
      if(!is.na(cl.nodes[i,j])){
        if (!(j == 1)) line <- paste0(line,',')
        line <- paste0(line,'"',cl.nodes[i,j],'"')
      } else {
        break
      }
    }
    line <- paste0(line,']')
    write(line, file=filepath, append=T)
    
    # End cluster object
    line <- paste0('\t}')
    if (!(i == nn)) line <- paste0(line,',')
    write(line, file=filepath, append=T)
  }
  
  # End file
  write('}', file=filepath, append=T)
  
  ######################################
  # Prepare junctions.json to be written
  ######################################
  jct.names <- cl.names
  jct.coords.true <- coords.clusters$centers
  jct.coords.norm <- norm_coords(jct.coords.true,xmin = 0,xmax = 1,ymin = 0,ymax = 1)
  
  ######################################
  # Write junctions.json
  ######################################
  filepath <- paste0(output.dir,project.name,'.junctions.json')
  
  # Start file
  write('{', file=filepath)
  
  # Add object
  for (i in 1:nn){
    # Begin junction object
    line <- paste0('\t"',jct.names[i],'": {')
    write(line, file=filepath, append=T)
    
    # id
    line <- paste0('\t\t"id": "',jct.names[i],'",')
    write(line, file=filepath, append=T)
    
    # true_center_coords
    line <- paste0('\t\t"true_center_coords": [', jct.coords.true[i,'x'],',',jct.coords.true[i,'y'],'],')
    write(line, file=filepath, append=T)
    
    # normal_center_coords
    line <- paste0('\t\t"normal_center_coords": [', jct.coords.norm[i,'x'],',',jct.coords.norm[i,'y'],']')
    write(line, file=filepath, append=T)
    
    # End cluster object
    line <- paste0('\t}')
    if (!(i == nn)) line <- paste0(line,',')
    write(line, file=filepath, append=T)
  }
  
  # End file
  write('}', file=filepath, append=T)
  
  ######################################
  # Prepare edges.json to be written
  ######################################
  # Get number of unique connections between nodes
  nconn <- sum(adjacency.matrix != 0)
  
  # Names
  edges.names <- rownames(adjacency.matrix)
  
  # Initialize variables
  edges.id <- rep(NA_character_,nconn)
  edges.from <- rep(NA_character_,nconn)
  edges.to <- rep(NA_character_,nconn)
  edges.weight <- rep(NA_integer_,nconn)
  
  # True coords
  edges.coords.true.from <- matrix(data=rep(NA_integer_,2*nconn),nrow=nconn,ncol=2)
  colnames(edges.coords.true.from) <- c('x','y')
  edges.coords.true.to <- matrix(data=rep(NA_integer_,2*nconn),nrow=nconn,ncol=2)
  colnames(edges.coords.true.to) <- c('x','y')
  
  # Normal coords
  edges.coords.norm.from <- matrix(data=rep(NA_integer_,2*nconn),nrow=nconn,ncol=2)
  colnames(edges.coords.norm.from) <- c('x','y')
  edges.coords.norm.to <- matrix(data=rep(NA_integer_,2*nconn),nrow=nconn,ncol=2)
  colnames(edges.coords.norm.to) <- c('x','y')
  
  # Populate our varaibles
  counter <- 1
  for (index.from in 1:nrow(adjacency.matrix)){
    for (index.to in 1:ncol(adjacency.matrix)){
      if (adjacency.matrix[index.from,index.to] != 0){
        # From
        edges.from[counter] <- edges.names[index.from]
        
        # to
        edges.to[counter] <- edges.names[index.to]
        
        # id
        edges.id[counter] <- paste0(edges.from[counter],'_to_',edges.to[counter])
        
        # Weight
        edges.weight[counter] <- as.integer(adjacency.matrix[index.from,index.to])
        
        # True Coordinates
        edges.coords.true.from[counter,'x'] <- coords.clusters$centers[index.from,'x']
        edges.coords.true.from[counter,'y'] <- coords.clusters$centers[index.from,'y']
        edges.coords.true.to[counter,'x'] <- coords.clusters$centers[index.to,'x']
        edges.coords.true.to[counter,'y'] <- coords.clusters$centers[index.to,'y']
        
        # Normal Coordinates
        edges.coords.norm.from[counter,'x'] <- jct.coords.norm[index.from,'x']
        edges.coords.norm.from[counter,'y'] <- jct.coords.norm[index.from,'y']
        edges.coords.norm.to[counter,'x'] <- jct.coords.norm[index.to,'x']
        edges.coords.norm.to[counter,'y'] <- jct.coords.norm[index.to,'y']
        
        # Increment counter
        counter <- counter + 1
      }
    }
  }
  ######################################
  # Write edges.json
  ######################################
  filepath <- paste0(output.dir,project.name,'.edges.json')
  
  # Start file
  write('{', file=filepath)
  
  # Add object
  for (i in 1:nconn){
    # Begin junction object
    line <- paste0('\t"',edges.id[i],'": {')
    write(line, file=filepath, append=T)
    
    # id
    line <- paste0('\t\t"id": "',edges.id[i],'",')
    write(line, file=filepath, append=T)
    
    # from
    line <- paste0('\t\t"from": "',edges.from[i],'",')
    write(line, file=filepath, append=T)
    
    # to
    line <- paste0('\t\t"to": "',edges.to[i],'",')
    write(line, file=filepath, append=T)
    
    # weight
    line <- paste0('\t\t"weight": ',edges.weight[i],',')
    write(line, file=filepath, append=T)
    
    # true_center_coords
    line <- paste0(
      '\t\t"true_center_coords": [[',
      edges.coords.true.from[i,'x'],',',edges.coords.true.from[i,'y'],'],[',
      edges.coords.true.to[i,'x'],',',edges.coords.true.to[i,'y'],']]',','
    )
    write(line, file=filepath, append=T)
    
    # normal_center_coords
    line <- paste0(
      '\t\t"normal_center_coords": [[',
      edges.coords.norm.from[i,'x'],',',edges.coords.norm.from[i,'y'],'],[',
      edges.coords.norm.to[i,'x'],',',edges.coords.norm.to[i,'y'],']]'
    )
    write(line, file=filepath, append=T)
    
    # End cluster object
    line <- paste0('\t}')
    if (!(i == nconn)) line <- paste0(line,',')
    write(line, file=filepath, append=T)
  }
  
  # End file
  write('}', file=filepath, append=T)
}

########################################################################
######## clusterSUMO.project.R
########################################################################
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

########################################################################
######## SUMO.project2plot.R
########################################################################
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

########################################################################
######## sumo_dg_viewer.R
########################################################################



# Creates an adjancecy matrix from a `junctions.json` and a
# `edges.json` file.
#
# @param string project.name <- name of the map project
#  (i.e. "3choices")
# @param string jsons.dir <- path to jsons director
# (i.e. ./jsons.dir)
# @return matrix <- adjacency.matrix ready to be plotted to a graph
json2am <- function(project.name,jsons.dir='../jsons/'){
  junctions.json <- paste0(jsons.dir, project.name, '/junctions.json')
  edges.json <- paste0(jsons.dir, project.name, '/edges.json')
  junctions.list <- fromJSON(junctions.json)
  edges.list <- fromJSON(edges.json)
  
  junctions.matrix <- sym_nested_list_to_matrix(junctions.list)
  edges.matrix <- sym_nested_list_to_matrix(edges.list)
  
  adjacency.matrix <- jmem2am(jm=junctions.matrix,em=edges.matrix)
  
  return(adjacency.matrix)
}

# Creates a matrix for edges and junctions from a `junctions.json`
# and an `edges.json` file.
#
# @param string project.name <- name of the map project
#  (i.e. "3choices")
# @param string jsons.dir <- path to jsons director
# (i.e. ./jsons.dir)
# @returns 2 matrixes <- c(junctions.matrix,edges.matrix)
json2matrices <- function(project.name,jsons.dir='../jsons/'){
  junctions.json <- paste0(jsons.dir, project.name, '/junctions.json')
  edges.json <- paste0(jsons.dir, project.name, '/edges.json')
  junctions.list <- fromJSON(junctions.json)
  edges.list <- fromJSON(edges.json)
  
  junctions.matrix <- sym_nested_list_to_matrix(junctions.list)
  edges.matrix <- sym_nested_list_to_matrix(edges.list)
  
  return(list(junctions.matrix,edges.matrix))
}

# Takes a symmetrical nested list and creates a matrix.
# The names of dimension 1 are used as column names and
# the names of dimension 2 are used as row names.
#
# @param somedata.list <- a nested list.
# @return <- a named matrix of the input somedata.list
sym_nested_list_to_matrix <- function(somedata.list){
  # Gather data from our list
  nrows <- length(somedata.list)
  ncols <- length(somedata.list[[1]])
  rnames <- names(somedata.list)
  cnames <- names(somedata.list[[1]])
  
  # Create an empty matrix of lists
  somedata.matrix <- matrix(
    data=rep(list(),nrows*ncols),
    nrow=nrows,
    ncol=ncols,
    byrow=FALSE,
    dimnames=NULL
  )
  
  # Add row and column names
  rownames(somedata.matrix) <- rnames
  colnames(somedata.matrix) <- cnames
  
  # Copy data one cell at a time
  for (row.index in 1:nrows){
    for (col.index in 1:ncols){
      somedata.matrix[row.index,col.index] <- somedata.list[[row.index]][col.index]
    }
  }
  
  return(somedata.matrix)
}

# Creates an adjacency matrix by combining data from junctions.matrix and edges.matrix
#
# @param matrix jm <- junctions.matrix
# @param matrix em <- edges.matrix
# @return matrix am <- adjacency.matrix
jmem2am <- function(jm,em){
  # Get the size of the ajacentcy matrix
  jm.amt <- length(names(jm[,1]))
  
  # Store the for the rows and columns
  jm.names <- names(jm[,1])
  
  # Initialize an empty matrix
  am <- matrix(
    data=rep(0, jm.amt * jm.amt),
    ncol=jm.amt,
    nrow=jm.amt,
    byrow=FALSE
  )
  
  # Set row and column names
  rownames(am) <- jm.names
  colnames(am) <- jm.names
  
  # Fill the adjacency matrix
  print("Filling adjaency matrix")
  pb <- txtProgressBar(min=0,max=nrow(em))
  for (i in 1:nrow(em)){
    setTxtProgressBar(pb,i)
    from <- paste0(em[i,'from'])
    to <- paste0(em[i,'to'])
    row.index <- match(from,rownames(am))
    col.index <- match(to,colnames(am))
    am[row.index,col.index] <- 1
  }
  
  return(am)
}

# Creates an igraph directed graph from an adjacency matrix
#
# @param am <- adjacency matrix
am2igraph <- function(am){
  am.network <- graph_from_adjacency_matrix(
    am,
    mode="directed"
  )
  plot.igraph(
    am.network,
    edge.color="black"
  )
}

# Creates a statnet directed graph from an adjacency matrix
#
# @param am <-  adjacency matrix
am2statnet <- function(am){
  am.network <- network(
    am,
    matrix.type="adjacency",
    directed=FALSE,
    cignore.eval=FALSE,
    names.eval="value"
  )
  gplot(
    am.network,
    gmode="digraph",
    label=am.network%v%'vertex.names',
    boxed.labels=TRUE,
    vertex.cex=0.3,
    arrowhead.cex=0.3
  )
}