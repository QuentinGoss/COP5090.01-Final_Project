library(jsonlite) # JSON read + write
library(igraph) # Directed graph style 1
library(statnet) # Directed graph style 2

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