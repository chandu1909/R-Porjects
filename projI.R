#Author: Team 3
#Project 1
#This project compute the Sunlet and Barbell Graphs using iGraph.
library(igraph)

######## Sunlet Graph #########
sunletGraph <- function(m) {
  #computes a sunlet graph by using the IGraph
  
  # Order and size of the Cycle (make_ring)
  orderOfGraph <- m
  sizeOfGraph <- m
  
  # Order and size of the n-Sunlet Graph
  sunletGraphOrder <- 2 * m
  sunletGraphSize <- 2 * m
  
  # creating cycle for the sunlet graph
  sunlet <- make_ring(m, circular = TRUE) 
  
  # Creating dataframes for vertices and edges 
  vertexDF <- as_data_frame(sunlet, what = "edges")
  edgeDF <- data.frame(from = NA, to = NA)[numeric(0),]
  i <- 1
  for (i in 1:nrow(vertexDF)) {
    edgeDF[nrow(edgeDF) + 1,] <- c(i, i + vcount(sunlet)) # adding edges to the sunlet graph
  }
  vertexEdgeBind<- rbind(vertexDF, edgeDF) # binding the vertex and edge dataframes
  
  # convert the dataframes to an IGraph graph with graph_from_data_frame
  g <- graph_from_data_frame(vertexEdgeBind, directed = FALSE)
  
  # set graph name 
  g$name <- paste("sunlet(", m, ")", sep = '')
  g$cycle <- m
  return(g)
}

sunletVertexInfo <- function(g, v) {
  #Computes the name of the given vertex
  #Args:
  # g <- A Sunlet graph
  # v - The number of vertex
  #
  # Returns: 
  # "Cycle" if v is in the cycle of the graph
  # "leaf" if v is outside of the cycle 
  # NULL if v is not a vertex number 
  
  # if v is not a vertex number, return NULL
  if (v > vcount(g) || (v < 1)) {
    return (NULL)
  }
  
  # Now test to see where the vertex lies.
  if ((v <= g$cycle)) {
    return("Cycle")
  } else {
    return("Leaf")
  }
}

sunletEdgeInfo <- function(g, e) {
  #Computes the nature of the given edge.
  #Args:
  # g <- A Sunlet graph
  # e - The number of an edge
  #
  # Returns:
  # "Cycle" if e goes to one of the 
  #   edges in ring of g
  # "Ray" if e is outside of the cycle or ring
  # Null if e id not an edge number 
  
  #if e is not an edge number, return NULL
  if ((e > ecount(g)) || (e < 1)) {
    return(NULL)
  }
  
  #Now test to see where the edge lies 
  if ((e <= g$cycle)) {
    return("Cycle")
  } else {
    return("Ray")
  }
}


######Barbell Graph#########

barbellGraph <- function(n){
  
  #Determining number of verticies and edges of the n-barbell graph
  
  order <- n #to store original number of vertices
  edges<-((n*n)-n)/2  #size of complete graph
  
  size <- ((n*n)-n)+1 # determining size of the n-barbell graph
  barbellOrder <- 2*n # Determining order of the n-barbell graph
  
  #Data frames to hold vertices and edges list
  
  vertexDF <- data.frame("Vertices"=1:barbellOrder) #Holds the total number of vertices
  edgeDF <- data.frame(from=NA,to=NA)[numeric(0), ] #Holds total number of edges with connections
  
  i<-1
  j<-1
  #Adding edge connections for first graph
  for(i in 1:order){
    for(j in i:order){
      if(j>i)
      edgeDF<-rbind(edgeDF,data.frame(from=i,to=j))
     
    }    
  }
  #generating replica of the complete graph generated earlier 
    for(i in n+1:barbellOrder){
    for(j in n:barbellOrder){
      if(j>i)
        edgeDF<-rbind(edgeDF,data.frame(from=i,to=j)) #adding edges in replica
      
    }    
  }
  
  #adding bridge edge to data frame edgeDF- connecting first node of both the graphs
  
  first_node<-vertexDF[1,]
  edgeDF<-rbind(edgeDF,data.frame(from=first_node,to=n+1))
  
  g<-graph_from_data_frame(edgeDF, directed = FALSE, vertices = vertexDF)
  
  g$name <- paste("barbell(",n,")",sep = "")
  g$complete <- order
  #uncomment the below line if you would like to plot the graph 
  #plot(g) 
  
  return(g)
  
}

barbellVertexInfo <- function(g,v){
  #Computes the nature of the given Vertex
  #Args:
  #g- A Path graph
  #V- The number of Vertex
  #
  #Returns:
  #Complete1 if the vertex is in the first complete graph
  #Complete2 if the vertex is in the second complete graph
  #NULL if the supplied vertex number is not in the graph.
  
  if(v>1 && v < vcount(g)/2){
    return("Complete1")
  }else if(v>vcount(g)/2 && v < vcount(g)){
    return("Complete2")
  }
  else{
    return(NULL)
  }

}
barbellEdgeInfo <- function(g,e){
  #computes the nature of the given edge
  #Args:
  #g- A barbell graph
  #e- The number of an edge
  #
  #Returns:
  #"Complete1" if the given edge is in the first Complete Graph.
  #"Coplete2" if the given edge is in the second complete graph
  #"Bridge" if the given edge is the bridge
  #NULL if the edge is not valid
  
  if(e>1 && e< ecount(g)/2){
    return("Complete1")
  }else if(e> ecount(g)/2 && e< ecount(g)){
    return("Complete 2")
  }else if(e == ecount(g)){
    return("Bridge")
  }
  else
    return(NULL)
}

