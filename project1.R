#Author: Chandra sekhar Polavarapu
#Project 1
#Team 3
#This project explores the Sunlet and Barbell Graphs.
library(igraph)

######Barbell Graphs#########

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
 
  #print(edgeDF)
  plot(g)
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
  
  
  
}

