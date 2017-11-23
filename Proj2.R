#Author : Chandra sekhar Polavarapu

library(igraph)

# Function Graph Categorical Product
# Description: Consider two graphs g1 and g2, g1 has 'v' vertices and g2 has 'w' vertices
# task is to find the cartesian product of these two sets. Using condition if and only if 
# V set of vertices are adjacent to each other in graph g1 and W set of vertices are 
# adjacent to each other in graph g2,  draw an edge between the cartesian products.

graphCategoricalProduct <- function(g1, g2) {
  graph1 <- make_ring(g1, circular = FALSE) #function for generating path graph 1
  graph2 <- make_ring(g2, circular = FALSE) #function for generating path graph 2
  
  vert <- list(V(graph1), V(graph2)) #collecting the vertices of two graphs as list for future use
  edgeDF <- data.frame(from=NA,to=NA)[numeric(0), ] #making data frame to collect edges of cartesian product of both the graphs
 
 #filling the data frame with initial connections(graph1 and graph 2). 
  for(i in V(graph1)){
    if(i<vcount(graph1)){ 
      edgeDF<-rbind(edgeDF,data.frame(from=i, to = i+1)) #filling graph1 edges
    }
  }
  init <- vcount(graph1)+1 #variable holds the first node of secon graph
  
  for(i in V(graph2)){
    if(i!=vcount(graph2))
      edgeDF<-rbind(edgeDF,data.frame(from=init, to = init+1)) #filling graph2 edges
      init=init+1;
  }
  #from here we fill the edges of cartesian product of two graphs

  for (i in V(graph1)) { #iterate through the vertices of graph g1
    product_element = vcount(graph1)+1 #to identify the starting vertex of second graph
    for (j in V(graph2)) { #iterate through the vertices of graph g2
      
      edgeDF <- rbind(edgeDF,data.frame(from=i, to=product_element)) #adding caretsian product edges 
      
      }
  }
  
  adjmat <- get.adjacency(graph.edgelist(as.matrix(edgeDF), directed=FALSE)) #adjacency matrix of whole data frame
  
  #adjacency matrix is the better way to identify the adjacency among vertices
  #***************************************************************************
  
  graphedges <- data.frame(from=NA,to=NA)[numeric(0), ] #another data frame to store the edges which satisfy the main condition.

  rcheck = c() #vector which holds the combination of pairs
  #loops to check the condition if the vertices are adjacent to each other. each loop is responsble
  #for each element in both pairs eg: ((a1,b1)? (a2,b2)) 
  # please asume i <- a1, k <- a2 and b1, b2 are same 
  
  for (i in V(graph1)){ #iterates till end of the graph 1 ()
    b1 = vcount(graph1) #holds the first vertex of graph2 when incremented
    for(j in V(graph2)){ #iterates till end of the graph 2 
      b1= b1+1 #b1 represents the second element of first pair
      for(k in V(graph1)){ #k represents the first element in second pair
        b2 = vcount(graph1)+1 #b2 represents the second element of second pair 
        for(l in V(graph2)){
          
          if(adjmat[i,k]==1 && adjmat[b1,b2]==1){ #checking if those vertices are adjacent
            
            pair1 <- paste(i,b1,k,b2) #if yes, that combination will be added to the vector to check the redundancy
            
            #pair2 will be used to eliminate the back edges(avoiding redundancy)
            pair2 <- paste(k,b2,i,b1) 
            #if the pair is not available in the vector then it will be added to data frame
            if(is.element(pair1,rcheck)== FALSE || is.element(pair2,rcheck)==FALSE){
              graphedges <- rbind(graphedges,data.frame(from=paste(i,",",b1), to=paste(k,",",b2))) #adding it to final data frame 
              rcheck <- append(rcheck,pair1) #updating vector 
              rcheck <- append(rcheck,pair2) #updating vector
            }
            else{
             break()
            }
          }
          b2 = b2+1 #incrementing b2 to check the next combination
        }
      }
    }
  }
  #***************************************************************************
  #if you would like to see the final dataframe and graph please uncomment the below 2 two comments.
 
  cartGrapg <- graph_from_data_frame(graphedges,directed = FALSE)
  #print(graphedges)
  #plot.igraph(cartGrapg)
  return(cartGrapg)
  
}