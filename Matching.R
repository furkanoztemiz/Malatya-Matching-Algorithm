

library(igraph)
library(threejs)
library(visNetwork)
#g <- sample_bipartite(n1 = 5,n2 = 8,p = 1)

#g<- make_lattice(length = 5,dim = 3)




#################  3*3 grid graph and line'ı

g <- graph(edges <- c(1 , 2, 1 , 4, 2 , 3, 2 , 5, 3 , 6, 4 , 5, 4 , 7, 5 , 6, 5 ,
                      8, 6 , 9, 7 , 8, 8 , 9),directed = FALSE )

g <- graph(edges <- c("1" , "2", "1" ,"3", "1" , "4", "2" , "6", "2" , "7", "3" , "4", "3" , "5", "4" , "6", "4" , 
                      "8", "4" , "9", "5" , "8", "5" , "10", "6" , "7", "6" , "8", "6" , "9", "7" , "11", "8" , "9", 
                      "8" , "10", "9" , "11", "9" , "12", "10" , "12", "11" , "12"),directed = FALSE )


#### Q(3) Hypergraph - hem lattice hem bipartite 
g <- graph(edges <- c("1" , "2", "1" , "3", "1", "4", "1" , "5", "2" , "3","2" , "6", "2" , "7", "3" ,"9", "3" , 
                      "10", "4" , "5", "4" , "6", "4" , "8", "5" , "9", "5" , "11", "6" , "7", "6" , "8", "7" , "10",
                      "7" , "12", "8" , "11", "8" , "12", "9" , "10", "9" , "11", "10" , "12", "11" , "12"),directed = FALSE )


g <- graph(edges <- c(1 , 2, 1 , 3, 1 , 5, 2 , 4, 2 , 6, 3 , 4, 3 , 7, 4 , 8, 5 ,6, 5 , 7, 6, 8, 7 , 8),directed = FALSE )


g<- line.graph(g)
vector = c()
MalatyaCentrality <- function(g){
  vertexList <- c(V(g))
  for (i in vertexList) { 
    Vdegree <-degree(g,v = V(g)[i])
    KomsuDegree <- degree(g,v = neighbors(g,v = V(g)[i]))
    Value <- Vdegree/KomsuDegree
    vector <- c(vector, sum(Value))
  }
  return(vector)
}
######## Find Minimum Vertex
FindMinimum <- function(graph){
  data <- data.frame(MalatyaCentrality(graph))
  return (order(data$MalatyaCentrality.graph.,decreasing = FALSE)[1])
}

################### Maksimum Matching ####

MatchingSet = c()
while(vcount(g) > 0 ){
  silinecekNodes =c()
  komsu <- NULL 
  MatchingSet <- append(MatchingSet,V(g)[FindMinimum(g)])
  minNode <- V(g)[FindMinimum(g)]
  komsu <- neighbors(g,v = minNode)
  silinecekNodes <- append(silinecekNodes,minNode)
  silinecekNodes <- append(silinecekNodes,komsu)
  g <- delete_vertices(g,silinecekNodes )
}
matchingNumber <- length(MatchingSet)
matchingNumber
## E(g)[MatchingSet]





#V(g)$name <- V(g)
## görselleştirme
graph2ddata <- as_edgelist(g,names = TRUE)
id <-  V(g)$name
nodes <-  data.frame(id,label = paste("Düğüm", V(g)$name),font.size = c(40))
edges <- data.frame(from = graph2ddata[,1], to = graph2ddata[,2])
#nodes$color.background <- c("slategrey", "tomato", "gold")
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"
nodes$shape <- "dot"  
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$title <-  V(g)$name
nodes$label <- V(g)$name # Node label
nodes$size <- 15 #MalatyaCentrality(g)*2  
nodes$borderWidth <- 2 # Node border width
edges$label <- E(g)$weight 
edges$width <- E(g)$weight*8
visNetwork(nodes, edges,main = "Graf Yapısı", submain = "Dominating Set Değerleri") %>%
  visInteraction(hideEdgesOnDrag = FALSE,multiselect = TRUE,hover = TRUE,hoverConnectedEdges = TRUE,selectable = TRUE) %>%
  visIgraphLayout() %>% #fizik motorunu kapatıyor
  visNodes(color = "lightblue",shadow = TRUE)%>%
  visEdges(color=list(color="grey", highlight = "orange")) %>%    ## arrows = "to"
  visOptions(highlightNearest = TRUE, nodesIdSelection = list(enabled = TRUE,
                                                              values = V(g)$name,
                                                              style = 'width: 200px; height: 26px;
                                 background: #f8f8f8;
                                 color: darkgreen;
                                 border:none;
                                 outline:none;'),autoResize = TRUE)


