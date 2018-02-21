library(dplyr)
require(shiny)
require(visNetwork)
library(RODBC)
library(class)
library(XLConnect)
library(arules)
library(data.table)
library(igraph)
library(magrittr)
library(visNetwork)

setwd('D:/Gdrive/Bamilo Project Packs/Bamilo Open Project Packs/1_cross_department/2_customer_cube/data/cluster-relationship')

# REQUIRED: FINAL OUTPUT FROM APRIORI_ANALYSIS AND CLUSTER_NAME_MAP FROM THE INTERPRETATION OF K_MEANS_ANALYSIS RESULTS

edges <- read.csv('final_output.csv')
nodes <- read.csv('cluster_name_map.csv')

names(edges)[1] <- 'cluster_id'
edges <- merge(edges, nodes, by = 'cluster_id', all=T)
names(edges)[1] <- 'origin_cluster'
names(edges)[5] <- 'cluster_name_o'
names(edges)[6] <- 'retention_o'
edges <- edges[1:6]

names(edges)[2] <- 'cluster_id'
edges <- merge(edges, nodes, by = 'cluster_id', all=T)
names(edges)[1] <- 'destination_cluster'
names(edges)[7] <- 'cluster_name_d'
names(edges)[8] <- 'retention_d'
edges <- edges[1:8]

edges$retention_value_o <- case_when(
  edges$retention_o == 'high_retention' ~ 6,
  edges$retention_o == 'many_orders' ~ 5,
  edges$retention_o == 'engaged' ~ 4,
  edges$retention_o == 'returning' ~ 3,
  edges$retention_o == 'low_retention' ~ 2,
  edges$retention_o == 'very_low_retention' ~ 1
)

edges$retention_value_d <- case_when(
  edges$retention_d == 'high_retention' ~ 6,
  edges$retention_d == 'many_orders' ~ 5,
  edges$retention_d == 'engaged' ~ 4,
  edges$retention_d == 'returning' ~ 3,
  edges$retention_d == 'low_retention' ~ 2,
  edges$retention_d == 'very_low_retention' ~ 1
)

edges$color <- case_when(
  edges$retention_value_d-edges$retention_value_o <0 ~ 'red',
  edges$retention_value_d-edges$retention_value_o >0 ~ 'green',
  edges$retention_value_d-edges$retention_value_o == 0 ~ 'grey'
)

nodes$node_id <- paste(nodes$cluster_id, nodes$cluster_name, sep = '_')
edges$origin <- paste(edges$origin_cluster, edges$cluster_name_o, sep = '_')
edges$destination <- paste(edges$destination_cluster, edges$cluster_name_d, sep = '_')

ledges <- data.frame(color = c("green", "red"),
                     label = c("Good move", "Bad move"), length = c(250,250))

    
    edges <- edges[edges[,3] >= min_proba,]
    
    f_node_1 <- data.frame(edges$origin_cluster)
    f_node_2 <- data.frame(edges$destination_cluster)
    names(f_node_1)[1] <- 'cluster_id'
    names(f_node_2)[1] <- 'cluster_id'
    f_node_3 <- rbind(f_node_1,f_node_2)
    cluster_id <- unique(f_node_3[is.na(f_node_3[,1]) == F ,])
    f_node_3 <- data.frame(cluster_id)
    nodes <- merge(f_node_3, nodes, all.x=T)
    
    nodes <- data.frame(id = nodes$node_id, label ='',
                        group = nodes$retention, value =nodes$size_sqrt_0.55 ,
                        title =paste0('<h6>',nodes$node_id, '<br>Number of customers: ', nodes$size))
    edges <- data.frame(from = edges$origin ,
                        to = edges$destination,
                        value = edges$avg_dest_proba,
                        title = paste0('<h6>Probability: ',as.integer(edges$avg_dest_proba*100),'%','<br>','From ',edges$origin , '<br>',' To ',edges$destination)
                        ,color = edges$color)
    
    visNetwork(nodes, edges, main = '<h4>Probability that customers will move to another cluster within 3 months</h4><h5>1 dot = 1 cluster | big arrow = high-probability | big bubble = many customers</h5>', height = "100%", width = "100%") %>% 
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = F),
                 nodesIdSelection = list(enabled = TRUE,
                                         style = 'width: 80%; height: 26px;'),
                 selectedBy = list(variable = 'group',
                                   style = 'width: 40%; height: 26px;'),
                 height = '500px', width = '100%') %>%
      visEdges(arrows = "to") %>%
      visLegend(addEdges = ledges, stepY = 40 , width = '0.3') %>%
      visGroups(groupname = "high_retention", color = list(background = "yellow",border = "orange", highlight = 'yellow')) %>% 
      visGroups(groupname = "many_orders", color = list(background = "lightgreen",border = "green", highlight ='lightgreen')) %>%
      visGroups(groupname = "engaged", color  = list(background = "lightblue",border = "blue", highlight = 'lightblue')) %>%
      visGroups(groupname = "returning", color = list(background = "lightgrey",border = "black", highlight = 'lightgrey')) %>%
      visGroups(groupname = "low_retention", color  = list(background = "#D8BFD8",border = "purple", highlight = '#D8BFD8')) %>%
      visGroups(groupname = "very_low_retention", color  = list(background = "#F9CCCA",border = "darkred", highlight ='#F9CCCA')) %>%
      visLayout(randomSeed = 1234)
