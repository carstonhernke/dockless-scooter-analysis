library(readr)
library(igraph)
library(dplyr)

# import trip data from csv
trip_data <- read_csv("enriched_scooter_trips.csv")

# remove rows that contain an NA
trip_data <- trip_data[complete.cases(trip_data),]

# create an edge list from the Start_TAZ and End_TAZ columns
edge_list <- trip_data[,c('Start_TAZ','End_TAZ')]

# create an igraph from the edge list
trip_graph <- graph_from_data_frame(edge_list, directed = TRUE)
trip_graph_undirected  <- graph_from_data_frame(edge_list, directed = FALSE)

# add weights
E(trip_graph)$weight <- 1
trip_graph <- simplify(trip_graph, edge.attr.comb=list(weight="sum"))

V(trip_graph)
E(trip_graph)

louvain <- cluster_louvain(trip_graph_undirected, weights = NULL)

memberships <- membership(louvain)
network.result <- as.data.frame(cbind(names(memberships), membership(louvain)))
names(network.result) <- c("name", "group_assignment")
network.result

# import TAZ data (for visualization)
library(sf)
taz_data <- st_read("data/taz_shapefiles/TAZOfficialWCurrentForecasts.shp")
taz_geo <- taz_data[,c('TAZ','geometry')]
taz_geo$TAZ <- factor(taz_geo$TAZ)
taz_geo <- left_join(taz_geo, network.result, by = c("TAZ" = "name"))
taz_geo <- taz_geo[!is.na(taz_geo[,2]),] # remove TAZ with no trips

plot(taz_geo)

library(data.table)
taz2 <- taz_geo

taz2$Hub_Score <- hub_score(trip_graph, weights = NA)$vector
taz2$Betweenness <- betweenness(trip_graph, directed = 'T', weights = NA)
taz2$Closeness <- closeness(trip_graph, mode = 'all', weights = NA)

taz2 <- st_sf(taz2)
plot(taz2)

# https://www.r-bloggers.com/three-ways-of-visualizing-a-graph-on-a-map/

# weights
adj_with_weights <- as_adj(trip_graph,attr = 'weight')

write.csv(edge_list,"edge_list.csv")
