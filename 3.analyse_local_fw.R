#FMestre
#12-06-2023

library(cheddar)
library(bipartite)
library(igraph)
library(NetIndices)
library(terra)

getwd()

#required function
#Required function
ToIgraph <- function(community, weight=NULL)
{
  if(is.null(TLPS(community)))
  {
    stop('The community has no trophic links')
  }
  else
  {
    tlps <- TLPS(community, link.properties=weight)
    if(!is.null(weight))
    {
      tlps$weight <- tlps[,weight]
    }
    return (graph.data.frame(tlps,
                             vertices=NPS(community),
                             directed=TRUE))
  }
}

########################################################################################
# 1. Iberian Peninsula Example             
########################################################################################

#Get grids
ib_grids <- read.csv("grids_ib_pen.csv", header = TRUE)
ib_grids <- ib_grids$PageName.C.254 

#Get road value on grids
grids_grilo <- terra::vect("shapes/Nvulnerablegrid50_wgs84_2.shp")
grids_grilo <- data.frame(grids_grilo$PageName, grids_grilo$kmkm2)
#head(grids_grilo)

all_species_vulnerability_2 <- all_species_vulnerability_1[,1:2]
#head(all_species_vulnerability_1)
#head(all_species_vulnerability_2)
#sort(all_species_vulnerability_2$Median_MAXroad.RM.1000.)

iberian_fw_MAIORANO <- local_fw_MAIORANO[ib_grids]

#length(iberian_fw_MAIORANO)
#length(ib_grids)


#Go into a for loop

cheddar1 <- iberian_fw_MAIORANO[[2]]

igraph1 <- ToIgraph(cheddar1)

grid_road_density <- grids_grilo[grids_grilo$grids_grilo.PageName == cheddar1$properties$title, ]$grids_grilo.kmkm2

removed_species <- cheddar1$nodes[cheddar1$nodes$Median_MAXroad.RM.1000.<=grid_road_density,]$node #Species to remove

removed_species

new_title <- paste0("removed_species_", cheddar1$properties$title)

cheddar2 <- RemoveNodes(cheddar1, remove = removed_species, title = new_title, method='cascade')

head(iberian_fw_MAIORANO[[2]]$nodes)

test.graph.adj1 <- get.adjacency(igraph1, sparse = TRUE)

igraph2 <- ToIgraph(cheddar2)

test.graph.adj2 <- get.adjacency(igraph2, sparse = TRUE)

metrics1 <- GenInd(as.matrix(test.graph.adj1))

metrics2 <- GenInd(as.matrix(test.graph.adj2))

n_species_0 <- nrow(cheddar1$nodes)

n_species_1 <- nrow(cheddar2$nodes)

ratio_species <- n_species_1/n_species_0

d_connectance <-  metrics2$C - metrics1$C

d_compart <- metrics2$Cbar - metrics1$Cbar





