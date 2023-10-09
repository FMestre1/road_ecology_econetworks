################################################################################
################################################################################
#                   SCRIPT 3 - SIMULATING CASCADING EFFECTS
################################################################################
################################################################################

#FMestre
#28-09-2023

#Load packages
library(cheddar)
library(igraph)
library(NetIndices)
library(terra)
library(taxize)

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

################################################################################
# 1. Simulate Cascading effects of primary extinctions         
################################################################################

#Get road value on grids
grids_grilo_shape <- terra::vect("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\data_artigo_clara_grilo\\Nvulnerablegrid50_wgs84_2.shp")
grids_grilo <- data.frame(grids_grilo_shape$PageName, grids_grilo_shape$kmkm2)
#head(grids_grilo)

all_species_vulnerability_2 <- all_species_vulnerability_1[,1:2]
#head(all_species_vulnerability_1)
#head(all_species_vulnerability_2)

#To save the networks after primary extinctions
local_fw_MAIORANO_REMOVED <- vector(mode = "list", length = length(local_fw_MAIORANO))
names(local_fw_MAIORANO_REMOVED) <- names(local_fw_MAIORANO)

#To save results...
species_loss <- rep(NA, length(local_fw_MAIORANO_REMOVED))
connectance_dif <- rep(NA, length(local_fw_MAIORANO_REMOVED))
compart_dif <- rep(NA, length(local_fw_MAIORANO_REMOVED))

#Pair pagenumber with pagename
pair_pagenumber_pagename <- terra::vect("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\data_artigo_clara_grilo\\Nvulnerablegrid50_wgs84_2.shp")
pair_pagenumber_pagename <- pair_pagenumber_pagename[,1:2]
pair_pagenumber_pagename <- as.data.frame(pair_pagenumber_pagename)

#Simulate cascading effects of extinctions
for(i in 1:length(local_fw_MAIORANO_REMOVED)){
  
  cheddar1 <- local_fw_MAIORANO[[i]]
  fw_pagenumber <- as.numeric(names(local_fw_MAIORANO)[i])
  fw_pagename <- pair_pagenumber_pagename[as.numeric(pair_pagenumber_pagename$PageNumber) == fw_pagenumber, ][,1]

  if(any(!is.na(cheddar1))){
    
    grid_road_density <- grids_grilo[grids_grilo$grids_grilo_shape.PageName == fw_pagename, ]$grids_grilo_shape.kmkm2
    removed_species <- cheddar1$nodes[cheddar1$nodes$Median_MAXroad.RM.1000. <= grid_road_density,]$node #Species to remove
    new_title <- paste0("Removed Species ", cheddar1$properties$title, "_", fw_pagename)
    
    if(length(removed_species)!=0) cheddar2 <- RemoveNodes(cheddar1, remove = removed_species, title = new_title, method = 'secondary')
    if(length(removed_species)==0) cheddar2 <- cheddar1
    
    local_fw_MAIORANO_REMOVED[[i]] <- cheddar2 #Adding to new list
    
    if(cheddar::NumberOfTrophicLinks(cheddar1)!=0){
      
      igraph1 <- ToIgraph(cheddar1)
      
      test.graph.adj1 <- get.adjacency(igraph1, sparse = TRUE)
      
      metrics1 <- GenInd(as.matrix(test.graph.adj1))
      
    }
    
    if(cheddar::NumberOfTrophicLinks(cheddar2)!=0){
      
      igraph2 <- ToIgraph(cheddar2)
      
      test.graph.adj2 <- get.adjacency(igraph2, sparse = TRUE)
      
      metrics2 <- GenInd(as.matrix(test.graph.adj2))
    
    }
    
    n_species_0 <- nrow(cheddar1$nodes)
    
    n_species_1 <- nrow(cheddar2$nodes)
    
    species_loss[i] <- n_species_1/n_species_0
    
    if(cheddar::NumberOfTrophicLinks(cheddar1)!=0 && cheddar::NumberOfTrophicLinks(cheddar2)!=0) {
      
      connectance_dif[i] <-  metrics2$C - metrics1$C
      
      compart_dif[i] <- metrics2$Cbar - metrics1$Cbar
      
    } 
    
  } 
  
  if(any(is.na(cheddar1))) {
    
    local_fw_MAIORANO_REMOVED[[i]] <- NA 
    species_loss[i] <- NA
    connectance_dif[i] <- NA
    compart_dif[i] <- NA
    
  }
  
  message(i)
  
}

#save(local_fw_MAIORANO_REMOVED, file = "local_fw_MAIORANO_REMOVED_CASCADING_EFF_28set2023.RData")
#load("local_fw_MAIORANO_REMOVED_CASCADING_EFF_28set2023.RData")

#Check results
species_loss
connectance_dif
compart_dif


#Create data frame
result_sec_ext <- data.frame(
  names(local_fw_MAIORANO),
  species_loss,
  connectance_dif,
  compart_dif
)

names(result_sec_ext)[1] <- "grid"
#head(result_sec_ext)

result_sec_ext_pair_pagenumber_pagename <- merge(x = pair_pagenumber_pagename,
                                                 y = result_sec_ext,
                                                 by.x = "PageNumber",
                                                 by.y = "grid")

#View(result_sec_ext_pair_pagenumber_pagename)
names(result_sec_ext_pair_pagenumber_pagename)

grids_grilo_shape_species_loss <- merge(x=grids_grilo_shape, y=result_sec_ext_pair_pagenumber_pagename, by.x="PageName", by.y= "PageName")
#terra::writeVector(grids_grilo_shape_species_loss, "pre_after_road_06out23.shp")

################################################################################
# 2. How many interactions lost? - with primary extinctions
################################################################################

nr_lost_interactions_sec <- data.frame(matrix(nrow=length(local_fw_MAIORANO_REMOVED), ncol = 2))
names(nr_lost_interactions_sec) <- c("grid","lost_interactions")
#head(nr_lost_interactions_prim)

for(i in 1:nrow(nr_lost_interactions_sec)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_lost_interactions_sec[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    if(!is.null(nrow(local_fw_MAIORANO[[i]]$trophic.links))) nr_lost_interactions_sec[i,2] <- nrow(local_fw_MAIORANO[[i]]$trophic.links) - nrow(local_fw_MAIORANO_REMOVED[[i]]$trophic.links) else nr_lost_interactions_prim[i,2] <- 0
  }
  
}

names(grids_grilo_shape)
names(nr_lost_interactions_sec)

lost_interactions_with_secondary_extinctions <- merge(x=grids_grilo_shape, y=nr_lost_interactions_sec, by.x="PageNumber", by.y="grid")
#terra::writeVector(lost_interactions_with_secondary_extinctions, "lost_interactions_with_secondary_extinctions_06OUT23.shp")
