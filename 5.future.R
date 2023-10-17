################################################################################
################################################################################
#                  SCRIPT 5. - FUTURE SIMULATIONS
################################################################################
################################################################################

#FMestre
#16-10-2023

#Load packages
library(igraph)
library(cheddar)
library(terra)
library(NetIndices)

#Load current road density
grids_grilo_shape <- terra::vect("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\data_artigo_clara_grilo\\Nvulnerablegrid50_wgs84_2.shp")

#a 10% increase
kmkm2_10 <- (grids_grilo_shape$kmkm2*0.1) + grids_grilo_shape$kmkm2

grids_grilo_shape_2 <- grids_grilo_shape
grids_grilo_shape_2 <- terra::cbind2(grids_grilo_shape, data.frame(kmkm2_10))
#ncol(grids_grilo_shape_2)
#names(grids_grilo_shape_2)
grids_grilo_shape_2 <- grids_grilo_shape_2[, -c(3:14)]

terra::writeVector(grids_grilo_shape_2, "grids_grilo_shape_2.shp")

################################################################################
#                  SIMULATIONS FUTURE - PRIMARY EXTINCTIONS
################################################################################

local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE <- vector(mode = "list", length = length(local_fw_MAIORANO))
names(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE) <- names(local_fw_MAIORANO)

species_loss_prim_ext_FUTURE <- rep(NA, length(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE))
connectance_dif_prim_ext_FUTURE <- rep(NA, length(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE))
compart_dif_prim_ext_FUTURE <- rep(NA, length(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE))

#Simulate primary extinctions
for(i in 1:length(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE)){
  
  cheddar1_pex <- local_fw_MAIORANO[[i]]
  fw_pagenumber <- as.numeric(names(local_fw_MAIORANO)[i])
  fw_pagename <- paired_pagename_pagenumber[paired_pagename_pagenumber$PageNumber == fw_pagenumber,][,1]
  
  if(any(!is.na(cheddar1_pex))){
    
    grid_road_density <- grids_grilo_shape_2[grids_grilo_shape_2$PageName == fw_pagename, ]$kmkm2_10
    removed_species <- cheddar1_pex$nodes[cheddar1_pex$nodes$grilo_threshold <= grid_road_density,]$node #Species to remove
    new_title <- paste0("FUTURE Removed Species ", cheddar1_pex$properties$title, "_", fw_pagename)
    
    if(length(removed_species)!=0) cheddar2_pex <- RemoveNodes(cheddar1_pex, remove = removed_species, title = new_title, method = 'direct')
    if(length(removed_species)==0) cheddar2_pex <- cheddar1_pex
    
    local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE[[i]] <- cheddar2_pex #Adding to new list
    
    if(cheddar::NumberOfTrophicLinks(cheddar1_pex)!=0){
      
      igraph1 <- ToIgraph(cheddar1_pex)
      
      test.graph.adj1 <- get.adjacency(igraph1, sparse = TRUE)
      
      metrics1 <- GenInd(as.matrix(test.graph.adj1))
      
    }
    
    if(cheddar::NumberOfTrophicLinks(cheddar2_pex)!=0){
      
      igraph2 <- ToIgraph(cheddar2_pex)
      
      test.graph.adj2 <- get.adjacency(igraph2, sparse = TRUE)
      
      metrics2 <- GenInd(as.matrix(test.graph.adj2))
      
      
    }
    
    n_species_0 <- nrow(cheddar1_pex$nodes)
    
    n_species_1 <- nrow(cheddar2_pex$nodes)
    
    species_loss_prim_ext_FUTURE[i] <- 1-(n_species_1/n_species_0)
    
    if(cheddar::NumberOfTrophicLinks(cheddar1_pex)!=0 && cheddar::NumberOfTrophicLinks(cheddar2_pex)!=0) {
      
      connectance_dif_prim_ext_FUTURE[i] <-  metrics2$C - metrics1$C
      
      compart_dif_prim_ext_FUTURE[i] <- metrics2$Cbar - metrics1$Cbar
      
    } 
    
  } 
  
  if(any(is.na(cheddar1_pex))) {
    
    local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE[[i]] <- NA 
    species_loss_prim_ext_FUTURE[i] <- NA
    connectance_dif_prim_ext_FUTURE[i] <- NA
    compart_dif_prim_ext_FUTURE[i] <- NA
    
  }
  
  message(i)
  
}

#Save
#save(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE, file = "local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE_16OUT23.RData")

################################################################################
#                  SIMULATIONS FUTURE - SECONDARY EXTINCTIONS
################################################################################

local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE <- vector(mode = "list", length = length(local_fw_MAIORANO))
names(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE) <- names(local_fw_MAIORANO)

species_loss_sec_ext_FUTURE <- rep(NA, length(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE))
connectance_dif_sec_ext_FUTURE <- rep(NA, length(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE))
compart_dif_sec_ext_FUTURE <- rep(NA, length(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE))

#Simulate primary extinctions
for(i in 1:length(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE)){
  
  cheddar1_sec_fut <- local_fw_MAIORANO[[i]]
  fw_pagenumber <- as.numeric(names(local_fw_MAIORANO)[i])
  fw_pagename <- paired_pagename_pagenumber[paired_pagename_pagenumber$PageNumber == fw_pagenumber,][,1]
  
  if(any(!is.na(cheddar1_sec_fut))){
    
    grid_road_density <- grids_grilo_shape_2[grids_grilo_shape_2$PageName == fw_pagename, ]$kmkm2_10
    removed_species <- cheddar1_sec_fut$nodes[cheddar1_sec_fut$nodes$grilo_threshold <= grid_road_density,]$node #Species to remove
    new_title <- paste0("FUTURE Removed Species ", cheddar1_sec_fut$properties$title, "_", fw_pagename)
    
    if(length(removed_species)!=0) cheddar2_sec <- RemoveNodes(cheddar1_sec_fut, remove = removed_species, title = new_title, method = 'secondary')
    if(length(removed_species)==0) cheddar2_sec <- cheddar1_sec_fut
    
    local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE[[i]] <- cheddar2_sec #Adding to new list
    
    if(cheddar::NumberOfTrophicLinks(cheddar1_sec_fut)!=0){
      
      igraph1 <- ToIgraph(cheddar1_sec_fut)
      
      test.graph.adj1 <- get.adjacency(igraph1, sparse = TRUE)
      
      metrics1 <- GenInd(as.matrix(test.graph.adj1))
      
    }
    
    if(cheddar::NumberOfTrophicLinks(cheddar2_sec)!=0){
      
      igraph2 <- ToIgraph(cheddar2_sec)
      
      test.graph.adj2 <- get.adjacency(igraph2, sparse = TRUE)
      
      metrics2 <- GenInd(as.matrix(test.graph.adj2))
      
      
    }
    
    n_species_0 <- nrow(cheddar1_sec_fut$nodes)
    
    n_species_1 <- nrow(cheddar2_sec$nodes)
    
    species_loss_sec_ext_FUTURE[i] <- 1-(n_species_1/n_species_0)
    
    if(cheddar::NumberOfTrophicLinks(cheddar1_sec_fut)!=0 && cheddar::NumberOfTrophicLinks(cheddar2_sec)!=0) {
      
      connectance_dif_sec_ext_FUTURE[i] <-  metrics2$C - metrics1$C
      
      compart_dif_sec_ext_FUTURE[i] <- metrics2$Cbar - metrics1$Cbar
      
    } 
    
  } 
  
  if(any(is.na(cheddar1_sec_fut))) {
    
    local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE[[i]] <- NA 
    species_loss_sec_ext_FUTURE[i] <- NA
    connectance_dif_sec_ext_FUTURE[i] <- NA
    compart_dif_sec_ext_FUTURE[i] <- NA
    
  }
  
  message(i)
  
}

#Save
#save(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE, file = "local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE_16OUT23.RData")


