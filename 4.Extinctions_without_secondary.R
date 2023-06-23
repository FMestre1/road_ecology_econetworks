################################################################################
# Extinctions without secondary extinctions
################################################################################
#23-06-2023

library(terra)
library(stringr)
library(taxize)
library(cheddar)

#local_fw_MAIORANO # original local networks
#local_fw_MAIORANO_REMOVED[[2]]

local_fw_MAIORANO_REMOVED_PRIMARY_EX <- vector(mode = "list", length = length(local_fw_MAIORANO))
names(local_fw_MAIORANO_REMOVED_PRIMARY_EX) <- names(local_fw_MAIORANO)

species_loss_prim_ext <- rep(NA, length(local_fw_MAIORANO_REMOVED_PRIMARY_EX))
connectance_dif_prim_ext <- rep(NA, length(local_fw_MAIORANO_REMOVED_PRIMARY_EX))
compart_dif_prim_ext <- rep(NA, length(local_fw_MAIORANO_REMOVED_PRIMARY_EX))

#LOOP
for(i in 1:length(local_fw_MAIORANO_REMOVED_PRIMARY_EX)){
  
  cheddar1_pex <- local_fw_MAIORANO[[i]]
  
  if(any(!is.na(cheddar1_pex))){
    
    grid_road_density <- grids_grilo[grids_grilo$grids_grilo.PageName == cheddar1_pex$properties$title, ]$grids_grilo.kmkm2
    
    removed_species <- cheddar1_pex$nodes[cheddar1_pex$nodes$Median_MAXroad.RM.1000.<=grid_road_density,]$node #Species to remove
    
    new_title <- paste0("Removed Species ", cheddar1_pex$properties$title)
    
    if(length(removed_species)!=0) cheddar2_pex <- RemoveNodes(cheddar1_pex, remove = removed_species, title = new_title, method = 'direct')
    if(length(removed_species)==0) cheddar2_pex <- cheddar1_pex
    
    local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]] <- cheddar2_pex #Adding to new list
    
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
    
    species_loss_prim_ext[i] <- n_species_1/n_species_0
    
    if(cheddar::NumberOfTrophicLinks(cheddar1_pex)!=0 && cheddar::NumberOfTrophicLinks(cheddar2_pex)!=0) {
      
      connectance_dif_prim_ext[i] <-  metrics2$C - metrics1$C
      
      compart_dif_prim_ext[i] <- metrics2$Cbar - metrics1$Cbar
      
    } 
    
  } 
  
  if(any(is.na(cheddar1_pex))) {
    
    local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]] <- NA 
    species_loss_prim_ext[i] <- NA
    connectance_dif_prim_ext[i] <- NA
    compart_dif_prim_ext[i] <- NA
    
  }
  
  message(i)
  
}