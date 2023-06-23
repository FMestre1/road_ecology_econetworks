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

#local_fw_MAIORANO[[100]]
#local_fw_MAIORANO_REMOVED[[100]]
#local_fw_MAIORANO_REMOVED_PRIMARY_EX[[100]]

result_prim_ext <- data.frame(
  names(local_fw_MAIORANO_REMOVED_PRIMARY_EX),
  species_loss_prim_ext,
  connectance_dif_prim_ext,
  compart_dif_prim_ext
)

names(result_prim_ext)[1] <- "grid"

grids_grilo_shape_species_loss_prim_ext <- merge(x=grids_grilo_shape, y=result_prim_ext, by.x="PageName", by.y= "grid")
#terra::writeVector(grids_grilo_shape_species_loss_prim_ext, "pre_after_road_prim_ext.shp")


################################################################################
# How many interactions lost? - with primary extinctions
################################################################################
#23-06-2023

nr_lost_interactions_prim <- data.frame(matrix(nrow=length(local_fw_MAIORANO_REMOVED_PRIMARY_EX), ncol = 2))
names(nr_lost_interactions_prim) <- c("grid","lost_interactions")
head(nr_lost_interactions_prim)

for(i in 1:nrow(nr_lost_interactions)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_lost_interactions_prim[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    if(!is.null(nrow(local_fw_MAIORANO[[i]]$trophic.links))) nr_lost_interactions_prim[i,2] <- nrow(local_fw_MAIORANO[[i]]$trophic.links) - nrow(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]]$trophic.links) else nr_lost_interactions_prim[i,2]<-0
  }
  
}


lost_interactions_with_primary_extinctions <- merge(x=grids_grilo_shape, y=nr_lost_interactions_prim, by.x="PageName", by.y="grid")
#terra::writeVector(lost_interactions_with_primary_extinctions, "lost_interactions_with_primary_extinctions.shp")



