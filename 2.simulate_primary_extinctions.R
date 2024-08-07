################################################################################
################################################################################
#                SCRIPT 2 - SIMULATING PRIMARY EXTINCTIONS
################################################################################
################################################################################

# Road density simplifies regional food webs
# F. Mestre, V.A.G. Bastazini, F. Ascensão

#Load packages
library(terra)
library(stringr)
library(taxize)
library(cheddar)
library(NetIndices)
library(igraph)

#Get road value on grids
grids_grilo_shape <- terra::vect("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\data_artigo_clara_grilo\\Nvulnerablegrid50_wgs84_2.shp")
template_grilo <- terra::vect("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\fernando_26set_2023\\template_grilo.shp")
#load("C:\\Users\\asus\\Documents\\github\\road_ecoloy_econetworks\\local_fw_MAIORANO_with_metaweb_TL_13OUT23.RData")

paired_pagename_pagenumber <- data.frame(grids_grilo_shape)[,1:2]
#head(paired_pagename_pagenumber)

################################################################################
# 1. Extinction simulation
################################################################################

local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS <- vector(mode = "list", length = length(local_fw_MAIORANO))
names(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS) <- names(local_fw_MAIORANO)

species_loss_prim_ext <- rep(NA, length(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS))
connectance_dif_prim_ext <- rep(NA, length(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS))
compart_dif_prim_ext <- rep(NA, length(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS))

#Simulate primary extinctions
for(i in 1:length(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS)){
  
  cheddar1_pex <- local_fw_MAIORANO[[i]]
  fw_pagenumber <- as.numeric(names(local_fw_MAIORANO)[i])
  fw_pagename <- paired_pagename_pagenumber[paired_pagename_pagenumber$PageNumber == fw_pagenumber,][,1]
  
  if(any(!is.na(cheddar1_pex))){
    
    grid_road_density <- grids_grilo[grids_grilo$grids_grilo_shape.PageName == fw_pagename, ]$grids_grilo_shape.kmkm2
    removed_species <- cheddar1_pex$nodes[cheddar1_pex$nodes$grilo_threshold <= grid_road_density,]$node #Species to remove
    new_title <- paste0("Removed Species ", cheddar1_pex$properties$title, "_", fw_pagename)
    
    if(length(removed_species)!=0) cheddar2_pex <- RemoveNodes(cheddar1_pex, remove = removed_species, title = new_title, method = 'direct')
    if(length(removed_species)==0) cheddar2_pex <- cheddar1_pex
    
    local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[i]] <- cheddar2_pex #Adding to new list
    
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
    
    species_loss_prim_ext[i] <- 1-(n_species_1/n_species_0)
    
    if(cheddar::NumberOfTrophicLinks(cheddar1_pex)!=0 && cheddar::NumberOfTrophicLinks(cheddar2_pex)!=0) {
      
      connectance_dif_prim_ext[i] <-  metrics2$C - metrics1$C
      
      compart_dif_prim_ext[i] <- metrics2$Cbar - metrics1$Cbar
      
    } 
    
  } 
  
  if(any(is.na(cheddar1_pex))) {
    
    local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[i]] <- NA 
    species_loss_prim_ext[i] <- NA
    connectance_dif_prim_ext[i] <- NA
    compart_dif_prim_ext[i] <- NA
    
  }
  
  message(i)
  
}

#Save
#save(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS, file = "local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_METAWEB_TL_08NOV23.RData")

result_prim_ext <- data.frame(
  names(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS),
  species_loss_prim_ext,
  connectance_dif_prim_ext,
  compart_dif_prim_ext
)

names(result_prim_ext)[1] <- "grid"
#View(result_prim_ext)
names(grids_grilo_shape)
names(result_prim_ext)

grids_grilo_shape_species_loss_prim_ext <- merge(x=grids_grilo_shape, y=result_prim_ext, by.x="PageNumber", by.y= "grid")
#terra::writeVector(grids_grilo_shape_species_loss_prim_ext, "pre_after_road_prim_ext_08NOV23.shp")

################################################################################
# 2. How many interactions lost? - with primary extinctions
################################################################################

nr_lost_interactions_prim <- data.frame(matrix(nrow=length(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS), ncol = 2))
names(nr_lost_interactions_prim) <- c("grid","lost_interactions")
#head(nr_lost_interactions_prim)

for(i in 1:nrow(nr_lost_interactions_prim)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_lost_interactions_prim[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    if(!is.null(nrow(local_fw_MAIORANO[[i]]$trophic.links))) nr_lost_interactions_prim[i,2] <- nrow(local_fw_MAIORANO[[i]]$trophic.links) - nrow(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[i]]$trophic.links) else nr_lost_interactions_prim[i,2]<-0
  }
message(i) 
}

#names(grids_grilo_shape)
#names(nr_lost_interactions_prim)

lost_interactions_with_primary_extinctions <- merge(x=template_grilo, y=nr_lost_interactions_prim, by.x="PageNumber", by.y="grid")
#terra::writeVector(lost_interactions_with_primary_extinctions, "nr_lost_interactions_with_primary_extinctions_08NOV23.shp", overwrite=TRUE)
