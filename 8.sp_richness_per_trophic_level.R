################################################################################
################################################################################
#                 SCRIPT 8 - SPECIES RICHNNESS PER TROPHIC LEVEL 
################################################################################
################################################################################

#FMestre
#30-09-2023

#Loading packages
library(cheddar)
library(terra)
library(dplyr)
library(tidyverse)

#Having
local_fw_MAIORANO #the initial FW structures

nr_species_per_grid_per_tl <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 7))
names(nr_species_per_grid_per_tl) <- c("grid", "total_richness","top", "intermediate", "basal", "connected", "not_connected")
head(nr_species_per_grid_per_tl)

for(i in 1:nrow(nr_species_per_grid_per_tl)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_species_per_grid_per_tl[i,1] <- local_fw_MAIORANO[[i]]$properties$title #Name
    nr_species_per_grid_per_tl[i,2] <- nrow(local_fw_MAIORANO[[i]]$nodes) #Total Richness
    nr_species_per_grid_per_tl[i,3] <- length(cheddar::TopLevelNodes(local_fw_MAIORANO[[i]])) #Nr of top level species
    nr_species_per_grid_per_tl[i,4] <- length(cheddar::IntermediateNodes(local_fw_MAIORANO[[i]])) #Nr of mid level species
    nr_species_per_grid_per_tl[i,5] <- length(cheddar::BasalNodes(local_fw_MAIORANO[[i]])) #Nr of basal level species
    nr_species_per_grid_per_tl[i,6] <- length(cheddar::ConnectedNodes(local_fw_MAIORANO[[i]])) #Connected nodes
    nr_species_per_grid_per_tl[i,7] <- nrow(local_fw_MAIORANO[[i]]$nodes) - length(cheddar::ConnectedNodes(local_fw_MAIORANO[[i]])) #Non connected nodes
    
      }
  message(i)
}

#View(nr_species_per_grid_per_tl)

#saving as shapefile
sp_richness_per_trophic_level <- merge(x=grids_grilo_shape, y=nr_species_per_grid_per_tl, by.x="PageNumber", by.y="grid")
#terra::writeVector(sp_richness_per_trophic_level, "sp_richness_per_trophic_level_11OUT23.shp")
#terra::plet(sp_richness_per_trophic_level, "top")
#terra::plet(sp_richness_per_trophic_level, "intermediate")
#terra::plet(sp_richness_per_trophic_level, "basal")

################################################################################
#             1. Creating map o relative loss of interactions 
################################################################################

local_fw_MAIORANO #the initial FW structures
local_fw_MAIORANO_REMOVED_PRIMARY_EX #priamary extinctions
local_fw_MAIORANO_REMOVED #cascading effect

#Primary
nr_lost_interactions_prim_RELATIVE <- data.frame(matrix(nrow=length(local_fw_MAIORANO_REMOVED_PRIMARY_EX), ncol = 2))
names(nr_lost_interactions_prim_RELATIVE) <- c("grid","lost_interactions_relative")
#head(nr_lost_interactions_prim_RELATIVE)

for(i in 1:nrow(nr_lost_interactions_prim_RELATIVE)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_lost_interactions_prim_RELATIVE[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    if(!is.null(nrow(local_fw_MAIORANO[[i]]$trophic.links))) {
      initial_int <- nrow(local_fw_MAIORANO[[i]]$trophic.links)
      final_int <- nrow(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]]$trophic.links)
      
      nr_lost_interactions_prim_RELATIVE[i,2] <- ((initial_int - final_int) * 100)/initial_int
        
    } else nr_lost_interactions_prim_RELATIVE[i,2]<- 0
}
  
}

lost_interactions_with_primary_extinctions_RELATIVE <- merge(x=grids_grilo_shape, y=nr_lost_interactions_prim_RELATIVE, by.x="PageNumber", by.y="grid")
#terra::writeVector(lost_interactions_with_primary_extinctions_RELATIVE, "lost_interactions_with_primary_extinctions_RELATIVE_11OUT23.shp")
#terra::plet(lost_interactions_with_primary_extinctions_RELATIVE, "lost_interactions_relative")


#Cascading
nr_lost_interactions_cascading_RELATIVE <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 2))
names(nr_lost_interactions_cascading_RELATIVE) <- c("grid","lost_interactions_relative")
#head(nr_lost_interactions_cascading_RELATIVE)

for(i in 1:nrow(nr_lost_interactions_cascading_RELATIVE)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_lost_interactions_cascading_RELATIVE[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    if(!is.null(nrow(local_fw_MAIORANO[[i]]$trophic.links)))
      {
      
    initial_int2 <- nrow(local_fw_MAIORANO[[i]]$trophic.links)
    final_int2 <- nrow(local_fw_MAIORANO_REMOVED[[i]]$trophic.links)
      
    nr_lost_interactions_cascading_RELATIVE[i,2] <- ((initial_int2 - final_int2) * 100)/initial_int2
      
      } else nr_lost_interactions_cascading_RELATIVE[i,2] <- 0
  }
  
}

lost_interactions_with_sec_extinctions_RELATIVE <- merge(x=grids_grilo_shape, y=nr_lost_interactions_cascading_RELATIVE, by.x="PageNumber", by.y="grid")
#terra::writeVector(lost_interactions_with_sec_extinctions_RELATIVE, "lost_interactions_with_sec_extinctions_RELATIVE_11OUT23.shp")
#terra::plet(lost_interactions_with_sec_extinctions_RELATIVE, "lost_interactions_relative")



# Use dplyr to calculate the mean of 'Value' by 'Group'
result <- tl_positions %>%
  group_by(group) %>%
  summarize(Mean_Value = mean(tl, na.rm = TRUE))

# Print the result
print(result)

