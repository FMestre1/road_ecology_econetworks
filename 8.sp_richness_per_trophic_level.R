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
#terra::writeVector(sp_richness_per_trophic_level, "sp_richness_per_trophic_level_30SET23.shp")
terra::plet(sp_richness_per_trophic_level, "top")
terra::plet(sp_richness_per_trophic_level, "intermediate")
terra::plet(sp_richness_per_trophic_level, "basal")

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
#terra::writeVector(lost_interactions_with_primary_extinctions_RELATIVE, "lost_interactions_with_primary_extinctions_RELATIVE_30_SET23.shp")
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
#terra::writeVector(lost_interactions_with_sec_extinctions_RELATIVE, "lost_interactions_with_sec_extinctions_RELATIVE_30SET23.shp")
#terra::plet(lost_interactions_with_sec_extinctions_RELATIVE, "lost_interactions_relative")

################################################################################
# 2.The trophic level of removed and remaining species in the two extinction steps
################################################################################

previous_tl <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 4, nrow = length(local_fw_MAIORANO)))
names(previous_tl) <- c("grid", 
                        "averaged_tl_original_networks_of_removed_in_prim_ext", 
                        "averaged_tl_original_networks_of_removed_in_sec_ext", 
                        "averaged_tl_original_networks_of_remaining_in_prim_ext", 
                        "averaged_tl_original_networks_of_remaining_in_sec_ext" 
) 

#LOOP
for(i in 1:length(local_fw_MAIORANO)){
  
  if(any(class(local_fw_MAIORANO[[i]]) == "Community")){#is this position a community #START
  before_net <- local_fw_MAIORANO[[i]]
  prim_net <- local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]]
  sec_net <- local_fw_MAIORANO_REMOVED[[i]]
  
  averagedTL_before <- data.frame(ChainAveragedTrophicLevel(before_net, include.isolated=TRUE))
  averagedTL_primary <- data.frame(ChainAveragedTrophicLevel(prim_net, include.isolated=TRUE))
  averagedTL_secondary <- data.frame(ChainAveragedTrophicLevel(sec_net, include.isolated=TRUE))
  
  averagedTL_before <- data.frame(rownames(averagedTL_before),averagedTL_before)
  rownames(averagedTL_before) <- 1:nrow(averagedTL_before)
  names(averagedTL_before) <- c("species", "av_tl")
  #
  averagedTL_primary <- data.frame(rownames(averagedTL_primary),averagedTL_primary)
  rownames(averagedTL_primary) <- 1:nrow(averagedTL_primary)
  names(averagedTL_primary) <- c("species", "av_tl")
  #
  #averagedTL_secondary <- data.frame(rownames(averagedTL_secondary),averagedTL_secondary)
  #rownames(averagedTL_secondary) <- 1:nrow(averagedTL_secondary)
  #names(averagedTL_secondary) <- c("species", "av_tl")
  
  if(unique(!is.na(before_net)) && unique(!is.na(prim_net)) && unique(!is.na(sec_net))) {
    
    removed_species_or_prim <- before_net$nodes$node[!(before_net$nodes$node %in% prim_net$nodes$node)]
    removed_species_prim_sec <- prim_net$nodes$node[!(prim_net$nodes$node %in% sec_net$nodes$node)]
    
    if(length(removed_species_or_prim)!=0){
      
      df_tl_removed_species <- averagedTL_before[averagedTL_before$species %in% removed_species_or_prim,]
      previous_tl[i,2] <- mean(df_tl_removed_species[,2])
      #
      df_tl_remaining_species <- averagedTL_before[!(averagedTL_before$species %in% removed_species_or_prim),]
      previous_tl[i,4] <- mean(df_tl_remaining_species[,2])
      
      
    } 
    
    if(length(removed_species_prim_sec)!=0){
      
      df_tl_removed_species2 <- averagedTL_primary[averagedTL_primary$species %in% removed_species_prim_sec,]
      previous_tl[i,3] <- mean(df_tl_removed_species2[,2])
      #
      df_tl_remaining_species2 <- averagedTL_primary[!(averagedTL_primary$species %in% removed_species_prim_sec),]
      previous_tl[i,5] <- mean(df_tl_remaining_species2[,2])
    } 
    
  }
  }#is this position a community #END
  message(i)
  
}

#View(previous_tl)

#Arrange o plot
removed_p <- data.frame(previous_tl$averaged_tl_original_networks_of_removed_in_prim_ext, rep("removed primary", nrow(previous_tl)))
removed_s <- data.frame(previous_tl$averaged_tl_original_networks_of_removed_in_sec_ext, rep("removed secondary", nrow(previous_tl)))
remaining_p <- data.frame(previous_tl$averaged_tl_original_networks_of_remaining_in_prim_ext, rep("remaining primary", nrow(previous_tl)))
remaining_s <- data.frame(previous_tl$averaged_tl_original_networks_of_remaining_in_sec_ext, rep("remaining secondary", nrow(previous_tl)))
#
names(removed_p) <- c("tl", "group")
names(removed_s) <- c("tl", "group")
names(remaining_p) <- c("tl", "group")
names(remaining_s) <- c("tl", "group")

tl_positions <- rbind(removed_p, removed_s, remaining_p, remaining_s)

tl_positions$group <- as.factor(tl_positions$group)

tl_positions$group

tl_positions$group <- factor(tl_positions$group, 
                             levels = c("remaining primary", 
                                      "removed primary", 
                                      "remaining secondary", 
                                      "removed secondary"
                                      )
                              )

#Plot
tl_previous_nr <- ggplot(tl_positions, aes(x = group, y = tl))

tl_previous_nr2 <- tl_previous_nr + geom_violin(aes(fill = group),) +
  ylab("Trophic position") +
  xlab("Removed and remaining species in each step") +
  scale_fill_manual(values = c("#008B00", "#8B1A1A", "#008B00", "#8B1A1A"))

tl_previous_nr2

# Use dplyr to calculate the mean of 'Value' by 'Group'
result <- tl_positions %>%
  group_by(group) %>%
  summarize(Mean_Value = mean(tl, na.rm = TRUE))

# Print the result
print(result)
