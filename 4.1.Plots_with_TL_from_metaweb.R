################################################################################
################################################################################
#                  SCRIPT 4.1 - PLOTS WITH TL FROM METAWEB
################################################################################
################################################################################

#FMestre
#11-10-2023

#Load packages
library(ggplot2)
library(igraph)
library(cheddar)

grids_grilo_shape <- terra::vect("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\data_artigo_clara_grilo\\Nvulnerablegrid50_wgs84_2.shp")

#load("C:\\Users\\asus\\Documents\\github\\road_ecoloy_econetworks\\local_fw_MAIORANO_with_metaweb_TL_10OUT23.RData")
#load("C:\\Users\\asus\\Documents\\github\\road_ecoloy_econetworks\\local_fw_MAIORANO_REMOVED_PRIMARY_EX_with_metaweb_TL_11OUT23.RData")
#load("C:\\Users\\asus\\Documents\\github\\road_ecoloy_econetworks\\local_fw_MAIORANO_REMOVED_CASCADING_EFF_with_metaweb_TL_11OUT23.RData")

#Data
head(local_fw_MAIORANO[[1]]$nodes) # original FW
head(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[1]]$nodes) # primary extinctions FW
head(local_fw_MAIORANO_REMOVED[[1]]$nodes) # cascading effects FW
#
#save(new_properties_ORIGINAL, file = "new_properties_ORIGINAL.RData")
#save(new_properties_PRIMARY, file = "new_properties_PRIMARY.RData")
#save(new_properties_SECONDARY, file = "new_properties_SECONDARY.RData")
#
#overall_previous_positions #categorical classification of the TL
#head(metaweb_TL) #TLevel
#head(tax_table_3)

new_properties_ORIGINAL <- vector(mode='list', length=length(local_fw_MAIORANO))
new_properties_PRIMARY <- vector(mode='list', length=length(local_fw_MAIORANO_REMOVED_PRIMARY_EX))
new_properties_SECONDARY <- vector(mode='list', length=length(local_fw_MAIORANO_REMOVED))

for(i in 1:length(local_fw_MAIORANO)){

net0 <- local_fw_MAIORANO[[i]]
net1 <- local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]]
net2 <- local_fw_MAIORANO_REMOVED[[i]]

tl_class0 <- overall_previous_positions[overall_previous_positions$species %in% net0$nodes$node,]
tl_class1 <- overall_previous_positions[overall_previous_positions$species %in% net1$nodes$node,]
tl_class2 <- overall_previous_positions[overall_previous_positions$species %in% net2$nodes$node,]

props0 <- merge(x=net0$nodes, y=tl_class0, by.x="node", by.y="species", all=TRUE)
props1 <- merge(x=net1$nodes, y=tl_class1, by.x="node", by.y="species", all=TRUE)
props2 <- merge(x=net2$nodes, y=tl_class2, by.x="node", by.y="species", all=TRUE)

new_properties_ORIGINAL[[i]] <- props0
new_properties_PRIMARY[[i]] <- props1
new_properties_SECONDARY[[i]] <- props2

message(i)
}

names(new_properties_ORIGINAL) <- names(local_fw_MAIORANO)
names(new_properties_PRIMARY) <- names(local_fw_MAIORANO_REMOVED_PRIMARY_EX)
names(new_properties_SECONDARY) <- names(local_fw_MAIORANO_REMOVED)
#
plot(local_fw_MAIORANO[[3]]) # original FW
plot(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[3]]) # primary extinctions FW
plot(local_fw_MAIORANO_REMOVED[[3]]) # cascading effects FW
#
View(local_fw_MAIORANO[[3]]$nodes) # original FW
View(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[3]]$nodes) # primary extinctions FW
View(local_fw_MAIORANO_REMOVED[[3]]$nodes) # cascading effects FW
#
View(new_properties_ORIGINAL[[1]])
View(new_properties_PRIMARY[[1]])
View(new_properties_SECONDARY[[1]])


################################################################################
# FIGURE 1 - MAP OF NR OF NODES, NR OF INTERACTIONS, ROAD DENSITY, LOST INTERACTIONS (SECONDARY)
################################################################################

############################## MAP OF NR OF NODES ##############################

#Having
#local_fw_MAIORANO #the initial FW structures
#new_properties_ORIGINAL#with the TL T-I-B classification

nr_species_per_grid_per_tl_METAWEB <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 7))
names(nr_species_per_grid_per_tl_METAWEB) <- c("grid", "total_richness","top", "intermediate", "basal", "connected", "not_connected")
head(nr_species_per_grid_per_tl_METAWEB)

for(i in 1:nrow(nr_species_per_grid_per_tl_METAWEB)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_species_per_grid_per_tl_METAWEB[i,1] <- names(new_properties_ORIGINAL)[[i]] #Name
    nr_species_per_grid_per_tl_METAWEB[i,2] <- nrow(new_properties_ORIGINAL[[i]]) #Total Richness
    levels1 <- data.frame(table(new_properties_ORIGINAL[[i]]$position))
    #
    if(any(levels1 == "top")) nr_species_per_grid_per_tl_METAWEB[i,3] <-  levels1[levels1$Var1 == "top",2]#Nr of top level species
    if(any(levels1 == "intermediate")) nr_species_per_grid_per_tl_METAWEB[i,4] <-  levels1[levels1$Var1 == "intermediate",2]#Nr of mid level species
    if(any(levels1 == "basal")) nr_species_per_grid_per_tl_METAWEB[i,5] <-  levels1[levels1$Var1 == "basal",2]#Nr of basal level species
    nr_species_per_grid_per_tl_METAWEB[i,6] <- length(cheddar::ConnectedNodes(local_fw_MAIORANO[[i]])) #Connected nodes
    nr_species_per_grid_per_tl_METAWEB[i,7] <- nrow(local_fw_MAIORANO[[i]]$nodes) - length(cheddar::ConnectedNodes(local_fw_MAIORANO[[i]])) #Non connected nodes
    
  }
  message(i)
}

#View(nr_species_per_grid_per_tl_METAWEB)

#saving as shapefile
sp_richness_per_trophic_level_METAWEB <- merge(x=grids_grilo_shape, y=nr_species_per_grid_per_tl_METAWEB, by.x="PageNumber", by.y="grid")
#terra::writeVector(sp_richness_per_trophic_level_METAWEB, "sp_richness_per_trophic_level_METAWEB_12OUT23.shp")

############################## NR OF INTERACTIONS ##############################



################################ ROAD DENSITY ##################################

#Get road value on grids
grids_grilo_shape <- terra::vect("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\data_artigo_clara_grilo\\Nvulnerablegrid50_wgs84_2.shp")
grids_grilo <- data.frame(grids_grilo_shape$PageName, grids_grilo_shape$kmkm2)

#relating it with the number of top predators
head(grids_grilo)
sp_rich_TIB <- data.frame(sp_richness_per_trophic_level_METAWEB)
head(sp_rich_TIB)

names(grids_grilo)
names(sp_rich_TIB)

density_top_predators <- merge(x = sp_rich_TIB,
      y = grids_grilo,
      by.x = "PageName",
      by.y = "grids_grilo_shape.PageName")

rm(density_top_predators)

plot(sp_rich_TIB$kmkm2, sp_rich_TIB$top)


p_top_density <- ggplot(sp_rich_TIB, aes(top, kmkm2))
p_top_density + geom_point()

######################## LOST INTERACTIONS (PRIMARY) ###########################

nr_lost_interactions_prim <- data.frame(matrix(nrow=length(local_fw_MAIORANO_REMOVED_PRIMARY_EX), ncol = 2))
names(nr_lost_interactions_prim) <- c("grid","lost_interactions")
#head(nr_lost_interactions_prim)

for(i in 1:nrow(nr_lost_interactions_prim)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_lost_interactions_prim[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    if(!is.null(nrow(local_fw_MAIORANO[[i]]$trophic.links))) nr_lost_interactions_prim[i,2] <- nrow(local_fw_MAIORANO[[i]]$trophic.links) - nrow(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]]$trophic.links) else nr_lost_interactions_prim[i,2]<-0
  }
  message(i) 
}

#names(grids_grilo_shape)
#names(nr_lost_interactions_prim)

lost_interactions_with_primary_extinctions <- merge(x=template_grilo, y=nr_lost_interactions_prim, by.x="PageNumber", by.y="grid")
#terra::writeVector(lost_interactions_with_primary_extinctions, "lost_interactions_with_primary_extinctions_12OUT23_version_2.shp", overwrite=TRUE)

####################### LOST INTERACTIONS (SECONDARY) ##########################

nr_lost_interactions_sec <- data.frame(matrix(nrow=length(local_fw_MAIORANO_REMOVED), ncol = 2))
names(nr_lost_interactions_sec) <- c("grid","lost_interactions")

for(i in 1:nrow(nr_lost_interactions_sec)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_lost_interactions_sec[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    if(!is.null(nrow(local_fw_MAIORANO[[i]]$trophic.links))) nr_lost_interactions_sec[i,2] <- nrow(local_fw_MAIORANO[[i]]$trophic.links) - nrow(local_fw_MAIORANO_REMOVED[[i]]$trophic.links) else nr_lost_interactions_prim[i,2] <- 0
  }
  
}

#head(nr_lost_interactions_sec)
#names(grids_grilo_shape)
#names(nr_lost_interactions_sec)

lost_interactions_with_secondary_extinctions <- merge(x=grids_grilo_shape, y=nr_lost_interactions_sec, by.x="PageNumber", by.y="grid")
#terra::writeVector(lost_interactions_with_secondary_extinctions, "lost_interactions_with_secondary_extinctions_12OUT23.shp")

################################################################################
#     FIGURE 2 - VULNERABILITY, PRIMARY AND SECONDARY EXTINCTIONS PER TL
################################################################################

################################# VULNERABILITY ################################

TL_VULN <- merge(x = tax_table_3,
                 y = metaweb_TL,
                 by.x = "species_maiorano",
                 by.y = "species",
                 all=FALSE
)

names(TL_VULN)

hist(TL_VULN$TL)

ggplot(TL_VULN, aes(x=TL, y=grilo_threshold)) + geom_point()

################### TL OF PRIMARY AND SECONDARY EXTINCTIONS ####################

proportion_previous_level_METAWEB <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 10, nrow = length(local_fw_MAIORANO)))
names(proportion_previous_level_METAWEB) <- c("grid", 
                                      "ORIG_PRI_TL_remaining_sp", 
                                      "ORIG_PRI_TL_extinct_sp", 
                                      "PRI_SEC__TL_remaining_sp",
                                      "PRI_SEC_extinct_sp",
                                      "proportion_top_removed_orig_prim",
                                      "proportion_intermediate_removed_orig_prim",
                                      "proportion_basal_removed_orig_prim",
                                      "proportion_top_removed_prim_sec",
                                      "proportion_intermediate_removed_prim_sec",
                                      "proportion_basal_removed_prim_sec"
                                      )
#head(proportion_previous_level_METAWEB)

#LOOP
for(i in 1:length(local_fw_MAIORANO)){
  
  before_net <- local_fw_MAIORANO[[i]]
  prim_net <- local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]]
  sec_net <- local_fw_MAIORANO_REMOVED[[i]]
  
  before_net_props <- new_properties_ORIGINAL[[i]]
  prim_net_props <- new_properties_PRIMARY[[i]]
  sec_net_props <- new_properties_SECONDARY[[i]]
  
  top_species_nt <- subset(before_net_props, position == "top")
  top_species_nt <- top_species_nt$node
  
  mid_species_nt <- subset(before_net_props, position == "intermediate")
  mid_species_nt <- mid_species_nt$node  
  
  basal_species_nt <- subset(before_net_props, position == "basal")
  basal_species_nt <- basal_species_nt$node
  
  #PRIMARY
  if(unique(!is.na(before_net)) && unique(!is.na(prim_net))) {
    
    removed_species_or_prim <- before_net$nodes$node[!(before_net$nodes$node %in% prim_net$nodes$node)]
    
    if(length(removed_species_or_prim)!=0){
      
      top_removed <- sum(removed_species_or_prim %in% top_species_nt)
      intermediate_removed <- sum(removed_species_or_prim %in% mid_species_nt)
      basal_removed <- sum(removed_species_or_prim %in% basal_species_nt)
      
      prop_top <- top_removed/length(removed_species_or_prim)
      prop_interm <- intermediate_removed/length(removed_species_or_prim)
      prop_basal <- basal_removed/length(removed_species_or_prim)
      
      av_removed <- mean(before_net$nodes[before_net$nodes$node %in% removed_species_or_prim,]$TL)
      av_remaining <- mean(before_net$nodes[!(before_net$nodes$node %in% removed_species_or_prim),]$TL)
      
      #Original to primary
      proportion_previous_level_METAWEB$ORIG_PRI_TL_remaining_sp[i] <- av_remaining
      proportion_previous_level_METAWEB$ORIG_PRI_TL_extinct_sp[i] <- av_removed
      proportion_previous_level_METAWEB$proportion_top_removed_orig_prim[i] <- prop_top
      proportion_previous_level_METAWEB$proportion_intermediate_removed_orig_prim[i] <- prop_interm
      proportion_previous_level_METAWEB$proportion_basal_removed_orig_prim[i] <- prop_basal

    } 
    
  }
  
  #SECONDARY
  if(unique(!is.na(prim_net)) && unique(!is.na(sec_net))) {
    
    removed_species_prim_sec <- prim_net$nodes$node[!(prim_net$nodes$node %in% sec_net$nodes$node)]
    
    if(length(removed_species_prim_sec)!=0){
      
      top_removed2 <- sum(removed_species_prim_sec %in% top_species_nt)
      intermediate_removed2 <- sum(removed_species_prim_sec %in% mid_species_nt)
      basal_removed2 <- sum(removed_species_prim_sec %in% basal_species_nt)
      
      prop_top2 <- top_removed2/length(removed_species_prim_sec)
      prop_interm2 <- intermediate_removed2/length(removed_species_prim_sec)
      prop_basal2 <- basal_removed2/length(removed_species_prim_sec)
      
      av_removed2 <- mean(prim_net$nodes[prim_net$nodes$node %in% removed_species_prim_sec,]$TL)
      av_remaining2 <- mean(prim_net$nodes[!(prim_net$nodes$node %in% removed_species_prim_sec),]$TL)
      
      #Original to primary
      proportion_previous_level_METAWEB$PRI_SEC__TL_remaining_sp[i] <- av_remaining2
      proportion_previous_level_METAWEB$PRI_SEC_extinct_sp[i] <- av_removed2
      proportion_previous_level_METAWEB$proportion_top_removed_prim_sec[i] <- prop_top2
      proportion_previous_level_METAWEB$proportion_intermediate_removed_prim_sec[i] <- prop_interm2
      proportion_previous_level_METAWEB$proportion_basal_removed_prim_sec[i] <- prop_basal2
 
    }
    
  }
  
  message(i)
  
}

View(proportion_previous_level_METAWEB)


#PROPORTION OF SPECIES REMOVED PER TROPHIC LEVEL IN EACH EXTINCTION

  #Primaary
top_orig_prim_metaweb <- data.frame(proportion_previous_level_METAWEB$proportion_top_removed_orig_prim, "top")
mid_orig_prim_metaweb <- data.frame(proportion_previous_level_METAWEB$proportion_intermediate_removed_orig_prim, "intermediate")
basal_orig_prim_metaweb <- data.frame(proportion_previous_level_METAWEB$proportion_basal_removed_orig_prim, "basal")

names(top_orig_prim_metaweb) <- c("proportion", "level")
names(mid_orig_prim_metaweb) <- c("proportion", "level")
names(basal_orig_prim_metaweb) <- c("proportion", "level")

removed_position_orig_prim_metaweb <- rbind(top_orig_prim_metaweb, mid_orig_prim_metaweb, basal_orig_prim_metaweb)

removed_position_orig_prim_metaweb$level <- as.factor(removed_position_orig_prim_metaweb$level)

#Reorder factor levels
removed_position_orig_prim_metaweb$level <- factor(removed_position_orig_prim_metaweb$level,     
                                           c("top", "intermediate", "basal"))

rem_orig_prim_metaweb <- ggplot(removed_position_orig_prim_metaweb, aes(x = level, y = proportion))

rem_orig_prim2_metaweb <- rem_orig_prim_metaweb + geom_violin(aes(fill = level),) +
  ylab("proportion of removed species") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))

rem_orig_prim2_metaweb + labs(fill = "trophic level")


#TROPHIC HEIGHT OF REMAINING AND REMOVED SPECIES


################################################################################
#                             FIGURE 3 - EFFECT SIZE
################################################################################

################################################################################
#               FIGURE 4 - TL OF REMAINING AND LOST SPECIES
################################################################################





