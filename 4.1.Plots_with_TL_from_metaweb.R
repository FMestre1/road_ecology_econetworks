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

############################## NR OF INTERACTIONS ##############################

################################ ROAD DENSITY ##################################

####################### LOST INTERACTIONS (SECONDARY) ##########################



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

########################## TL OF PRIMARY EXTINCTIONS ###########################

proportion_previous_level_METAWEB <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 6, nrow = length(local_fw_MAIORANO)))
names(proportion_previous_level) <- c("grid", 
                                      "ORIG_PRI_top_level", 
                                      "ORIG_PRI_interm_level", 
                                      "ORIG_PRI_basal_level", 
                                      "PRI_SEC_top_level",
                                      "PRI_SEC_interm_level",
                                      "PRI_SEC_basal_level"
)


#LOOP
for(i in 1:length(local_fw_MAIORANO_REMOVED)){
  
  before_net <- local_fw_MAIORANO[[i]]
  prim_net <- local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]]
  sec_net <- local_fw_MAIORANO_REMOVED[[i]]
  
  if(unique(!is.na(before_net)) && unique(!is.na(prim_net))) {
    
    removed_species_or_prim <- before_net$nodes$node[!(before_net$nodes$node %in% prim_net$nodes$node)]
    
    if(length(removed_species_or_prim)!=0){
      
      top_removed <- sum(removed_species_or_prim %in% cheddar::TopLevelNodes(before_net))
      intermediate_removed <- sum(removed_species_or_prim %in% cheddar::IntermediateNodes(before_net))
      basal_removed <- sum(removed_species_or_prim %in% cheddar::BasalNodes(before_net))
      
      prop_top <- top_removed/length(removed_species_or_prim)
      prop_interm <- intermediate_removed/length(removed_species_or_prim)
      prop_basal <- basal_removed/length(removed_species_or_prim)
      
      #Original to primary
      proportion_previous_level$ORIG_PRI_top_level[i] <- prop_top
      proportion_previous_level$ORIG_PRI_interm_level[i] <- prop_interm
      proportion_previous_level$ORIG_PRI_basal_level[i] <- prop_basal
      
    } 
    
  }
  
  if(unique(!is.na(prim_net)) && unique(!is.na(sec_net))) {
    
    removed_species_prim_sec <- prim_net$nodes$node[!(prim_net$nodes$node %in% sec_net$nodes$node)]
    
    if(length(removed_species_prim_sec)!=0){
      
      top_removed2 <- sum(removed_species_prim_sec %in% cheddar::TopLevelNodes(before_net))
      intermediate_removed2 <- sum(removed_species_prim_sec %in% cheddar::IntermediateNodes(before_net))
      basal_removed2 <- sum(removed_species_prim_sec %in% cheddar::BasalNodes(before_net))
      
      prop_top2 <- top_removed2/length(removed_species_prim_sec)
      prop_interm2 <- intermediate_removed2/length(removed_species_prim_sec)
      prop_basal2 <- basal_removed2/length(removed_species_prim_sec)
      
      #Primary to cascading
      proportion_previous_level$PRI_SEC_top_level[i] <- prop_top2
      proportion_previous_level$PRI_SEC_interm_level[i] <- prop_interm2
      proportion_previous_level$PRI_SEC_basal_level[i] <- prop_basal2
      
    } 
    
  }
  
  message(i)
  
}
#View(proportion_previous_level)

########################## TL OF SECONDARY EXTINCTIONS #########################


################################################################################
#                             FIGURE 3 - EFFECT SIZE
################################################################################



################################################################################
#               FIGURE 4 - TL OF REMAINING AND LOST SPECIES
################################################################################





