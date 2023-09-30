################################################################################
################################################################################
#                             SCRIPT 4 - PLOTTING
################################################################################
################################################################################

#FMestre
#28-09-2023

#Load packages
library(ggplot2)

################################################################################
# 1. Boxplots of the fraction of top, intermediate, and basal nodes
################################################################################

#Proportion of top, intermediate and basal nodes before and after
fractions_top_intermediate_basal_nodes <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 7, nrow = length(local_fw_MAIORANO)))
names(fractions_top_intermediate_basal_nodes) <- c("grid", "BEFORE_top_level", "BEFORE_Interm_level", "BEFORE_basal_level", 
                                                   "AFTER__top_level", "AFTER_Interm_level", "AFTER_basal_level", "changed?") 

#LOOP
for(i in 1:length(local_fw_MAIORANO)){
  
  before_net <- local_fw_MAIORANO[[i]]#before disturbance
  after_net <- local_fw_MAIORANO_REMOVED[[i]]#after cascading effects
  #
  
  if(unique(!is.na(before_net))) fractions_top_intermediate_basal_nodes$BEFORE_top_level[i] <- cheddar::FractionTopLevelNodes(before_net)
  if(unique(!is.na(before_net))) fractions_top_intermediate_basal_nodes$BEFORE_Interm_level[i] <- cheddar::FractionIntermediateNodes(before_net)
  if(unique(!is.na(before_net))) fractions_top_intermediate_basal_nodes$BEFORE_basal_level[i] <- cheddar::FractionBasalNodes(before_net)
  #
  if(unique(!is.na(after_net))) fractions_top_intermediate_basal_nodes$AFTER__top_level[i] <- cheddar::FractionTopLevelNodes(after_net)
  if(unique(!is.na(after_net))) fractions_top_intermediate_basal_nodes$AFTER_Interm_level[i] <- cheddar::FractionIntermediateNodes(after_net)
  if(unique(!is.na(after_net)))fractions_top_intermediate_basal_nodes$AFTER_basal_level[i] <- cheddar::FractionBasalNodes(after_net)
  #
  if(unique(!is.na(before_net)) && unique(!is.na(after_net))){
    if(cheddar::NumberOfNodes(before_net) == cheddar::NumberOfNodes(after_net)) {
      
      fractions_top_intermediate_basal_nodes$`changed?`[i] <- "no"
      
    } else fractions_top_intermediate_basal_nodes$`changed?`[i] <- "yes"
  }
  
  message(i)
  
}
#head(fractions_top_intermediate_basal_nodes)
#View(fractions_top_intermediate_basal_nodes)

#Load & Save
#save(fractions_top_intermediate_basal_nodes, file = "fractions_top_intermediate_basal_nodes_30set23.RData")
#load("fractions_top_intermediate_basal_nodes_30set23.RData")


before_nt <- fractions_top_intermediate_basal_nodes[,1:4]
after_nt <- fractions_top_intermediate_basal_nodes[,c(1,5,6,7)]
names(before_nt)[-1] <- c("top", "mid", "basal")
names(after_nt)[-1] <- c("top", "mid", "basal")
before_nt <- data.frame(before_nt, "before")
after_nt <- data.frame(after_nt, "after")
names(before_nt)[5] <- "before_after"
names(after_nt)[5] <- "before_after"
#head(before_nt)
#head(after_nt)

net_changes <- rbind(before_nt, after_nt)
net_changes$before_after <- as.factor(net_changes$before_after)

#Reorder factor levels
net_changes$before_after <- factor(net_changes$before_after,     
                                   c("before", "after"))
### Boxplot ###

#TOP
top_box <- ggplot(net_changes, aes(x = before_after, y = top))

top_box2 <- top_box + geom_boxplot(aes(fill = before_after),) +
  scale_fill_manual(values = c("#999999", "#E69F00"))

top_box2

#MID
#TOP
mid <- ggplot(net_changes, aes(x = before_after, y = mid))

mid2 <- mid + geom_boxplot(aes(fill = before_after),) +
  scale_fill_manual(values = c("#999999", "#E69F00"))

mid2

#BASAL
#TOP
basal <- ggplot(net_changes, aes(x = before_after, y = basal))

basal2 <- basal + geom_boxplot(aes(fill = before_after),) +
  scale_fill_manual(values = c("#999999", "#E69F00"))

basal2

################################################################################
# 2. In which trophic level are the removed species? 
################################################################################

extinctions_levels <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 6, nrow = length(local_fw_MAIORANO)))
names(extinctions_levels) <- c("grid", 
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
  
  if(unique(!is.na(before_net)) && unique(!is.na(prim_net)) && unique(!is.na(sec_net))) {
    
    removed_species_or_prim <- before_net$nodes$node[!(before_net$nodes$node %in% prim_net$nodes$node)]
    removed_species_prim_sec <- prim_net$nodes$node[!(prim_net$nodes$node %in% sec_net$nodes$node)]
    
    if(length(removed_species_or_prim)!=0){
      
      #Original networks
      tp_0 <- length(cheddar::TopLevelNodes(before_net))
      it_0 <- length(cheddar::IntermediateNodes(before_net))
      bs_0 <- length(cheddar::BasalNodes(before_net))
      #Primary extinctions
      tp_1 <- length(cheddar::TopLevelNodes(prim_net))
      it_1 <- length(cheddar::IntermediateNodes(prim_net))
      bs_1 <- length(cheddar::BasalNodes(prim_net))
      
      #Original to primary
      extinctions_levels$ORIG_PRI_top_level[i] <- 1-(tp_1/tp_0)
      extinctions_levels$ORIG_PRI_interm_level[i] <- 1-(it_1/it_0)
      extinctions_levels$ORIG_PRI_basal_level[i] <- 1-(bs_1/bs_0)
      
    } 
    
    if(length(removed_species_prim_sec)!=0){
      
      #Primary extinctions
      tp_1 <- length(cheddar::TopLevelNodes(prim_net))
      it_1 <- length(cheddar::IntermediateNodes(prim_net))
      bs_1 <- length(cheddar::BasalNodes(prim_net))
      #Cascading effects
      tp_2 <- length(cheddar::TopLevelNodes(sec_net))
      it_2 <- length(cheddar::IntermediateNodes(sec_net))
      bs_2 <- length(cheddar::BasalNodes(sec_net))
      
      #Primary to cascading
      extinctions_levels$PRI_SEC_top_level[i] <- 1-(tp_2/tp_1)
      extinctions_levels$PRI_SEC_interm_level[i] <- 1-(it_2/it_1)
      extinctions_levels$PRI_SEC_basal_level[i] <- 1-(bs_2/bs_1)
      
    } 
    
  }
  
  message(i)
  
}
#View(extinctions_levels)

#Save & Load
#save(extinctions_levels, file = "extinctions_levels_30SET23.RData")
#load("extinctions_levels_30SET23.RData")

# Plot it...

#1. From original to primary extinctions
top_orig_prim <- data.frame(extinctions_levels$ORIG_PRI_top_level, "top")
mid_orig_prim <- data.frame(extinctions_levels$ORIG_PRI_interm_level, "intermediate")
basal_orig_prim <- data.frame(extinctions_levels$ORIG_PRI_basal_level, "basal")

names(top_orig_prim) <- c("rate", "level")
names(mid_orig_prim) <- c("rate", "level")
names(basal_orig_prim) <- c("rate", "level")

removed_position_orig_prim <- rbind(top_orig_prim, mid_orig_prim, basal_orig_prim)

removed_position_orig_prim$level <- as.factor(removed_position_orig_prim$level)

#Reorder factor levels
removed_position_orig_prim$level <- factor(removed_position_orig_prim$level,     
                                           c("top", "intermediate", "basal"))

rem_orig_prim <- ggplot(removed_position_orig_prim, aes(x = level, y = rate))

rem_orig_prim2 <- rem_orig_prim + geom_violin(aes(fill = level),) +
  ylab("rate of change") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))

rem_orig_prim2 + labs(fill = "trophic level")
#YES

#save(removed_position_orig_prim, file = "removed_position_orig_prim_fig2b_30SET23.RData")

# Compute the analysis of variance
names(removed_position_orig_prim)

orig_prim_aov <- aov(rate ~ level, data = removed_position_orig_prim)
# Summary of the analysis
summary(orig_prim_aov)

#2. From primary extinctions to cascading effects
top_prim_sec <- data.frame(extinctions_levels$PRI_SEC_top_level, "top")
mid_prim_sec <- data.frame(extinctions_levels$PRI_SEC_interm_level, "intermediate")
basal_prim_sec <- data.frame(extinctions_levels$PRI_SEC_basal_level, "basal")

names(top_prim_sec) <- c("rate", "level")
names(mid_prim_sec) <- c("rate", "level")
names(basal_prim_sec) <- c("rate", "level")

removed_position_prim_sec <- rbind(top_prim_sec, mid_prim_sec, basal_prim_sec)

removed_position_prim_sec$level <- as.factor(removed_position_prim_sec$level)

#Reorder factor levels
removed_position_prim_sec$level <- factor(removed_position_prim_sec$level,     
                                          c("top", "intermediate", "basal"))

rem_prim_sec <- ggplot(removed_position_prim_sec, aes(x = level, y = rate))

rem_prim_sec2 <- rem_prim_sec + geom_violin(aes(fill = level),) +
  ylab("rate of change") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))

rem_prim_sec2 + labs(fill = "trophic level") 
#YES

#save(removed_position_prim_sec, file = "removed_position_prim_sec_fig2c_30SET23.RData")

# Compute the analysis of variance
names(removed_position_prim_sec)

prim_sec_aov <- aov(rate ~ level, data = removed_position_prim_sec)
# Summary of the analysis
summary(prim_sec_aov)

#####################################################################################
# 3. In each transition, how many species occupied a given position in the previous FW?
#####################################################################################

proportion_previous_level <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 6, nrow = length(local_fw_MAIORANO)))
names(proportion_previous_level) <- c("grid", 
                                      "ORIG_PRI_top_level", 
                                      "ORIG_PRI_interm_level", 
                                      "ORIG_PRI_basal_level", 
                                      "PRI_SEC_top_level",
                                      "PRI_SEC_interm_level",
                                      "PRI_SEC_basal_level"
)

#which(names(local_fw_MAIORANO) == "BW39")
#i = 3461

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

#save(proportion_previous_level, file = "proportion_previous_level_30SET23.RData")
#load("proportion_previous_level_30SET23.RData")

# Plot it...

#1. From original to primary extinctions
top_orig_prim_v2 <- data.frame(proportion_previous_level$ORIG_PRI_top_level, "top")
mid_orig_prim_v2 <- data.frame(proportion_previous_level$ORIG_PRI_interm_level, "intermediate")
basal_orig_prim_v2 <- data.frame(proportion_previous_level$ORIG_PRI_basal_level, "basal")

names(top_orig_prim_v2) <- c("rate", "level")
names(mid_orig_prim_v2) <- c("rate", "level")
names(basal_orig_prim_v2) <- c("rate", "level")

removed_position_orig_prim_v2 <- rbind(top_orig_prim_v2, mid_orig_prim_v2, basal_orig_prim_v2)

removed_position_orig_prim_v2$level <- as.factor(removed_position_orig_prim_v2$level)

#Reorder factor levels
removed_position_orig_prim_v2$level <- factor(removed_position_orig_prim_v2$level,     
                                              c("top", "intermediate", "basal"))

rem_orig_prim_v2 <- ggplot(removed_position_orig_prim_v2, aes(x = level, y = rate))

rem_orig_prim2_v2 <- rem_orig_prim_v2 + geom_violin(aes(fill = level),) +
  ylab("previous trophic level") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))


rem_orig_prim2_v2 + labs(fill = "trophic level")
#NOT

#2. From primary extinctions to cascading effects
top_prim_sec_v2 <- data.frame(proportion_previous_level$PRI_SEC_top_level, "top")
mid_prim_sec_v2 <- data.frame(proportion_previous_level$PRI_SEC_interm_level, "intermediate")
basal_prim_sec_v2 <- data.frame(proportion_previous_level$PRI_SEC_basal_level, "basal")

names(top_prim_sec_v2) <- c("rate", "level")
names(mid_prim_sec_v2) <- c("rate", "level")
names(basal_prim_sec_v2) <- c("rate", "level")

removed_position_prim_sec_v2 <- rbind(top_prim_sec_v2, mid_prim_sec_v2, basal_prim_sec_v2)

removed_position_prim_sec_v2$level <- as.factor(removed_position_prim_sec_v2$level)

#Reorder factor levels
removed_position_prim_sec_v2$level <- factor(removed_position_prim_sec_v2$level,     
                                             c("top", "intermediate", "basal"))

rem_prim_sec_v2 <- ggplot(removed_position_prim_sec_v2, aes(x = level, y = rate))

rem_prim_sec2_v2 <- rem_prim_sec_v2 + geom_violin(aes(fill = level),) +
  ylab("previous trophic level") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))

rem_prim_sec2_v2
#NOT

################################################################################
# 4. Species in each grid
################################################################################

#grids_grilo_shape
#local_fw_MAIORANO

nr_species_per_grid <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 2))
names(nr_species_per_grid) <- c("grid","sp_richness")
head(nr_species_per_grid)

for(i in 1:nrow(nr_species_per_grid)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_species_per_grid[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    nr_species_per_grid[i,2] <- nrow(local_fw_MAIORANO[[i]]$nodes)
  }
  
}

names(grids_grilo_shape)
names(nr_species_per_grid)

sp_richness <- merge(x=grids_grilo_shape, y=nr_species_per_grid, by.x="PageNumber", by.y="grid")
#terra::writeVector(sp_richness, "sp_richness_30SET23.shp")
#terra::plet(sp_richness, "sp_richness")

################################################################################
# 5. Coonectance
################################################################################

#grids_grilo_shape
#local_fw_MAIORANO

connectance_original_networks <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 2))
names(connectance_original_networks) <- c("grid","connectance")
#head(connectance_original_networks)

for(i in 1:nrow(connectance_original_networks)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    connectance_original_networks[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    connectance_original_networks[i,2] <- DirectedConnectance(local_fw_MAIORANO[[i]])
  }
  
}

connectance_original <- merge(x=grids_grilo_shape, y=connectance_original_networks, by.x="PageNumber", by.y="grid")
#terra::writeVector(connectance_original, "connectance_30set23.shp")
#terra::plet(connectance_original, "connectance")

################################################################################
# 6. How many interactions lost? - with secondary extinctions
################################################################################

nr_lost_interactions_cascading_RELATIVE <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 2))
names(nr_lost_interactions_cascading_RELATIVE) <- c("grid","lost_interactions")

#local_fw_MAIORANO[[100]]
#local_fw_MAIORANO_REMOVED[[100]]

for(i in 1:nrow(nr_lost_interactions_cascading_RELATIVE)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_lost_interactions_cascading_RELATIVE[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    if(!is.null(nrow(local_fw_MAIORANO[[i]]$trophic.links))) nr_lost_interactions_cascading_RELATIVE[i,2] <- nrow(local_fw_MAIORANO[[i]]$trophic.links) - nrow(local_fw_MAIORANO_REMOVED[[i]]$trophic.links) else nr_lost_interactions_cascading_RELATIVE[i,2] <- 0
  }
  
}

lost_interactions_with_sec_extinctions <- merge(x=grids_grilo_shape, y=nr_lost_interactions_cascading_RELATIVE, by.x="PageNumber", by.y="grid")
#terra::writeVector(lost_interactions_with_sec_extinctions, "lost_interactions_with_sec_extinctions_30SET23.shp")
#terra::plet(lost_interactions_with_sec_extinctions, "lost_interactions")
