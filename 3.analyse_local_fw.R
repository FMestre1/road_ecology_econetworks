########################################################################################
#                          ROAD ECOLOGY - NETWORK ECOLOGY         
########################################################################################

#FMestre
#12-06-2023

library(cheddar)
library(igraph)
library(NetIndices)
library(terra)
library(taxize)
library(ggplot2)

#required function
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

########################################################################################
# 1. Pre and after Road           
########################################################################################

#Get road value on grids
grids_grilo_shape <- terra::vect("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\data_artigo_clara_grilo\\Nvulnerablegrid50_wgs84_2.shp")
grids_grilo <- data.frame(grids_grilo_shape$PageName, grids_grilo_shape$kmkm2)
#head(grids_grilo)

all_species_vulnerability_2 <- all_species_vulnerability_1[,1:2]
#head(all_species_vulnerability_1)
#head(all_species_vulnerability_2)
#sort(all_species_vulnerability_2$Median_MAXroad.RM.1000.)

local_fw_MAIORANO_REMOVED <- vector(mode = "list", length = length(local_fw_MAIORANO))
names(local_fw_MAIORANO_REMOVED) <- names(local_fw_MAIORANO)

species_loss <- rep(NA, length(local_fw_MAIORANO_REMOVED))
connectance_dif <- rep(NA, length(local_fw_MAIORANO_REMOVED))
compart_dif <- rep(NA, length(local_fw_MAIORANO_REMOVED))

#LOOP
for(i in 1:length(local_fw_MAIORANO_REMOVED)){
  
cheddar1 <- local_fw_MAIORANO[[i]]

if(any(!is.na(cheddar1))){

grid_road_density <- grids_grilo[grids_grilo$grids_grilo_shape.PageName == cheddar1$properties$title, ]$grids_grilo_shape.kmkm2

removed_species <- cheddar1$nodes[cheddar1$nodes$Median_MAXroad.RM.1000.<=grid_road_density,]$node #Species to remove

new_title <- paste0("Removed Species ", cheddar1$properties$title)

if(length(removed_species)!=0) cheddar2 <- RemoveNodes(cheddar1, remove = removed_species, title = new_title, method = 'cascade')
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

#save(local_fw_MAIORANO_REMOVED, file = "local_fw_MAIORANO_REMOVED_6set2023.RData")
#load("local_fw_MAIORANO_REMOVED_6set2023.RData")

#local_fw_MAIORANO_REMOVED
#head(local_fw_MAIORANO_REMOVED)
#local_fw_MAIORANO[[43]]
#local_fw_MAIORANO_REMOVED[[43]]

species_loss
connectance_dif
compart_dif

result_sec_ext <- data.frame(
  names(local_fw_MAIORANO),
  species_loss,
  connectance_dif,
  compart_dif
  )

names(result_sec_ext)[1] <- "grid"
#head(result_sec_ext)
#max(result_sec_ext$species_loss, na.rm=TRUE)
#result_sec_ext
#result_sec_ext

grids_grilo_shape_species_loss <- merge(x=grids_grilo_shape, y=result_sec_ext, by.x="PageName", by.y= "grid")
#terra::writeVector(grids_grilo_shape_species_loss, "pre_after_road.shp")

####

#Proportion of top, intermediate and basal nodes before and after
fractions_top_intermediate_basal_nodes <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 7, nrow = length(local_fw_MAIORANO)))
names(fractions_top_intermediate_basal_nodes) <- c("grid", "BEFORE_top_level", "BEFORE_Interm_level", "BEFORE_basal_level", 
                                                   "AFTER__top_level", "AFTER_Interm_level", "AFTER_basal_level", "changed?") 

#LOOP
for(i in 1:length(local_fw_MAIORANO)){

before_net <- local_fw_MAIORANO[[i]]
after_net <- local_fw_MAIORANO_REMOVED[[i]]
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

head(fractions_top_intermediate_basal_nodes)

#Save
#save(fractions_top_intermediate_basal_nodes, file = "fractions_top_intermediate_basal_nodes.RData")
#load("fractions_top_intermediate_basal_nodes.RData")

################################################################################
# Boxplots of the fraction of top, intermediate, and basal nodes
################################################################################

head(fractions_top_intermediate_basal_nodes)
#View(fractions_top_intermediate_basal_nodes)

head(fractions_top_intermediate_basal_nodes[,1:4])
head(fractions_top_intermediate_basal_nodes[,c(1,5,6,7)])

before_nt <- fractions_top_intermediate_basal_nodes[,1:4]
after_nt <- fractions_top_intermediate_basal_nodes[,c(1,5,6,7)]

names(before_nt)[-1] <- c("top", "mid", "basal")
names(after_nt)[-1] <- c("top", "mid", "basal")

before_nt <- data.frame(before_nt, "before")
after_nt <- data.frame(after_nt, "after")

names(before_nt)[5] <- "before_after"
names(after_nt)[5] <- "before_after"

head(before_nt)
head(after_nt)

net_changes <- rbind(before_nt, after_nt)
net_changes$before_after <- as.factor(net_changes$before_after)

#View(net_changes)

#Reorder factor levels
net_changes$before_after <- factor(net_changes$before_after,     
                                   c("before", "after"))
### Boxplot ###
#names(net_changes)
#str(net_changes)

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
# Where are the removed species, in which trophic level? 
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
#head(extinctions_levels)

#local_fw_MAIORANO[["BW39"]] # original FW
#local_fw_MAIORANO_REMOVED_PRIMARY_EX[["BW39"]] # primary extinctions FW
#local_fw_MAIORANO_REMOVED[["BW39"]] # cascading effects FW

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

#Save
#save(extinctions_levels, file = "extinctions_levels_07_09_2023.RData")
#load("extinctions_levels_07_09_2023.RData")

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

rem_orig_prim2 <- rem_orig_prim + geom_boxplot(aes(fill = level),) +
  ylab("rate of change") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))

rem_orig_prim2

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

rem_prim_sec2 <- rem_prim_sec + geom_boxplot(aes(fill = level),) +
  ylab("rate of change") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))

rem_prim_sec2

#####################################################################################
# In each transition, how many species occupied a given position in the starting FW
#####################################################################################
#07-09-2023

#AQUI

proportion_previous_level <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 6, nrow = length(local_fw_MAIORANO)))
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
  
  if(unique(!is.na(before_net)) && unique(!is.na(prim_net)) && unique(!is.na(sec_net))) {
    
    removed_species_or_prim <- before_net$nodes$node[!(before_net$nodes$node %in% prim_net$nodes$node)]
    removed_species_prim_sec <- prim_net$nodes$node[!(prim_net$nodes$node %in% sec_net$nodes$node)]
    
    if(length(removed_species_or_prim)!=0){
      
      top_removed <- sum(removed_species_or_prim %in% cheddar::TopLevelNodes(before_net))
      intermediate_removed <- sum(removed_species_or_prim %in% cheddar::IntermediateNodes(before_net))
      basal_removed <- sum(removed_species_or_prim %in% cheddar::BasalNodes(before_net))
      
      prop_top <- top_removed/length(removed_species_or_prim)
      prop_interm <- intermediate_removed/length(removed_species_or_prim)
      prop_basal <- basal_removed/length(removed_species_or_prim)
      
      #Original to primary
      proportion_previous_level$PRI_SEC_top_level[i] <- prop_top
      proportion_previous_level$PRI_SEC_interm_level[i] <- prop_interm
      proportion_previous_level$PRI_SEC_basal_level[i] <- prop_basal

    } 
    
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

#save(proportion_previous_level, file = "proportion_previous_level.RData")
#load("proportion_previous_level.RData")

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

rem_orig_prim2_v2 <- rem_orig_prim_v2 + geom_boxplot(aes(fill = level),) +
  ylab("previous trophic level") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))

rem_orig_prim2_v2


################################################################################
# Plot vulnerability vs body size (dispersal proxy)
################################################################################

#all_species_vulnerability_2
#species_occ_merged_maiorano_grilo_2

#Load Elton
mammal_elton <- read.delim("C:\\Users\\asus\\Documents\\0. Posdoc\\CONTRATO\\species_databases\\elton_traits\\MamFuncDat.txt")
#View(mammal_elton)
names(mammal_elton)
head(mammal_elton)

bird_elton <- read.delim("C:\\Users\\asus\\Documents\\0. Posdoc\\CONTRATO\\species_databases\\elton_traits\\BirdFuncDat.txt")
#View(bird_elton)
names(bird_elton)
head(bird_elton)

mammals_elton_gbif_id <- rep(NA, nrow(mammal_elton))
for(i in 1:nrow(mammal_elton)) {
  
id_m  <- taxize::get_gbifid(mammal_elton$Scientific[i],
                            rank = "species",
                            )

mammals_elton_gbif_id[i] <- as.numeric(id_m[1])

}
#
birds_elton_gbif_id <- rep(NA, nrow(bird_elton))
for(i in 1:nrow(bird_elton)){ 
  
  id_b  <- taxize::get_gbifid(bird_elton$Scientific[i],
                              rank = "species"
                              )
  
  birds_elton_gbif_id[i] <- as.numeric(id_b[1])
  
  
}

mammals_elton_gbif_id_2 <- data.frame(mammal_elton$Scientific, mammals_elton_gbif_id)
birds_elton_gbif_id_2 <- data.frame(bird_elton$Scientific, birds_elton_gbif_id)
#View(mammals_elton_gbif_id_2)
#View(birds_elton_gbif_id_2)

birds_elton_gbif_id_2 <- birds_elton_gbif_id_2[complete.cases(birds_elton_gbif_id_2),]
mammals_elton_gbif_id_2 <- mammals_elton_gbif_id_2[complete.cases(mammals_elton_gbif_id_2),]

names(mammals_elton_gbif_id_2) <- c("species", "gbif_id")
names(birds_elton_gbif_id_2) <- c("species", "gbif_id")

mammal_elton_2 <- merge(x=mammals_elton_gbif_id_2, y=mammal_elton, by.x="species", by.y="Scientific", all.x=T)
#View(mammal_elton_2)

bird_elton_2 <- merge(x=birds_elton_gbif_id_2, y=bird_elton, by.x="species", by.y="Scientific", all.x=T)
#View(bird_elton_2)

names(mammal_elton_2)
mammal_elton_3 <- mammal_elton_2[,c(1,2,25)]
head(mammal_elton_3)
names(mammal_elton_3)

names(bird_elton_2)
bird_elton_3 <- bird_elton_2[,c(1,2,37)]
head(bird_elton_3)
names(bird_elton_3)

birds_and_mammals_3 <- rbind(bird_elton_3, mammal_elton_3)
head(birds_and_mammals_3)

#save(birds_and_mammals_3, file = "birds_and_mammals_3_06set23.RData")
#load("birds_and_mammals_3_06set23.RData")

###

#str(species_occ_merged_maiorano_grilo_2)
species_occ_merged_maiorano_grilo_2$gbif_id <- as.numeric(species_occ_merged_maiorano_grilo_2$gbif_id)
species_occ_merged_maiorano_grilo_2_and_bodymass <- merge(x=species_occ_merged_maiorano_grilo_2, y=birds_and_mammals_3, by.x="gbif_id", by.y="gbif_id", all.x=TRUE)

#all_species_vulnerability_2
species_occ_merged_maiorano_grilo_2_and_bodymass_and_vulnerability <- merge(x=species_occ_merged_maiorano_grilo_2_and_bodymass, y=all_species_vulnerability_2, 
                                                                            by.x="grilo_data", by.y="Species", all.x=TRUE)

#head(species_occ_merged_maiorano_grilo_2_and_bodymass_and_vulnerability)
#nrow(species_occ_merged_maiorano_grilo_2_and_bodymass_and_vulnerability)

plot(species_occ_merged_maiorano_grilo_2_and_bodymass_and_vulnerability$BodyMass.Value, 
     species_occ_merged_maiorano_grilo_2_and_bodymass_and_vulnerability$Median_MAXroad.RM.1000.)

################################################################################
# Species in each grid
################################################################################
#23-06-2023

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

sp_richness <- merge(x=grids_grilo_shape, y=nr_species_per_grid, by.x="PageName", by.y="grid")
#terra::writeVector(sp_richness, "sp_richness.shp")
#terra::plet(sp_richness, "sp_richness")

################################################################################
# How many interactions lost? - with secondary extinctions
################################################################################
#23-06-2023 

nr_lost_interactions <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 2))
names(nr_lost_interactions) <- c("grid","lost_interactions")
head(nr_lost_interactions)

#local_fw_MAIORANO[[100]]
#local_fw_MAIORANO_REMOVED[[100]]

for(i in 1:nrow(nr_lost_interactions)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_lost_interactions[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    if(!is.null(nrow(local_fw_MAIORANO[[i]]$trophic.links))) nr_lost_interactions[i,2] <- nrow(local_fw_MAIORANO[[i]]$trophic.links) - nrow(local_fw_MAIORANO_REMOVED[[i]]$trophic.links) else nr_lost_interactions[i,2]<-0
  }
  
}

lost_interactions_with_sec_extinctions <- merge(x=grids_grilo_shape, y=nr_lost_interactions, by.x="PageName", by.y="grid")
#terra::writeVector(lost_interactions_with_sec_extinctions, "lost_interactions_with_sec_extinctions.shp")
#terra::plet(lost_interactions_with_sec_extinctions, "lost_interactions")
