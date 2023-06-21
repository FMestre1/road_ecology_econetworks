########################################################################################
#                          ROAD ECOLOGY - NETWORK ECOLOGY         
########################################################################################


#FMestre
#12-06-2023

library(cheddar)
library(igraph)
library(NetIndices)
library(terra)

getwd()

#required function
#Required function
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
grids_grilo_shape <- terra::vect("shapes/Nvulnerablegrid50_wgs84_2.shp")
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

grid_road_density <- grids_grilo[grids_grilo$grids_grilo.PageName == cheddar1$properties$title, ]$grids_grilo.kmkm2

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

#save(local_fw_MAIORANO_REMOVED, file = "local_fw_MAIORANO_REMOVED.RData")
#load("local_fw_MAIORANO_REMOVED.RData")

local_fw_MAIORANO_REMOVED
head(local_fw_MAIORANO_REMOVED)

species_loss
connectance_dif
compart_dif


result1 <- data.frame(
  names(local_fw_MAIORANO),
  species_loss,
  connectance_dif,
  compart_dif
  )

names(result1)[1] <- "grid"

grids_grilo_shape_species_loss <- merge(x=grids_grilo_shape, y=result1, by.x="PageName", by.y= "grid")
terra::writeVector(grids_grilo_shape_species_loss, "pre_after_road.shp")

####

#Porportion of top, intermediate and basal nodes before and after

fractions_top_intermediate_basal_nodes <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 7, nrow = length(local_fw_MAIORANO)))
names(fractions_top_intermediate_basal_nodes) <- c("grid", "BEFORE_top_level", "BEFORE_Interm_level", "BEFORE_basal_level", 
                                                   "AFTER__top_level", "AFTER_Interm_level", "AFTER_basal_level", "changed?") 

#LOOP
for(i in 1:length(local_fw_MAIORANO)){

before_net <- local_fw_MAIORANO[[i]]
after_net <- local_fw_MAIORANO_REMOVED[[i]]
#

if(!is.na(before_net)) fractions_top_intermediate_basal_nodes$BEFORE_top_level[i] <- cheddar::FractionTopLevelNodes(before_net)
if(!is.na(before_net)) fractions_top_intermediate_basal_nodes$BEFORE_Interm_level[i] <- cheddar::FractionIntermediateNodes(before_net)
if(!is.na(before_net)) fractions_top_intermediate_basal_nodes$BEFORE_basal_level[i] <- cheddar::FractionBasalNodes(before_net)
#
if(!is.na(after_net)) fractions_top_intermediate_basal_nodes$AFTER__top_level[i] <- cheddar::FractionTopLevelNodes(after_net)
if(!is.na(after_net)) fractions_top_intermediate_basal_nodes$AFTER_Interm_level[i] <- cheddar::FractionIntermediateNodes(after_net)
if(!is.na(after_net))fractions_top_intermediate_basal_nodes$AFTER_basal_level[i] <- cheddar::FractionBasalNodes(after_net)
#
if(!is.na(before_net) && !is.na(after_net)){
  if(cheddar::NumberOfNodes(before_net) == cheddar::NumberOfNodes(after_net)) {

fractions_top_intermediate_basal_nodes$`changed?`[i] <- "no"

  } else fractions_top_intermediate_basal_nodes$`changed?`[i] <- "yes"
}

message(i)

}

head(fractions_top_intermediate_basal_nodes)

#Save
#save(fractions_top_intermediate_basal_nodes, file = "fractions_top_intermediate_basal_nodes.RData")


################################################################################
# Boxplots of the fraction of top, intermediate, and basal nodes
################################################################################

library(ggplot2)

head(fractions_top_intermediate_basal_nodes)
View(fractions_top_intermediate_basal_nodes)

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

#Reorder factor levels
net_changes$before_after <- factor(net_changes$before_after,     
                                   c("before", "after"))
### Boxplot ###
names(net_changes)
str(net_changes)

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

removed_position <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 4, nrow = length(local_fw_MAIORANO)))
names(removed_position) <- c("grid", "top_level", "interm_level", "basal_level", "changed?") 

head(removed_position)

#LOOP
for(i in 1:length(local_fw_MAIORANO_REMOVED)){
  
  before_net <- local_fw_MAIORANO[[i]]
  after_net <- local_fw_MAIORANO_REMOVED[[i]]
  
  if(!is.na(before_net) && !is.na(after_net)) {
  
  removed_species <- before_net$nodes$node[!(before_net$nodes$node %in% after_net$nodes$node)]
  
  if(length(removed_species)){
    
    tp1 <- cheddar::TopLevelNodes(before_net)
    it1 <- cheddar::IntermediateNodes(before_net)
    bs1 <- cheddar::BasalNodes(before_net)
    
    removed_position$top_level[i] <- sum(removed_species %in% tp1)/length(removed_species)
    removed_position$interm_level[i] <- sum(removed_species %in% it1)/length(removed_species)
    removed_position$basal_level[i] <- sum(removed_species %in% bs1)/length(removed_species)
    
    removed_position$`changed?`[i] <- "yes"
    
  } else removed_position$`changed?`[i] <- "no"

}
  
message(i)

}

#Save
#save(removed_position, file = "removed_position.RData")


################################################################################
# Plot it...
################################################################################

top_rm <- data.frame(removed_position$top_level, "top")
mid_rm <- data.frame(removed_position$interm_level, "mid")
basal_rm <- data.frame(removed_position$basal_level, "basal")

names(top_rm) <- c("proportion", "level")
names(mid_rm) <- c("proportion", "level")
names(basal_rm) <- c("proportion", "level")

head(top_rm)
head(mid_rm)
head(basal_rm)

removed_position <- rbind(top_rm, mid_rm, basal_rm)

removed_position$level <- as.factor(removed_position$level)

#Reorder factor levels
removed_position$level <- factor(removed_position$level,     
                                 c("top", "mid", "basal"))

rem_tp <- ggplot(removed_position, aes(x = level, y = proportion))

rem_tp2 <- rem_tp + geom_boxplot(aes(fill = level),) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#E80F00"))

rem_tp2

