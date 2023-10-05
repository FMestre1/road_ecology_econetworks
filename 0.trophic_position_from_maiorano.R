################################################################################
################################################################################
#           SCRIPT 0 - DERIVING NODE TROPHIC LEVEL FROM MAIORANO
################################################################################
################################################################################

#FMestre
#September 2023

#Load packages
library(igraph)
library(cheddar)
library(ggplot2)

################################################################################
#                       Loading Maiorano´s dataset
################################################################################

#Maiorano, L., Montemaggiori, A., Ficetola, G. F., O’connor, L., & Thuiller, W. (2020). 
#TETRA‐EU 1.0: a species‐level trophic metaweb of European tetrapods.
#Global Ecology and Biogeography, 29(9), 1452-1457.

maiorano_metaweb <- read.csv("C:\\Users\\asus\\Documents\\0. Posdoc\\CONTRATO\\species_databases\\food_webs_tetrapods_europe\\dataset\\Metaweb_adults.csv", header = T)
rownames(maiorano_metaweb) <- maiorano_metaweb$X
maiorano_metaweb <- maiorano_metaweb[,-1]
#View(maiorano_metaweb)

#CREATE IGRAPH NETWORK

maiorano_igraph <- igraph::graph_from_adjacency_matrix(as.matrix(t(maiorano_metaweb)), mode = "directed")
plot(maiorano_igraph)
#igraph::vertex(maiorano_igraph)

#CREATE CHEDDAR NETWORK

nodes <- as.data.frame(colnames(maiorano_metaweb))
colnames(nodes) <- "node"

trophiclinks <- data.frame(matrix(ncol = 2))
colnames(trophiclinks) <- c("resource", "consumer")

for(i in 1:nrow(maiorano_metaweb)){
  row1 <- maiorano_metaweb[i,]
  predator1 <- rownames(row1)
  preys1 <- colnames(row1[,which(row1 == 1)])
  predator2 <- rep(predator1, length(preys1))
  df1 <- data.frame(preys1, predator2)
  if(nrow(df1)!=0) {colnames(df1) <- c("resource", "consumer")
  trophiclinks <- rbind(trophiclinks, df1)}
}

trophiclinks <- trophiclinks[-1,]

maiorano_cheddar <- cheddar::Community(nodes, properties = list(title = "MMetaweb"), trophic.links = trophiclinks)
plot(maiorano_cheddar)

#Overall positions
overall_basal <- cheddar::BasalNodes(maiorano_cheddar)
overall_intermediate <- cheddar::IntermediateNodes(maiorano_cheddar)
overall_top <- cheddar::TopLevelNodes(maiorano_cheddar)

overall_basal_2 <- data.frame(overall_basal, rep("basal", length(overall_basal)))
overall_intermediate_2 <- data.frame(overall_intermediate, rep("intermediate", length(overall_intermediate)))
overall_top_2 <- data.frame(overall_top, rep("top", length(overall_top)))

names(overall_basal_2) <- c("species", "position")
names(overall_intermediate_2) <- c("species", "position")
names(overall_top_2) <- c("species", "position")

overall_previous_positions <- rbind(overall_top_2,
                                    overall_intermediate_2,
                                    overall_basal_2)


################################################################################
#                                   NEW FIG2
################################################################################

##################################### FIG2a ####################################

#species_grilo_merged_maiorano_complete
#all_species_vulnerability_1

all_species_vulnerability_maiorano_position <- merge(x=all_species_vulnerability_1,
      y=overall_previous_positions,
      by.x = "Species",
      by.y = "species")

names(all_species_vulnerability_maiorano_position)[2] <- "vulnerability"

all_species_vulnerability_maiorano_position$position <- as.factor(all_species_vulnerability_maiorano_position$position)

all_species_vulnerability_maiorano_position$position <- factor(all_species_vulnerability_maiorano_position$position,     
                                              c("top", "intermediate", "basal"))

#View(all_species_vulnerability_maiorano_position)

tl_vuln_v2 <- ggplot(all_species_vulnerability_maiorano_position, aes(x = position, y = vulnerability))

tl_vuln2_v2 <- tl_vuln_v2 + geom_violin(aes(fill = position),) +
  ylab("Vulnerability") +
  xlab("Trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))

tl_vuln2_v2

##################################### FIG2b ####################################

#all_species_vulnerability_maiorano_position

extinctions_levels2 <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 6, nrow = length(local_fw_MAIORANO)))
names(extinctions_levels2) <- c("grid", 
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
      
      removed_primary_position <- all_species_vulnerability_maiorano_position[all_species_vulnerability_maiorano_position$Species %in% removed_species_or_prim,]$position
      removed_primary_position <- data.frame(table(removed_primary_position))
      #AQUI
      
      #Original networks
      #tp_0 <- length(cheddar::TopLevelNodes(before_net))
      #it_0 <- length(cheddar::IntermediateNodes(before_net))
      #bs_0 <- length(cheddar::BasalNodes(before_net))
      #Primary extinctions
      #tp_1 <- length(cheddar::TopLevelNodes(prim_net))
      #it_1 <- length(cheddar::IntermediateNodes(prim_net))
      #bs_1 <- length(cheddar::BasalNodes(prim_net))
      
      #Original to primary
      #extinctions_levels$ORIG_PRI_top_level[i] <- 1-(tp_1/tp_0)
      #extinctions_levels$ORIG_PRI_interm_level[i] <- 1-(it_1/it_0)
      #extinctions_levels$ORIG_PRI_basal_level[i] <- 1-(bs_1/bs_0)
      
    } 
    
    if(length(removed_species_prim_sec)!=0){
      
      #Primary extinctions
      #tp_1 <- length(cheddar::TopLevelNodes(prim_net))
      #it_1 <- length(cheddar::IntermediateNodes(prim_net))
      #bs_1 <- length(cheddar::BasalNodes(prim_net))
      #Cascading effects
      #tp_2 <- length(cheddar::TopLevelNodes(sec_net))
      #it_2 <- length(cheddar::IntermediateNodes(sec_net))
      #bs_2 <- length(cheddar::BasalNodes(sec_net))
      
      #Primary to cascading
      #extinctions_levels$PRI_SEC_top_level[i] <- 1-(tp_2/tp_1)
      #extinctions_levels$PRI_SEC_interm_level[i] <- 1-(it_2/it_1)
      #extinctions_levels$PRI_SEC_basal_level[i] <- 1-(bs_2/bs_1)
      
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

##################################### FIG2c ####################################
