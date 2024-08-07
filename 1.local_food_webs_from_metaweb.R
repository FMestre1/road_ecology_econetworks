################################################################################
################################################################################
#                    SCRIPT 1 - DERIVING LOCAL FOOD WEBS
################################################################################
################################################################################

# Road density simplifies regional food webs
# F. Mestre, V.A.G. Bastazini, F. Ascensão

#Load packages
library(terra)
library(stringr)
library(taxize)
library(igraph)
library(cheddar)
library(ggplot2)
library(gridExtra)
library(NetIndices)

################################################################################
# 0. Species vulnerability from Grilo et al paper             
################################################################################

#Grilo, C., Koroleva, E., Andrášik, R., Bíl, M., & González‐Suárez, M. (2020).
#Roadkill risk and population vulnerability in European birds and mammals. 
#Frontiers in Ecology and the Environment, 18(6), 323-328.

#https://doi.org/10.1002/fee.2216

#Load vulnerability information from Grilo et al
birds_vulnerability <- read.csv("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\9 RankingvulnerableBirds.csv", header = TRUE)
mammals_vulnerability <- read.csv("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\10 RankingvulnerableMammals.csv", header = TRUE)
all_species_vulnerability <- rbind(mammals_vulnerability, birds_vulnerability)

#Load spatial information, study site, grids and species occurrence (based on Grilo et al)
mammals <- read.csv("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\fernando_26set_2023\\matrix_occurrence_mammals.csv")
birds <- read.csv("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\fernando_26set_2023\\matrix_occurrence_birds.csv")
mammals <- mammals[,-2]
birds <- birds[,-2]

all_species <- merge(x=birds,
      y=mammals,
      by.x="PageNumber",
      by.y="PageNumber",
      all = TRUE)

View(all_species)

colnames(mammals)[-1] <- stringr::str_replace(colnames(mammals)[-c(1)], "\\.", " ")
colnames(birds)[-1] <- stringr::str_replace(colnames(birds)[-c(1)], "\\.", " ")
colnames(all_species)[-1] <- stringr::str_replace(colnames(all_species)[-c(1)], "\\.", " ")
#
rownames(mammals) <- mammals$PageNumber
rownames(birds) <- birds$PageNumber
rownames(all_species) <- all_species$PageNumber
#
mammals <- mammals[,-1]
birds <- birds[,-1]
all_species <- all_species[,-1]
#

template_grilo <- terra::vect("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\fernando_26set_2023\\template_grilo.shp")

################################################################################
# 1. Maiorano et al. European Metaweb of trophic interactions             
################################################################################

#Maiorano, L., Montemaggiori, A., Ficetola, G. F., O’connor, L., & Thuiller, W. (2020). 
#TETRA‐EU 1.0: a species‐level trophic metaweb of European tetrapods.
#Global Ecology and Biogeography, 29(9), 1452-1457.

maiorano_metaweb <- read.csv("C:\\Users\\asus\\Documents\\0. Posdoc\\CONTRATO\\species_databases\\food_webs_tetrapods_europe\\dataset\\Metaweb_adults.csv", header = T)
rownames(maiorano_metaweb) <- maiorano_metaweb$X
maiorano_metaweb <- maiorano_metaweb[,-1]

#write.csv(maiorano_metaweb, file = "maiorano_metaweb.csv")

#View(maiorano_metaweb)
#test1 <- maiorano_metaweb[rownames(maiorano_metaweb) == "Coracias garrulus",]

#Replace the codes by the species names
rn <- rownames(maiorano_metaweb)
cn <- colnames(maiorano_metaweb)

spp_maiorano <- read.delim("C:\\Users\\asus\\Documents\\0. Posdoc\\CONTRATO\\species_databases\\food_webs_tetrapods_europe\\dataset\\Spp_Id.txt")

#
for(i in 1:length(rn)) rn[i] <- spp_maiorano[spp_maiorano$ID == rn[i],]$SPPname
for(i in 1:length(cn)) cn[i] <- spp_maiorano[spp_maiorano$ID == cn[i],]$SPPname
#
rownames(maiorano_metaweb) <- rn
colnames(maiorano_metaweb) <- cn
  
colnames(maiorano_metaweb) <- stringr::str_replace(colnames(maiorano_metaweb), "_", " ")
rownames(maiorano_metaweb) <- stringr::str_replace(rownames(maiorano_metaweb), "_", " ")

################################################################################
# 3.                 DERIVING NODE TROPHIC LEVEL FROM MAIORANO
################################################################################

#1.CREATE CHEDDAR NETWORK

nodes <- as.data.frame(colnames(maiorano_metaweb))
colnames(nodes) <- "node"

trophiclinks <- data.frame(matrix(ncol = 2))
colnames(trophiclinks) <- c("resource", "consumer")

for(i in 1:nrow(maiorano_metaweb)){
  
  row1 <- maiorano_metaweb[i,]
  predator1 <- rownames(row1)
  #preys1 <- colnames(row1[,which(row1 == 1)])
  preys1 <- colnames(row1)[which(row1 == 1)]
  predator2 <- rep(predator1, length(preys1))
  df1 <- data.frame(preys1, predator2)
  if(nrow(df1)!=0) {
    colnames(df1) <- c("resource", "consumer")
    trophiclinks <- rbind(trophiclinks, df1)
                   }
  message(i)
  }

trophiclinks <- trophiclinks[-1,]

maiorano_cheddar <- cheddar::Community(nodes, properties = list(title = "MMetaweb"), trophic.links = trophiclinks)
#plot(maiorano_cheddar)
#save(maiorano_cheddar, file = "maiorano_cheddar_08NOV23.RData")

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

#View(overall_previous_positions)
#save(overall_previous_positions, file = "overall_previous_positions.RData")

#2.CREATE IGRAPH NETWORK
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

maiorano_igraph <- ToIgraph(maiorano_cheddar)
gorder(maiorano_igraph) #nr nodes
gsize(maiorano_igraph) #nr interactions

################################################################################
# 4.                Computing trophic height in the Metaweb
################################################################################

maiorano_cheddar_matrix <- cheddar::PredationMatrix(maiorano_cheddar)
metaweb_TL <- NetIndices::TrophInd(maiorano_cheddar_matrix)
metaweb_TL <- data.frame(rownames(metaweb_TL), metaweb_TL$TL)
names(metaweb_TL) <- c("species", "TL")

################################################################################
#   5.Create a matched table of scientific names across the datasets in use
################################################################################

match_table <- read.csv("match_datasets_master_table.csv", sep = ";")
#View(match_table)
#head(match_table)
#head(all_species_vulnerability)

master_table_vuln <- merge(x = match_table,
                           y = all_species_vulnerability,
                           by.x = "Species_grilo",
                           by.y = "Species"
)

names(master_table_vuln)[9] <- "grilo_threshold"
master_table_vuln <- master_table_vuln[,-c(10:12)] 

#View(master_table_vuln)

################################################################################
#   6.Creating local networks
################################################################################

master_table_vuln_complete <- master_table_vuln[complete.cases(master_table_vuln),]

local_fw_MAIORANO <- vector(mode = "list", length = length(template_grilo$PageNumber))
names(local_fw_MAIORANO) <- template_grilo$PageNumber

for(i in 1:length(local_fw_MAIORANO)){#START LOCAL
  
  grid_id <- names(local_fw_MAIORANO[i]) #GRID ID
  #species_in_grids
  
  grid_df <- t(all_species[grid_id,])
  grid_df <- data.frame(rownames(grid_df),grid_df[,1])
  rownames(grid_df) <- 1:nrow(grid_df)
  names(grid_df) <- c("species", "presence")
  grid_df_1 <- grid_df[grid_df$presence == 1,]#Species with presence
  grid_df_2 <- master_table_vuln_complete[master_table_vuln_complete$Species_maiorano %in% grid_df_1$species,]
  grid_df_3 <- data.frame(grid_df_2[,4], 
  grid_df_2[,1], 
  grid_df_2[,3], 
  grid_df_2[,5:9]) 
  names(grid_df_3) <- c("species_maiorano", "species_grilo", "gbif_maiorano", "gbif_grilo",
                        "class", "order", "family", "grilo_threshold"
                        )
  
  #head(grid_df_3)
  
  TIB_level <- overall_previous_positions[overall_previous_positions$species %in% grid_df_3$species_maiorano,]
  T_height_level <- metaweb_TL[metaweb_TL$species %in% grid_df_3$species_maiorano,]
  #
  grid_df_4 <- merge(x=grid_df_3, y=TIB_level, by.x="species_maiorano", by.y="species")
  grid_df_5 <- merge(x=grid_df_4, y=T_height_level, by.x="species_maiorano", by.y="species")

  if (length(maiorano_present_species)!=0){
    
    #Nodes
    nodes1 <- grid_df_5
    names(nodes1)[1] <- "node"
    rownames(nodes1) <- 1:nrow(nodes1)
    
    if(any(table(nodes1$node)==2) && !any(table(nodes1$species_grilo)==2)){#this is mostly because Picus viridis and Picus sharpei are discriminated in Grili, but not Maiorano. It might happen (and it does) that both are present in the same grid, thus having repeated node names.
      df_0 <- data.frame(table(nodes1$node))
      repeated_species <- as.character(df_0[which(df_0[,2] == 2),][,1])
      removed_repetition <- which((nodes1$node == repeated_species & nodes1$species_grilo == repeated_species) == TRUE)
      nodes1 <- nodes1[-removed_repetition,]
    }
    
    #Properties
    prop1 <- list()
    prop1[[1]] <- grid_id
    names(prop1)[1] <- "title" 
    
    #Trophic links
    #Should have "resource" and "consumer" columns
#    if (length(nodes1$node)>1){
      
      #Create base dataframe for the interactions
      tlinks_df2 <- as.data.frame(matrix(ncol = 2, nrow = 0))
      names(tlinks_df2) <- c("resource", "consumer")
      
      #Getting the prey and predator species of the focal species - START
      for(j in 1:length(nodes1$node)){
        focal_sp <- nodes1[j,]
        focal_sp_maiorano <- focal_sp$node
        foca_sp_grilo <- focal_sp$species_grilo
        preys3 <- maiorano_metaweb[which(rownames(maiorano_metaweb) == focal_sp_maiorano), ]
        preys3 <- colnames(preys3[,which(preys3[,] == 1)])
        preys3 <- nodes1$node[nodes1$node %in% preys3]#only those in the square grid
        #
        predators3 <- maiorano_metaweb[,which(colnames(maiorano_metaweb) == focal_sp_maiorano)]
        predators3 <- data.frame(rownames(maiorano_metaweb), maiorano_metaweb[,which(colnames(maiorano_metaweb) == focal_sp_maiorano)])
        predators3 <- predators3[predators3[,2]==1,]
        predators3 <- predators3[!is.na(predators3$rownames.maiorano_metaweb.),]
        predators3 <- predators3$rownames.maiorano_metaweb.
        predators3 <- nodes1$node[nodes1$node %in% predators3]#only those in the square grid
        #
        if(length(preys3)!=0){
          nr_preys3 <- length(preys3)
          nr_pred3 <- rep(focal_sp_maiorano, nr_preys3)
          df_focal_as_predator <- data.frame(cbind(preys3, nr_pred3))
          names(df_focal_as_predator) <- c("resource", "consumer")
          tlinks_df2 <- rbind(tlinks_df2, df_focal_as_predator)
          rm(df_focal_as_predator)
        }
        
        if(length(predators3)!=0){
          nr_pred4 <- length(predators3)
          nr_preys4 <- rep(focal_sp_maiorano, length(predators3))
          df_focal_as_prey <- data.frame(cbind(nr_preys4, predators3))
          names(df_focal_as_prey) <- c("resource", "consumer")
          tlinks_df2 <- rbind(tlinks_df2, df_focal_as_prey)
          rm(df_focal_as_prey)
        }
        
      }#Getting the prey and predator species of the focal species - END
    
      tlinks_df2 <- unique(tlinks_df2)
          
      if(nrow(tlinks_df2)!=0){ 
        
        comm1_2 <- cheddar::Community(nodes = nodes1, properties = prop1, trophic.links = tlinks_df2)
        
      } else comm1_2 <- cheddar::Community(nodes = nodes1, properties = prop1, trophic.links = NULL)
      
      local_fw_MAIORANO[[i]] <- comm1_2
      
#    } else {
      #comm2 <- cheddar::Community(nodes = nodes1, properties = prop1, trophic.links = NULL)
#      comm2 <- NA
#      local_fw_MAIORANO[[i]] <- comm2
 #   }
  } else local_fw_MAIORANO[[i]] <- NA
  
  message(i)
  #gc()
  
}#END LOCAL

#Load & Save
#save(local_fw_MAIORANO, file = "local_fw_MAIORANO_with_metaweb_TL_08NOV23.RData")
#load("local_fw_MAIORANO_with_metaweb_TL_08NOV23.RData")