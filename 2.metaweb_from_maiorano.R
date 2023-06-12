#FMestre
#12-06-2023

library(terra)
library(stringr)

########################################################################################
# 1. Loading and editing the Maiorano Metaweb             
########################################################################################

maiorano_metaweb <- read.csv("C:\\Users\\FMest\\Documents\\0. Posdoc\\CONTRATO\\species_databases\\food_webs_tetrapods_europe\\dataset\\Metaweb_adults.csv", header = T)
rownames(maiorano_metaweb) <- maiorano_metaweb$X
maiorano_metaweb <- maiorano_metaweb[,-1]
#View(maiorano_metaweb)

maiorano_groups <- read.csv("C:\\Users\\FMest\\Documents\\0. Posdoc\\CONTRATO\\species_databases\\food_webs_tetrapods_europe\\dataset\\SBMgroups_spp.csv", sep=";", header = T)
#head(maiorano_groups)

spp_maiorano <- read.delim("C:\\Users\\FMest\\Documents\\0. Posdoc\\CONTRATO\\species_databases\\food_webs_tetrapods_europe\\dataset\\Spp_Id.txt")
spp_maiorano$SPPname <- stringr::str_replace(spp_maiorano$SPPname,"_", " ")
#head(spp_maiorano)

#Replace the codes by the species names
rn <- rownames(maiorano_metaweb)
cn <- colnames(maiorano_metaweb)
#
for(i in 1:length(rn)) rn[i] <- spp_maiorano[spp_maiorano$ID == rn[i],]$SPPname
for(i in 1:length(cn)) cn[i] <- spp_maiorano[spp_maiorano$ID == cn[i],]$SPPname
#
rownames(maiorano_metaweb) <- rn
colnames(maiorano_metaweb) <- cn
#

#View(maiorano_metaweb)
#save(maiorano_metaweb, file = "maiorano_metaweb.RData")

########################################################################################
# 2.Deriving local networks - SAMPLE GRIDS
########################################################################################

ssite <- terra::vect("C:/Users/FMest/Documents/0. Artigos/roads_networks/data/area_roads_eco_networks.shp")
plot(ssite)

ssite_ibpen <- terra::vect("C:/Users/FMest/Documents/0. Artigos/roads_networks/data/ibpen3.shp")
plot(ssite_ibpen)

# 2.1. Getting the grids ###############################################################
grid_50_ibp <- terra::vect("C:/Users/FMest/Documents/0. Artigos/roads_networks/data/grids_ib_pen.shp")
plot(grid_50_ibp)

local_fw_MAIORANO_ibp <- vector(mode = "list", length = length(grid_50_ibp))
names(local_fw_MAIORANO_ibp) <- grid_50_ibp$PageName
#head(local_fw_MAIORANO_ibp)

# 2.2. Getting the species in each grid ################################################

#mammal_dist <- terra::vect("D:\\Dados biológicos\\FAscensao_Species_IUCN\\MAMMALS_TERRESTRIAL_ONLY\\MAMMALS_TERRESTRIAL_ONLY.shp")
#bird_dist <- terra::vect("D:\\Dados biológicos\\FAscensao_Species_IUCN\\BOTW\\birds.shp")

#terra::writeVector(mammal_dist_ibpen, filename = "mammal_dist_ibpen.shp", filetype = "ESRI Shapefile")
#terra::writeVector(bird_dist_ibpen, filename = "bird_dist_ibpen.shp", filetype = "ESRI Shapefile")



# 2.3. Local networks ##################################################################

#Fill the list - CORRIGIR
for(i in 1:length(iberian_fw_MAIORANO)){
  
  fw_names1 <- names(grid10_birds_and_mammals_table_SPATIAL@data)[grid10_birds_and_mammals_table_SPATIAL@data[i,]==1]
  grid_ID <- grid10_birds_and_mammals_table_SPATIAL@data[i,]$UTMCODE
  
  if (length(fw_names1)!=0){
    
    #Nodes
    nodes1 <- database_FM2[database_FM2$species %in% fw_names1,]
    names(nodes1)[1] <- "node"
    nodes1$node <- stringr::str_replace(nodes1$node,"\\.", " ")
    
    #Properties
    prop1 <- list()
    prop1[[1]] <- names(iberian_fw)[i]
    names(prop1)[1] <- "title" 
    #
    coords1 <- data.frame(rgeos::gCentroid(grid10_birds_and_mammals_table_SPATIAL[i,]))
    #
    prop1[[2]] <- as.numeric(coords1[1])
    names(prop1)[2] <- "centroid_x"
    #
    prop1[[3]] <- as.numeric(coords1[2])
    names(prop1)[3] <- "centroid_y"
    
    #Trophic links
    #Should have "resource" and "consumer" columns
    if (length(nodes1$node)>1){
      
      tlinks_df2 <- as.data.frame(matrix(ncol = 2, nrow = 0))
      names(tlinks_df2) <- c("resource", "consumer")
      
      for(j in 1:length(nodes1$node)){
        
        focal_sp <- nodes1$node[j]
        #Using taxize to evaluate synonims
        if(!(focal_sp %in% rownames(maiorano_metaweb)) & !(focal_sp %in% colnames(maiorano_metaweb))){
          #focal_sp_2 <- get_nbnid(sci_com=focal_sp)
          focal_sp_3 <- nbn_synonyms(focal_sp)
          focal_sp <- c(nodes1$node[j], focal_sp_3$nameString)
        }
        
        preys3 <- maiorano_metaweb[which(rownames(maiorano_metaweb) == unique(focal_sp)), ]
        preys3 <- colnames(preys3[,which(preys3[,] == 1)])
        preys3 <- nodes1$node[nodes1$node %in% preys3]
        #
        predators3 <- maiorano_metaweb[,which(colnames(maiorano_metaweb) == unique(focal_sp))]
        predators3 <- data.frame(rownames(maiorano_metaweb), maiorano_metaweb[,which(colnames(maiorano_metaweb) == focal_sp)])
        #if(ncol(predators3)!=0) predators3 <- predators3[which(predators3[,2]==1),][,1]
        predators3 <- nodes1$node[nodes1$node %in% predators3]
        #
        if(length(preys3)!=0){
          nr_preys3 <- length(preys3)
          nr_pred3 <- rep(focal_sp, nr_preys3)
          df_focal_as_predator <- data.frame(cbind(preys3, nr_pred3))
          names(df_focal_as_predator) <- c("resource", "consumer")
          tlinks_df2 <- rbind(tlinks_df2, df_focal_as_predator)
          rm(df_focal_as_predator)
        }
        if(length(predators3)!=0){
          nr_pred4 <- length(predators3)
          nr_preys4 <- rep(focal_sp, length(predators3))
          df_focal_as_prey <- data.frame(cbind(nr_preys4, predators3))
          names(df_focal_as_prey) <- c("resource", "consumer")
          tlinks_df2 <- rbind(tlinks_df2, df_focal_as_prey)
          rm(df_focal_as_prey)
        }
        
      }
      
      tlinks_df2 <- unique(tlinks_df2)
      
      if(nrow(tlinks_df2)!=0){ 
        
        comm1_2 <- cheddar::Community(nodes = nodes1, properties = prop1, trophic.links = tlinks_df2)
        
      } else comm1_2 <- cheddar::Community(nodes = nodes1, properties = prop1, trophic.links = NULL)
      
      save(comm1_2, file = paste0("0.MAIORANO_interactions_dataset/MAIORANO_fw_community_", grid_ID, ".RData"))
      iberian_fw_MAIORANO[[i]] <- comm1_2
      
    } else {
      comm2 <- cheddar::Community(nodes = nodes1, properties = prop1, trophic.links = NULL)
      iberian_fw_MAIORANO[[i]] <- comm2
      save(comm2, file = paste0("0.MAIORANO_interactions_dataset/MAIORANO_fw_community_", grid_ID, ".RData"))
    }
  }else iberian_fw_MAIORANO[[i]] <- NA
  
  message(paste0("Did ", i, " of ", length(iberian_fw_new_GLOBI),"! - ", as.character(round((i*100)/length(iberian_fw_new_GLOBI),3))))
  #gc()
  
}#END


########################################################################################
# 2.Deriving local networks - FULL CODE
########################################################################################

grid_50 <- terra::vect("C:/Users/FMest/Documents/0. Artigos/roads_networks/data/grids.shp")
plot(grid_50)

grid_50$PageName

local_fw_MAIORANO <- vector(mode = "list", length = length(grid_50))
names(local_fw_MAIORANO) <- grid_50$PageName
#head(local_fw_MAIORANO)

#AQUI

#Fill the list - CORRIGIR
for(i in 1:length(iberian_fw_MAIORANO)){
  
  fw_names1 <- names(grid10_birds_and_mammals_table_SPATIAL@data)[grid10_birds_and_mammals_table_SPATIAL@data[i,]==1]
  grid_ID <- grid10_birds_and_mammals_table_SPATIAL@data[i,]$UTMCODE
  
  if (length(fw_names1)!=0){
    
    #Nodes
    nodes1 <- database_FM2[database_FM2$species %in% fw_names1,]
    names(nodes1)[1] <- "node"
    nodes1$node <- stringr::str_replace(nodes1$node,"\\.", " ")
    
    #Properties
    prop1 <- list()
    prop1[[1]] <- names(iberian_fw)[i]
    names(prop1)[1] <- "title" 
    #
    coords1 <- data.frame(rgeos::gCentroid(grid10_birds_and_mammals_table_SPATIAL[i,]))
    #
    prop1[[2]] <- as.numeric(coords1[1])
    names(prop1)[2] <- "centroid_x"
    #
    prop1[[3]] <- as.numeric(coords1[2])
    names(prop1)[3] <- "centroid_y"
    
    #Trophic links
    #Should have "resource" and "consumer" columns
    if (length(nodes1$node)>1){
      
      tlinks_df2 <- as.data.frame(matrix(ncol = 2, nrow = 0))
      names(tlinks_df2) <- c("resource", "consumer")
      
      for(j in 1:length(nodes1$node)){
        
        focal_sp <- nodes1$node[j]
        #Using taxize to evaluate synonims
        if(!(focal_sp %in% rownames(maiorano_metaweb)) & !(focal_sp %in% colnames(maiorano_metaweb))){
          #focal_sp_2 <- get_nbnid(sci_com=focal_sp)
          focal_sp_3 <- nbn_synonyms(focal_sp)
          focal_sp <- c(nodes1$node[j], focal_sp_3$nameString)
        }
        
        preys3 <- maiorano_metaweb[which(rownames(maiorano_metaweb) == unique(focal_sp)), ]
        preys3 <- colnames(preys3[,which(preys3[,] == 1)])
        preys3 <- nodes1$node[nodes1$node %in% preys3]
        #
        predators3 <- maiorano_metaweb[,which(colnames(maiorano_metaweb) == unique(focal_sp))]
        predators3 <- data.frame(rownames(maiorano_metaweb), maiorano_metaweb[,which(colnames(maiorano_metaweb) == focal_sp)])
        #if(ncol(predators3)!=0) predators3 <- predators3[which(predators3[,2]==1),][,1]
        predators3 <- nodes1$node[nodes1$node %in% predators3]
        #
        if(length(preys3)!=0){
          nr_preys3 <- length(preys3)
          nr_pred3 <- rep(focal_sp, nr_preys3)
          df_focal_as_predator <- data.frame(cbind(preys3, nr_pred3))
          names(df_focal_as_predator) <- c("resource", "consumer")
          tlinks_df2 <- rbind(tlinks_df2, df_focal_as_predator)
          rm(df_focal_as_predator)
        }
        if(length(predators3)!=0){
          nr_pred4 <- length(predators3)
          nr_preys4 <- rep(focal_sp, length(predators3))
          df_focal_as_prey <- data.frame(cbind(nr_preys4, predators3))
          names(df_focal_as_prey) <- c("resource", "consumer")
          tlinks_df2 <- rbind(tlinks_df2, df_focal_as_prey)
          rm(df_focal_as_prey)
        }
        
      }
      
      tlinks_df2 <- unique(tlinks_df2)
      
      if(nrow(tlinks_df2)!=0){ 
        
        comm1_2 <- cheddar::Community(nodes = nodes1, properties = prop1, trophic.links = tlinks_df2)
        
      } else comm1_2 <- cheddar::Community(nodes = nodes1, properties = prop1, trophic.links = NULL)
      
      save(comm1_2, file = paste0("0.MAIORANO_interactions_dataset/MAIORANO_fw_community_", grid_ID, ".RData"))
      iberian_fw_MAIORANO[[i]] <- comm1_2
      
    } else {
      comm2 <- cheddar::Community(nodes = nodes1, properties = prop1, trophic.links = NULL)
      iberian_fw_MAIORANO[[i]] <- comm2
      save(comm2, file = paste0("0.MAIORANO_interactions_dataset/MAIORANO_fw_community_", grid_ID, ".RData"))
    }
  }else iberian_fw_MAIORANO[[i]] <- NA
  
  message(paste0("Did ", i, " of ", length(iberian_fw_new_GLOBI),"! - ", as.character(round((i*100)/length(iberian_fw_new_GLOBI),3))))
  #gc()
  
}#END

