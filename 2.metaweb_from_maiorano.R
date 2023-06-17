#FMestre
#12-06-2023

library(terra)
library(stringr)
library(taxize)

########################################################################################
# 0. Species vulnerability from Grilo et al paper             
########################################################################################

#Reference:

#Grilo, C., Koroleva, E., Andrášik, R., Bíl, M., & González‐Suárez, M. (2020).
#Roadkill risk and population vulnerability in European birds and mammals. 
#Frontiers in Ecology and the Environment, 18(6), 323-328.

#https://doi.org/10.1002/fee.2216

birds_vulnerability <- read.csv("9 RankingvulnerableBirds.csv", header = TRUE)
mammals_vulnerability <- read.csv("10 RankingvulnerableMammals.csv", header = TRUE)

all_species_vulnerability <- rbind(mammals_vulnerability, birds_vulnerability)

all_species_vulnerability_1 <- merge(x=all_species_vulnerability, id_grilo_data, by.x="Species", by.y="all_species_vulnerability.Species", all=T) #creation of id_grilo_data comes later in the code 


#head(birds_vulnerability)
#head(mammals_vulnerability)
#head(all_species_vulnerability)
#nrow(all_species_vulnerability)

########################################################################################
# 1. Maiorano et al European Metaweb of trophic interactions             
########################################################################################

maiorano_metaweb <- read.csv("C:\\Users\\fmestre\\road_ecoloy_econetworks\\food_webs_tetrapods_europe\\dataset\\Metaweb_adults.csv", header = T)
rownames(maiorano_metaweb) <- maiorano_metaweb$X
maiorano_metaweb <- maiorano_metaweb[,-1]
#View(maiorano_metaweb)

maiorano_groups <- read.csv("C:\\Users\\fmestre\\road_ecoloy_econetworks\\food_webs_tetrapods_europe\\dataset\\SBMgroups_spp.csv", sep=";", header = T)
#head(maiorano_groups)

spp_maiorano <- read.delim("C:\\Users\\fmestre\\road_ecoloy_econetworks\\food_webs_tetrapods_europe\\dataset\\Spp_Id.txt")
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
#load("maiorano_metaweb.RData")

########################################################################################
# 2.Deriving local networks
########################################################################################

ssite <- terra::vect("C:\\Users\\fmestre\\road_ecoloy_econetworks\\shapes\\area_roads_eco_networks.shp")
#plot(ssite)

# 2.1. Getting the grids ###############################################################
grid_50 <- terra::vect("C:\\Users\\fmestre\\road_ecoloy_econetworks\\shapes\\grids.shp")
#plot(grid_50)

# 2.2. Getting the species in each grid ################################################

mammals <- read.csv("C:\\Users\\fmestre\\road_ecoloy_econetworks\\fernando\\mydf_mammals.csv", header = T)
birds <- read.csv("C:\\Users\\fmestre\\road_ecoloy_econetworks\\fernando\\mydf_birds.csv", header = T)

View(mammals)
View(birds)

species <- c(mammals$my_taxa, birds$my_taxa)
species <- unique(species)
#length(species)

grids <- unique(c(mammals$PageName, birds$PageName))
#length(grids)

#Create data frame species x grids with 0 and 1 (species presence per grid)
species_in_grids <- as.data.frame(matrix(ncol = length(grids), nrow = length(species)))
rownames(species_in_grids) <- species
colnames(species_in_grids) <- grids

for(i in 1:nrow(mammals)){
  
  row_mamm <- mammals[i,]
  species1 <- row_mamm$my_taxa
  grid1 <- row_mamm$PageName
  
  species_in_grids[species1, grid1] <- 1
  
  message(i)
}
  
for(i in 1:nrow(birds)){
  
  row_bird <- birds[i,]
  species2 <- row_bird$my_taxa
  grid2 <- row_bird$PageName
  
  species_in_grids[species2, grid2] <- 1
  
  message(i)
}

species_in_grids[is.na(species_in_grids)] <- 0

View(species_in_grids)

####################

#Create a matched table of scientific names across the datasets in use

species #co-occurrence dataset
rownames(maiorano_metaweb) #maiorano metaweb
all_species_vulnerability$Species #vulnerability from Clara et al paper

#save(species, file = "species.RData")
#save(maiorano_metaweb, file = "maiorano_metaweb_15june23.RData")
#save(all_species_vulnerability, file = "all_species_vulnerability_15june23.RData")

## Occurrence data

id_ocurrence_species_data <- rep(NA, length(species))

for(i in 1:length(id_ocurrence_species_data)){
  spe1_id <- get_gbifid(sci= species[i])
  id_ocurrence_species_data[i] <- spe1_id[1]
}

id_ocurrence_species_data <- data.frame(species, id_ocurrence_species_data)
names(id_ocurrence_species_data)[2] <- "gbif_id"
head(id_ocurrence_species_data)

##Maiorano Metaweb Data

id_maiorano_data <- rep(NA, length(rownames(maiorano_metaweb)))

for(i in 1:length(id_maiorano_data)){
  spe2_id <- get_gbifid(sci= rownames(maiorano_metaweb)[i])
  id_maiorano_data[i] <- spe2_id[1]
}

id_maiorano_data <- data.frame(rownames(maiorano_metaweb), id_maiorano_data)
names(id_maiorano_data)[2] <- "gbif_id"
head(id_maiorano_data)

#id_maiorano_data[id_maiorano_data$rownames.maiorano_metaweb. %in% focal_sp,]
#as.character(id_maiorano_data[id_maiorano_data$gbif_id %in% 4265021 , ][1]) 
#focal_sp

##Grilo Vulnerability Data

id_grilo_data <- rep(NA, length(all_species_vulnerability$Species))

for(i in 1:length(id_grilo_data)){
  spe3_id <- get_gbifid(sci= all_species_vulnerability$Species[i])
  id_grilo_data[i] <- spe3_id[1]
}

id_grilo_data <- data.frame(all_species_vulnerability$Species, id_grilo_data)
names(id_grilo_data)[2] <- "gbif_id"
head(id_grilo_data)

##Match table

species_occ_merged_maiorano <- merge(id_ocurrence_species_data, id_maiorano_data, all=TRUE)
head(species_occ_merged_maiorano)
species_occ_merged_maiorano_grilo <- merge(species_occ_merged_maiorano, id_grilo_data, all=TRUE)
names(species_occ_merged_maiorano_grilo) <- c("gbif_id", "species_occurrence", "maiorano_data", "grilo_data")

species_occ_merged_maiorano_grilo[species_occ_merged_maiorano_grilo$species_occurrence %in% focal_sp,]

#(species_occ_merged_maiorano_grilo, file = "species_occ_merged_maiorano_grilo.RData")
#View(species_occ_merged_maiorano_grilo)


species_occ_merged_maiorano_grilo_2 <- species_occ_merged_maiorano_grilo[complete.cases(species_occ_merged_maiorano_grilo),]
#View(species_occ_merged_maiorano_grilo_2)
#nrow(species_occ_merged_maiorano_grilo_2)

#species_occ_merged_maiorano_grilo_2[species_occ_merged_maiorano_grilo_2$species_occurrence==focal_sp,]
#id_maiorano_data[id_maiorano_data$rownames.maiorano_metaweb. == focal_sp,]

####################

# 2.3. Creating local networks

local_fw_MAIORANO <- vector(mode = "list", length = ncol(species_in_grids))
names(local_fw_MAIORANO) <- colnames(species_in_grids)

#head(local_fw_MAIORANO)

#species_occ_merged_maiorano_grilo_2

for(i in 1:length(local_fw_MAIORANO)){
  
  grid_id <- names(local_fw_MAIORANO[i]) #GRID ID
  grid_df <- data.frame(rownames(species_in_grids), species_in_grids[,grid_id])
  names(grid_df) <- c("species", "presence")
  grid_df_1 <- id_ocurrence_species_data[id_ocurrence_species_data$species %in% grid_df$species,]
  grid_df <- merge(x=grid_df, y=grid_df_1, by.x="species", by.y="species")
  names(grid_df) <- c("species", "presence", "gbif_id")
  fw_names <- grid_df[grid_df$presence==1,] #SPECIES IN THE GRID
  present_species_id <-  fw_names$gbif_id #SPECIES IN THE GRID
  present_species_id <- species_occ_merged_maiorano_grilo_2[species_occ_merged_maiorano_grilo_2$gbif_id %in% present_species_id,]$gbif_id # keep only those in all datasets
  
  #fw_names <- fw_names[fw_names$gbif_id %in% species_occ_merged_maiorano_grilo_2$gbif_id,] #remove species not in the 3 datasets
  #fw_names <- id_maiorano_data[id_maiorano_data$rownames.maiorano_metaweb. %in% fw_names,]
  
  if (length(present_species_id)!=0){
  
    #Nodes
    nodes1 <- all_species_vulnerability_1[all_species_vulnerability_1$gbif_id %in% present_species_id,]
    names(nodes1)[1] <- "node"
    rownames(nodes1) <- 1:nrow(nodes1)
    
    for(x in 1:nrow(nodes1)){
      #tlinks_df2$resource
      node1_0 <- unique(data.frame(which(nodes1$node[x] == species_occ_merged_maiorano_grilo_2, arr.ind = TRUE))[,1])
      nodes1$node[x] <- species_occ_merged_maiorano_grilo_2[node1_0,2]
      }
    
    #Properties
    prop1 <- list()
    prop1[[1]] <- grid_id
    names(prop1)[1] <- "title" 

        #Trophic links
    #Should have "resource" and "consumer" columns
    if (length(nodes1$node)>1){
      
      tlinks_df2 <- as.data.frame(matrix(ncol = 2, nrow = 0))
      names(tlinks_df2) <- c("resource", "consumer")
      
      for(j in 1:length(nodes1$node)){
        
        focal_sp <- nodes1[j,]
        focal_sp_id <- focal_sp$gbif_id
        focal_sp_occurrence <- species_occ_merged_maiorano_grilo_2[species_occ_merged_maiorano_grilo_2$gbif_id == focal_sp_id,]$species_occurrence
        focal_sp_maiorano <- species_occ_merged_maiorano_grilo_2[species_occ_merged_maiorano_grilo_2$gbif_id == focal_sp_id,]$maiorano_data
        foca_sp_grilo <- species_occ_merged_maiorano_grilo_2[species_occ_merged_maiorano_grilo_2$gbif_id == focal_sp_id,]$grilo_data
        #
        preys3 <- maiorano_metaweb[which(rownames(maiorano_metaweb) == focal_sp_maiorano), ]
        preys3 <- colnames(preys3[,which(preys3[,] == 1)])
        preys3_id <- id_maiorano_data[id_maiorano_data$rownames.maiorano_metaweb. %in% preys3,]
        preys3 <- nodes1$node[nodes1$gbif_id %in% preys3_id$gbif_id]
        #
        predators3 <- maiorano_metaweb[,which(colnames(maiorano_metaweb) == focal_sp_maiorano)]
        predators3 <- data.frame(rownames(maiorano_metaweb), maiorano_metaweb[,which(colnames(maiorano_metaweb) == focal_sp_maiorano)])
        predators3 <- predators3[predators3[,2]==1,]
        predators3 <- predators3[!is.na(predators3$rownames.maiorano_metaweb.),]
        predators3 <- predators3$rownames.maiorano_metaweb.
        predators3_id <- id_maiorano_data[id_maiorano_data$rownames.maiorano_metaweb. %in% predators3,]
        predators3 <- nodes1$node[nodes1$gbif_id %in% predators3_id$gbif_id]
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
        
      }
      
      if(nrow(tlinks_df2)!=0){
        for(b in 1:nrow(tlinks_df2)){
      #tlinks_df2$resource
      tlink_res_0 <- data.frame(which(tlinks_df2$resource[b] == species_occ_merged_maiorano_grilo_2, arr.ind = TRUE))
      tlink_res_0_R <- unique(tlink_res_0$row)
      tlinks_df2$resource[b] <- species_occ_merged_maiorano_grilo_2[tlink_res_0_R,2]
      
      #tlinks_df2$consumer
      tlink_cons_0 <- data.frame(which(tlinks_df2$consumer[b] == species_occ_merged_maiorano_grilo_2, arr.ind = TRUE))
      tlink_cons_0_R <- unique(tlink_cons_0$row)
      tlinks_df2$consumer[b] <- species_occ_merged_maiorano_grilo_2[tlink_cons_0_R,2]
      
        }
        tlinks_df2 <- unique(tlinks_df2)
        
        }
      
      #species_occ_merged_maiorano_grilo_2
      
      if(nrow(tlinks_df2)!=0){ 
        
        comm1_2 <- cheddar::Community(nodes = nodes1, properties = prop1, trophic.links = tlinks_df2)
        
      } else comm1_2 <- cheddar::Community(nodes = nodes1, properties = prop1, trophic.links = NULL)
      
      local_fw_MAIORANO[[i]] <- comm1_2
      
    } else {
      comm2 <- cheddar::Community(nodes = nodes1, properties = prop1, trophic.links = NULL)
      local_fw_MAIORANO[[i]] <- comm2
    }
  } else local_fw_MAIORANO[[i]] <- NA
  
  message(i)
  #gc()
  
}#END

#Verify resulting networks XXXXX sTART

ncol(species_in_grids)
length(local_fw_MAIORANO)

plot(local_fw_MAIORANO[[1]])
plot(local_fw_MAIORANO[[2000]])
plot(local_fw_MAIORANO[[3000]])
plot(local_fw_MAIORANO[[4000]])
plot(local_fw_MAIORANO[[4501]])

class_list <- list()
for(i in 1:length(local_fw_MAIORANO)) class_list[[i]] <- class(local_fw_MAIORANO[[i]])[1]

how_many_species_df <- data.frame(colnames(species_in_grids), colSums(species_in_grids), unlist(class_list))

View(how_many_species_df)

local_fw_MAIORANO[["CJ51"]]
local_fw_MAIORANO[["BC29"]]
local_fw_MAIORANO[["BO21"]]

plot(local_fw_MAIORANO[["CJ40"]])

#Verify resulting networks XXXXX END

#SAVE
save(local_fw_MAIORANO, file = "local_fw_MAIORANO.RData")

rownames(species_in_grids)[species_in_grids[,"BO21"] == 1] %in% species_occ_merged_maiorano_grilo_2
#names(local_fw_MAIORANO)
#head(local_fw_MAIORANO)
