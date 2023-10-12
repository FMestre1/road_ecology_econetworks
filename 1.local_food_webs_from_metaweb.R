################################################################################
################################################################################
#                    SCRIPT 1 - DERIVING LOCAL FOOD WEBS
################################################################################
################################################################################

#FMestre
#September 2023

#Load packages
library(terra)
library(stringr)
library(taxize)
library(cheddar)

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
#all_species_vulnerability_1 <- merge(x=all_species_vulnerability, id_grilo_data, by.x="Species", by.y="all_species_vulnerability.Species", all=T) #creation of id_grilo_data comes later in the code 
#save(all_species_vulnerability_1, file = "all_species_vulnerability_1_grilo.RData")
#nrow(all_species_vulnerability)
#nrow(all_species_vulnerability_1)

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
#View(mammals)
#View(birds)
#nrow(all_species)
#View(all_species)

template_grilo <- terra::vect("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\fernando_26set_2023\\template_grilo.shp")
#plot(template_grilo)
#head(data.frame(template_grilo))

#Create table of species information
#species <- c(birds_vulnerability$Species, mammals_vulnerability$Species)

#species_FAMILY <- tax_name(species, get = 'family', db = 'itis')
#species_GENUS <- tax_name(species, get = 'genus', db = 'itis')
#
#species_df <- data.frame(species,
#                         species_GENUS,
#                         species_FAMILY
#)

#species_df <- species_df[,-c(2:3,5,6)]
#head(species_df)
#save(species_df, file = "species_df.RData")

match_table <- read.csv("match_datasets_master_table.csv", sep = ";")
#View(match_table)

head(match_table)
head(all_species_vulnerability)

master_table_vuln <- merge(x = match_table,
      y = all_species_vulnerability,
      by.x = "Species_grilo",
      by.y = "Species"
      )

names(master_table_vuln)[9] <- "grilo_threshold"
master_table_vuln <- master_table_vuln[,-c(10:12)] 
head(master_table_vuln)
#master_table_vuln$grilo_threshold

#v_mammal <- data.frame(mammals_vulnerability[,1:2], "mammal")
#v_bird <- data.frame(birds_vulnerability[,1:2],"bird")

#names(v_mammal)[3] <- "bm"
#names(v_bird)[3] <- "bm"

#vulnerability <- data.frame(rbind(v_bird, v_mammal))
#save(vulnerability, file = "vulnerability_grilo.RData")

#i_mammal <- data.frame(vuln_mammals[,c(1,5)], "mammal")
#i_bird <- data.frame(vuln_birds[,c(1,5)],"bird")

#names(i_mammal)[3] <- "bm"
#names(i_bird)[3] <- "bm"

#iucn <- data.frame(rbind(i_bird, i_mammal))
#head(iucn)
#tail(iucn)
#View(iucn)

################################################################################
# 1. Maiorano et al. European Metaweb of trophic interactions             
################################################################################

#Maiorano, L., Montemaggiori, A., Ficetola, G. F., O’connor, L., & Thuiller, W. (2020). 
#TETRA‐EU 1.0: a species‐level trophic metaweb of European tetrapods.
#Global Ecology and Biogeography, 29(9), 1452-1457.

maiorano_metaweb <- read.csv("C:\\Users\\asus\\Documents\\0. Posdoc\\CONTRATO\\species_databases\\food_webs_tetrapods_europe\\dataset\\Metaweb_adults.csv", header = T)
rownames(maiorano_metaweb) <- maiorano_metaweb$X
maiorano_metaweb <- maiorano_metaweb[,-1]
#View(maiorano_metaweb)
#save(maiorano_metaweb, file = "maiorano_metaweb.Rdata")

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

################################################################################
#   2.Create a matched table of scientific names across the datasets in use
################################################################################

#rownames(maiorano_metaweb) #maiorano metaweb
#all_species_vulnerability$Species #vulnerability from Clara et al paper

#2.1 Getting the id for the Maiorano Metaweb
id_maiorano_data <- rep(NA, length(rownames(maiorano_metaweb)))

for(i in 1:length(id_maiorano_data)){
  spe2_id <- get_gbifid(sci= rownames(maiorano_metaweb)[i])
  id_maiorano_data[i] <- spe2_id[1]
}

id_maiorano_data <- data.frame(rownames(maiorano_metaweb), id_maiorano_data)
names(id_maiorano_data)[2] <- "gbif_id"
#head(id_maiorano_data)

#2.2 Getting the GBIF id for the Grilo vulnerability data
id_grilo_data <- rep(NA, length(all_species_vulnerability$Species))

for(i in 1:length(id_grilo_data)){
  spe3_id <- get_gbifid(sci= all_species_vulnerability$Species[i])
  id_grilo_data[i] <- spe3_id[1]
}

id_grilo_data <- data.frame(all_species_vulnerability$Species, id_grilo_data)
#head(id_grilo_data)

names(id_grilo_data)[2] <- "gbif_id"

head(id_grilo_data)
head(id_maiorano_data)

#2.3. Using these ids to merge the dataset
species_grilo_merged_maiorano <- merge(id_maiorano_data, id_grilo_data, all=TRUE)
#save(species_grilo_merged_maiorano, file = "species_grilo_merged_maiorano.RData")
#load("species_grilo_merged_maiorano.RData")
#View(species_grilo_merged_maiorano)

species_grilo_merged_maiorano_complete <- species_grilo_merged_maiorano[complete.cases(species_grilo_merged_maiorano),]
names(species_grilo_merged_maiorano_complete) <- c("gbif_id", "maiorano", "grilo")

#species in both datasets
nrow(species_grilo_merged_maiorano_complete)
#species in grilo, not maiorano
table(is.na(species_grilo_merged_maiorano$rownames.maiorano_metaweb.) & !is.na(species_grilo_merged_maiorano$all_species_vulnerability.Species)) 
#species in maiorano, not grilo 
table(!is.na(species_grilo_merged_maiorano$rownames.maiorano_metaweb.) & is.na(species_grilo_merged_maiorano$all_species_vulnerability.Species)) 

################################################################################
#   3.Create a table species taxonomy
################################################################################

head(species_grilo_merged_maiorano_complete) #common species in both datasets matched by GBIFid
head(all_species_vulnerability_1) #species vulnerability

tax_table <- merge(x=all_species_vulnerability_1,
                   y=species_grilo_merged_maiorano_complete,
                   by.x="gbif_id",
                   by.y="gbif_id",
                   all=TRUE
  )

#View(tax_table)

tax_table_2 <- tax_table[complete.cases(tax_table),]
tax_table_2 <- tax_table_2[,-c(2,4:5)]
#View(tax_table_2)

#Get class, order, family

tax_table_2_class <- c()
tax_table_2_order <- c()
tax_table_2_family <- c()

for(i in 1:nrow(tax_table_2)){

  sp_m <- tax_table_2$maiorano[i]
  sp_g <- tax_table_2$grilo[i]
  
  if(!identical(sp_m, sp_g)){
    
    df_tax_m <- taxize::tax_name(sp_m, get = c("class", "order", "family"), db = 'itis')
    df_tax_g <- taxize::tax_name(sp_g, get = c("class", "order", "family"), db = 'itis')
    
    class_m <- df_tax_m$class
    order_m <- df_tax_m$order
    family_m <- df_tax_m$family
    class_g <- df_tax_g$class
    order_g <- df_tax_g$order
    family_g <- df_tax_g$family
    
    if(is.na(class_m) && !is.na(class_g)) class_mg <- class_g
    if(!is.na(class_m) && is.na(class_g)) class_mg <- class_m
    
    if(is.na(family_m) && !is.na(family_g)) family_mg <- family_g 
    if(!is.na(family_m) && is.na(family_g)) family_mg <- family_m
    
    if(is.na(order_m) && !is.na(order_g)) order_mg <- order_g
    if(!is.na(order_m) && is.na(order_g)) order_mg <- order_m
    
    tax_table_2_class[i] <- class_mg
    tax_table_2_order[i] <- order_mg
    tax_table_2_family[i] <- family_mg
    
  }
  
  df_tax <- taxize::tax_name(sp_m, get = c("class", "order", "family"), db = 'itis')
  
  tax_table_2_class[i] <- df_tax$class
  tax_table_2_order[i] <- df_tax$order
  tax_table_2_family[i] <- df_tax$family
  
  message(i)
  
}

tax_table_3 <- data.frame(tax_table_2, 
                          tax_table_2_class, 
                          tax_table_2_order, 
                          tax_table_2_family
                          )

names(tax_table_3) <- c("gbif_id", "grilo_threshold", "IUCN_status_grilo", "species_maiorano",
                        "species_grilo", "class", "order", "family")

#View(tax_table_3)
#head(tax_table_3)

#Save
#save(tax_table_3, file = "tax_table_3_10OUT23.RData")

################################################################################
#   4.Creating local networks
################################################################################

local_fw_MAIORANO <- vector(mode = "list", length = length(template_grilo$PageNumber))
names(local_fw_MAIORANO) <- template_grilo$PageNumber

for(i in 1:length(local_fw_MAIORANO)){#START LOCAL
  
  grid_id <- names(local_fw_MAIORANO[i]) #GRID ID
  #species_in_grids
  grid_df <- data.frame(rownames(species_in_grids), species_in_grids[,grid_id])
  names(grid_df) <- c("species", "presence")
  
  grid_df_1 <- id_grilo_data[id_grilo_data$all_species_vulnerability.Species %in% grid_df$species,]#Those that are in the occurrences dataset
  names(grid_df_1)[1] <- "species"
  grid_df <- merge(x=grid_df, y=grid_df_1, by.x="species", by.y="species")
  names(grid_df) <- c("species", "presence", "gbif_id")
  fw_names <- grid_df[grid_df$presence==1,] #df of species in the grid
  present_species_id <-  fw_names$gbif_id #id of species in the grid
  present_species_id <- species_grilo_merged_maiorano_complete[species_grilo_merged_maiorano_complete$gbif_id %in% present_species_id,]$gbif_id # keep only those in all datasets
  
  #fw_names <- fw_names[fw_names$gbif_id %in% species_occ_merged_maiorano_grilo_2$gbif_id,] #remove species not in the 3 datasets
  #fw_names <- id_maiorano_data[id_maiorano_data$rownames.maiorano_metaweb. %in% fw_names,]
  
  if (length(present_species_id)!=0){
    
    #Nodes
    nodes1 <- all_species_vulnerability_1[all_species_vulnerability_1$gbif_id %in% present_species_id,]
    names(nodes1)[1] <- "node"
    rownames(nodes1) <- 1:nrow(nodes1)
    
    nodes1_maiorano_names <- species_grilo_merged_maiorano_complete[species_grilo_merged_maiorano_complete$grilo %in% nodes1$node,]$maiorano
    tl_modes1 <- metaweb_TL[metaweb_TL$species %in% nodes1_maiorano_names,]
    tl_nodes1_join_grilo_names <- merge(x=species_grilo_merged_maiorano_complete,
          y=tl_modes1,
          by.x="maiorano",
          by.y="species"
    )
    
nodes1 <- merge(x=nodes1,
      y=tl_nodes1_join_grilo_names,
      by.x="node",
      by.y="grilo")

head(nodes1)
nodes1 <- nodes1[,-c(3,4,6)]
names(nodes1)[5] <- "gbif_id"

    for(x in 1:nrow(nodes1)){#Replace the name from that in the occurrence dataset to that in the metaweb - START
            #using this table species_grilo_merged_maiorano_complete
      node1_0 <- unique(data.frame(which(nodes1$gbif_id[x] == species_grilo_merged_maiorano_complete$gbif_id, arr.ind = TRUE))[,1])
      nodes1$node[x] <- species_grilo_merged_maiorano_complete[node1_0,3]
    }#Replace the name from that in the occurrence dataset to that in the metaweb - END
    
    #Properties
    prop1 <- list()
    prop1[[1]] <- grid_id
    names(prop1)[1] <- "title" 
    
    #Trophic links
    #Should have "resource" and "consumer" columns
    if (length(nodes1$node)>1){
      
      #Create base dataframe for the interactions
      tlinks_df2 <- as.data.frame(matrix(ncol = 2, nrow = 0))
      names(tlinks_df2) <- c("resource", "consumer")
      
      #Getting the prey and predator species of the focal species - START
      for(j in 1:length(nodes1$node)){
        
        focal_sp <- nodes1[j,]
        focal_sp_id <- focal_sp$gbif_id
        #focal_sp_occurrence <- species_grilo_merged_maiorano_complete[species_grilo_merged_maiorano_complete$gbif_id == focal_sp_id,]
        focal_sp_maiorano <- species_grilo_merged_maiorano_complete[species_grilo_merged_maiorano_complete$gbif_id == focal_sp_id,]$maiorano
        foca_sp_grilo <- species_grilo_merged_maiorano_complete[species_grilo_merged_maiorano_complete$gbif_id == focal_sp_id,]$grilo
        #
        preys3 <- maiorano_metaweb[which(rownames(maiorano_metaweb) == focal_sp_maiorano), ]
        preys3 <- colnames(preys3[,which(preys3[,] == 1)])
        preys3_id <- id_maiorano_data[id_maiorano_data$rownames.maiorano_metaweb. %in% preys3,]
        preys3 <- nodes1$node[nodes1$gbif_id %in% preys3_id$gbif_id]#only those in the square grid
        #
        predators3 <- maiorano_metaweb[,which(colnames(maiorano_metaweb) == focal_sp_maiorano)]
        predators3 <- data.frame(rownames(maiorano_metaweb), maiorano_metaweb[,which(colnames(maiorano_metaweb) == focal_sp_maiorano)])
        predators3 <- predators3[predators3[,2]==1,]
        predators3 <- predators3[!is.na(predators3$rownames.maiorano_metaweb.),]
        predators3 <- predators3$rownames.maiorano_metaweb.
        predators3_id <- id_maiorano_data[id_maiorano_data$rownames.maiorano_metaweb. %in% predators3,]
        predators3 <- nodes1$node[nodes1$gbif_id %in% predators3_id$gbif_id]#only those in the square grid
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
      
      if(nrow(tlinks_df2)!=0){
        for(b in 1:nrow(tlinks_df2)){
          
          #tlinks_df2$resource
          tlink_res_0 <- data.frame(which(tlinks_df2$resource[b] == species_grilo_merged_maiorano_complete, arr.ind = TRUE))
          tlink_res_0_R <- unique(tlink_res_0$row)
          #tlink_res_0_R <- unique(tlink_res_0$row)
          tlinks_df2$resource[b] <- species_grilo_merged_maiorano_complete[tlink_res_0_R,3]
          
          #tlinks_df2$consumer
          tlink_cons_0 <- data.frame(which(tlinks_df2$consumer[b] == species_grilo_merged_maiorano_complete, arr.ind = TRUE))
          tlink_cons_0_R <- unique(tlink_cons_0$row)
          #tlink_cons_0_R <- unique(tlink_cons_0$row)
          tlinks_df2$consumer[b] <- species_grilo_merged_maiorano_complete[tlink_cons_0_R,3]
          
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
  
}#END LOCAL

#Load & Save
#load("local_fw_MAIORANO_with_metaweb_TL_10OUT23.RData")
#save(local_fw_MAIORANO, file = "local_fw_MAIORANO_with_metaweb_TL_10OUT23.RData")

#head(local_fw_MAIORANO[[1]]$nodes)

#class_list <- list()
#for(i in 1:length(local_fw_MAIORANO)) class_list[[i]] <- class(local_fw_MAIORANO[[i]])[1]
#how_many_species_df <- data.frame(colnames(species_in_grids), colSums(species_in_grids), unlist(class_list))
#View(how_many_species_df)

