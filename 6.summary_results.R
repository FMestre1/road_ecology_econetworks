################################################################################
################################################################################
#                  SCRIPT 6. - RESULTS - SUMMARY
################################################################################
################################################################################

#Results
local_fw_MAIORANO
local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS
local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS
local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE
local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE

#FMestre
#17-10-2023

#1. Table with the number of species per TL in the start and at the end of each simulation #####################

table1_nr_species_steps <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 16))
names(table1_nr_species_steps) <- c(
                                    "grid", 
                                    "START top", 
                                    "START intermediate", 
                                    "START basal",
                                    "AFTER PRIMMARY EXTINCTIONS top", 
                                    "AFTER PRIMMARY EXTINCTIONS intermediate", 
                                    "AFTER PRIMMARY EXTINCTIONS basal",
                                    "AFTER SECONDARY EXTINCTIONS top", 
                                    "AFTER SECONDARY EXTINCTIONS intermediate", 
                                    "AFTER SECONDARY EXTINCTIONS basal",
                                    "FUTURE AFTER PRIMMARY EXTINCTIONS top", 
                                    "FUTURE AFTER PRIMMARY EXTINCTIONS intermediate", 
                                    "FUTURE AFTER PRIMMARY EXTINCTIONS basal",
                                    "FUTURE AFTER SECONDARY EXTINCTIONS top", 
                                    "FUTURE AFTER SECONDARY EXTINCTIONS intermediate", 
                                    "FUTURE AFTER SECONDARY EXTINCTIONS basal"
                                    )

for(i in 1:length(local_fw_MAIORANO)){
  
  net0 <- local_fw_MAIORANO[[i]]
  net1p <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[i]]
  net2p <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[i]]
  net1f <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE[[i]]
  net2f <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE[[i]]
  
  nr_net0 <- data.frame(table(net0$nodes$position))
  nr_net1p <- data.frame(table(net1p$nodes$position))
  nr_net2p <- data.frame(table(net2p$nodes$position))
  nr_net1f <- data.frame(table(net1f$nodes$position))
  nr_net2f <- data.frame(table(net2f$nodes$position))
  ##
  nr_net0_TOP <- as.numeric(nr_net0[nr_net0 == "top",][2])
  nr_net1p_TOP <- as.numeric(nr_net1p[nr_net1p == "top",][2])
  nr_net2p_TOP <- as.numeric(nr_net2p[nr_net2p == "top",][2])
  nr_net1f_TOP <- as.numeric(nr_net1f[nr_net1f == "top",][2])
  nr_net2f_TOP <- as.numeric(nr_net2f[nr_net2f == "top",][2])
  
  nr_net0_MID <- as.numeric(nr_net0[nr_net0 == "intermediate",][2])
  nr_net1p_MID <- as.numeric(nr_net1p[nr_net1p == "intermediate",][2])
  nr_net2p_MID <- as.numeric(nr_net2p[nr_net2p == "intermediate",][2])
  nr_net1f_MID <- as.numeric(nr_net1f[nr_net1f == "intermediate",][2])
  nr_net2f_MID <- as.numeric(nr_net2f[nr_net2f == "intermediate",][2])
  
  nr_net0_BASAL <- as.numeric(nr_net0[nr_net0 == "basal",][2])
  nr_net1p_BASAL <- as.numeric(nr_net1p[nr_net1p == "basal",][2])
  nr_net2p_BASAL <- as.numeric(nr_net2p[nr_net2p == "basal",][2])
  nr_net1f_BASAL <- as.numeric(nr_net1f[nr_net1f == "basal",][2])
  nr_net2f_BASAL <- as.numeric(nr_net2f[nr_net2f == "basal",][2])
  
  table1_nr_species_steps$grid[[i]] <- net0$properties$title
  table1_nr_species_steps$'START top'[[i]] <- nr_net0_TOP
  table1_nr_species_steps$'START intermediate'[[i]] <- nr_net0_MID
  table1_nr_species_steps$'START basal'[[i]] <- nr_net0_BASAL
  
  table1_nr_species_steps$'AFTER PRIMMARY EXTINCTIONS top'[[i]] <- nr_net1p_TOP
  table1_nr_species_steps$'AFTER PRIMMARY EXTINCTIONS intermediate'[[i]] <- nr_net1p_MID
  table1_nr_species_steps$'AFTER PRIMMARY EXTINCTIONS basal'[[i]] <- nr_net1p_BASAL
  
  table1_nr_species_steps$'AFTER SECONDARY EXTINCTIONS top'[[i]] <- nr_net2p_TOP
  table1_nr_species_steps$'AFTER SECONDARY EXTINCTIONS intermediate'[[i]] <- nr_net2p_MID
  table1_nr_species_steps$'AFTER SECONDARY EXTINCTIONS basal'[[i]] <- nr_net2p_BASAL
  
  table1_nr_species_steps$'FUTURE AFTER PRIMMARY EXTINCTIONS top'[[i]] <- nr_net1f_TOP
  table1_nr_species_steps$'FUTURE AFTER PRIMMARY EXTINCTIONS intermediate'[[i]] <- nr_net1f_MID
  table1_nr_species_steps$'FUTURE AFTER PRIMMARY EXTINCTIONS basal'[[i]] <- nr_net1f_BASAL
  
  table1_nr_species_steps$'FUTURE AFTER SECONDARY EXTINCTIONS top'[[i]] <- nr_net2f_TOP
  table1_nr_species_steps$'FUTURE AFTER SECONDARY EXTINCTIONS intermediate'[[i]] <- nr_net2f_MID
  table1_nr_species_steps$'FUTURE AFTER SECONDARY EXTINCTIONS basal'[[i]] <- nr_net2f_BASAL
  
message(i)

}

#View(table1_nr_species_steps)
#write.csv(table1_nr_species_steps, file = "C:\\Users\\asus\\Desktop\\table1_nr_species_steps_09NOV.csv")


#2. Table with the proportion of species per TL in the start and at the end of each simulation #####################

table2_proportion_species_steps <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 16))
names(table2_proportion_species_steps) <- c(
  "grid", 
  "START top", 
  "START intermediate", 
  "START basal",
  "AFTER PRIMMARY EXTINCTIONS top", 
  "AFTER PRIMMARY EXTINCTIONS intermediate", 
  "AFTER PRIMMARY EXTINCTIONS basal",
  "AFTER SECONDARY EXTINCTIONS top", 
  "AFTER SECONDARY EXTINCTIONS intermediate", 
  "AFTER SECONDARY EXTINCTIONS basal",
  "FUTURE AFTER PRIMMARY EXTINCTIONS top", 
  "FUTURE AFTER PRIMMARY EXTINCTIONS intermediate", 
  "FUTURE AFTER PRIMMARY EXTINCTIONS basal",
  "FUTURE AFTER SECONDARY EXTINCTIONS top", 
  "FUTURE AFTER SECONDARY EXTINCTIONS intermediate", 
  "FUTURE AFTER SECONDARY EXTINCTIONS basal"
)

for(i in 1:length(local_fw_MAIORANO)){
  
  net0 <- local_fw_MAIORANO[[i]]
  net1p <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[i]]
  net2p <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[i]]
  net1f <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE[[i]]
  net2f <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE[[i]]
  
  nr_net0 <- data.frame(table(net0$nodes$position))
  nr_net1p <- data.frame(table(net1p$nodes$position))
  nr_net2p <- data.frame(table(net2p$nodes$position))
  nr_net1f <- data.frame(table(net1f$nodes$position))
  nr_net2f <- data.frame(table(net2f$nodes$position))
  
  nr_net0_TOP <- as.numeric(nr_net0[nr_net0 == "top",][2])
  nr_net1p_TOP <- as.numeric(nr_net1p[nr_net1p == "top",][2])
  nr_net2p_TOP <- as.numeric(nr_net2p[nr_net2p == "top",][2])
  nr_net1f_TOP <- as.numeric(nr_net1f[nr_net1f == "top",][2])
  nr_net2f_TOP <- as.numeric(nr_net2f[nr_net2f == "top",][2])
  
  nr_net0_MID <- as.numeric(nr_net0[nr_net0 == "intermediate",][2])
  nr_net1p_MID <- as.numeric(nr_net1p[nr_net1p == "intermediate",][2])
  nr_net2p_MID <- as.numeric(nr_net2p[nr_net2p == "intermediate",][2])
  nr_net1f_MID <- as.numeric(nr_net1f[nr_net1f == "intermediate",][2])
  nr_net2f_MID <- as.numeric(nr_net2f[nr_net2f == "intermediate",][2])
  
  nr_net0_BASAL <- as.numeric(nr_net0[nr_net0 == "basal",][2])
  nr_net1p_BASAL <- as.numeric(nr_net1p[nr_net1p == "basal",][2])
  nr_net2p_BASAL <- as.numeric(nr_net2p[nr_net2p == "basal",][2])
  nr_net1f_BASAL <- as.numeric(nr_net1f[nr_net1f == "basal",][2])
  nr_net2f_BASAL <- as.numeric(nr_net2f[nr_net2f == "basal",][2])
  
  table2_proportion_species_steps$grid[[i]] <- net0$properties$title
  table2_proportion_species_steps$'START top'[[i]] <- nr_net0_TOP
  table2_proportion_species_steps$'START intermediate'[[i]] <- nr_net0_MID
  table2_proportion_species_steps$'START basal'[[i]] <- nr_net0_BASAL
  
  table2_proportion_species_steps$'AFTER PRIMMARY EXTINCTIONS top'[[i]] <- (nr_net0_TOP-nr_net1p_TOP)/nr_net0_TOP
  table2_proportion_species_steps$'AFTER PRIMMARY EXTINCTIONS intermediate'[[i]] <- (nr_net0_MID-nr_net1p_MID)/nr_net0_MID
  table2_proportion_species_steps$'AFTER PRIMMARY EXTINCTIONS basal'[[i]] <- (nr_net0_BASAL-nr_net1p_BASAL)/nr_net0_BASAL
  
  table2_proportion_species_steps$'AFTER SECONDARY EXTINCTIONS top'[[i]] <- (nr_net1p_TOP-nr_net2p_TOP)/nr_net1p_TOP
  table2_proportion_species_steps$'AFTER SECONDARY EXTINCTIONS intermediate'[[i]] <- (nr_net1p_MID-nr_net2p_MID)/nr_net1p_MID
  table2_proportion_species_steps$'AFTER SECONDARY EXTINCTIONS basal'[[i]] <- (nr_net1p_BASAL-nr_net2p_BASAL)/nr_net1p_BASAL
  
  table2_proportion_species_steps$'FUTURE AFTER PRIMMARY EXTINCTIONS top'[[i]] <- (nr_net0_TOP-nr_net1f_TOP)/nr_net0_TOP
  table2_proportion_species_steps$'FUTURE AFTER PRIMMARY EXTINCTIONS intermediate'[[i]] <- (nr_net0_MID-nr_net1f_MID)/nr_net0_MID
  table2_proportion_species_steps$'FUTURE AFTER PRIMMARY EXTINCTIONS basal'[[i]] <- (nr_net0_BASAL-nr_net1f_BASAL)/nr_net0_BASAL
  
  table2_proportion_species_steps$'FUTURE AFTER SECONDARY EXTINCTIONS top'[[i]] <- (nr_net1f_TOP-nr_net2f_TOP)/nr_net1f_TOP
  table2_proportion_species_steps$'FUTURE AFTER SECONDARY EXTINCTIONS intermediate'[[i]] <- (nr_net1f_MID-nr_net2f_MID)/nr_net1f_MID
  table2_proportion_species_steps$'FUTURE AFTER SECONDARY EXTINCTIONS basal'[[i]] <- (nr_net1f_BASAL-nr_net2f_BASAL)/nr_net1f_BASAL
  
  message(i)
  
}

#View(table2_proportion_species_steps)
#write.csv(table2_proportion_species_steps, file = "C:\\Users\\asus\\Desktop\\table2_proportion_species_steps_09NOV.csv")


#3. Table with the average trophic height (TH) and that of remaining and extinct species ######################

table3_trophic_height <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 14))
names(table3_trophic_height) <- c(
                                    "grid", 
                                    "START AVERAGE trophic height",
                                    "AFTER PRIMMARY EXTINCTIONS average TH", 
                                    "AFTER PRIMMARY EXTINCTIONS average TH of remaining species", 
                                    "AFTER PRIMMARY EXTINCTIONS average TH of extinct species", 
                                    "AFTER SECONDARY EXTINCTIONS average TH", 
                                    "AFTER SECONDARY EXTINCTIONS average TH of remaining species",
                                    "AFTER SECONDARY EXTINCTIONS average TH of extinct species", 
                                    "FUTURE - AFTER PRIMMARY EXTINCTIONS average TH", 
                                    "FUTURE - AFTER PRIMMARY EXTINCTIONS average TH of remaining species",
                                    "FUTURE - AFTER PRIMMARY EXTINCTIONS average TH of extinct species", 
                                    "FUTURE - AFTER SECONDARY EXTINCTIONS average TH", 
                                    "FUTURE - AFTER SECONDARY EXTINCTIONS average TH of remaining species",
                                    "FUTURE - AFTER SECONDARY EXTINCTIONS average TH of extinct species"
                                    )


for(i in 1:length(local_fw_MAIORANO)){
  
  net0 <- local_fw_MAIORANO[[i]]
  net1p <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[i]]
  net2p <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[i]]
  net1f <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE[[i]]
  net2f <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE[[i]]
  

  av_tl_net0 <- mean(net0$nodes$TL)
  av_tl_net1p <- mean(net1p$nodes$TL)
  av_tl_net2p <- mean(net2p$nodes$TL)
  av_tl_net1f <- mean(net1f$nodes$TL)
  av_tl_net2f <- mean(net2f$nodes$TL)
  
  #From start to primary extinctions present
  remaining_from_start_to_ptim <- mean(net0$nodes[(net0$nodes$node %in% net1p$nodes$node),]$TL) 
  removed_from_start_to_ptim <- mean(net0$nodes[!(net0$nodes$node %in% net1p$nodes$node),]$TL) 
  
  #From primary to secondary extinctions present
  remaining_from_primmary_to_sec <- mean(net1p$nodes[(net1p$nodes$node %in% net2p$nodes$node),]$TL) 
  removed_from_primmary_to_sec <- mean(net1p$nodes[!(net1p$nodes$node %in% net2p$nodes$node),]$TL) 
  
  #From secondary extinctions in the present to primary extinctions future
  remaining_from_secondary_to_primm_f <- mean(net2p$nodes[(net2p$nodes$node %in% net1f$nodes$node),]$TL) 
  removed_from_secondary_to_primm_f <- mean(net2p$nodes[!(net2p$nodes$node %in% net1f$nodes$node),]$TL)
  
  #From primary extinctions to secondary future
  remaining_from_primmary_f_to_sec_f <- mean(net1f$nodes[(net1f$nodes$node %in% net2f$nodes$node),]$TL) 
  removed_from_primmary_f_to_sec_f <- mean(net1f$nodes[!(net1f$nodes$node %in% net2f$nodes$node),]$TL)
  
  table3_trophic_height$grid[[i]] <- net0$properties$title
  table3_trophic_height$`START AVERAGE trophic height`[[i]] <- av_tl_net0
  table3_trophic_height$`AFTER PRIMMARY EXTINCTIONS average TH`[[i]] <- av_tl_net1p
  table3_trophic_height$`AFTER PRIMMARY EXTINCTIONS average TH of remaining species`[[i]] <- remaining_from_start_to_ptim
  table3_trophic_height$`AFTER PRIMMARY EXTINCTIONS average TH of extinct species`[[i]] <- removed_from_start_to_ptim  
  #  
  table3_trophic_height$`AFTER SECONDARY EXTINCTIONS average TH`[[i]] <- av_tl_net2p
  table3_trophic_height$`AFTER SECONDARY EXTINCTIONS average TH of remaining species`[[i]] <- remaining_from_primmary_to_sec
  table3_trophic_height$`AFTER SECONDARY EXTINCTIONS average TH of extinct species`[[i]] <- removed_from_primmary_to_sec
  #  
  table3_trophic_height$`FUTURE - AFTER PRIMMARY EXTINCTIONS average TH`[[i]] <- av_tl_net1f
  table3_trophic_height$`FUTURE - AFTER PRIMMARY EXTINCTIONS average TH of remaining species`[[i]] <- remaining_from_secondary_to_primm_f
  table3_trophic_height$`FUTURE - AFTER PRIMMARY EXTINCTIONS average TH of extinct species`[[i]] <- removed_from_secondary_to_primm_f
  #
  table3_trophic_height$`FUTURE - AFTER SECONDARY EXTINCTIONS average TH`[[i]] <- av_tl_net2f
  table3_trophic_height$`FUTURE - AFTER SECONDARY EXTINCTIONS average TH of remaining species`[[i]] <- remaining_from_primmary_f_to_sec_f
  table3_trophic_height$`FUTURE - AFTER SECONDARY EXTINCTIONS average TH of extinct species`[[i]] <- removed_from_primmary_f_to_sec_f

  message(i)
  
}

#View(table3_trophic_height)
#write.csv(table3_trophic_height, file = "C:\\Users\\asus\\Desktop\\table3_trophic_height_09NOV.csv")


#4. Table with the number of interactions in the start and at the end of each simulation ######################

table4_nr_interactions_steps <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 6))
names(table4_nr_interactions_steps) <- c(
                                    "grid", 
                                    "START number of interactions",
                                    "AFTER PRIMMARY EXTINCTIONS",
                                    "AFTER SECONDARY EXTINCTIONS",
                                    "FUTURE - AFTER PRIMMARY EXTINCTIONS",
                                    "FUTURE - AFTER SECONDARY EXTINCTIONS"
                                    )


for(i in 1:length(local_fw_MAIORANO)){
  
  net0 <- local_fw_MAIORANO[[i]]
  net1p <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[i]]
  net2p <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[i]]
  net1f <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE[[i]]
  net2f <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE[[i]]
  
  nlinks_net0 <- cheddar::NumberOfTrophicLinks(net0)
  nlinks_net1p <- cheddar::NumberOfTrophicLinks(net1p)
  nlinks_net2p <- cheddar::NumberOfTrophicLinks(net2p)
  nlinks_net1f <- cheddar::NumberOfTrophicLinks(net1f)
  nlinks_net2f <- cheddar::NumberOfTrophicLinks(net2f)
  
  table4_nr_interactions_steps$grid[[i]] <- net0$properties$title
  table4_nr_interactions_steps$`START number of interactions`[[i]] <- nlinks_net0
  table4_nr_interactions_steps$`AFTER PRIMMARY EXTINCTIONS`[[i]] <- nlinks_net1p
  table4_nr_interactions_steps$`AFTER SECONDARY EXTINCTIONS`[[i]] <- nlinks_net2p
  table4_nr_interactions_steps$`FUTURE - AFTER PRIMMARY EXTINCTIONS`[[i]] <- nlinks_net1f
  table4_nr_interactions_steps$`FUTURE - AFTER SECONDARY EXTINCTIONS`[[i]] <- nlinks_net2f
  
  
  message(i)
  
}

#View(table4_nr_interactions_steps)
#write.csv(table4_nr_interactions_steps, file = "C:\\Users\\asus\\Desktop\\table4_nr_interactions_steps_09NOV.csv")


#5. Table with the proportion of interactions lost in each step ######################

table5_proportion_interactions_lost_steps <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 6))
names(table5_proportion_interactions_lost_steps) <- c(
  "grid", 
  "START",
  "AFTER PRIMMARY EXTINCTIONS", 
  "AFTER SECONDARY EXTINCTIONS", 
  "FUTURE AFTER PRIMMARY EXTINCTIONS", 
  "FUTURE AFTER SECONDARY EXTINCTIONS"
)


for(i in 1:length(local_fw_MAIORANO)){
  
  net0 <- local_fw_MAIORANO[[i]]
  net1p <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[i]]
  net2p <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[i]]
  net1f <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE[[i]]
  net2f <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE[[i]]
  
  nlinks_net0 <- cheddar::NumberOfTrophicLinks(net0)
  nlinks_net1p <- cheddar::NumberOfTrophicLinks(net1p)
  nlinks_net2p <- cheddar::NumberOfTrophicLinks(net2p)
  nlinks_net1f <- cheddar::NumberOfTrophicLinks(net1f)
  nlinks_net2f <- cheddar::NumberOfTrophicLinks(net2f)

  table5_proportion_interactions_lost_steps$grid[[i]] <- net0$properties$title
  table5_proportion_interactions_lost_steps$START[[i]] <- nlinks_net0
  table5_proportion_interactions_lost_steps$`AFTER PRIMMARY EXTINCTIONS`[[i]] <- (nlinks_net0-nlinks_net1p)/nlinks_net0 
  table5_proportion_interactions_lost_steps$`AFTER SECONDARY EXTINCTIONS`[[i]] <- (nlinks_net1p-nlinks_net2p)/nlinks_net1p
  table5_proportion_interactions_lost_steps$`FUTURE AFTER PRIMMARY EXTINCTIONS`[[i]] <- (nlinks_net0-nlinks_net1f)/nlinks_net0
  table5_proportion_interactions_lost_steps$`FUTURE AFTER SECONDARY EXTINCTIONS`[[i]] <- (nlinks_net1f-nlinks_net2f)/nlinks_net1f
  
  message(i)
  
}

#View(table5_proportion_interactions_lost_steps)
#write.csv(table5_proportion_interactions_lost_steps, file = "C:\\Users\\asus\\Desktop\\table5_proportion_interactions_lost_steps_09NOV.csv")

#6. Number of grids where each species is present #############################################################

comm_colection <- cheddar::CommunityCollection(local_fw_MAIORANO)
species <- cheddar::CollectionNPS(comm_colection)
species <- unique(species$node)

table6_grids_of_presence_per_species <- data.frame(matrix(nrow=length(species), ncol = 6))
names(table6_grids_of_presence_per_species) <- c("SPECIES", 
                                          "NUMBER OF SQUARE GRIDS AT START",
                                          "NUMBER OF GRIDS WHERE IT IS PRESENT AFTER START TO PRIMARY EXTINCTIONS", 
                                          "NUMBER OF GRIDS WHERE IT IS PRESENT AFTER PRIMARY TO SECONDARY EXTINCTIONS", 
                                          "NUMBER OF GRIDS WHERE IT IS PRESENT AFTER SECONDARY TO PRIMARY FUTURE", 
                                          "NUMBER OF GRIDS WHERE IT IS PRESENT AFTER PRIMARY FUTURE TO SECONDARY FUTURE" 
                                          )

table6_grids_of_presence_per_species[,] <- 0
table6_grids_of_presence_per_species$SPECIES <- species

for(i in 1:length(local_fw_MAIORANO)){
  
  net0 <- local_fw_MAIORANO[[i]]
  net1p <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[i]]
  net2p <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[i]]
  net1f <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE[[i]]
  net2f <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE[[i]]
  
  sp_net0 <- net0$nodes$node
  sp_net1p <- net1p$nodes$node
  sp_net2p <- net2p$nodes$node
  sp_net1f <- net1f$nodes$node
  sp_net2f <- net2f$nodes$node

  for(j in 1:length(sp_net0)){
    sp_net0_sp <- sp_net0[[j]]
    prev_sp_net0 <- table6_grids_of_presence_per_species[table6_grids_of_presence_per_species$SPECIES == sp_net0_sp,]$`NUMBER OF SQUARE GRIDS AT START`
    table6_grids_of_presence_per_species[table6_grids_of_presence_per_species$SPECIES == sp_net0_sp,]$`NUMBER OF SQUARE GRIDS AT START` <- prev_sp_net0 + 1
  }
  
  for(l in 1:length(sp_net1p)){
    sp_net1p_sp <- sp_net1p[[l]]
    prev_sp_net1p <- table6_grids_of_presence_per_species[table6_grids_of_presence_per_species$SPECIES == sp_net1p_sp,]$`NUMBER OF GRIDS WHERE IT IS PRESENT AFTER START TO PRIMARY EXTINCTIONS`
    table6_grids_of_presence_per_species[table6_grids_of_presence_per_species$SPECIES == sp_net1p_sp,]$`NUMBER OF GRIDS WHERE IT IS PRESENT AFTER START TO PRIMARY EXTINCTIONS` <- prev_sp_net1p + 1
  }
  
  for(m in 1:length(sp_net2p)){
    sp_net2p_sp <- sp_net2p[[m]]
    prev_sp_net2p <- table6_grids_of_presence_per_species[table6_grids_of_presence_per_species$SPECIES == sp_net2p_sp,]$`NUMBER OF GRIDS WHERE IT IS PRESENT AFTER PRIMARY TO SECONDARY EXTINCTIONS`
    table6_grids_of_presence_per_species[table6_grids_of_presence_per_species$SPECIES == sp_net2p_sp,]$`NUMBER OF GRIDS WHERE IT IS PRESENT AFTER PRIMARY TO SECONDARY EXTINCTIONS` <- prev_sp_net2p + 1
  }

  for(n in 1:length(sp_net1f)){
    sp_net1f_sp <- sp_net1f[[n]]
    prev_sp_net1f <- table6_grids_of_presence_per_species[table6_grids_of_presence_per_species$SPECIES == sp_net1f_sp,]$`NUMBER OF GRIDS WHERE IT IS PRESENT AFTER SECONDARY TO PRIMARY FUTURE`
    table6_grids_of_presence_per_species[table6_grids_of_presence_per_species$SPECIES == sp_net1f_sp,]$`NUMBER OF GRIDS WHERE IT IS PRESENT AFTER SECONDARY TO PRIMARY FUTURE` <- prev_sp_net1f + 1
  }

  for(o in 1:length(sp_net2f)){
    sp_net2f_sp <- sp_net2f[[o]]
    prev_sp_net2f <- table6_grids_of_presence_per_species[table6_grids_of_presence_per_species$SPECIES == sp_net2f_sp,]$`NUMBER OF GRIDS WHERE IT IS PRESENT AFTER PRIMARY FUTURE TO SECONDARY FUTURE`
    table6_grids_of_presence_per_species[table6_grids_of_presence_per_species$SPECIES == sp_net2f_sp,]$`NUMBER OF GRIDS WHERE IT IS PRESENT AFTER PRIMARY FUTURE TO SECONDARY FUTURE` <- prev_sp_net2f + 1
  }
  
  message(i)
  
}

#View(table6_grids_of_presence_per_species)
#write.csv(table6_grids_of_presence_per_species, file = "C:\\Users\\asus\\Desktop\\table6_grids_of_presence_per_species_09NOV.csv")


#7. Number of grids lost per species in each step  ######################################################################

table7_grids_lost_per_species <- data.frame(matrix(nrow=length(species), ncol = 5))
names(table7_grids_lost_per_species) <- c("SPECIES", 
                                          "GRIDS LOST FROM START TO PRIMARY EXTINCTIONS", 
                                          "GRIDS LOST FROM PRIMARY TO SECONDARY EXTINCTIONS", 
                                          "GRIDS LOST FROM START TO PRIMARY FUTURE", 
                                          "GRIDS LOST FROM PRIMARY FUTURE TO SECONDARY FUTURE" 
)

names(table6_grids_of_presence_per_species)

table7_grids_lost_per_species$SPECIES <- species
table7_grids_lost_per_species$`GRIDS LOST FROM START TO PRIMARY EXTINCTIONS` <- table6_grids_of_presence_per_species[,2] - table6_grids_of_presence_per_species[,3]
table7_grids_lost_per_species$`GRIDS LOST FROM PRIMARY TO SECONDARY EXTINCTIONS`<- table6_grids_of_presence_per_species[,3] - table6_grids_of_presence_per_species[,4]
table7_grids_lost_per_species$`GRIDS LOST FROM START TO PRIMARY FUTURE` <- table6_grids_of_presence_per_species[,2] - table6_grids_of_presence_per_species[,5]
table7_grids_lost_per_species$`GRIDS LOST FROM PRIMARY FUTURE TO SECONDARY FUTURE`<- table6_grids_of_presence_per_species[,5] - table6_grids_of_presence_per_species[,6]

#View(table7_grids_lost_per_species)
#write.csv(table7_grids_lost_per_species, file = "C:\\Users\\asus\\Desktop\\table7_grids_lost_per_species_09NOV.csv")



#8. Number of  interactions per species as predator and prey in each step  ######################################################################


COMM_COLL_local_fw_MAIORANO <- cheddar::CommunityCollection(local_fw_MAIORANO)
COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS <- cheddar::CommunityCollection(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS)
COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS <- cheddar::CommunityCollection(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS)
COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE <- cheddar::CommunityCollection(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE)
COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE <- cheddar::CommunityCollection(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE)

tlinks_COMM_COLL_local_fw_MAIORANO <- cheddar::CollectionTLPS(COMM_COLL_local_fw_MAIORANO)
tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS <- cheddar::CollectionTLPS(COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS)
tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS <- cheddar::CollectionTLPS(COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS)
tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE <- cheddar::CollectionTLPS(COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE)
tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE <- cheddar::CollectionTLPS(COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE)

table8_interactions_lost_per_species <- data.frame(matrix(nrow=length(species), ncol = 11))
names(table8_interactions_lost_per_species) <- c(
  "SPECIES", 
  "INTERACTIONS AS PREDATOR AT START",
  "INTERACTIONS AS PREY AT START",
  "INTERACTIONS AS PREDATOR AFTER PRIMARY EXTINCTIONS",
  "INTERACTIONS AS PREY AFTER PRIMARY EXTINCTIONS",
  "INTERACTIONS AS PREDATOR AFTER SECONDARY EXTINCTIONS",
  "INTERACTIONS AS PREY AFTER SECONDARY EXTINCTIONS",
  "INTERACTIONS AS PREDATOR AFTER PRIMARY FUTURE EXTINCTIONS",
  "INTERACTIONS AS PREY AFTER PRIMARY FUTURE EXTINCTIONS",
  "INTERACTIONS AS PREDATOR AFTER SECONDARY FUTURE EXTINCTIONS",
  "INTERACTIONS AS PREY AFTER SECONDARY FUTURE EXTINCTIONS"
)

table8_interactions_lost_per_species[,] <- 0
table8_interactions_lost_per_species$SPECIES <- species

for(i in 1:length(species)){
  
  table8_interactions_lost_per_species[which(table8_interactions_lost_per_species[,1] == species[i]),]$'INTERACTIONS AS PREDATOR AT START' <- sum(tlinks_COMM_COLL_local_fw_MAIORANO$consumer == species[i])
  table8_interactions_lost_per_species[which(table8_interactions_lost_per_species[,1] == species[i]),]$'INTERACTIONS AS PREY AT START' <- sum(tlinks_COMM_COLL_local_fw_MAIORANO$resource == species[i])
  
  table8_interactions_lost_per_species[which(table8_interactions_lost_per_species[,1] == species[i]),]$'INTERACTIONS AS PREDATOR AFTER PRIMARY EXTINCTIONS' <- sum(tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS$consumer == species[i])
  table8_interactions_lost_per_species[which(table8_interactions_lost_per_species[,1] == species[i]),]$'INTERACTIONS AS PREY AFTER PRIMARY EXTINCTIONS' <- sum(tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS$resource == species[i])
  
  table8_interactions_lost_per_species[which(table8_interactions_lost_per_species[,1] == species[i]),]$'INTERACTIONS AS PREDATOR AFTER SECONDARY EXTINCTIONS' <- sum(tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS$consumer == species[i])
  table8_interactions_lost_per_species[which(table8_interactions_lost_per_species[,1] == species[i]),]$'INTERACTIONS AS PREY AFTER SECONDARY EXTINCTIONS' <- sum(tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS$resource == species[i])
  
  table8_interactions_lost_per_species[which(table8_interactions_lost_per_species[,1] == species[i]),]$'INTERACTIONS AS PREDATOR AFTER PRIMARY FUTURE EXTINCTIONS' <- sum(tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE$consumer == species[i])
  table8_interactions_lost_per_species[which(table8_interactions_lost_per_species[,1] == species[i]),]$'INTERACTIONS AS PREY AFTER PRIMARY FUTURE EXTINCTIONS' <- sum(tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE$resource == species[i])
  
  table8_interactions_lost_per_species[which(table8_interactions_lost_per_species[,1] == species[i]),]$'INTERACTIONS AS PREDATOR AFTER SECONDARY FUTURE EXTINCTIONS' <- sum(tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE$consumer == species[i])
  table8_interactions_lost_per_species[which(table8_interactions_lost_per_species[,1] == species[i]),]$'INTERACTIONS AS PREY AFTER SECONDARY FUTURE EXTINCTIONS' <- sum(tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE$resource == species[i])
  
  message(i)
  
}

#View(table8_interactions_lost_per_species)
#write.csv(table8_interactions_lost_per_species, file = "C:\\Users\\asus\\Desktop\\table8_interactions_lost_per_species_09NOV.csv")

#9. Number of  lost interactions per species as predator and prey in each step  ######################################################################

table9_interactions_lost_per_species <- data.frame(matrix(nrow=length(species), ncol = 9))
names(table9_interactions_lost_per_species) <- c("SPECIES", 
                                                 "INTERACTIONS LOST FROM START TO PRIMARY EXTINCTIONS AS PREDATOR",
                                                 "INTERACTIONS LOST FROM START TO PRIMARY EXTINCTIONS AS PREY", 
                                                 "INTERACTIONS LOST FROM PRIMARY TO SECONDARY EXTINCTIONS AS PREDATOR", 
                                                 "INTERACTIONS LOST FROM PRIMARY TO SECONDARY EXTINCTIONS AS PREY", 
                                                 "INTERACTIONS LOST FROM START TO PRIMARY FUTURE AS PREDATOR", 
                                                 "INTERACTIONS LOST FROM START TO PRIMARY FUTURE AS PREY", 
                                                 "INTERACTIONS LOST FROM PRIMARY FUTURE TO SECONDARY FUTURE AS PREDATOR", 
                                                 "INTERACTIONS LOST FROM PRIMARY FUTURE TO SECONDARY FUTURE AS PREY" 
)

table9_interactions_lost_per_species[,] <- 0
table9_interactions_lost_per_species$SPECIES <- species
#
table9_interactions_lost_per_species$`INTERACTIONS LOST FROM START TO PRIMARY EXTINCTIONS AS PREDATOR` <- table8_interactions_lost_per_species$`INTERACTIONS AS PREDATOR AT START` - table8_interactions_lost_per_species$`INTERACTIONS AS PREDATOR AFTER PRIMARY EXTINCTIONS`
table9_interactions_lost_per_species$`INTERACTIONS LOST FROM START TO PRIMARY EXTINCTIONS AS PREY` <- table8_interactions_lost_per_species$`INTERACTIONS AS PREY AT START` - table8_interactions_lost_per_species$`INTERACTIONS AS PREY AFTER PRIMARY EXTINCTIONS`
table9_interactions_lost_per_species$`INTERACTIONS LOST FROM PRIMARY TO SECONDARY EXTINCTIONS AS PREDATOR` <- table8_interactions_lost_per_species$`INTERACTIONS AS PREDATOR AFTER PRIMARY EXTINCTIONS` - table8_interactions_lost_per_species$`INTERACTIONS AS PREDATOR AFTER SECONDARY EXTINCTIONS`
table9_interactions_lost_per_species$`INTERACTIONS LOST FROM PRIMARY TO SECONDARY EXTINCTIONS AS PREY` <- table8_interactions_lost_per_species$`INTERACTIONS AS PREY AFTER PRIMARY EXTINCTIONS` - table8_interactions_lost_per_species$`INTERACTIONS AS PREY AFTER SECONDARY EXTINCTIONS`
table9_interactions_lost_per_species$`INTERACTIONS LOST FROM START TO PRIMARY FUTURE AS PREDATOR` <- table8_interactions_lost_per_species$`INTERACTIONS AS PREDATOR AT START` - table8_interactions_lost_per_species$`INTERACTIONS AS PREDATOR AFTER PRIMARY FUTURE EXTINCTIONS`
table9_interactions_lost_per_species$`INTERACTIONS LOST FROM START TO PRIMARY FUTURE AS PREY` <- table8_interactions_lost_per_species$`INTERACTIONS AS PREY AT START` - table8_interactions_lost_per_species$`INTERACTIONS AS PREY AFTER PRIMARY FUTURE EXTINCTIONS`
table9_interactions_lost_per_species$`INTERACTIONS LOST FROM PRIMARY FUTURE TO SECONDARY FUTURE AS PREDATOR`<- table8_interactions_lost_per_species$`INTERACTIONS AS PREDATOR AFTER PRIMARY FUTURE EXTINCTIONS` - table8_interactions_lost_per_species$`INTERACTIONS AS PREDATOR AFTER SECONDARY FUTURE EXTINCTIONS`
table9_interactions_lost_per_species$`INTERACTIONS LOST FROM PRIMARY FUTURE TO SECONDARY FUTURE AS PREY` <- table8_interactions_lost_per_species$`INTERACTIONS AS PREY AFTER PRIMARY FUTURE EXTINCTIONS` - table8_interactions_lost_per_species$`INTERACTIONS AS PREY AFTER SECONDARY FUTURE EXTINCTIONS`

#View(table9_interactions_lost_per_species)
#write.csv(table9_interactions_lost_per_species, file = "C:\\Users\\asus\\Desktop\\table9_interactions_lost_per_species_09NOV.csv")




#10 to 14. Full tables of links, per grid, and link   ######################################################################

COMM_COLL_local_fw_MAIORANO <- cheddar::CommunityCollection(local_fw_MAIORANO)
COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS <- cheddar::CommunityCollection(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS)
COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS <- cheddar::CommunityCollection(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS)
COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE <- cheddar::CommunityCollection(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE)
COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE <- cheddar::CommunityCollection(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE)

tlinks_COMM_COLL_local_fw_MAIORANO <- cheddar::CollectionTLPS(COMM_COLL_local_fw_MAIORANO)
tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS <- cheddar::CollectionTLPS(COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS)
tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS <- cheddar::CollectionTLPS(COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS)
tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE <- cheddar::CollectionTLPS(COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE)
tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE <- cheddar::CollectionTLPS(COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE)

#write.csv(tlinks_COMM_COLL_local_fw_MAIORANO, file = "C:\\Users\\asus\\Desktop\\table_10_all_links_local_fw_MAIORANO_09NOV23.csv")
#write.csv(tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS, file = "C:\\Users\\asus\\Desktop\\table_11_all_links_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_09NOV23.csv")
#write.csv(tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS, file = "C:\\Users\\asus\\Desktop\\table_12_all_links_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_09NOV23.csv")
#write.csv(tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE, file = "C:\\Users\\asus\\Desktop\\table_13_all_links_local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE_09NOV23.csv")
#write.csv(tlinks_COMM_COLL_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE, file = "C:\\Users\\asus\\Desktop\\table_14_all_links_local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE_09NOV23.csv")

#15. Number of  lost interactions per TL as predator and prey in each step   ######################################################################

table15_lost_per_TL <- merge(x = table9_interactions_lost_per_species,
      y = overall_previous_positions,
      by.x = "SPECIES",
      by.y = "species",
      all = FALSE)

names(table15_lost_per_TL)
table15_lost_per_TL$position <- as.factor(table15_lost_per_TL$position)
table15_lost_per_TL_2 <- group_by(table15_lost_per_TL, position)
table15_lost_per_TL_2 <- table15_lost_per_TL_2[,-1]
table15_lost_per_TL_3 <- as.data.frame(summarise(table15_lost_per_TL_2, across(everything(), sum)))

#View(table15_lost_per_TL_3)
#write.csv(table15_lost_per_TL_3, file = "C:\\Users\\asus\\Desktop\\table15_lost_per_TL_09NOV23.csv")





#16. Full information table   ######################################################################


local_fw_MAIORANO
local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS
local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS
local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_FUTURE
local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_FUTURE
#
grids_grilo # vulnerability information
paired_pagename_pagenumber #pair the information on pagename and pagenumber

names_grid <- names(local_fw_MAIORANO)

table16_full_information_20NOV23 <- data.frame(matrix(ncol = 7))
names(table16_full_information_20NOV23) <- c("grid_number", "grid_name", "road_density",
  "prey_species", "prey_species_vuln",
  "predator_species", "predator_species_vuln")

for(i in 1:length(names_grid)){
  
  grid_id_name <- names_grid[i]
  
  vulnerability_grid <- paired_pagename_pagenumber[which(paired_pagename_pagenumber$PageNumber == grid_id_name),][,1]
  vulnerability_grid2 <- grids_grilo[which(grids_grilo$grids_grilo_shape.PageName == vulnerability_grid),][,2]
  
  t_links1 <- local_fw_MAIORANO[[i]]$trophic.links
  t_nodes1 <- local_fw_MAIORANO[[i]]$nodes[,c(1,8)]
  
  prey1 <- t_links1$resource
  predator1 <- t_links1$consumer
  
  #Set prey vulnerability
  prey_vuln <- c()
  for(j in 1:length(prey1)){
    
  prey1_1 <- prey1[j]
  prey_vuln[j] <- t_nodes1[which(t_nodes1$node == prey1_1),][,2]
  
  }
  prey2 <- data.frame(prey1, prey_vuln)
  
  #Set predator vulnerability
  predator_vuln <- c()
  for(j in 1:length(predator1)){
    
    predator1_1 <- predator1[j]
    predator_vuln[j] <- t_nodes1[which(t_nodes1$node == predator1_1),][,2]
    
  }
  predator2 <- data.frame(predator1, predator_vuln)
  
  if(!is.null(t_links1))
    {
    grid1_df <- cbind(rep(grid_id_name, nrow(t_links1)), 
                    rep(vulnerability_grid, nrow(t_links1)),
                    rep(vulnerability_grid2, nrow(t_links1)),
                    prey2, 
                    predator2
                    )
  
  names(grid1_df) <- c("grid_number", "grid_name", "road_density",
                       "prey_species", "prey_species_vuln",
                       "predator_species", "predator_species_vuln")
  
  table16_full_information_20NOV23 <- rbind(table16_full_information_20NOV23,grid1_df)
  }
  
  rm(grid_id_name,
     vulnerability_grid,
     vulnerability_grid2,
     t_links1,
     t_nodes1,
     prey1,
     predator1,
     prey2,
     predator2,
     prey_vuln,
     predator_vuln,
     grid1_df)
  
  message(i)
  
}

head(table16_full_information_20NOV23)
table16_full_information_20NOV23 <- table16_full_information_20NOV23[-1,]
nrow(table16_full_information_20NOV23)

#write.csv(table16_full_information_20NOV23, file = "C:\\Users\\asus\\Desktop\\table16_full_information_20NOV23_20NOV23.csv")
