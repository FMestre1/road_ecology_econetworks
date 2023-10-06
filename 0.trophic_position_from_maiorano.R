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
library(gridExtra)

################################################################################
#                       Loading Maiorano´s dataset
################################################################################

#Maiorano, L., Montemaggiori, A., Ficetola, G. F., O’connor, L., & Thuiller, W. (2020). 
#TETRA‐EU 1.0: a species‐level trophic metaweb of European tetrapods.
#Global Ecology and Biogeography, 29(9), 1452-1457.

#CREATE IGRAPH NETWORK

maiorano_igraph <- igraph::graph_from_adjacency_matrix(as.matrix(t(maiorano_metaweb)), mode = "directed")
#plot(maiorano_igraph)
#igraph::vertex(maiorano_igraph)

gorder(maiorano_igraph) #nr nodes
gsize(maiorano_igraph) #nr interactions

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
#plot(maiorano_cheddar)

#Overall positions
overall_basal <- cheddar::BasalNodes(maiorano_cheddar)
overall_intermediate <- cheddar::IntermediateNodes(maiorano_cheddar)
overall_top <- cheddar::TopLevelNodes(maiorano_cheddar)

#save(overall_basal, file = "overall_basal.RData")
#save(overall_intermediate, file = "overall_intermediate.RData")
#save(overall_top, file = "overall_top.RData")

overall_basal_2 <- data.frame(overall_basal, rep("basal", length(overall_basal)))
overall_intermediate_2 <- data.frame(overall_intermediate, rep("intermediate", length(overall_intermediate)))
overall_top_2 <- data.frame(overall_top, rep("top", length(overall_top)))

names(overall_basal_2) <- c("species", "position")
names(overall_intermediate_2) <- c("species", "position")
names(overall_top_2) <- c("species", "position")

overall_previous_positions <- rbind(overall_top_2,
                                    overall_intermediate_2,
                                    overall_basal_2)


#Compute trophic height
#cheddar::TrophicHeight(maiorano_cheddar, include.isolated=TRUE)
#ERROR
#This network has a lot of paths, possibly too many to compute

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
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E")) +
  ggtitle("A - Vulnerability per trophic level")

tl_vuln2_v2

################################### FIG2b and c ################################

#all_species_vulnerability_maiorano_position

extinctions_levels2 <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 9, nrow = length(local_fw_MAIORANO)))
names(extinctions_levels2) <- c("grid", 
                               "BEFORE_top_level", 
                               "BEFORE_interm_level", 
                               "BEFORE_basal_level", 
                               "PRIM_top_level",
                               "PRIM_interm_level",
                               "PRIM_basal_level",
                               "SEC_top_level",
                               "SEC_interm_level",
                               "SEC_basal_level"
) 

#View(extinctions_levels2)

for(i in 1:length(local_fw_MAIORANO_REMOVED)){
  
  before_net <- local_fw_MAIORANO[[i]]
  prim_net <- local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]]
  sec_net <- local_fw_MAIORANO_REMOVED[[i]]
  
  #grid_name <- local_fw_MAIORANO[[i]]$properties$title
  
  before_net_species <- before_net$nodes$node
  prim_net_species <- prim_net$nodes$node
  sec_net_species <- sec_net$nodes$node
    
  before_net_species_position <- all_species_vulnerability_maiorano_position[all_species_vulnerability_maiorano_position$Species %in% before_net_species,]$position
  prim_net_species_position <- all_species_vulnerability_maiorano_position[all_species_vulnerability_maiorano_position$Species %in% prim_net_species,]$position
  sec_net_species_position <- all_species_vulnerability_maiorano_position[all_species_vulnerability_maiorano_position$Species %in% sec_net_species,]$position
    
  before_net_species_position_table <- data.frame(table(before_net_species_position))[,2]
  prim_net_species_position_table <- data.frame(table(prim_net_species_position))[,2]
  sec_net_species_position_table <- data.frame(table(sec_net_species_position))[,2]
  
  extinctions_levels2$BEFORE_top_level[i] <- before_net_species_position_table[1]
  extinctions_levels2$BEFORE_interm_level[i] <-  before_net_species_position_table[2]
  extinctions_levels2$BEFORE_basal_level[i] <- before_net_species_position_table[3]
  extinctions_levels2$PRIM_top_level[i] <- prim_net_species_position_table[1]
  extinctions_levels2$PRIM_interm_level[i] <- prim_net_species_position_table[2]
  extinctions_levels2$PRIM_basal_level[i] <- prim_net_species_position_table[3]
  extinctions_levels2$SEC_top_level[i] <- sec_net_species_position_table[1]
  extinctions_levels2$SEC_interm_level[i] <- sec_net_species_position_table[2]
  extinctions_levels2$SEC_basal_level[i] <- sec_net_species_position_table[3]
  
  message(i)
  
}

#What proportion of species was affected in each Trophic level

relative_tl_effects <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 6, nrow = length(local_fw_MAIORANO)))
names(relative_tl_effects) <- c("grid", 
                                "BEFORE_to_PRIMARY_top_level", 
                                "BEFORE_to_PRIMARY_interm_level", 
                                "BEFORE_to_PRIMARY_basal_level", 
                                "PRIMARY_to_CASCADING_top_level",
                                "PRIMARY_to_CASCADING_interm_level",
                                "PRIMARY_to_CASCADING_basal_level"
) 

for(i in 1:nrow(relative_tl_effects)){
  
  tp_0 <- extinctions_levels2[i,2]
  it_0 <- extinctions_levels2[i,3]
  bs_0 <- extinctions_levels2[i,4]
  tp_1 <- extinctions_levels2[i,5]
  it_1 <- extinctions_levels2[i,6]
  bs_1 <- extinctions_levels2[i,7]
  tp_2 <- extinctions_levels2[i,8]
  it_2 <- extinctions_levels2[i,9]
  bs_2 <- extinctions_levels2[i,10]
  
  relative_tl_effects$BEFORE_to_PRIMARY_top_level[i] <- 1-(tp_1/tp_0)
  relative_tl_effects$BEFORE_to_PRIMARY_interm_level[i] <- 1-(it_1/it_0)
  relative_tl_effects$BEFORE_to_PRIMARY_basal_level[i] <- 1-(bs_1/bs_0)
  
  relative_tl_effects$PRIMARY_to_CASCADING_top_level[i] <- 1-(tp_2/tp_1)
  relative_tl_effects$PRIMARY_to_CASCADING_interm_level[i] <- 1-(it_2/it_1)
  relative_tl_effects$PRIMARY_to_CASCADING_basal_level[i] <- 1-(bs_2/bs_1)  
  
}

#View(relative_tl_effects)

#Save & Load
#save(relative_tl_effects, file = "relative_tl_effects_06OUT23.RData")
#load("relative_tl_effects_05OUT23.RData")

# Plot it...

#1. From original to primary extinctions
top_orig_prim2 <- data.frame(relative_tl_effects$BEFORE_to_PRIMARY_top_level, "top")
mid_orig_prim2 <- data.frame(relative_tl_effects$BEFORE_to_PRIMARY_interm_level, "intermediate")
basal_orig_prim2 <- data.frame(relative_tl_effects$BEFORE_to_PRIMARY_basal_level, "basal")

names(top_orig_prim2) <- c("rate", "level")
names(mid_orig_prim2) <- c("rate", "level")
names(basal_orig_prim2) <- c("rate", "level")

removed_position_orig_prim2 <- rbind(top_orig_prim2, mid_orig_prim2, basal_orig_prim2)

removed_position_orig_prim2$level <- as.factor(removed_position_orig_prim2$level)

#Reorder factor levels
removed_position_orig_prim2$level <- factor(removed_position_orig_prim2$level,     
                                           c("top", "intermediate", "basal"))

rem_orig_prim2 <- ggplot(removed_position_orig_prim2, 
                         aes(x = level, y = rate))

rem_orig_prim2_violin <- rem_orig_prim2 + geom_violin(aes(fill = level)) +
  ylab("Rate of change") +
  xlab("Trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E")) +
  ggtitle("B - Primary extinctions")

rem_orig_prim2_violin


#2. From primary to cascading effects
top_prim_casc2 <- data.frame(relative_tl_effects$PRIMARY_to_CASCADING_top_level, "top")
mid_prim_casc2 <- data.frame(relative_tl_effects$PRIMARY_to_CASCADING_interm_level, "intermediate")
basal_prim_casc2 <- data.frame(relative_tl_effects$PRIMARY_to_CASCADING_basal_level, "basal")

names(top_prim_casc2) <- c("rate", "level")
names(mid_prim_casc2) <- c("rate", "level")
names(basal_prim_casc2) <- c("rate", "level")

removed_position_prim_casc2 <- rbind(top_prim_casc2, mid_prim_casc2, basal_prim_casc2)

removed_position_prim_casc2$level <- as.factor(removed_position_prim_casc2$level)

#Reorder factor levels
removed_position_prim_casc2$level <- factor(removed_position_prim_casc2$level,     
                                            c("top", "intermediate", "basal"))

rem_prim_casc2 <- ggplot(removed_position_prim_casc2, 
                         aes(x = level, y = rate))

rem_prim_casc2_violin <- rem_prim_casc2 + geom_violin(aes(fill = level)) +
  ylab("Rate of change") +
  xlab("Trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E")) +
  ggtitle("C - Cascading effects")

rem_prim_casc2_violin

#Combine three plots for fig.2
grid.arrange(tl_vuln2_v2, 
             rem_orig_prim2_violin, 
             rem_prim_casc2_violin, 
             nrow = 3)

#Save data
#save(all_species_vulnerability_maiorano_position, file = "fig2a_06OUT23.RData")
#save(removed_position_orig_prim2, file = "fig2b_06OUT23.RData")
#save(removed_position_prim_casc2, file = "fig2c_06OUT23.RData")

################################################################################
#    New figure with the absolute number of species per trophic level
################################################################################

names(extinctions_levels2)
str(extinctions_levels2)
colMeans(extinctions_levels2[,-1])

top_start <- data.frame(extinctions_levels2$BEFORE_top_level, "top_start")
mid_start <- data.frame(extinctions_levels2$BEFORE_interm_level, "intermediate_start")
basal_start <- data.frame(extinctions_levels2$BEFORE_basal_level, "basal_start")
#
top_prim <- data.frame(extinctions_levels2$PRIM_top_level, "top_prim")
mid_prim <- data.frame(extinctions_levels2$PRIM_interm_level, "intermediate_prim")
basal_prim <- data.frame(extinctions_levels2$PRIM_basal_level, "basal_prim")
#
top_sec <- data.frame(extinctions_levels2$SEC_top_level, "top_sec")
mid_sec <- data.frame(extinctions_levels2$SEC_interm_level, "intermediate_sec")
basal_sec <- data.frame(extinctions_levels2$SEC_basal_level, "basal_sec")

names(top_start) <- c("species", "level_step")
names(mid_start) <- c("species", "level_step")
names(basal_start) <- c("species", "level_step")
#
names(top_prim) <- c("species", "level_step")
names(mid_prim) <- c("species", "level_step")
names(basal_prim) <- c("species", "level_step")
#
names(top_sec) <- c("species", "level_step")
names(mid_sec) <- c("species", "level_step")
names(basal_sec) <- c("species", "level_step")

species_tl_step <- rbind(top_start, mid_start, basal_start,
                         top_prim,mid_prim,basal_prim,
                         top_sec,mid_sec,basal_sec)

species_tl_step$level_step <- as.factor(species_tl_step$level_step)

unique(species_tl_step$level_step)

#Reorder factor levels
species_tl_step$level_step <- factor(species_tl_step$level_step,     
                                            c("top_start", "intermediate_start", "basal_start",
                                              "top_prim", "intermediate_prim", "basal_prim",
                                              "top_sec", "intermediate_sec", "basal_sec")
                                             )

species_tl_step_plot <- ggplot(species_tl_step, 
                         aes(x = level_step, y = species))

species_tl_step_plot2 <- species_tl_step_plot + geom_violin(aes(fill = level_step)) +
  ylab("Number of Species per Trophic Level") +
  xlab("Trophic level - Time step") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E",
                               "#E70F00", "#E69F00", "#1E811E",
                               "#E70F00", "#E69F00", "#1E811E")) +
  ggtitle("Number of Species")

species_tl_step_plot2

################################################################################
# Plot per trophic level
################################################################################

#levels(species_tl_step$level_step)

top_tl_species <- species_tl_step[species_tl_step$level_step %in% c("top_start", "top_prim" , "top_sec"), ]      
mid_tl_species <- species_tl_step[species_tl_step$level_step %in% c("intermediate_start", "intermediate_prim" , "intermediate_sec"), ]      
basal_tl_species <- species_tl_step[species_tl_step$level_step %in% c("basal_start", "basal_prim" , "basal_sec"), ]      

# TOP
top_plot <- ggplot(top_tl_species, aes(x = level_step, y = species))

top_plot_plot2 <- top_plot + geom_violin(aes(fill = level_step)) +
  ylab("Number of Species - Top Trophic level") +
  xlab("Time step") +
  scale_fill_manual(values = c("#C19235","#82BC48", "#DCCD3D")) +
  ggtitle("Number of Species")

top_plot_plot2

# MID
mid_plot <- ggplot(mid_tl_species, aes(x = level_step, y = species))

mid_plot_plot2 <- mid_plot + geom_violin(aes(fill = level_step)) +
  ylab("Number of Species - Intermediate Trophic level") +
  xlab("Time step") +
  scale_fill_manual(values = c("#C19235","#82BC48", "#DCCD3D")) +
  ggtitle("Number of Species")

mid_plot_plot2

#BASAL
basal_plot <- ggplot(basal_tl_species, aes(x = level_step, y = species))

basal_plot_plot2 <- basal_plot + geom_violin(aes(fill = level_step)) +
  ylab("Number of Species - Basal Trophic level") +
  xlab("Time step") +
  scale_fill_manual(values = c("#C19235","#82BC48", "#DCCD3D")) +
  ggtitle("Number of Species")

basal_plot_plot2


################################################################################
#                              Plot removed species
################################################################################

#using:
#extinctions_levels2

lost_species <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 6, nrow = length(local_fw_MAIORANO)))
names(lost_species) <- c("grid", 
                                "BEFORE_to_PRIMARY_top_level", 
                                "BEFORE_to_PRIMARY_interm_level", 
                                "BEFORE_to_PRIMARY_basal_level", 
                                "PRIMARY_to_CASCADING_top_level",
                                "PRIMARY_to_CASCADING_interm_level",
                                "PRIMARY_to_CASCADING_basal_level"
) 
#View(lost_species)

for(i in 1:nrow(lost_species)){
  
 tp_0 <- extinctions_levels2[i,2]
 it_0 <- extinctions_levels2[i,3]
 bs_0 <- extinctions_levels2[i,4]
 tp_1 <- extinctions_levels2[i,5]
 it_1 <- extinctions_levels2[i,6]
 bs_1 <- extinctions_levels2[i,7]
 tp_2 <- extinctions_levels2[i,8]
 it_2 <- extinctions_levels2[i,9]
 bs_2 <- extinctions_levels2[i,10]
 
 lost_species$BEFORE_to_PRIMARY_top_level[i] <- tp_0-tp_1
 lost_species$BEFORE_to_PRIMARY_interm_level[i] <- it_0-it_1
 lost_species$BEFORE_to_PRIMARY_basal_level[i] <- bs_0-bs_1
 
 lost_species$PRIMARY_to_CASCADING_top_level[i] <- tp_1-tp_2
 lost_species$PRIMARY_to_CASCADING_interm_level[i] <- it_1-it_2
 lost_species$PRIMARY_to_CASCADING_basal_level[i] <- bs_1-bs_2 
 
  
  message(i)
  
}

#View(lost_species)

#1. Original to primary #####
top_orig_prim3 <- data.frame(lost_species$BEFORE_to_PRIMARY_top_level, "top")
mid_orig_prim3 <- data.frame(lost_species$BEFORE_to_PRIMARY_interm_level, "intermediate")
basal_orig_prim3 <- data.frame(lost_species$BEFORE_to_PRIMARY_basal_level, "basal")

names(top_orig_prim3) <- c("species", "level")
names(mid_orig_prim3) <- c("species", "level")
names(basal_orig_prim3) <- c("species", "level")

removed_position_orig_prim3 <- rbind(top_orig_prim3, mid_orig_prim3, basal_orig_prim3)

removed_position_orig_prim3$level <- as.factor(removed_position_orig_prim3$level)

#Reorder factor levels
removed_position_orig_prim3$level <- factor(removed_position_orig_prim3$level,     
                                            c("top", "intermediate", "basal"))

data_means_orig_prim <- aggregate(removed_position_orig_prim3$species,
                                 list(removed_position_orig_prim3$level),
                                 mean)

rem_orig_prim3 <- ggplot(removed_position_orig_prim3, 
                         aes(x = level, y = species))

rem_orig_prim3_violin <- rem_orig_prim3 + geom_boxplot(aes(fill = level)) +
  ylab("Number of species") +
  xlab("Trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E")) +
  ggtitle("Primary extinctions") +
  stat_summary(fun = mean, geom = "point", col = "black") +
  stat_summary(fun = mean, geom = "text", col = "black",
               vjust = 1.5, aes(label = round(..y.., digits = 1)))

rem_orig_prim3_violin

#2. Primary to Cascading #####
top_prim_sec3 <- data.frame(lost_species$PRIMARY_to_CASCADING_top_level, "top")
mid_prim_sec3 <- data.frame(lost_species$PRIMARY_to_CASCADING_interm_level, "intermediate")
basal_prim_sec3 <- data.frame(lost_species$PRIMARY_to_CASCADING_basal_level, "basal")

names(top_prim_sec3) <- c("species", "level")
names(mid_prim_sec3) <- c("species", "level")
names(basal_prim_sec3) <- c("species", "level")

removed_position_prim_sec3 <- rbind(top_prim_sec3, mid_prim_sec3, basal_prim_sec3)

removed_position_prim_sec3$level <- as.factor(removed_position_prim_sec3$level)

#Reorder factor levels
removed_position_prim_sec3$level <- factor(removed_position_prim_sec3$level,     
                                            c("top", "intermediate", "basal"))

data_means_prim_sec <- aggregate(removed_position_prim_sec3$species,
                        list(removed_position_prim_sec3$level),
                        mean)

rem_prim_sec3 <- ggplot(removed_position_prim_sec3, 
                         aes(x = level, y = species)) 

  text(x = 1:nrow(data_means_prim_sec),
       y = 10,
       labels = paste("Mean:", round(data_means_prim_sec$x, 1)),
       col = "black")

rem_prim_sec3_violin <- rem_prim_sec3 + geom_boxplot(aes(fill = level)) +
  ylab("Number of species") +
  xlab("Trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E")) +
  ggtitle("Cascading effect") +
  stat_summary(fun = mean, geom = "point", col = "black") +
  stat_summary(fun = mean, geom = "text", col = "black",
               vjust = 1.5, aes(label = round(..y.., digits = 1)))

rem_prim_sec3_violin

#Combine three plots for fig.2
grid.arrange(rem_orig_prim3_violin, 
             rem_prim_sec3_violin, 
             nrow = 2)
