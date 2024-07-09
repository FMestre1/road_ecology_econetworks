################################################################################
################################################################################
#                  SCRIPT 4. - FIGURES WITH TL FROM METAWEB
################################################################################
################################################################################

# Road density simplifies regional food webs
# F. Mestre, V.A.G. Bastazini, F. Ascensão

#Load packages
library(ggplot2)
library(igraph)
library(cheddar)
library(gridExtra)
library(effectsize)
library(dplyr)

grids_grilo_shape <- terra::vect("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\data_artigo_clara_grilo\\Nvulnerablegrid50_wgs84_2.shp")

################################################################################
#       FIGURE 1 - MAP OF NR OF NODES, NR OF INTERACTIONS, ROAD DENSITY
################################################################################

############################## MAP OF NR OF NODES ##############################

nr_species_per_grid_per_tl_METAWEB <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 7))
names(nr_species_per_grid_per_tl_METAWEB) <- c("grid", "total_richness","top", "intermediate", "basal", "connected", "not_connected")
head(nr_species_per_grid_per_tl_METAWEB)

for(i in 1:nrow(nr_species_per_grid_per_tl_METAWEB)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    
    network1 <- local_fw_MAIORANO[[i]]
    
    nr_species_per_grid_per_tl_METAWEB[i,1] <- local_fw_MAIORANO[[i]]$properties$title #Name
    nr_species_per_grid_per_tl_METAWEB[i,2] <- nrow(local_fw_MAIORANO[[i]]$nodes) #Total Richness
    levels1 <- network1$nodes$position
    
    nr_species_tl <- data.frame(table(levels1))
    
    #
    if(any(levels1 == "top")) nr_species_per_grid_per_tl_METAWEB[i,3] <-  as.numeric(nr_species_tl[nr_species_tl$levels1 == "top",][2])#Nr of top level species
    if(any(levels1 == "intermediate")) nr_species_per_grid_per_tl_METAWEB[i,4] <-  as.numeric(nr_species_tl[nr_species_tl$levels1 == "intermediate",][2])#Nr of mid level species
    if(any(levels1 == "basal")) nr_species_per_grid_per_tl_METAWEB[i,5] <-  as.numeric(nr_species_tl[nr_species_tl$levels1 == "basal",][2])#Nr of basal level species
    nr_species_per_grid_per_tl_METAWEB[i,6] <- length(cheddar::ConnectedNodes(local_fw_MAIORANO[[i]])) #Connected nodes
    nr_species_per_grid_per_tl_METAWEB[i,7] <- nrow(local_fw_MAIORANO[[i]]$nodes) - length(cheddar::ConnectedNodes(local_fw_MAIORANO[[i]])) #Non connected nodes
    
  }
  message(i)
}

#saving as shapefile
sp_richness_per_trophic_level_METAWEB <- merge(x=grids_grilo_shape, y=nr_species_per_grid_per_tl_METAWEB, by.x="PageNumber", by.y="grid")
#terra::writeVector(sp_richness_per_trophic_level_METAWEB, "sp_richness_per_trophic_level_METAWEB_08NOV23.shp")

############################## NR OF INTERACTIONS ##############################

nr_interactions_per_grid_METAWEB <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 2))
names(nr_interactions_per_grid_METAWEB) <- c("grid", "nr_interactions")
#head(nr_interactions_per_grid_METAWEB)

for(i in 1:nrow(nr_interactions_per_grid_METAWEB)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    
    network1 <- local_fw_MAIORANO[[i]]
    nr_interactions_per_grid_METAWEB$grid[i] <- network1$properties$title
    nr_interactions_per_grid_METAWEB$nr_interactions[i] <- cheddar::NumberOfTrophicLinks(network1)
    
  }
  message(i)
}

#saving as shapefile
nr_interactions_METAWEB <- merge(x=grids_grilo_shape, y=nr_interactions_per_grid_METAWEB, by.x="PageNumber", by.y="grid")
#terra::writeVector(nr_interactions_METAWEB, "nr_interactions_METAWEB_08NOV23.shp")

################################ ROAD DENSITY ##################################

#QGIS

################################################################################
#     FIGURE 2 - VULNERABILITY, PRIMARY AND SECONDARY EXTINCTIONS PER TL
################################################################################

################### TL OF PRIMARY AND SECONDARY EXTINCTIONS ####################

proportion_previous_level_METAWEB <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 10, nrow = length(local_fw_MAIORANO)))
names(proportion_previous_level_METAWEB) <- c("grid", 
                                      "ORIG_PRI_TL_remaining_sp", 
                                      "ORIG_PRI_TL_extinct_sp", 
                                      "PRI_SEC__TL_remaining_sp",
                                      "PRI_SEC_TL_extinct_sp",
                                      "proportion_top_removed_orig_prim",
                                      "proportion_intermediate_removed_orig_prim",
                                      "proportion_basal_removed_orig_prim",
                                      "proportion_top_removed_prim_sec",
                                      "proportion_intermediate_removed_prim_sec",
                                      "proportion_basal_removed_prim_sec"
                                      )
#head(proportion_previous_level_METAWEB)

#LOOP
for(i in 1:length(local_fw_MAIORANO)){
  
  before_net <- local_fw_MAIORANO[[i]]
  prim_net <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[i]]
  sec_net <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[i]]
  
  before_net_props <- before_net$nodes
  prim_net_props <- prim_net$nodes
  sec_net_props <- sec_net$nodes
  
  top_species_nt <- subset(before_net_props, position == "top")
  top_species_nt <- top_species_nt$node
  
  mid_species_nt <- subset(before_net_props, position == "intermediate")
  mid_species_nt <- mid_species_nt$node  
  
  basal_species_nt <- subset(before_net_props, position == "basal")
  basal_species_nt <- basal_species_nt$node
  
  #PRIMARY
  if(unique(!is.na(before_net)) && unique(!is.na(prim_net))) {
    
    removed_species_or_prim <- before_net$nodes$node[!(before_net$nodes$node %in% prim_net$nodes$node)]
    
    if(length(removed_species_or_prim)!=0){
      
      top_removed <- sum(removed_species_or_prim %in% top_species_nt)
      intermediate_removed <- sum(removed_species_or_prim %in% mid_species_nt)
      basal_removed <- sum(removed_species_or_prim %in% basal_species_nt)
      
      prop_top <- top_removed/length(removed_species_or_prim)
      prop_interm <- intermediate_removed/length(removed_species_or_prim)
      prop_basal <- basal_removed/length(removed_species_or_prim)
      
      av_removed <- mean(before_net$nodes[before_net$nodes$node %in% removed_species_or_prim,]$TL)
      av_remaining <- mean(before_net$nodes[!(before_net$nodes$node %in% removed_species_or_prim),]$TL)
      
      #Original to primary
      proportion_previous_level_METAWEB$ORIG_PRI_TL_remaining_sp[i] <- av_remaining
      proportion_previous_level_METAWEB$ORIG_PRI_TL_extinct_sp[i] <- av_removed
      proportion_previous_level_METAWEB$proportion_top_removed_orig_prim[i] <- prop_top
      proportion_previous_level_METAWEB$proportion_intermediate_removed_orig_prim[i] <- prop_interm
      proportion_previous_level_METAWEB$proportion_basal_removed_orig_prim[i] <- prop_basal

    }
    
  }
  
  #SECONDARY
  if(unique(!is.na(prim_net)) && unique(!is.na(sec_net))) {
    
    removed_species_prim_sec <- prim_net$nodes$node[!(prim_net$nodes$node %in% sec_net$nodes$node)]
    
    if(length(removed_species_prim_sec)!=0){
      
      top_removed2 <- sum(removed_species_prim_sec %in% top_species_nt)
      intermediate_removed2 <- sum(removed_species_prim_sec %in% mid_species_nt)
      basal_removed2 <- sum(removed_species_prim_sec %in% basal_species_nt)
      
      prop_top2 <- top_removed2/length(removed_species_prim_sec)
      prop_interm2 <- intermediate_removed2/length(removed_species_prim_sec)
      prop_basal2 <- basal_removed2/length(removed_species_prim_sec)
      
      av_removed2 <- mean(prim_net$nodes[prim_net$nodes$node %in% removed_species_prim_sec,]$TL)
      av_remaining2 <- mean(prim_net$nodes[!(prim_net$nodes$node %in% removed_species_prim_sec),]$TL)
      
      #Original to primary
      proportion_previous_level_METAWEB$PRI_SEC__TL_remaining_sp[i] <- av_remaining2
      proportion_previous_level_METAWEB$PRI_SEC_TL_extinct_sp[i] <- av_removed2
      proportion_previous_level_METAWEB$proportion_top_removed_prim_sec[i] <- prop_top2
      proportion_previous_level_METAWEB$proportion_intermediate_removed_prim_sec[i] <- prop_interm2
      proportion_previous_level_METAWEB$proportion_basal_removed_prim_sec[i] <- prop_basal2
 
    }
    
  }
  
  message(i)
  
}

#PROPORTION OF SPECIES REMOVED PER TROPHIC LEVEL IN EACH EXTINCTION

  #Primary
top_orig_prim_metaweb <- data.frame(proportion_previous_level_METAWEB$proportion_top_removed_orig_prim, "top")
mid_orig_prim_metaweb <- data.frame(proportion_previous_level_METAWEB$proportion_intermediate_removed_orig_prim, "intermediate")
basal_orig_prim_metaweb <- data.frame(proportion_previous_level_METAWEB$proportion_basal_removed_orig_prim, "basal")

names(top_orig_prim_metaweb) <- c("proportion", "level")
names(mid_orig_prim_metaweb) <- c("proportion", "level")
names(basal_orig_prim_metaweb) <- c("proportion", "level")

removed_position_orig_prim_metaweb <- rbind(top_orig_prim_metaweb, mid_orig_prim_metaweb, basal_orig_prim_metaweb)

removed_position_orig_prim_metaweb$level <- as.factor(removed_position_orig_prim_metaweb$level)

#Reorder factor levels
removed_position_orig_prim_metaweb$level <- factor(removed_position_orig_prim_metaweb$level,     
                                           c("top", "intermediate", "basal"))

rem_orig_prim_metaweb <- ggplot(removed_position_orig_prim_metaweb, aes(x = level, y = proportion))

rem_orig_prim2_metaweb <- rem_orig_prim_metaweb + geom_violin(aes(fill = level),) +
  ylab("proportion of removed species") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))

rem_orig_prim2_metaweb + labs(fill = "trophic level")


##################################### FIG2a ####################################

head(match_table)#matching grilo and maiorano
head(all_species_vulnerability)#vulnerability (grilo)
head(overall_previous_positions)#previous positions

match_with_maiorano_vulnerability <- merge(x=all_species_vulnerability,
                                                     y=match_table,
                                                     by.x = "Species",
                                                     by.y = "Species_grilo")
#head(match_with_maiorano_vulnerability)

all_species_vulnerability_maiorano_position <- merge(x=match_with_maiorano_vulnerability,
                                           y=overall_previous_positions,
                                           by.x = "Species_maiorano",
                                           by.y = "species")

all_species_vulnerability_maiorano_position <- all_species_vulnerability_maiorano_position[,c(1:3, 10:13)]

names(all_species_vulnerability_maiorano_position)[3] <- "vulnerability"

all_species_vulnerability_maiorano_position$position <- as.factor(all_species_vulnerability_maiorano_position$position)

all_species_vulnerability_maiorano_position$position <- factor(all_species_vulnerability_maiorano_position$position,     
                                                               c("top", "intermediate", "basal"))

tl_vuln_v2 <- ggplot(all_species_vulnerability_maiorano_position, aes(x = position, y = vulnerability))

tl_vuln2_v2 <- tl_vuln_v2 + geom_violin(aes(fill = position),) +
  ylab("Vulnerability") +
  xlab("Trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E")) +
  ggtitle("A - Vulnerability per trophic level")

tl_vuln2_v2

#save(all_species_vulnerability_maiorano_position, file = "data_fig_2a.RData")

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

for(i in 1:length(local_fw_MAIORANO)){
  
  before_net <- local_fw_MAIORANO[[i]]
  prim_net <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[i]]
  sec_net <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[i]]
  
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

#Save
#save(relative_tl_effects, file = "relative_tl_effects_08NOV23.RData")

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

#save(removed_position_orig_prim2, file = "data_fig_2b_08NOV23.RData")


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

#save(removed_position_prim_casc2, file = "data_fig_2c_08NOV23.RData")

#Combine three plots for fig.2
grid.arrange(tl_vuln2_v2, 
             rem_orig_prim2_violin, 
             rem_prim_casc2_violin, 
             nrow = 3)

################################################################################
#                             FIGURE 3 - EFFECT SIZE
################################################################################

#using
View(extinctions_levels2)

##calculating effect size, Cohen's D

#Top Level
effectsize::cohens_d(extinctions_levels2$PRIM_top_level,
                     extinctions_levels2$BEFORE_top_level,
                     na.rm=TRUE)

#Intermediate Level
effectsize::cohens_d(extinctions_levels2$PRIM_interm_level,
                     extinctions_levels2$BEFORE_interm_level,
                     na.rm=TRUE)

#Basal Level
effectsize::cohens_d(extinctions_levels2$PRIM_basal_level,
                     extinctions_levels2$BEFORE_basal_level,
                     na.rm=TRUE)

## ploting effect size (95%CI)
par(mar=c(4,8,1,1))
x=c(1,2,3)

#Values for point estimate and CI
avg=c(-0.03,-0.06,-0.17)
lower=c(-0.07,-0.10,-0.21)
upper=c(0.01,-0.02,-0.13)

#Plot
plot(1, type="n", ylab="", yaxt="n", xlab="Cohen's d (IC95%)", xlim=c(-0.5, .5), ylim=c(0, 4), main="")
points(avg,x, pch=16,cex=1)
arrows(lower, x, upper,  x, length=0.05, angle=90, code=3,lwd=2.5)
abline(v=0,lty=2, lwd=3,col="red")
axis(side=2,at=x,label=c("Top","Intermediate", "Basal"),cex.lab=14, las=2,xlim=c(0.67,0.83))

################################################################################
#               FIGURE 4 - TL OF REMAINING AND LOST SPECIES
################################################################################

previous_tl <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 4, nrow = length(local_fw_MAIORANO)))
names(previous_tl) <- c("grid", 
                        "averaged_tl_original_networks_of_removed_in_prim_ext", 
                        "averaged_tl_original_networks_of_removed_in_sec_ext", 
                        "averaged_tl_original_networks_of_remaining_in_prim_ext", 
                        "averaged_tl_original_networks_of_remaining_in_sec_ext" 
) 

#LOOP
for(i in 1:length(local_fw_MAIORANO)){
  
  if(any(class(local_fw_MAIORANO[[i]]) == "Community")){#is this position a community #START
    before_net <- local_fw_MAIORANO[[i]]
    prim_net <- local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[i]]
    sec_net <- local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[i]]
    
    averagedTL_before <- data.frame(before_net$nodes$node, before_net$nodes$TL)
    averagedTL_primary <- data.frame(prim_net$nodes$node, prim_net$nodes$TL)
    averagedTL_secondary <- data.frame(sec_net$nodes$node, sec_net$nodes$TL)
    
    names(averagedTL_before) <- c("species", "av_tl")
    names(averagedTL_primary) <- c("species", "av_tl")
    names(averagedTL_secondary) <- c("species", "av_tl")

    if(unique(!is.na(before_net)) && unique(!is.na(prim_net)) && unique(!is.na(sec_net))) {
      
      removed_species_or_prim <- before_net$nodes$node[!(before_net$nodes$node %in% prim_net$nodes$node)]
      removed_species_prim_sec <- prim_net$nodes$node[!(prim_net$nodes$node %in% sec_net$nodes$node)]
      
      if(length(removed_species_or_prim)!=0){
        
        df_tl_removed_species <- averagedTL_before[averagedTL_before$species %in% removed_species_or_prim,]
        previous_tl[i,2] <- mean(df_tl_removed_species[,2])
        #
        df_tl_remaining_species <- averagedTL_before[!(averagedTL_before$species %in% removed_species_or_prim),]
        previous_tl[i,4] <- mean(df_tl_remaining_species[,2])
        
        
      } 
      
      if(length(removed_species_prim_sec)!=0){
        
        df_tl_removed_species2 <- averagedTL_primary[averagedTL_primary$species %in% removed_species_prim_sec,]
        previous_tl[i,3] <- mean(df_tl_removed_species2[,2])
        #
        df_tl_remaining_species2 <- averagedTL_primary[!(averagedTL_primary$species %in% removed_species_prim_sec),]
        previous_tl[i,5] <- mean(df_tl_remaining_species2[,2])
      } 
      
    }
  }#is this position a community #END
  message(i)
  
}

#View(previous_tl)

#Arrange o plot
removed_p <- data.frame(previous_tl$averaged_tl_original_networks_of_removed_in_prim_ext, rep("removed primary", nrow(previous_tl)))
removed_s <- data.frame(previous_tl$averaged_tl_original_networks_of_removed_in_sec_ext, rep("removed secondary", nrow(previous_tl)))
remaining_p <- data.frame(previous_tl$averaged_tl_original_networks_of_remaining_in_prim_ext, rep("remaining primary", nrow(previous_tl)))
remaining_s <- data.frame(previous_tl$averaged_tl_original_networks_of_remaining_in_sec_ext, rep("remaining secondary", nrow(previous_tl)))
#
names(removed_p) <- c("tl", "group")
names(removed_s) <- c("tl", "group")
names(remaining_p) <- c("tl", "group")
names(remaining_s) <- c("tl", "group")

tl_positions <- rbind(removed_p, removed_s, remaining_p, remaining_s)

tl_positions$group <- as.factor(tl_positions$group)

tl_positions$group

tl_positions$group <- factor(tl_positions$group, 
                             levels = c("remaining primary", 
                                        "removed primary", 
                                        "remaining secondary", 
                                        "removed secondary"
                             )
)

#Plot
tl_previous_nr <- ggplot(tl_positions, aes(x = group, y = tl))

#Fig4
tl_previous_nr2 <- tl_previous_nr + geom_boxplot(aes(fill = group),) +
  ylab("Trophic position") +
  xlab("Removed and remaining species in each step") +
  scale_fill_manual(values = c("#008B00", "#FFB90F", "#8B795E", "#8B1A1A"))

tl_previous_nr2

tl_positions

#save(tl_positions, file = "fig4_08NOV23.RData")

#Summarize
tl_positions[complete.cases(tl_positions),] %>% 
  group_by(group) %>% 
  summarise(Average=mean(tl))

################################################################################
#               FIGURE x - Extinctions per trophic level
################################################################################

#head(relative_tl_effects)

species_loss_per_trophic_level <- merge(x=grids_grilo_shape, y=relative_tl_effects, by.x="PageNumber", by.y="grid")
species_loss_per_trophic_level_2 <- species_loss_per_trophic_level[,c(1:2,15:20)]
#terra::writeVector(species_loss_per_trophic_level_2, "species_loss_per_trophic_level_08NOV23.shp")
#save(species_loss_per_trophic_level_2, file = "species_loss_per_trophic_level_dataframe_08NOV23.RData")

################################################################################
#               FIGURE x - Average trophic height of lost species
################################################################################
