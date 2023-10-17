################################################################################
#                               DISCARDED CODE
################################################################################

#GLOBI_metaweb

GLOBI_metaweb <- data.frame(matrix(ncol = length(species), nrow = length(species)))
colnames(GLOBI_metaweb) <- species
rownames(GLOBI_metaweb) <- species
#View(GLOBI_metaweb)

for(i in 1:nrow(GLOBI_metaweb)){
  
  species_row <- GLOBI_metaweb[i,]
  focal_species <- rownames(species_row)
  
  #Preys upon...
  preys_upon1 <- rglobi::get_interactions(taxon = focal_species, interaction.type = "preysOn")
  preys_of_focal1 <- preys_upon1$target_taxon_name
  if(length(preys_of_focal1)!=0) preys_S <- unique(species_df[which(unique(preys_of_focal1[preys_of_focal1 %in% species_df$species]) == species_df$query),]$species)
  if(length(preys_of_focal1)!=0) preys_G <- unique(species_df[which(unique(preys_of_focal1[preys_of_focal1 %in% species_df$genus]) == species_df$genus),]$species)
  #
  if(length(preys_of_focal1)!=0) preys_TOTAL <- unique(c(preys_S, preys_G))
  if(length(preys_of_focal1)!=0) rm(preys_S, preys_G)
  
  if(exists("preys_TOTAL")) {
    GLOBI_metaweb[focal_species, preys_TOTAL] <- 1
    rm(preys_TOTAL)
  }
  
  #Preyed by...
  preyed_on_by1 <- rglobi::get_interactions(taxon = focal_species, interaction.type = c("eatenBy", "preyedUponBy"))
  predators_of_focal1 <- preyed_on_by1$target_taxon_name
  #     
  if(length(predators_of_focal1)!=0) predators_S <- species_df[species_df$species %in% unique(predators_of_focal1[predators_of_focal1 %in% species_df$species]),]$species
  if(length(predators_of_focal1)!=0) predators_G <- species_df[species_df$genus %in% unique(predators_of_focal1[predators_of_focal1 %in% species_df$genus]),]$species
  #
  predators_TOTAL <- unique(c(predators_S, predators_G))
  #
  if(exists("predators_TOTAL")) {
    GLOBI_metaweb[predators_TOTAL, focal_species] <- 1
    rm(predators_TOTAL)
  }
  
  message("Just did row ", i, "!")
  
}#END

GLOBI_metaweb[is.na(GLOBI_metaweb)] <- 0
#View(GLOBI_metaweb)

df_edges <- as.data.frame(which(GLOBI_metaweb == 1, arr.ind = TRUE))
class(df_edges)

for(i in 1:nrow(df_edges)){
  
  row1 <- df_edges[i,]
  df_edges[i,3] <- colnames(GLOBI_metaweb)[row1[,1]]
  df_edges[i,4] <- rownames(GLOBI_metaweb)[row1[,2]]
  
  message(i)
  
}

df_edges <- df_edges[,-c(1:2)]
rownames(df_edges) <- 1:nrow(df_edges)
df_edges <- data.frame(df_edges[,2], df_edges[,1])
names(df_edges)[1] <- "COLUMN"
names(df_edges)[2] <- "ROW"
head(df_edges)

df_edges2 <- as.matrix(df_edges)

metaweb_GLOBI <- igraph::graph_from_data_frame(df_edges2)

igraph::V(metaweb_GLOBI)
igraph::E(metaweb_GLOBI)

igraph::plot.igraph(metaweb_GLOBI,
                    layout=layout.circle
)

igraph::plot.igraph(metaweb_GLOBI,
                    vertex.label.cex=1,
                    vertex.size=3,
                    edge.arrow.size=.25
)

#lay <- matrix(nrow=123,ncol=2) # create a matrix with one column as runif, the other as trophic level
#lay[,1] <- runif(123)
#lay[,2] <- TrophInd(predweb.adj[[1]])$TL-1

nodes_c <- data.frame(iucn, vulnerability)
nodes_c <- nodes_c[,-c(3,4)]
names(nodes_c) <- c("node", "iucn_status", "Median_MAXroad.RM.1000", "bm")
head(nodes_c)

names(df_edges) <- c("resource", "consumer")

cheddar_metaweb <- cheddar::Community(nodes = nodes_c, properties = list(title="European Metaweb infered wit GLOBI"), trophic.links = as.matrix(df_edges))

#cheddar::TopLevelNodes(cheddar_metaweb)
#cheddar::IntermediateNodes(cheddar_metaweb)
#cheddar::BasalNodes(cheddar_metaweb)
#cheddar::Cannibals(cheddar_metaweb)

#cheddar::PlotPredationMatrix(cheddar_metaweb)

#CommunityPropertyNames(cheddar_metaweb)
#cheddar::NodePropertyNames(TL84)
#cheddar::NodePropertyNames(cheddar_metaweb)

#PlotCircularWeb(cheddar_metaweb)
t_similarity <- cheddar::TrophicSimilarity(cheddar_metaweb)
#write.csv(t_similarity, "t_similarity.csv", row.names=TRUE)

##### SAVE #####
#save(cheddar_metaweb, file = "cheddar_metaweb.RData")
#igraph_metaweb <- metaweb_GLOBI
#save(igraph_metaweb, file = "igraph_metaweb.RData")
#load("cheddar_metaweb.RData")
#load("igraph_metaweb.RData")
################


################################################################################
# Plot vulnerability vs body size (dispersal proxy)
################################################################################
#FMestre
#07-09-2023

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
#      What is the vulnerability of the species in each trophic level?
################################################################################
#FMestre
#06-09-2023

library(cheddar)
library(ggplot2)

vuln_tl_table <- c()

for(j in 1:length(local_fw_MAIORANO)){
  
  if(unique(!is.na(local_fw_MAIORANO[[j]])))
    
  {
    
    tp_level <- as.vector(cheddar::IsTopLevelNode(local_fw_MAIORANO[[j]]))
    interm_level <- as.vector(cheddar::IsIntermediateNode(local_fw_MAIORANO[[j]]))
    basal_level <- as.vector(cheddar::IsBasalNode(local_fw_MAIORANO[[j]]))
    
    df1 <- data.frame(cheddar::NPS(local_fw_MAIORANO[[j]]), 
                      tp_level,
                      interm_level,
                      basal_level)
    
    df1$tp_level[df1$tp_level] <- "top"                  
    df1$interm_level[df1$interm_level] <- "intermediate"
    df1$basal_level[df1$basal_level] <- "basal"
    
    tl <- rep(NA, nrow(df1))
    
    for(i in 1:nrow(df1)){
      
      if(df1$tp_level[i] != FALSE) tl[i] <- "top"
      if(df1$interm_level[i] != FALSE) tl[i] <- "intermediate"
      if(df1$basal_level[i] != FALSE) tl[i] <- "basal"
      if(df1$tp_level[i] == FALSE & df1$interm_level[i] == FALSE & df1$basal_level[i] == FALSE) tl[i] <- "none"
      
      
    }
    
    df2 <- data.frame(cheddar::NPS(local_fw_MAIORANO[[j]]), tl)
    
    df2$tl <- as.factor(df2$tl)
    
    df2 <- df2[,c(1,2,7)]
    
    vuln_tl_table <- rbind(vuln_tl_table, df2)
    
  }
  
  message(j)
  
}

nrow(vuln_tl_table)
str(vuln_tl_table)
vuln_tl_table$tl

#Reorder factor levels
vuln_tl_table_0 <- vuln_tl_table
vuln_tl_table <- vuln_tl_table_0
vuln_tl_table_0$tl

#vuln_tl_table$tl <- factor(vuln_tl_table$tl, c("top", "mid", "basal", "none"))

vuln_tl_table$tl <- factor(vuln_tl_table$tl, 
                           levels = c("top", "intermediate", "basal", "none")
)

tl_vuln <- ggplot(vuln_tl_table, aes(x = tl, y = Median_MAXroad.RM.1000.))

tl_vuln2 <- tl_vuln + geom_boxplot(aes(fill = tl),) +
  ylab("vulnerability") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E", "#999999"))

tl_vuln2

################################################################################

#Which have rabbit and lynx? none?!

#is_rabbit_lynx <- data.frame(matrix(ncol = 3, nrow = length(local_fw_MAIORANO)))
#names(is_rabbit_lynx) <- c("LP", "OC", "VV")

#for(i in 1:length(local_fw_MAIORANO)){

#if(cheddar::is.Community(local_fw_MAIORANO[[i]]))
#{
#is_rabbit_lynx[i,1] <- ifelse("Lynx pardinus" %in% cheddar::NPS(local_fw_MAIORANO[[i]])$node, "present", "absent")
#is_rabbit_lynx[i,2] <- ifelse("Oryctolagus cuniculus" %in% cheddar::NPS(local_fw_MAIORANO[[i]])$node, "present", "absent") 
#is_rabbit_lynx[i,3] <- ifelse("Vulpes vulpes" %in% cheddar::NPS(local_fw_MAIORANO[[i]])$node, "present", "absent") 

#}else{
#  is_rabbit_lynx[i,1] <- NA 
#  is_rabbit_lynx[i,2] <- NA
#  is_rabbit_lynx[i,3] <- NA
#}

#message(i)

#}

#which(is_rabbit_lynx$LP == "present")
#local_fw_MAIORANO[[3660]]
#cheddar::plot.Community(local_fw_MAIORANO[[3660]], node.labels="node", show.nodes.as="both")

#is_rabbit_lynx[is_rabbit_lynx$VV == "present" & is_rabbit_lynx$OC == "present",]


################################################################################

#cheddar::plot.Community(local_fw_MAIORANO[["BW39"]], node.labels="node", show.nodes.as="both")
#cheddar::plot.Community(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["BW39"]], node.labels="node", show.nodes.as="both")
#cheddar::plot.Community(local_fw_MAIORANO_REMOVED[["BW39"]], node.labels="node", show.nodes.as="both")

#Plot links going and coming from Vulpes vulpes
#links <- cbind(TLPS(local_fw_MAIORANO[["BW39"]]), colour="#c7c7c788")

################################################################################


local_fw_MAIORANO_IGRAPH <- vector(mode = "list", length = length(local_fw_MAIORANO))
local_fw_MAIORANO_REMOVED_PRIMARY_EX_IGRAPH <- vector(mode = "list", length = length(local_fw_MAIORANO_REMOVED_PRIMARY_EX))
local_fw_MAIORANO_REMOVED_IGRAPH <- vector(mode = "list", length = length(local_fw_MAIORANO_REMOVED))

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

#COnvert to igraph
for(i in 1:length(local_fw_MAIORANO)) {
  
  if(unique(!is.na(local_fw_MAIORANO[[i]])) && cheddar::NumberOfTrophicLinks(local_fw_MAIORANO[[i]]) != 0) local_fw_MAIORANO_IGRAPH[[i]] <- ToIgraph(local_fw_MAIORANO[[i]])
  
  message(i)
  
}

for(i in 1:length(local_fw_MAIORANO_REMOVED_PRIMARY_EX)) {
  
  if(unique(!is.na(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]])) && cheddar::NumberOfTrophicLinks(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]]) != 0) local_fw_MAIORANO_REMOVED_PRIMARY_EX_IGRAPH[[i]] <- ToIgraph(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]])
  
  message(i)
  
}

for(i in 1:length(local_fw_MAIORANO_REMOVED)) {
  
  if(unique(!is.na(local_fw_MAIORANO_REMOVED[[i]])) && cheddar::NumberOfTrophicLinks(local_fw_MAIORANO_REMOVED[[i]]) != 0) local_fw_MAIORANO_REMOVED_IGRAPH[[i]] <- ToIgraph(local_fw_MAIORANO_REMOVED[[i]])
  
  message(i)
  
}

names(local_fw_MAIORANO_IGRAPH) <- names(local_fw_MAIORANO)
names(local_fw_MAIORANO_REMOVED_PRIMARY_EX_IGRAPH) <- names(local_fw_MAIORANO_REMOVED_PRIMARY_EX)
names(local_fw_MAIORANO_REMOVED_IGRAPH) <- names(local_fw_MAIORANO_REMOVED)

################################################################################
#                         Plotting FW for the figure
################################################################################

#Lets use the grids BW39!
length(cheddar::NPS(local_fw_MAIORANO[["4458"]])$node)
length(cheddar::NPS(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["4458"]])$node)
length(cheddar::NPS(local_fw_MAIORANO_REMOVED[["4458"]])$node)
#
cheddar::TopLevelNodes(local_fw_MAIORANO[["4458"]])
cheddar::TopLevelNodes(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["4458"]])
cheddar::TopLevelNodes(local_fw_MAIORANO_REMOVED[["4458"]])
#
cheddar::BasalNodes(local_fw_MAIORANO[["4458"]])
cheddar::BasalNodes(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["4458"]])
cheddar::BasalNodes(local_fw_MAIORANO_REMOVED[["4458"]])

#
links <- cbind(TLPS(local_fw_MAIORANO[["4458"]]), colour="#c7c7c788")
links$colour["Buteo buteo" == links$resource] <- "red"
links$colour["Buteo buteo" == links$consumer] <- "blue"

#cheddar::plot.Community(network_list_cheddar[[116183]], node.labels="node", show.nodes.as="both", link.col=links$colour)
cheddar::plot.Community(local_fw_MAIORANO[["4458"]], link.col=links$colour)
#s

#Plot links going and coming from Circus cyaneus
links0 <- cbind(TLPS(local_fw_MAIORANO[["4458"]]), colour="#c7c7c788")
#
links0 <- cbind(TLPS(local_fw_MAIORANO[["4458"]]), colour="#ffffff00")
links0$colour["Hieraaetus pennatus" == links0$resource] <- "red"
links0$colour["Hieraaetus pennatus" == links0$consumer] <- "blue"

cheddar::plot.Community(local_fw_MAIORANO[["BW39"]], node.labels="node", show.nodes.as="both", link.col=links$colour)
cheddar::plot.Community(local_fw_MAIORANO[["BW39"]], link.col=links0$colour)



################################################################################
################################################################################

View(species_grilo_merged_maiorano)
View(tax_table_3)

species_grilo_merged_maiorano_tax <- merge(
  x = species_grilo_merged_maiorano,
  y = tax_table_3,
  by.x = "gbif_id",
  by.y = "gbif_id",
  all =TRUE
)

View(species_grilo_merged_maiorano_tax)

write.csv(species_grilo_merged_maiorano, file = "species_grilo_merged_maiorano.csv")
write.csv(species_grilo_merged_maiorano_tax, file = "species_grilo_merged_maiorano_tax.csv")
write.csv(tax_table_3, file = "tax_table_3.csv")


################################################################################
################################################################################
#                          SCRIPT 6 - PLOR FIGURE
################################################################################
################################################################################

#FMestre
#30-09-2023

#Load Packages
library(cheddar)
library(igraph)

nr_lost_interactions_sec
nr_lost_interactions_prim

#Chose a good network to show in the figure
View(data.frame(nr_lost_interactions_prim$grid, 
                nr_lost_interactions_prim$lost_interactions, 
                nr_lost_interactions_sec$lost_interactions, 
                nr_lost_interactions_prim$lost_interactions != nr_lost_interactions_sec$lost_interactions,
                as.numeric(nr_lost_interactions_sec$lost_interactions) - as.numeric(nr_lost_interactions_prim$lost_interactions)))

pair_pagenumber_pagename[pair_pagenumber_pagename$PageNumber =="4458",]

par(mfrow=c(1,3))
plot(local_fw_MAIORANO[["4458"]], main = "CU14")
plot(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["4458"]], main = "CU14")
plot(local_fw_MAIORANO_REMOVED[["4458"]], main = "CU14")


################################################################################
################################################################################
#           SCRIPT 2 - FIGURES
################################################################################
################################################################################

#FMestre
#September 2023

#Load packages
library(ggplot2)
library(terra)

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
#                             Plot per trophic level
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


################################################################################
################################################################################
#               SCRIPT 7 - TROPHIC LEVEL OF MOST VULNERABLE SPECIES
################################################################################
################################################################################

#FMestre
#28-09-2023

#Load packages
library(cheddar)
library(ggplot2)
library(dplyr)

vuln_tl_table <- data.frame(matrix(nrow = length(local_fw_MAIORANO), ncol = 3))
names(vuln_tl_table) <- c("top_vuln", "intermediate_vuln", "basal_vuln")
#head(vuln_tl_table)

for(j in 1:length(local_fw_MAIORANO)){
  
  if(unique(!is.na(local_fw_MAIORANO[[j]])))
    
  {
    
    tp_level <- as.vector(cheddar::IsTopLevelNode(local_fw_MAIORANO[[j]]))
    interm_level <- as.vector(cheddar::IsIntermediateNode(local_fw_MAIORANO[[j]]))
    basal_level <- as.vector(cheddar::IsBasalNode(local_fw_MAIORANO[[j]]))
    
    df1 <- data.frame(cheddar::NPS(local_fw_MAIORANO[[j]]), 
                      tp_level,
                      interm_level,
                      basal_level)
    
    df1$tp_level[df1$tp_level] <- "top"                  
    df1$interm_level[df1$interm_level] <- "intermediate"
    df1$basal_level[df1$basal_level] <- "basal"
    
    tl <- rep(NA, nrow(df1))
    
    for(i in 1:nrow(df1)){
      
      if(df1$tp_level[i] != FALSE) tl[i] <- "top"
      if(df1$interm_level[i] != FALSE) tl[i] <- "intermediate"
      if(df1$basal_level[i] != FALSE) tl[i] <- "basal"
      if(df1$tp_level[i] == FALSE & df1$interm_level[i] == FALSE & df1$basal_level[i] == FALSE) tl[i] <- "none"
      
      
    }
    
    df2 <- data.frame(cheddar::NPS(local_fw_MAIORANO[[j]]), tl)
    
    df2$tl <- as.factor(df2$tl)
    
    df2 <- df2[,c(1,2,7)]
    
    df3 <- as.data.frame(df2 %>% group_by (tl) %>% summarise(Average=mean(Median_MAXroad.RM.1000.)))
    
    vuln_tl_table$top_vuln[j] <- df3[3,2]
    vuln_tl_table$intermediate_vuln[j] <- df3[2,2]
    vuln_tl_table$basal_vuln[j] <- df3[1,2]
    
  }
  
  message(j)
  
}

#nrow(vuln_tl_table)
#str(vuln_tl_table)
#head(vuln_tl_table)

top1 <- data.frame(vuln_tl_table$top_vuln, rep("top", nrow(vuln_tl_table)))
int1 <- data.frame(vuln_tl_table$intermediate_vuln, rep("intermediate", nrow(vuln_tl_table)))
bas1 <- data.frame(vuln_tl_table$basal_vuln, rep("basal", nrow(vuln_tl_table)))

names(top1) <- c("vulnerability", "tl")
names(int1) <- c("vulnerability", "tl")
names(bas1) <- c("vulnerability", "tl")

vuln_tl_table <- rbind(top1, int1, bas1)
vuln_tl_table$tl <- as.factor(vuln_tl_table$tl)

vuln_tl_table$tl <- factor(vuln_tl_table$tl, 
                           levels = c("top", "intermediate", "basal", "none")
)

tl_vuln <- ggplot(vuln_tl_table, aes(x = tl, y = vulnerability))

tl_vuln2 <- tl_vuln + geom_violin(aes(fill = tl),) +
  ylab("vulnerability") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))


tl_vuln2 + labs(fill = "trophic level")

#save(vuln_tl_table, file = "vuln_tl_table_fig2a_NEW.RData")

# Compute an analysis of variance
vulnerability_tl_aov <- aov(vulnerability ~ tl, data = vuln_tl_table)
# Summary of the analysis
summary(vulnerability_tl_aov)


################################################################################
################################################################################
#                 SCRIPT 8 - SPECIES RICHNNESS PER TROPHIC LEVEL 
################################################################################
################################################################################

#FMestre
#30-09-2023

#Loading packages
library(cheddar)
library(terra)
library(dplyr)
library(tidyverse)

#Having
local_fw_MAIORANO #the initial FW structures

nr_species_per_grid_per_tl <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 7))
names(nr_species_per_grid_per_tl) <- c("grid", "total_richness","top", "intermediate", "basal", "connected", "not_connected")
head(nr_species_per_grid_per_tl)

for(i in 1:nrow(nr_species_per_grid_per_tl)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_species_per_grid_per_tl[i,1] <- local_fw_MAIORANO[[i]]$properties$title #Name
    nr_species_per_grid_per_tl[i,2] <- nrow(local_fw_MAIORANO[[i]]$nodes) #Total Richness
    nr_species_per_grid_per_tl[i,3] <- length(cheddar::TopLevelNodes(local_fw_MAIORANO[[i]])) #Nr of top level species
    nr_species_per_grid_per_tl[i,4] <- length(cheddar::IntermediateNodes(local_fw_MAIORANO[[i]])) #Nr of mid level species
    nr_species_per_grid_per_tl[i,5] <- length(cheddar::BasalNodes(local_fw_MAIORANO[[i]])) #Nr of basal level species
    nr_species_per_grid_per_tl[i,6] <- length(cheddar::ConnectedNodes(local_fw_MAIORANO[[i]])) #Connected nodes
    nr_species_per_grid_per_tl[i,7] <- nrow(local_fw_MAIORANO[[i]]$nodes) - length(cheddar::ConnectedNodes(local_fw_MAIORANO[[i]])) #Non connected nodes
    
  }
  message(i)
}

#View(nr_species_per_grid_per_tl)

#saving as shapefile
sp_richness_per_trophic_level <- merge(x=grids_grilo_shape, y=nr_species_per_grid_per_tl, by.x="PageNumber", by.y="grid")
#terra::writeVector(sp_richness_per_trophic_level, "sp_richness_per_trophic_level_11OUT23.shp")
#terra::plet(sp_richness_per_trophic_level, "top")
#terra::plet(sp_richness_per_trophic_level, "intermediate")
#terra::plet(sp_richness_per_trophic_level, "basal")

################################################################################
#             1. Creating map o relative loss of interactions 
################################################################################

local_fw_MAIORANO #the initial FW structures
local_fw_MAIORANO_REMOVED_PRIMARY_EX #priamary extinctions
local_fw_MAIORANO_REMOVED #cascading effect

#Primary
nr_lost_interactions_prim_RELATIVE <- data.frame(matrix(nrow=length(local_fw_MAIORANO_REMOVED_PRIMARY_EX), ncol = 2))
names(nr_lost_interactions_prim_RELATIVE) <- c("grid","lost_interactions_relative")
#head(nr_lost_interactions_prim_RELATIVE)

for(i in 1:nrow(nr_lost_interactions_prim_RELATIVE)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_lost_interactions_prim_RELATIVE[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    if(!is.null(nrow(local_fw_MAIORANO[[i]]$trophic.links))) {
      initial_int <- nrow(local_fw_MAIORANO[[i]]$trophic.links)
      final_int <- nrow(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]]$trophic.links)
      
      nr_lost_interactions_prim_RELATIVE[i,2] <- ((initial_int - final_int) * 100)/initial_int
      
    } else nr_lost_interactions_prim_RELATIVE[i,2]<- 0
  }
  
}

lost_interactions_with_primary_extinctions_RELATIVE <- merge(x=grids_grilo_shape, y=nr_lost_interactions_prim_RELATIVE, by.x="PageNumber", by.y="grid")
#terra::writeVector(lost_interactions_with_primary_extinctions_RELATIVE, "lost_interactions_with_primary_extinctions_RELATIVE_11OUT23.shp")
#terra::plet(lost_interactions_with_primary_extinctions_RELATIVE, "lost_interactions_relative")


#Cascading
nr_lost_interactions_cascading_RELATIVE <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 2))
names(nr_lost_interactions_cascading_RELATIVE) <- c("grid","lost_interactions_relative")
#head(nr_lost_interactions_cascading_RELATIVE)

for(i in 1:nrow(nr_lost_interactions_cascading_RELATIVE)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_lost_interactions_cascading_RELATIVE[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    if(!is.null(nrow(local_fw_MAIORANO[[i]]$trophic.links)))
    {
      
      initial_int2 <- nrow(local_fw_MAIORANO[[i]]$trophic.links)
      final_int2 <- nrow(local_fw_MAIORANO_REMOVED[[i]]$trophic.links)
      
      nr_lost_interactions_cascading_RELATIVE[i,2] <- ((initial_int2 - final_int2) * 100)/initial_int2
      
    } else nr_lost_interactions_cascading_RELATIVE[i,2] <- 0
  }
  
}

lost_interactions_with_sec_extinctions_RELATIVE <- merge(x=grids_grilo_shape, y=nr_lost_interactions_cascading_RELATIVE, by.x="PageNumber", by.y="grid")
#terra::writeVector(lost_interactions_with_sec_extinctions_RELATIVE, "lost_interactions_with_sec_extinctions_RELATIVE_11OUT23.shp")
#terra::plet(lost_interactions_with_sec_extinctions_RELATIVE, "lost_interactions_relative")



# Use dplyr to calculate the mean of 'Value' by 'Group'
result <- tl_positions %>%
  group_by(group) %>%
  summarize(Mean_Value = mean(tl, na.rm = TRUE))

# Print the result
print(result)


################################################################################
################################################################################
#                             SCRIPT 4 - PLOTTING
################################################################################
################################################################################

#FMestre
#28-09-2023

#Load packages
library(ggplot2)

################################################################################
# 1. Boxplots of the fraction of top, intermediate, and basal nodes
################################################################################

#Proportion of top, intermediate and basal nodes before and after
fractions_top_intermediate_basal_nodes <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 7, nrow = length(local_fw_MAIORANO)))
names(fractions_top_intermediate_basal_nodes) <- c("grid", "BEFORE_top_level", "BEFORE_Interm_level", "BEFORE_basal_level", 
                                                   "AFTER__top_level", "AFTER_Interm_level", "AFTER_basal_level", "changed?") 
#LOOP
for(i in 1:length(local_fw_MAIORANO)){
  
  before_net <- local_fw_MAIORANO[[i]]#before disturbance
  after_net <- local_fw_MAIORANO_REMOVED[[i]]#after cascading effects
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
#head(fractions_top_intermediate_basal_nodes)
#View(fractions_top_intermediate_basal_nodes)

#Load & Save
#save(fractions_top_intermediate_basal_nodes, file = "fractions_top_intermediate_basal_nodes_11OUT23.RData")
#load("fractions_top_intermediate_basal_nodes_11OUT23.RData")


before_nt <- fractions_top_intermediate_basal_nodes[,1:4]
after_nt <- fractions_top_intermediate_basal_nodes[,c(1,5,6,7)]
names(before_nt)[-1] <- c("top", "mid", "basal")
names(after_nt)[-1] <- c("top", "mid", "basal")
before_nt <- data.frame(before_nt, "before")
after_nt <- data.frame(after_nt, "after")
names(before_nt)[5] <- "before_after"
names(after_nt)[5] <- "before_after"
#head(before_nt)
#head(after_nt)

net_changes <- rbind(before_nt, after_nt)
net_changes$before_after <- as.factor(net_changes$before_after)

#Reorder factor levels
net_changes$before_after <- factor(net_changes$before_after,     
                                   c("before", "after"))
### Boxplot ###

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
# 2. In which trophic level are the removed species? 
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

#Save & Load
#save(extinctions_levels, file = "extinctions_levels_11OUT23.RData")
#load("extinctions_levels_11OUT23.RData")

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

#save(removed_position_orig_prim, file = "removed_position_orig_prim_fig2b_11OUT23_NEW.RData")

# Compute the analysis of variance
names(removed_position_orig_prim)

orig_prim_aov <- aov(rate ~ level, data = removed_position_orig_prim)
# Summary of the analysis
summary(orig_prim_aov)

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

rem_prim_sec2 <- rem_prim_sec + geom_violin(aes(fill = level),) +
  ylab("rate of change") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))

rem_prim_sec2 + labs(fill = "trophic level") 
#YES

#save(removed_position_prim_sec, file = "removed_position_prim_sec_fig2c_11OUT23_NEW.RData")

# Compute the analysis of variance
names(removed_position_prim_sec)

prim_sec_aov <- aov(rate ~ level, data = removed_position_prim_sec)
# Summary of the analysis
summary(prim_sec_aov)

#####################################################################################
# 3. In each transition, how many species occupied a given position in the previous FW?
#####################################################################################

proportion_previous_level <- data.frame(names(local_fw_MAIORANO), matrix(ncol = 6, nrow = length(local_fw_MAIORANO)))
names(proportion_previous_level) <- c("grid", 
                                      "ORIG_PRI_top_level", 
                                      "ORIG_PRI_interm_level", 
                                      "ORIG_PRI_basal_level", 
                                      "PRI_SEC_top_level",
                                      "PRI_SEC_interm_level",
                                      "PRI_SEC_basal_level"
)

#which(names(local_fw_MAIORANO) == "BW39")
#i = 3461

#LOOP
for(i in 1:length(local_fw_MAIORANO_REMOVED)){
  
  before_net <- local_fw_MAIORANO[[i]]
  prim_net <- local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]]
  sec_net <- local_fw_MAIORANO_REMOVED[[i]]
  
  if(unique(!is.na(before_net)) && unique(!is.na(prim_net))) {
    
    removed_species_or_prim <- before_net$nodes$node[!(before_net$nodes$node %in% prim_net$nodes$node)]
    
    if(length(removed_species_or_prim)!=0){
      
      top_removed <- sum(removed_species_or_prim %in% cheddar::TopLevelNodes(before_net))
      intermediate_removed <- sum(removed_species_or_prim %in% cheddar::IntermediateNodes(before_net))
      basal_removed <- sum(removed_species_or_prim %in% cheddar::BasalNodes(before_net))
      
      prop_top <- top_removed/length(removed_species_or_prim)
      prop_interm <- intermediate_removed/length(removed_species_or_prim)
      prop_basal <- basal_removed/length(removed_species_or_prim)
      
      #Original to primary
      proportion_previous_level$ORIG_PRI_top_level[i] <- prop_top
      proportion_previous_level$ORIG_PRI_interm_level[i] <- prop_interm
      proportion_previous_level$ORIG_PRI_basal_level[i] <- prop_basal
      
    } 
    
  }
  
  if(unique(!is.na(prim_net)) && unique(!is.na(sec_net))) {
    
    removed_species_prim_sec <- prim_net$nodes$node[!(prim_net$nodes$node %in% sec_net$nodes$node)]
    
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
#View(proportion_previous_level)

#save(proportion_previous_level, file = "proportion_previous_level_11OUT23.RData")
#load("proportion_previous_level_11OUT23.RData")

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

rem_orig_prim2_v2 <- rem_orig_prim_v2 + geom_violin(aes(fill = level),) +
  ylab("previous trophic level") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))


rem_orig_prim2_v2 + labs(fill = "trophic level")
#NOT

#2. From primary extinctions to cascading effects
top_prim_sec_v2 <- data.frame(proportion_previous_level$PRI_SEC_top_level, "top")
mid_prim_sec_v2 <- data.frame(proportion_previous_level$PRI_SEC_interm_level, "intermediate")
basal_prim_sec_v2 <- data.frame(proportion_previous_level$PRI_SEC_basal_level, "basal")

names(top_prim_sec_v2) <- c("rate", "level")
names(mid_prim_sec_v2) <- c("rate", "level")
names(basal_prim_sec_v2) <- c("rate", "level")

removed_position_prim_sec_v2 <- rbind(top_prim_sec_v2, mid_prim_sec_v2, basal_prim_sec_v2)

removed_position_prim_sec_v2$level <- as.factor(removed_position_prim_sec_v2$level)

#Reorder factor levels
removed_position_prim_sec_v2$level <- factor(removed_position_prim_sec_v2$level,     
                                             c("top", "intermediate", "basal"))

rem_prim_sec_v2 <- ggplot(removed_position_prim_sec_v2, aes(x = level, y = rate))

rem_prim_sec2_v2 <- rem_prim_sec_v2 + geom_violin(aes(fill = level),) +
  ylab("previous trophic level") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))

rem_prim_sec2_v2
#NOT

################################################################################
# 4. Species in each grid
################################################################################

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

names(grids_grilo_shape)
names(nr_species_per_grid)

sp_richness <- merge(x=grids_grilo_shape, y=nr_species_per_grid, by.x="PageNumber", by.y="grid")
#terra::writeVector(sp_richness, "sp_richness_11OUT23.shp")
#terra::plet(sp_richness, "sp_richness")

################################################################################
# 5. Coonectance
################################################################################

#grids_grilo_shape
#local_fw_MAIORANO

connectance_original_networks <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 2))
names(connectance_original_networks) <- c("grid","connectance")
#head(connectance_original_networks)

for(i in 1:nrow(connectance_original_networks)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    connectance_original_networks[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    connectance_original_networks[i,2] <- DirectedConnectance(local_fw_MAIORANO[[i]])
  }
  
}

connectance_original <- merge(x=grids_grilo_shape, y=connectance_original_networks, by.x="PageNumber", by.y="grid")
#terra::writeVector(connectance_original, "connectance_11OUT23.shp")
#terra::plet(connectance_original, "connectance")

################################################################################
# 6. How many interactions lost? - with secondary extinctions
################################################################################

nr_lost_interactions_cascading_RELATIVE <- data.frame(matrix(nrow=length(local_fw_MAIORANO), ncol = 2))
names(nr_lost_interactions_cascading_RELATIVE) <- c("grid","lost_interactions")

#local_fw_MAIORANO[[100]]
#local_fw_MAIORANO_REMOVED[[100]]

for(i in 1:nrow(nr_lost_interactions_cascading_RELATIVE)){
  
  if(any(!is.na(local_fw_MAIORANO[[i]]))){
    nr_lost_interactions_cascading_RELATIVE[i,1] <- local_fw_MAIORANO[[i]]$properties$title
    if(!is.null(nrow(local_fw_MAIORANO[[i]]$trophic.links))) nr_lost_interactions_cascading_RELATIVE[i,2] <- nrow(local_fw_MAIORANO[[i]]$trophic.links) - nrow(local_fw_MAIORANO_REMOVED[[i]]$trophic.links) else nr_lost_interactions_cascading_RELATIVE[i,2] <- 0
  }
  
}

lost_interactions_with_sec_extinctions <- merge(x=grids_grilo_shape, y=nr_lost_interactions_cascading_RELATIVE, by.x="PageNumber", by.y="grid")
terra::writeVector(lost_interactions_with_sec_extinctions, "lost_interactions_with_sec_extinctions_11OUT23.shp")
#terra::plet(lost_interactions_with_sec_extinctions, "lost_interactions")

################################################################################

"Coracias garrulus"

here_it_is <- c()

for(i in 1:length(local_fw_MAIORANO)) here_it_is[i] <- "Coracias garrulus" %in% local_fw_MAIORANO[[i]]$nodes$node

which(here_it_is == TRUE)

local_fw_MAIORANO[[7]]$trophic.links[local_fw_MAIORANO[[7]]$trophic.links$consumer == "Coracias garrulus",]
local_fw_MAIORANO[[7]]$trophic.links[local_fw_MAIORANO[[7]]$trophic.links$resource == "Coracias garrulus",]

