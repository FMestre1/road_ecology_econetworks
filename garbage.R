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
