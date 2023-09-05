################################################################################
#                               CREATE METAWEB
################################################################################
#FMestre
#29-05-2023

#load packages
library(terra)
library(igraph)
library(rglobi)
library(taxize)
library(cheddar)

#taxize::use_entrez()
#usethis::edit_r_environ()
#ENTREZ_KEY='fafd2118668fc6bacdf37d11c7c1885f5308'#mykey - have to reload R

#Create table of species information
species_FAMILY <- tax_name(species, get = 'family', db = 'itis')
species_GENUS <- tax_name(species, get = 'genus', db = 'itis')

#
species_df <- data.frame(species,
                         species_GENUS,
                         species_FAMILY
                         )

species_df <- species_df[,-c(2:3,5,6)]
head(species_df)

####################

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

igraph::plot.igraph(metaweb_GLOBI,layout=layout.circle)

plot.igraph(metaweb_GLOBI,
           vertex.label.cex=1,
           vertex.size=3,
           edge.arrow.size=.25)

lay<-matrix(nrow=123,ncol=2) # create a matrix with one column as runif, the other as trophic level
lay[,1]<-runif(123)
lay[,2]<-TrophInd(predweb.adj[[1]])$TL-1

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



