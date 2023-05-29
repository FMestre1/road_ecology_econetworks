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

#taxize::use_entrez()
#usethis::edit_r_environ()
#ENTREZ_KEY='fafd2118668fc6bacdf37d11c7c1885f5308'#mykey - have to reload R

#Create table of species information
species_FAMILY <- tax_name(species, get = 'family', db = 'itis')

#
species_df <- data.frame(species,
                         species_FAMILY
                         )

head(species_df)
species_df <- species_df[,-c(2:3)]

####################

GLOBI_metaweb <- data.frame(matrix(ncol = length(species), nrow = length(species)))
colnames(GLOBI_metaweb) <- species
rownames(GLOBI_metaweb) <- species
#View(GLOBI_metaweb)

#AQUI

for(i in 1:nrow(GLOBI_metaweb)){
  
  species_row <- GLOBI_metaweb[i,]
  focal_species <- rownames(species_row)
  focal_species <- stringr::str_replace(focal_species,"\\.", " ")
  
  #Preys upon...
  preys_upon1 <- rglobi::get_interactions(taxon = focal_species, interaction.type = "preysOn")
  preys_of_focal1 <- preys_upon1$target_taxon_name
  preys_S <- unique(species2_taxonomy[which(unique(preys_of_focal1[preys_of_focal1 %in% species2_taxonomy$query]) == species2_taxonomy$query),]$query) #are the preyed upon species on the IbPen
  preys_G <- unique(species2_taxonomy[which(unique(preys_of_focal1[preys_of_focal1 %in% species2_taxonomy$genus]) == species2_taxonomy$genus),]$query) #are the preyed upon genus on the IbPen
  preys_F <- unique(species2_taxonomy[which(unique(preys_of_focal1[preys_of_focal1 %in% species2_taxonomy$family]) == species2_taxonomy$family),]$query) #are the preyed upon family on the IbPen
  #preys_O <- unique(species2_taxonomy[which(unique(preys_of_focal1[preys_of_focal1 %in% species2_taxonomy$order]) == species2_taxonomy$order),]$query) #are the preyed upon order on the IbPen
  #preys_C <- unique(species2_taxonomy[which(unique(preys_of_focal1[preys_of_focal1 %in% species2_taxonomy$class]) == species2_taxonomy$class),]$query) #are the preyed upon class on the IbPen
  #  
  #preys_TOTAL <- c(preys_S, preys_G, preys_F, preys_O, preys_C)
  preys_TOTAL <- unique(c(preys_S, preys_G, preys_F))
  preys_TOTAL <- stringr::str_replace(preys_TOTAL," ", ".")
  
  focal_species2 <- stringr::str_replace(focal_species," ", ".")
  
  GLOBI_metaweb[focal_species2, preys_TOTAL] <- 1
  
  
  #Preyed by...
  preyed_on_by1 <- rglobi::get_interactions(taxon = focal_species, interaction.type = c("eatenBy", "preyedUponBy"))
  predators_of_focal1 <- preyed_on_by1$target_taxon_name
  #     
  predators_S <- species2_taxonomy[species2_taxonomy$query %in% unique(predators_of_focal1[predators_of_focal1 %in% species2_taxonomy$query]),]$query
  predators_G <- species2_taxonomy[species2_taxonomy$genus %in% unique(predators_of_focal1[predators_of_focal1 %in% species2_taxonomy$genus]),]$query
  predators_F <- species2_taxonomy[species2_taxonomy$family %in% unique(predators_of_focal1[predators_of_focal1 %in% species2_taxonomy$family]),]$query
  #
  predators_TOTAL <- unique(c(predators_S, predators_G, predators_F))
  predators_TOTAL <- stringr::str_replace(predators_TOTAL," ", ".")
  #
  GLOBI_metaweb[predators_TOTAL, focal_species2] <- 1
  
  message("Just did row ", i, "!")
  
}#END

