################################################################################
#                                  LOAD DATA
################################################################################
#FMestre
#29-05-2023


#load packages
library(terra)
library(igraph)

#Load spatial information
study_area <- terra::vect("C:/Users/FMest/Documents/0. Artigos/roads_networks/data/area_roads_eco_networks.shp")
grids_10km <- terra::vect("")
grids_50km <- terra::vect("C:/Users/FMest/Documents/0. Artigos/roads_networks/data/grids.shp")
mammals <- terra::vect("D:/Dados biológicos/FAscensao_Species_IUCN/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
birds <- terra::vect("D:/Dados biológicos/FAscensao_Species_IUCN/BOTW/birds.shp")

#Load vulnerability data
vuln_birds <- read.csv("C:\\Users\\FMest\\Documents\\0. Artigos\\roads_networks\\data\\9 RankingvulnerableBirds.csv")
vuln_mammals <- read.csv("C:\\Users\\FMest\\Documents\\0. Artigos\\roads_networks\\data\\10 RankingvulnerableMammals.csv")

species <- c(vuln_birds$Species, vuln_mammals$Species)


