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

head(vuln_birds)
head(vuln_mammals)

v_mammal <- data.frame(vuln_mammals[,1:2], "mammal")
v_bird <- data.frame(vuln_birds[,1:2],"bird")

names(v_mammal)[3] <- "bm"
names(v_bird)[3] <- "bm"

vulnerability <- data.frame(rbind(v_bird, v_mammal))

i_mammal <- data.frame(vuln_mammals[,c(1,5)], "mammal")
i_bird <- data.frame(vuln_birds[,c(1,5)],"bird")

names(i_mammal)[3] <- "bm"
names(i_bird)[3] <- "bm"

iucn <- data.frame(rbind(i_bird, i_mammal))
#head(iucn)
#tail(iucn)
