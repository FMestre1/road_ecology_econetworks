################################################################################
#                 MAPS OF SPECIES RICHNESS PER TROPHIC LEVEL
################################################################################

#Loading packages
library(cheddar)
library(terra)
  
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

View(nr_species_per_grid_per_tl)

#saving as shapefile
#Get road value on grids
grids_grilo_shape <- terra::vect("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\data_artigo_clara_grilo\\Nvulnerablegrid50_wgs84_2.shp")

#sp_richness_per_trophic_level <- merge(x=grids_grilo_shape, y=nr_species_per_grid_per_tl, by.x="PageName", by.y="grid")
#terra::writeVector(sp_richness_per_trophic_level, "sp_richness_per_trophic_level.shp")
#terra::plet(sp_richness_per_trophic_level, "top")
#terra::plet(sp_richness_per_trophic_level, "intermediate")
#terra::plet(sp_richness_per_trophic_level, "basal")

