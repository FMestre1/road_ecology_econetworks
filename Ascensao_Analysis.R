# Make rasters and calculus 
# Road density and Interactions
# Mestre et al.


library(sf)
library(dplyr)
library(tidyr)
library(terra)
library(viridis)
library(ggplot2)
library(cowplot)

#rm(list=ls())

# load data ---------------------------------------------------------------


#Europe
#Europe <- st_read("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/GIS/Europe.shp")
Europe <- st_read("C:/Users/asus/Documents/0. Artigos/roads_networks/data/area_roads_eco_networks.shp")

#Europe <- st_simplify(Europe, dTolerance = 1000)
#plot(st_geometry(Europe))

# template
template <- terra::vect("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\fernando_26set_2023\\template_grilo.shp")

# road density
road.dens <- st_read("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\data_artigo_clara_grilo\\Nroadkillgrid50.shp")

# taxonomy
#setwd("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/anexos_roads_networks")
#load("tax_table_3_10OUT23.Rdata")

tax_table_3

# datasets
#setwd("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/Excels")

myd.mamm <- read.csv("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\fernando_26set_2023\\matrix_occurrence_mammals.csv")
myd.birds <- read.csv("C:\\Users\\asus\\Documents\\0. Artigos\\roads_networks\\data\\fernando_26set_2023\\matrix_occurrence_birds.csv")

myd <- myd.mamm %>% left_join(myd.birds) %>% as_tibble()


# mapping species distributions and vulnerability -------------------------
tax_table_3 %>% as_tibble() 



(p.template <- ggplot() +
    geom_sf(data = Europe, fill="white", col="grey20", linewidth=.6) +
    theme_void())

table(tax_table_3$order)
carnivores <- tax_table_3$species_maiorano[tax_table_3$order=="Carnivora"]
carnivores <- tax_table_3[tax_table_3$order=="Carnivora",]

# mapping
pdf(file = "Carnivores.pdf")

for(i in 1:length(carnivores)){
  
  mytaxa1 <- carnivores[i]
  
  mytaxa.df = tax_table_3 %>%
    filter(species_maiorano == mytaxa1)
  
  mytaxa = gsub(" ", "\\.", carnivores[i])
  
  if(!is.na(mytaxa)){
    myd.sub <- myd %>% select(PageNumber , one_of(mytaxa))
    
    setwd("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/Images")
    
    if(nrow(myd.sub) > 0){
      
      names(myd.sub)[2] = "taxa"
      myd.sub <- myd.sub %>% filter(taxa==1)
      
      
      template.sub.distribution <- road.dens %>%
        filter(PageNumber %in% myd.sub$PageNumber) 
      
      template.sub.roads <- road.dens %>%
        filter(PageNumber %in% myd.sub$PageNumber &
                 kmkm2 >  mytaxa.df$grilo_threshold) 
      
      p = p.template +
        geom_sf(data=template.sub.distribution, col="darkgreen",  fill="darkgreen") +
        geom_sf(data=template.sub.roads, fill="darkred", col="darkred") +
        labs(title=paste(mytaxa.df$class, mytaxa.df$order, mytaxa.df$family, sep=", "),
             subtitle = mytaxa1) +
        theme_map() +
        theme(plot.subtitle = element_text(face = "italic"),
              text=element_text(size=14))
      
      print(p)
      
      
    }
  }
}

dev.off()













# OLD ---------------------------------------------------------------------


# load data ---------------------------------------------------------------

est.interactions <- st_read("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/anexos_roads_networks/nr_interactions23set23.shp")
richness <- st_read("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/anexos_roads_networks/S2.shp")
road.density <- st_read("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/SuppMat_ROADKILL RISK AND POPULATION VULNERABILITY IN EUROPEAN BIRDS AND MAMMALS/Nroadkillgrid50.shp")
interactions.lost <- st_read("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/anexos_roads_networks/fig1.shp")


richness <- st_transform(richness, 3035)
interactions.lost <- st_transform(interactions.lost, 3035)

# convert to raster -------------------------------------------------------
template <- rast("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/RASTERs/N_interactions.tif")
library(mgcv)

est.interactions.r <- rasterize(vect(est.interactions), template, field="nr_interac")
richness.r <- rasterize(vect(richness), template, field="sp_richnes")
road.density.r <- rasterize(vect(road.density), template, field="kmkm2")
interactions.lost.r <- rasterize(vect(interactions.lost), template, field="lost_inter")

lon <- init(template, 'x')
lat <- init(template, 'y')

lon <- mask(lon, template)
lat <- mask(lat, template)

names(lat) = "lat"
names(lon) = "lon"

plot(lon)
plot(lat)


mystack <- c(est.interactions.r, richness.r, road.density.r, interactions.lost.r, lat, lon)
plot(mystack)


rnd.pnts <- spatSample(mystack, size=1000, method="random", replace=FALSE, na.rm=T, 
                       as.raster=FALSE, as.df=TRUE, as.points=FALSE, 
                       values=TRUE, cells=FALSE, 
                       xy=TRUE)

rnd.pnts <- rnd.pnts %>% as_tibble()


summary(m1 <- lm(lost_inter ~ scale(kmkm2) + scale(sp_richnes) + scale(lon) + scale(lat), data=rnd.pnts))


(g1 <- ggplot(rnd.pnts, aes(sp_richnes, nr_interac)) +
    geom_smooth(col="grey") +
    geom_point(alpha=.5, size=2) +
    # scale_color_viridis() +
    theme_minimal())


(g2 <- ggplot(rnd.pnts, aes(kmkm2, lost_inter)) +
    geom_smooth(col="grey") +
    geom_point(alpha=.5, size=2) +
    # scale_color_viridis() +
    theme_minimal())


