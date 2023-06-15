# Species interactions and Road Density

library(sf)
library(dplyr)
library(tidyr)




# load data ---------------------------------------------------------------

# GRID
setwd("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/SuppMat_ROADKILL RISK AND POPULATION VULNERABILITY IN EUROPEAN BIRDS AND MAMMALS")
master.grid <- st_read("Nroadkillgrid50.shp")

master.grid.cnt <- st_centroid(master.grid)


# Mammals ----------------------------------------------------------
setwd("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/GIS")
mammals <- st_read("Mammals_Europe.shp")

# assign grid ID 

myspecies <- unique(mammals$sci_name)

mydf <- list()

for(i in 1:length(myspecies)){
  
  my_taxa = myspecies[i]
  my_poly = mammals %>% filter(sci_name == my_taxa)
  
  
  if(nrow(my_poly) > 0){
    
    my_poly = st_transform(my_poly, 3035)
    my_poly = st_crop(my_poly, st_bbox(master.grid))
    
    my_poly1 = st_simplify(my_poly, dTolerance = 100)
    
    # plot(st_geometry(my_poly))
    # plot(st_geometry(my_poly1))
    # plot(st_geometry(master.grid.cnt), add=T, cex=.1)
    
    x = st_intersects(master.grid.cnt, my_poly1, sparce= T)
    
    my.grid = master.grid.cnt[apply(x, 1, any),]
    
    # plot(st_geometry(my.grid), add=T, cex=.5, col=2)
    
    if(nrow(my.grid) > 0){
      myinfo <- data.frame(PageName=my.grid$PageName, my_taxa)
      
      mydf[[i]] <- myinfo
      
    }
    
  }
  
  print(paste(i, my_taxa))
  
  gc()
}


mydf_mammals <- do.call(rbind.data.frame, mydf)

mydf_mammals <- mydf_mammals %>% 
  left_join(master.grid %>% st_drop_geometry() %>% select(PageName, Roadlength, kmkm2))

setwd("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/Excels")
write.csv(mydf_mammals, "mydf_mammals.csv", row.names = F)


# Birds ----------------------------------------------------------
setwd("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/GIS")
birds <- st_read("Birds_Europe.shp")

# assign grid ID 

myspecies <- unique(birds$binomial)

mydf <- list()


for(i in 1:length(myspecies)){
  
  my_taxa = myspecies[i]
  
  my_poly = birds %>% filter(binomial == my_taxa & 
                               presence== 1 & 
                               origin == 1 &
                               seasonal == 1)
  
  if(nrow(my_poly) > 0){
    
    my_poly = st_transform(my_poly, 3035)
    
    # my_poly = st_crop(my_poly, st_bbox(master.grid))
    # my_poly1 = st_simplify(my_poly, dTolerance = 100)
    
    # plot(st_geometry(my_poly))
    # plot(st_geometry(my_poly1))
    # plot(st_geometry(master.grid.cnt), add=T, cex=.1)
    
    x = st_intersects(master.grid.cnt, my_poly, sparce= T)
    
    my.grid = master.grid.cnt[apply(x, 1, any),]
    
    if(nrow(my.grid) > 0){
    myinfo <- data.frame(PageName=my.grid$PageName, my_taxa)
    
    mydf[[i]] <- myinfo
    
    }
    
  }
  
  print(paste(i, my_taxa))
  
  gc()
}


mydf_birds <- do.call(rbind.data.frame, mydf)

mydf_birds <- mydf_birds %>% 
  left_join(master.grid %>% st_drop_geometry() 
            %>% select(PageName, Roadlength, kmkm2))


setwd("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/Excels")
write.csv(mydf_birds, "mydf_birds.csv", row.names = F)


