################################################################################
################################################################################
#                  SCRIPT 7. - RESULTS - SUMMARY
################################################################################
################################################################################

library(sf)
library(dplyr)
# library(tidyr)
# library(terra)
# library(viridis)
library(ggplot2)
library(cowplot)
# library(gridExtra)
# library(BAMMtools) # fast calculation of jenks natural breaks
# library(ggpubr)
# library(tidyverse)
# library(grid)
# library(gridtext)
# require(RColorBrewer)

rm(list=ls())


# load data --------------------------------------------------------------------
setwd("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/Data_20231120")
setwd("~/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/Data_20231120")
dir()

load("table16_full_information_20NOV23.RData")

tab16 <- table16_full_information_20NOV23
rm(table16_full_information_20NOV23)
gc()

tab16 <- tab16 %>% as_tibble()
tab16




# add trophic level ------------------------------------------------------------

prey <- unique(tab16$prey_species)
predator <- unique(tab16$predator_species)

basal <- prey[!prey %in% predator]
intermediate <- prey[prey %in% predator]
top <- predator[!predator %in% prey]

trophic.level.df <- data.frame(species = c(basal,
                                         intermediate,
                                         top),
                               level = rep(c("basal",
                                         "intermediate",
                                         "top"), c(length(basal),
                                                        length(intermediate),
                                                        length(top)))) %>%
  as_tibble()

trophic.level.df




# Overall patterns =============================================================

# number of interactions
#

#total interactions
nrow(tab16)

# mean per grid
tab16 %>% group_by(grid_number) %>%
  summarise(n = n()) %>%
  summarise(mean=mean(n),
            sd=sd(n),)



### Interactions at risk

## n interactions
x <- tab16 %>%
  filter(road_density < prey_species_vuln & road_density < predator_species_vuln)

# to be lost
round((1 - nrow(x) / nrow(tab16)) * 100, digits=1)
nrow(tab16) - nrow(x)


# how many local extinctions?
all.pred <- c(intermediate, top)

myd = data.frame()

## predators
for (i in 1:length(all.pred)){
  
  mysp = all.pred[i]
  
  x = tab16 %>% filter(predator_species == mysp)
  present.ncells = length(unique(x$grid_number))
  
  # after possible primary extinctions
  x2 = x %>%
    filter(road_density < predator_species_vuln)
  
  after.primary.ncells <- length(unique(x2$grid_number))
  
  # after possible seconday extinctions
  x3 = x2 %>%
    filter(road_density < prey_species_vuln)
  
  after.second.ncells <- length(unique(x3$grid_number))
  

  myd1 = data.frame(species = mysp,
                    present.ncells = present.ncells,
                    after.primary.ncells = after.primary.ncells,
                    after.second.ncells = after.second.ncells)
  

  myd = rbind(myd, myd1)  
  
  print(i)
}


myd <- myd %>%  left_join(trophic.level.df) %>% as_tibble()

myd %>% 
  filter(after.primary.ncells < present.ncells) %>%
  mutate(loss.grid = present.ncells - after.primary.ncells) %>%
  arrange(-loss.grid) %>%
  group_by(level) %>%
  summarise(n.species=n(), # 75 species
            sum.grid = sum(loss.grid),
            min.grid = min(loss.grid),
            max.grid = max(loss.grid),
            mean.grid = mean(loss.grid),
            sd.grid = sd(loss.grid))
  
primary.extinctions <- myd %>% filter(after.primary.ncells < present.ncells) #  species
table(primary.extinctions$level) # 75 :: 8 + 67

primary.extinctions %>%
  mutate(loss.grids = present.ncells - after.primary.ncells) %>% 
  group_by(level) %>%
  arrange(-loss.grids) %>%
  summarise(mean.loss = mean(loss.grids),
            sd.loss = sd(loss.grids))

primary.extinctions %>%
  mutate(loss.grids = present.ncells - after.primary.ncells) %>% 
  filter(level=="top") %>%
  arrange(-loss.grids)

myd %>% filter(after.second.ncells < after.primary.ncells) # 9 species



# basal?
mydb = data.frame()

for (i in 1:length(basal)){
  
  mysp = basal[i]
  
  x = tab16 %>% filter(prey_species == mysp)
  present.ncells = length(unique(x$grid_number))
  
  # after possible primary extinctions
  x2 = x %>%
    filter(road_density < predator_species_vuln)
  
  after.primary.ncells <- length(unique(x2$grid_number))
  
  myd1 = data.frame(species = mysp,
                    present.ncells = present.ncells,
                    after.primary.ncells = after.primary.ncells)
  
  
  mydb = rbind(mydb, myd1)  
  
  print(i)
}


mydb <- mydb %>%  left_join(trophic.level.df) %>% as_tibble()
primary.extinctions.basal <- mydb %>% filter(after.primary.ncells < present.ncells)
# 52 basal
primary.extinctions.basal %>%
  mutate(loss.grids = present.ncells - after.primary.ncells) %>% 
  group_by(level) %>%
  summarise(mean.loss = mean(loss.grids),
            sd.loss = sd(loss.grids))



# interactions loss per road density -------------------------------------------
# ( i didn't nothing new here)

template <- st_read("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/GIS/template_grilo.shp")
template <- st_read("~/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/GIS/template_grilo.shp")

template <- template %>%
  st_drop_geometry() %>%
  as_tibble()
template


road.dens <- tab16 %>%
  mutate(loss = ifelse(prey_species_vuln < road_density | predator_species_vuln  < road_density, 1, 0)) %>%
  group_by(grid_number, grid_name) %>%
  summarise(road_density = mean(road_density),
            n=n(),
            AFTER.PRIMMARY.EXTINCTIONS = sum(loss),
            prop.loss = AFTER.PRIMMARY.EXTINCTIONS / n)

road.dens
  
(g.loss.road.dens <- ggplot(road.dens, 
                            aes(x=road_density, y=prop.loss*100)) +
    geom_point(alpha=.2, show.legend = F) +
    geom_smooth(method = lm, col="darkred",
                linewidth=1.2, formula = y ~ splines::bs(x, 3), se = FALSE, show.legend = F) +
    labs(x=expression ("Road density in"~Km/Km^2), y="Loss of interactions (%)") +
    theme_minimal() +
    theme(text=element_text(size=14)))






# Species level ================================================================

# % of interaction loss in areas where they are likely to persit


## predators ?
all.pred <- c(intermediate, top)

myd = data.frame()

for (i in 1:length(all.pred)){
  
  mysp = all.pred[i]
  
  x = tab16 %>% filter(predator_species == mysp)
  
  Nint1 = nrow(x)
  
  # after possible primary extinctions
  x2 = x %>%
    filter(road_density < predator_species_vuln)
  
  Nint2 <- nrow(x2)
  
  
  # after extinctions of prey
  x3 = x2 %>%
    filter(road_density < prey_species_vuln)
  
  Nint3 <-  nrow(x3)
  
  
  myd1 = data.frame(species = mysp,
                    Nint1 = Nint1,
                    Nint2 = Nint2,
                    Nint3 = Nint3,
                    Nint2.prop = (1-Nint2/Nint1)*100,
                    Nint3.prop = (1-Nint3/Nint2)*100)
  
  
  myd = rbind(myd, myd1)  
  
  print(i)
}

myd <- myd %>%  left_join(trophic.level.df) %>% as_tibble()



# basal?
all.prey <- c(intermediate, basal)

mydb = data.frame()


for (i in 1:length(all.prey)){
  
  mysp = all.prey[i]
  
  x = tab16 %>% filter(prey_species == mysp)
  
  Nint1 = nrow(x)
  
  # after possible primary extinctions
  x2 = x %>%
    filter(road_density < prey_species_vuln )
  
  Nint2 <- nrow(x2)
  
  
  # after extinctions of predator
  x3 = x2 %>%
    filter(road_density < predator_species_vuln)
  
  Nint3 <-  nrow(x3)
  
  
  myd1 = data.frame(species = mysp,
                    Nint1 = Nint1,
                    Nint2 = Nint2,
                    Nint3 = Nint3,
                    Nint2.prop = (1-Nint2/Nint1)*100,
                    Nint3.prop = (1-Nint3/Nint2)*100)
  
  
  mydb = rbind(mydb, myd1)  
  
  print(i)
}


mydb <- mydb %>%  left_join(trophic.level.df) %>% as_tibble()

myd$type <- "As predator"
mydb$type <- "As prey"

myd_full <- rbind(myd, mydb)

f <- function(x){
  format(round(x,0), nsmall=0)
}

# Species level risk
(g.interactions <- ggplot(myd_full, 
                          aes(level, Nint3.prop, 
                              size=Nint2,
                              fill=level)) +
    geom_jitter(alpha=.5, show.legend = T, width=.2, height=0, 
                shape=21) +
    labs(x = "Trophic level", y = "Loss interactions (%)") +
    facet_wrap(~type, scales="free", ncol=1) +
    scale_y_continuous(labels=f) +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal() +
    theme(text=element_text(size=14),
          legend.position = "none"))


setwd("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/Images")
ggsave("Fig4.png", plot=g.interactions,  width=120, height=120, units="mm", dpi=300)


names(myd)

myd_full %>%
  arrange(-Nint3.prop) %>%
  group_by(type, level) %>%
  summarise(mean=mean(Nint3.prop, na.rm=T),
            sd=sd(Nint3.prop, na.rm=T))

myd_full %>%
  arrange(-Nint3.prop) %>%
  group_by(type, level) %>%
  slice_head(n=5)

################################################################################
#                       Plot for the Graphical abstract
################################################################################

#FMestre
#23-11-2023

#Load package
library(cheddar)

local_fw_MAIORANO[[4081]]
local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[4081]]
local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[4081]]

nodes0 <- NPS(local_fw_MAIORANO[[4081]])
nodes0$colour <- "black"
links0 <- cbind(TLPS(local_fw_MAIORANO[[4081]]), colour="#CCCCCC", width = 0.5)
####
nodes1 <- NPS(local_fw_MAIORANO[[4081]])
nodes1$colour <- "black"
links1 <- cbind(TLPS(local_fw_MAIORANO[[4081]]), colour="#CCCCCC", width = 0.5)
####
nodes2 <- NPS(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[4081]])
nodes2$colour <- "black"
links2 <- cbind(TLPS(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[4081]]), colour="#CCCCCC", width = 0.5)
####
nodes3 <- NPS(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[4081]])
nodes3$colour <- "black"
links3 <- cbind(TLPS(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[4081]]), colour="#CCCCCC", width = 0.5)
###
for(i in 1:nrow(links1)){
  row_links1 <- links1[i,]
  same_resource_df <- links2[row_links1$resource == links2$resource,]
  if(!row_links1$consumer %in% same_resource_df$consumer) {
    links1$colour[i] <- "orangered2"
    links1$width[i] <- 1.5
  }  
}
###
for(i in 1:nrow(links2)){
  row_links2 <- links2[i,]
  same_resource_df2 <- links3[row_links2$resource == links3$resource,]
  if(!row_links2$consumer %in% same_resource_df2$consumer) {
    links2$colour[i] <- "orangered2"
    links2$width[i] <- 1.5
  }
}
###

#as.character(paired_pagename_pagenumber[paired_pagename_pagenumber$PageNumber == 4081,][1])

par(mfrow=c(1,3))
PlotWebByLevel(local_fw_MAIORANO[[4081]], main = "CM32 - Start", pch=19, show.level.labels = FALSE, cex=1.5, link.col=links0$colour, link.lwd = links0$width)
PlotWebByLevel(local_fw_MAIORANO[[4081]], main = "Lost interactions - Primary Extinctions", pch=19, show.level.labels = FALSE, cex=1.5, link.col=links1$colour, link.lwd = links1$width)
PlotWebByLevel(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[4081]], main = "Lost interactions - Secondary Extinctions", pch=19, show.level.labels = FALSE, cex=1.5, link.col=links2$colour, link.lwd = links2$width)
#PlotWebByLevel(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[4081]], main = "CM32 - After secondary extinctions", pch=19, show.level.labels = FALSE, cex=1.5,link.col=links3$colour, link.lwd = links3$width)
