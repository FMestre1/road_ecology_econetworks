################################################################################
################################################################################
#                  SCRIPT 8 - CODE TO GENERATE THE FIGURES
################################################################################
################################################################################

#Load packages
library(sf)
library(dplyr)
library(tidyr)
library(viridis)
library(ggplot2)
library(cowplot)
library(gridExtra)
require(RColorBrewer)

rm(list=ls())

################################################################################
#                                  load data
################################################################################
#setwd()
dir()

# Load master table
load("table16_full_information_20NOV23.RData")

tab16 <- table16_full_information_20NOV23
rm(table16_full_information_20NOV23)
gc()

# visualize 
tab16 <- tab16 %>% as_tibble() %>%
  filter(predator_species != prey_species) # remove cannibalism
tab16


# GIS info
# road density
road.dens <- st_read("GIS/Nroadkillgrid50.shp")

#Europe
Europe <- st_read("GIS/Europe.shp")
Europe <- st_simplify(Europe, dTolerance = 10000)
plot(st_geometry(Europe))

# template
template <- st_read("GIS/template_grilo.shp")

################################################################################
#                              Add trophic level
################################################################################

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



################################################################################
#                Loss of interactions directly and indirectly
################################################################################

# taxonomy
#setwd()
taxonomy <- read.csv("taxonomy.csv", head=T) %>% as_tibble()


# All Species
all.species <- trophic.level.df$species

myd = data.frame()

for (i in 1:length(all.species)){
  
  mysp = all.species[i]
  
  x = tab16 %>% 
    filter(predator_species == mysp | prey_species == mysp)
  
  # AOO => How many grids I occupy?
  AOO = x %>%
    select(grid_name) %>%
    distinct()
  AOO = nrow(AOO)
  
  
  Nit_total = nrow(x)
  Nit_pred = nrow(x %>% filter(predator_species == mysp))
  Nit_prey = nrow(x %>% filter(prey_species == mysp))
  
  summary.int = x %>%
    group_by(grid_name) %>%
    summarise(sum = n()) %>%
    ungroup() %>%
    summarise(Mean.int = mean(sum),
              SD = sd(sum))
  
  # from how many grids I might get extinct from direct effects?
  direct.effect = x %>%
    filter((predator_species == mysp & road_density >= predator_species_vuln) |
             (prey_species == mysp & road_density >= prey_species_vuln))
  
  Nint.at.risk_Direct = nrow(direct.effect)
  Nint.at.risk_Direct.as.pred = nrow(direct.effect %>% filter(predator_species == mysp))
  Nint.at.risk_Direct.as.prey = nrow(direct.effect %>% filter(prey_species == mysp))
  
  direct.effect.area = length(unique(direct.effect$grid_name))
  
  gc()
  
  # Area of cascading zone
  cascading.effect = x %>%
    filter(!grid_name %in% direct.effect$grid_name) %>%
    filter((predator_species != mysp & road_density >= predator_species_vuln) |
             (prey_species != mysp & road_density >= prey_species_vuln))
  
  Nint.at.risk_Indirect = nrow(cascading.effect)
  Nint.at.risk_Indirect.as.pred = nrow(cascading.effect %>% filter(predator_species == mysp))
  Nint.at.risk_Indirect.as.prey = nrow(cascading.effect %>% filter(prey_species == mysp))
  
  cascading.effect.area = length(unique(cascading.effect$grid_name))
  
  gc()
  
  
  # join
  myd1 <- data.frame(species = mysp,
                     Nit_total = Nit_total,
                     Nit_pred = Nit_pred,
                     Nit_prey = Nit_prey,
                     Mean.int = summary.int$Mean.int,
                     SD.int = summary.int$SD,
                     
                     Nint.at.risk_Direct = Nint.at.risk_Direct,
                     Nint.at.risk_Direct.as.pred = Nint.at.risk_Direct.as.pred,
                     Nint.at.risk_Direct.as.prey = Nint.at.risk_Direct.as.prey,
                     
                     Nint.at.risk_Indirect = Nint.at.risk_Indirect,
                     Nint.at.risk_Indirect.as.pred = Nint.at.risk_Indirect.as.pred,
                     Nint.at.risk_Indirect.as.prey = Nint.at.risk_Indirect.as.prey,
                     
                     AOO = AOO,
                     
                     direct.effect.area = direct.effect.area,
                     cascading.effect.area = cascading.effect.area,
                     direct.effect.prop = direct.effect.area/AOO*100,
                     cascading.effect.prop = cascading.effect.area/AOO*100)
  
  
  myd = rbind(myd, myd1)  
  
  print(i)
  
}

gc()

# join info
myd_all <- myd %>% 
  left_join(trophic.level.df) %>% 
  left_join(taxonomy) %>%
  relocate(class, species, level) %>%
  arrange(class, level, species) %>%
  as_tibble()

setwd("Supp_Mat")
write.csv(myd_all, "SuppMatS1_20240709.csv", row.names = F)

###############################################################################
#                                  Results
###############################################################################

# Our pool consisted of 551 species  
# 12 predators (2.2% of total species), 107 (19.4%) intermediate, and 432 (78.4%) basal species.

nrow(trophic.level.df)
table(trophic.level.df$level)
(table(trophic.level.df$level)/nrow(trophic.level.df))*100

###############################################################################
#                                  Figure 1 
###############################################################################

### Make maps of distribution (Fig 1 top) ###
# The original figures were built in QGIS
# here we build a reproduction of those figures

# (p.template <- ggplot() +
#     geom_sf(data = Europe, fill="white", col="grey20", linewidth=1) +
#     theme_void())
# 
# 
# # distribution of top level
# #
# dist.top <- tab16 %>%
#   select(grid_name, predator_species ) %>%
#   distinct() %>%
#   filter(predator_species %in% trophic.level.df$species[trophic.level.df$level == "top"]) %>%
#   group_by(grid_name) %>%
#   summarise(Richness = n())
# 
# dist.top.spat <- template %>%
#   left_join(dist.top, by=c("PageName" = "grid_name"))
# 
# (p.top = p.template +
#     geom_sf(data=dist.top.spat, col=NA,  aes(fill=Richness)) +
#     scale_fill_distiller(palette = "YlOrBr",  direction=1) +
#     labs(title="Top level richness") +
#     theme_map() +
#     theme(plot.subtitle = element_text(face = "italic"),
#           text=element_text(size=14)))
# 
# 
# # distribution of intermediate level
# #
# dist.interm1 <- tab16 %>%
#   select(grid_name, predator_species) %>%
#   filter(predator_species %in% trophic.level.df$species[trophic.level.df$level == "intermediate"]) %>%
#   distinct() %>%
#   rename(species = 2)
# 
# dist.interm2 <- tab16 %>%
#   select(grid_name, prey_species) %>%
#   filter(prey_species %in% trophic.level.df$species[trophic.level.df$level == "intermediate"]) %>%
#   distinct() %>%
#   rename(species = 2)
# 
# dist.interm <- dist.interm1 %>%
#   bind_rows(dist.interm2) %>%
#   distinct() %>%
#   group_by(grid_name) %>%
#   summarise(Richness = n()) %>%
#   arrange(-Richness)
# 
# dist.interm.spat <- template %>%
#   left_join(dist.interm, by=c("PageName" = "grid_name"))
# 
# (p.interm = p.template +
#     geom_sf(data=dist.interm.spat, col=NA,  aes(fill=Richness)) +
#     scale_fill_distiller(palette = "YlOrBr",  direction=1) +
#     labs(title="Intermediate level richness") +
#     theme_map() +
#     theme(plot.subtitle = element_text(face = "italic"),
#           text=element_text(size=14)))
# 
# 
# # distribution of basal level
# #
# dist.basal <- tab16 %>%
#   select(grid_name, prey_species ) %>%
#   distinct() %>%
#   filter(prey_species %in% trophic.level.df$species[trophic.level.df$level == "basal"]) %>%
#   group_by(grid_name) %>%
#   summarise(Richness = n())
# 
# dist.basal.spat <- template %>%
#   left_join(dist.basal, by=c("PageName" = "grid_name"))
# 
# 
# (p.basal = p.template +
#     geom_sf(data=dist.basal.spat, col=NA,  aes(fill=Richness)) +
#     scale_fill_distiller(palette = "YlOrBr",  direction=1) +
#     labs(title="basal level richness") +
#     theme_map() +
#     theme(plot.subtitle = element_text(face = "italic"),
#           text=element_text(size=14)))
# 
# 
# grid.arrange(p.top,
#              p.interm,
#              p.basal, nrow=1)




# 5.0% of interactions (n=484,102) across Europe are at risk of being lost 
# due to roadkill i.e., where grid cells have a road density higher than a 
# specific threshold of vulnerability to roadkill (primary extinctions).

# total number of interactions
nrow(tab16)

# mean per grid
(pot.interactions <- tab16 %>% 
    group_by(grid_number) %>%
    summarise(n = n(),
              n.loss = sum(road_density > prey_species_vuln | 
                             road_density > predator_species_vuln)) %>%
    mutate(loss.prop = n.loss/n*100) %>%
    summarise(mean.int=mean(n),
              sd.int=sd(n),
              mean.loss=mean(n.loss),
              sd.loss=sd(n.loss),
              mean.loss.p=mean(loss.prop),
              sd.loss.p=sd(loss.prop)))

###############################################################################
#                                  Figure 2 
###############################################################################

(p.template <- ggplot() +
    geom_sf(data = Europe, fill="white", col="grey20", linewidth=1) +
    theme_void())

all.int <- tab16 %>%
  group_by(grid_name) %>%
  summarise(Interactions = n())

int.loss <- tab16 %>%
  filter(road_density > prey_species_vuln | road_density > predator_species_vuln) %>%
  group_by(grid_name) %>%
  summarise(Interactions.loss = n()) %>%
  left_join(all.int) %>%
  mutate(prop.loss = Interactions.loss / Interactions * 100)

int.loss.spat <- template %>%
  left_join(int.loss, by=c("PageName" = "grid_name"))


(p.int.risk = p.template +
    geom_sf(data=int.loss.spat, col=NA,  aes(fill=prop.loss)) +
    scale_fill_distiller(palette = "Spectral",  direction=-1) +
    labs(title="Interactions loss (%)") +
    theme_map() +
    theme(plot.subtitle = element_text(face = "italic"),
          text=element_text(size=14)))


(g.loss.road.dens <- ggplot(road.dens, 
                            aes(x=road_density, y=prop.loss*100)) +
    geom_point(alpha=.2, show.legend = F) +
    geom_smooth(method = lm, col="darkred",
                linewidth=1.2, formula = y ~ splines::bs(x, df=3), se = FALSE, show.legend = F) +
    labs(x=expression ("Road density in"~Km/Km^2), y="Loss of interactions (%)") +
    theme_minimal() +
    theme(text=element_text(size=14)))


setwd("Images")
# ggsave("Fig.png", plot=g.loss.road.dens,  width=90, height=90, units="mm", dpi=300)

grid.arrange( p.int.risk,
              g.loss.road.dens, nrow=1)


###############################################################################
#                                  Figure 3 
###############################################################################

setwd("Supp_Mat")

myd_all <- read.csv("SuppMatS1_20240708.csv", head=T) %>%
  as_tibble()

names(myd_all)


(x <- myd_all %>%
    mutate(prop.loss.riskZone = Nint.at.risk.RK/Nint1*100,
           prop.loss.safeZone = Nint.at.risk.SZ/Nint1*100) %>%
    group_by(level) %>%
    summarise(prop.riskZone = mean(prop.loss.riskZone),
              prop.riskZone.sd = sd(prop.loss.riskZone),
              prop.safeZone = mean(prop.loss.safeZone),
              prop.safeZone.sd = sd(prop.loss.safeZone)))


(x2 <- myd_all %>% 
    mutate(prop.loss.riskZone = Nint.at.risk.RK/Nint1*100,
           prop.loss.safeZone = Nint.at.risk.SZ/Nint1*100) %>%
    group_by(level) %>%
    filter(prop.loss.riskZone >= 10))

x2 %>% group_by(level) %>% summarise(n=n())

x2 %>% arrange(-prop.loss.riskZone) %>% 
  group_by(level) %>% 
  slice_head(n=5) %>% 
  select(species, AOO, prop.loss.riskZone)



#
# About Range Area
#
names(myd_all)

# nº species'
myd_all %>% 
  group_by(level) %>%
  summarise(n=n(),
            n.risk=sum(risk.zone.area > 50))

1+38+147
(1+38+147)/551

(z3 <- myd_all %>% 
    mutate(prop.area.riskZone = risk.zone.area/AOO*100,
           prop.area.safeZone = safe.zone.area/AOO*100,
           prop.area.cascading = cascading.zone.area/AOO*100))


z3 %>%
  group_by(level) %>%
  summarise(n.free=sum(prop.area.cascading > 10))
10+81+99
190/551


z3 %>% 
  group_by(level) %>%
  summarise(mean=mean(prop.area.riskZone),
            sd=sd(prop.area.riskZone))

z3 %>% 
  group_by(level) %>%
  summarise(mean=mean(prop.area.cascading),
            sd=sd(prop.area.cascading))


z3 %>% group_by(level) %>% 
  arrange(-prop.area.riskZone) %>%
  slice_head(n=5) %>% 
  select(species, AOO, prop.area.riskZone)


z3 %>% group_by(level) %>% 
  arrange(-prop.area.cascading) %>%
  slice_head(n=5) %>% 
  select(species, AOO, prop.area.cascading)


#############################################################################
#                                Plots
#############################################################################

names(myd_all)

myd_all$level <- factor(myd_all$level, levels=c('top', 'intermediate', 'basal'))

myd_all$class[myd_all$species=="Acomys minous"] = "Mammals"


#display factor levels for region
levels(myd_all$level) <- c('Top', 'Intermediate', 'Basal')

myd_all$level <- factor(myd_all$level, levels=c('Top', 'Intermediate', 'Basal'))


(g2 <- ggplot(myd_all, 
              aes(direct.effect.prop, cascading.effect.prop, 
                  size=AOO,
                  fill=level)) +
    geom_abline(intercept=100, slope=-1, col="grey90") +
    geom_point(alpha=.5, shape=21) +
    # geom_jitter(alpha=.5, shape=21, width = 1, height = 5) +
    labs(x = "Loss due to direct effect (%)",
         y = "Loss due to indirect effect (%)") +
    facet_grid(class~level) +
    coord_equal() +
    scale_y_continuous(breaks = c(20, 40, 60, 80), limits = c(0,100)) +
    scale_x_continuous(breaks = c(20, 40, 60, 80), limits = c(0,100)) +
    # ylim(0,100) +
    # scale_y_continuous(labels=f) +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal() +
    theme(text=element_text(size=14),
          legend.position = "none"))



setwd("Images")
ggsave("Fig3_20240709.png", plot=g2,  width=180, height=120, units="mm", dpi=300)

# notes on species. Maybe add a second species, one per class. In the text
names(myd_all)

# direct effects
myd_all %>%
  group_by(class, level) %>%
  filter(direct.effect.prop >= 10) %>%
  summarise(n=n())

myd_all %>%
  group_by(class, level) %>%
  select(species, class, level, direct.effect.prop) %>%
  arrange(-direct.effect.prop) %>%
  slice_head(n=2)

# indirect effects
myd_all %>%
  group_by(class, level) %>%
  filter(cascading.effect.prop >= 10) %>%
  summarise(n=n())

myd_all %>%
  group_by(class, level) %>%
  select(species, class, level, cascading.effect.prop) %>%
  arrange(-cascading.effect.prop) %>%
  slice_head(n=2)



191/551

#############################################################################
#                                Area Loss
#############################################################################

names(myd_all)
hist(myd_all$AOO)
quantile(myd_all$AOO)


(g.alluvial <- myd_all %>%
    group_by(level) %>%
    summarise(Risk = mean(risk.zone.area),
              Cascade = mean(cascading.zone.area),
              Safe = mean(safe.zone.area)) %>%
    gather(x,y, -level))

g.alluvial$x <- factor(g.alluvial$x, levels=c('Risk', 'Cascade', 'Safe'))

ggplot(g.alluvial %>% filter(x %in% c("Risk", "Cascade")),
       aes(axis1 = level, axis2 = x,
           y = y)) +
  scale_x_discrete(limits = c("Trophic level", "Zone"), expand = c(.05, .05)) +
  xlab("xxx") +
  ylab("Number of grid cells") +
  scale_fill_brewer(palette="Dark2")+
  geom_alluvium(aes(fill = level)) +
  geom_stratum(fill = "grey95", color="grey") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size=12))


setwd("C:/Users/fjascensao/OneDrive - Universidade de Lisboa/Documents/Interactions and Road density/Images")
# ggsave("Fig6.png",  width=100, height=100, units="mm", dpi=300)

#############################################################################
# Proportion of interactions at risk across whole species range and safe zone
#############################################################################

(g.interactions <- ggplot(myd_full, 
                          aes(level, Nint2.prop, 
                              size=Nint1,
                              fill=level)) +
    geom_jitter(alpha=.5, show.legend = T, width=.15, height=0,
                shape=21) +
    stat_summary(fun.y = mean, fill="white", size = 1, shape=21) +
    labs(title="A) Across species distribution", x = "Trophic level", y = "Loss interactions (%)") +
    facet_wrap(~type, scales="free", ncol=1) +
    # scale_y_continuous(labels=f) +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal() +
    theme(text=element_text(size=14),
          legend.position = "none"))


(g.interactions.2 <- ggplot(myd_full, 
                            aes(level, Nint3.prop, 
                                size=Nint1,
                                fill=level)) +
    geom_jitter(alpha=.5, show.legend = T, width=.15, height=0,
                shape=21) +
    stat_summary(fun.y = mean, fill="white", size = 1, shape=21) +
    labs(title="B) Within the 'safe zone'", x = "Trophic level", y = "Loss interactions (%)") +
    facet_wrap(~type, scales="free", ncol=1) +
    scale_y_continuous(labels=f) +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal() +
    theme(text=element_text(size=14),
          legend.position = "none"))


gx <- grid.arrange(g.interactions, g.interactions.2, ncol=2)


setwd("Images")
ggsave("Fig3.png", plot=gx,  width=180, height=120, units="mm", dpi=300)


#
## average loss
#

# all distribution range
#
myd_full %>%
  filter(Nint2.prop > 10 & type == "As predator") %>%
  group_by(level) %>%
  summarise(n=n())

myd_full %>%
  filter(Nint2.prop > 10 & type == "As prey") %>%
  group_by(level) %>%
  summarise(n=n())

myd_full %>%
  filter(Nint2.prop > 75 & type == "As prey") %>%
  group_by(level) %>%
  summarise(n=n())

#############################################################################
#                               Safe zone
#############################################################################

names(myd_full)

myd_full %>%
  arrange(-Nint3.prop) %>%
  group_by(type, level) %>%
  summarise(mean=mean(Nint3.prop, na.rm=T),
            sd=sd(Nint3.prop, na.rm=T))

myd_full %>%
  arrange(-Nint3.prop) %>%
  group_by(type, level) %>%
  slice_head(n=10) %>%
  print(n=100)

myd_full %>% filter(species == "Hieraaetus pennatus")


#############################################################################
#      Area to be lost across their distribution range, per species
#############################################################################

## predators
##
myd.pred.area = data.frame()


for (i in 1:length(all.pred)){
  
  mysp = all.pred[i]
  
  x = tab16 %>% filter(predator_species == mysp)
  present.ncells = length(unique(x$grid_number))
  
  
  # after possible primary extinctions
  x2 = x %>%
    filter(road_density < predator_species_vuln)
  
  after.primary.ncells <- length(unique(x2$grid_number))
  
  # after possible secondary extinctions
  x3 = x2 %>%
    filter(road_density < prey_species_vuln)
  
  after.second.ncells <- length(unique(x3$grid_number))
  
  
  myd1 = data.frame(species = mysp,
                    present.ncells = present.ncells,
                    after.primary.ncells = after.primary.ncells,
                    after.second.ncells = after.second.ncells)
  
  
  myd.pred.area = rbind(myd.pred.area, myd1)  
  
  print(i)
}

myd.pred.area <- myd.pred.area %>%  left_join(trophic.level.df) %>% as_tibble()

## prey
##
myd.prey.area = data.frame()

for (i in 1:length(all.prey)){
  
  mysp = all.prey[i]
  
  x = tab16 %>% filter(prey_species == mysp)
  present.ncells = length(unique(x$grid_number))
  
  
  # after possible primary extinctions
  x2 = x %>%
    filter(road_density < prey_species_vuln)
  
  after.primary.ncells <- length(unique(x2$grid_number))
  
  
  
  # release effect
  x3 = x %>%
    filter(road_density < prey_species_vuln & 
             road_density > predator_species_vuln)
  
  after.second.ncells <- length(unique(x3$grid_number))
  
  
  myd1 = data.frame(species = mysp,
                    present.ncells = present.ncells,
                    after.primary.ncells = after.primary.ncells,
                    after.second.ncells = after.second.ncells)
  
  
  myd.prey.area = rbind(myd.prey.area, myd1)  
  
  print(i)
}

myd.prey.area <- myd.prey.area %>%  left_join(trophic.level.df) %>% as_tibble()


# Predators
table(myd.pred.area$level)

# loss area due to Roadkill
pred.loss.area.by.RK <- myd.pred.area %>% filter(after.primary.ncells < present.ncells) #  species
table(pred.loss.area.by.RK$level) # 75 :: 8 + 67

# Loss area due to Prey loss
pred.loss.area.by.prey <- myd.pred.area %>% filter(after.second.ncells < after.primary.ncells) #  species
table(pred.loss.area.by.prey$level) # 9


# Prey
table(myd.prey.area$level)

# loss area due to Roadkill
prey.loss.area.by.RK <- myd.prey.area %>% filter(after.primary.ncells < present.ncells) #  species
table(prey.loss.area.by.RK$level) # 333 :: 266 + 67

prey.released.area.by.pred <- myd.prey.area %>% filter(after.second.ncells > 0) #  species
table(prey.released.area.by.pred$level) # 333 :: 266 + 67

# Plot 
myd.prey.area$type = "As prey"
myd.pred.area$type= "As predator"

myd.area <- myd.prey.area %>%
  bind_rows(myd.pred.area) %>%
  mutate(prop.area.loss.rk = (1-after.primary.ncells/present.ncells)*100,
         area.change.lossInt = ifelse(type=="As predator", 
                                      (1-after.second.ncells/after.primary.ncells)*100,
                                      (after.second.ncells/after.primary.ncells)*100))


myd.area %>%
  group_by(level, type) %>%
  summarise(dueToRK = mean(prop.area.loss.rk),
            dueToRK.sd = sd(prop.area.loss.rk),
            dueTolossInt = mean(area.change.lossInt, na.rm=T),
            dueTolossInt.sd = sd(area.change.lossInt, na.rm=T))

(g.area <- ggplot(myd.area, 
                  aes(level, prop.area.loss.rk, 
                      size=present.ncells,
                      fill=level)) +
    geom_jitter(alpha=.5, show.legend = T, width=.15, height=0,
                shape=21) +
    stat_summary(fun.y = mean, fill="white", size = 1, shape=21) +
    labs(title="A) Due to roadkill",
         subtitle="(across area of occurrence)",
         x = "Trophic level", y = "Area with losses(%)") +
    facet_wrap(~type, scales="free", ncol=1) +
    scale_y_continuous(labels=f) +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal() +
    theme(text=element_text(size=14),
          legend.position = "none"))


(g.area2 <- ggplot(myd.area, 
                   aes(level, area.change.lossInt, 
                       size=2,
                       fill=level)) +
    geom_jitter(alpha=.5, show.legend = T, width=.15, height=0,
                shape=21) +
    stat_summary(fun.y = mean, fill="white", size = 1, shape=21) +
    labs(title="B) Due to cascading effects",
         subtitle = "(within the safe zone)",
         x = "Trophic level", y = "Area with changes (%)") +
    facet_wrap(~type, scales="free", ncol=1) +
    scale_y_continuous(labels=f) +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal() +
    theme(text=element_text(size=14),
          legend.position = "none"))



gy <- grid.arrange(g.area, g.area2, ncol=2)

setwd("Images")
ggsave("Fig4.png", plot=gy,  width=180, height=120, units="mm", dpi=300)


# Table

setwd("Supp_Mat")
write.csv(myd.area, "SuppMatS2.csv", row.names = F)


#################################################################################
#                                 Discussion
#################################################################################

myd.area %>% filter(level == "top") %>% 
  mutate(prop.area.loss = (1 - after.primary.ncells/present.ncells) * 100) %>%
  arrange(-prop.area.loss)

myd %>% filter(level == "top")

loss.interactions <- data.frame()

for(i in 1:length(top)){
  
  mydf <- tab16 %>% filter(predator_species == top[i])
  
  distribution.area <- mydf %>%
    select(grid_name) %>%
    distinct()
  
  area.at.risk <- mydf %>%
    filter(road_density > predator_species_vuln) %>%
    select(grid_name) %>%
    distinct()
  
  all.interactions <- nrow(mydf)
  
  interactions.at.risk <- mydf %>%
    filter(road_density > prey_species_vuln)
  nrow(interactions.at.risk)  
  
  nrow(distribution.area)
  nrow(area.at.risk)
  
}

a1 <- nrow(tab16 %>% filter(predator_species == "Hieraaetus pennatus"))
a2 <- nrow(tab16 %>% filter(predator_species == "Hieraaetus pennatus" &
                              road_density > prey_species_vuln))
a2/a1*100

