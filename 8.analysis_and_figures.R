################################################################################
################################################################################
#                  SCRIPT 8 - CODE TO GENERATE THE FIGURES
################################################################################
################################################################################


# Road density simplifies regional food webs
# F. Mestre, V.A.G. Bastazini, F. Ascensão

#Load packages
library(viridis)
library(dplyr)
library(ggplot2)
library(cowplot)
library(sf)

rm(list=ls())

################################################################################
#                               Load data
################################################################################

#setwd()

# This table is in the Figshare repository for the paper: https://figshare.com/s/430ef7848f7036f13346
load("table16_full_information_20NOV23.RData")

taxonomy <- read.csv("taxonomy.csv", head=T) %>% as_tibble()

myd_all <- read.csv("SuppMat_S1.csv", head=T)

template <- st_read("template.shp")

gc()


# Add trophic level ------------------------------------------------------------

prey <- unique(table16_full_information_20NOV23$prey_species)
predator <- unique(table16_full_information_20NOV23$predator_species)

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
#                                    Results
################################################################################

# Our pool consisted of 551 species  
# 12 predators (2.2% of total species), 107 (19.4%) intermediate, and 432 (78.4%) basal species.

nrow(trophic.level.df)
table(trophic.level.df$level)
round((table(trophic.level.df$level)/nrow(trophic.level.df))*100, digits=1)

# total number of interactions
nrow(table16_full_information_20NOV23)

# mean per grid
(summary.interactions <- table16_full_information_20NOV23 %>% 
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


# Figure 2
(p.roadDens = ggplot() +
    geom_sf(data=template, col=NA,  aes(fill=kmkm2)) +
    scale_fill_viridis(option = "A") +
    labs(title="Road density") +
    theme_map() +
    theme(plot.subtitle = element_text(face = "italic"),
          text=element_text(size=14)))



names(table16_full_information_20NOV23)

road.dens <- table16_full_information_20NOV23 %>%
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
                linewidth=1.2, formula = y ~ splines::bs(x, df=3), se = FALSE, show.legend = F) +
    labs(x=expression ("Road density in"~Km/Km^2), y="Loss of interactions (%)") +
    theme_minimal() +
    theme(text=element_text(size=14)))

################################################################################
#                           About Range Area
################################################################################

# Figure 3 
names(myd_all)

myd_all$level <- factor(myd_all$level, levels=c('Top', 'Intermediate', 'Basal'))


(g3 <- ggplot(myd_all, 
              aes(direct.effect.prop, cascading.effect.prop, 
                  size=AOO,
                  fill=level)) +
    geom_abline(intercept=100, slope=-1, col="grey90") +
    geom_point(alpha=.5, shape=21) +
    labs(x = "Loss due to direct effect (%)",
         y = "Loss due to indirect effect (%)") +
    facet_grid(class~level) +
    coord_equal() +
    scale_y_continuous(breaks = c(20, 40, 60, 80), limits = c(0,100)) +
    scale_x_continuous(breaks = c(20, 40, 60, 80), limits = c(0,100)) +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal() +
    theme(text=element_text(size=14),
          legend.position = "none"))


# ggsave("Fig3_20240709.png", plot=g3,  width=180, height=120, units="mm", dpi=300)

################################################################################
#              Loss of interactions directly and indirectly
################################################################################


all.species <- trophic.level.df$species

myd = data.frame()

for (i in 1:length(all.species)){
  
  mysp = all.species[i]
  
  x = table16_full_information_20NOV23 %>%
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

# write.csv(myd_all, "SuppMat_S1.csv", row.names = F)
