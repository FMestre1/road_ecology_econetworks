################################################################################
#             Providing more Ecological Context to the Discussion
################################################################################

#FMestre
#03-10-2024

#Load datasets
load("~/github/road_ecoloy_econetworks/local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_METAWEB_TL_08NOV23.RData")
load("~/github/road_ecoloy_econetworks/local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_METAWEB_TL_08NOV23.RData")
load("~/github/road_ecoloy_econetworks/local_fw_MAIORANO_with_metaweb_TL_08NOV23.RData")

length(local_fw_MAIORANO)

local_fw_MAIORANO[[1]]$nodes
local_fw_MAIORANO[[1]]$trophic.links

#Lepus castroviejoi predator release from which predators? #####################

#Where is this species
fw_with_lcastro <- c()
for(i in 1:length(local_fw_MAIORANO)) fw_with_lcastro[i] <- "Lepus castroviejoi" %in% local_fw_MAIORANO[[i]]$nodes[,2]
#table(fw_with_lcastro)
fw_with_lcastro <- which(fw_with_lcastro == TRUE)

 #where did it lose predators, and which predators
predators_lcastro_start <- list()
predators_lcastro_primary <- list()
predators_lcastro_secondary <- list()

for(j in 1:length(fw_with_lcastro)){
  
  nr <- fw_with_lcastro[j]
  
  predators_lcastro_start[[j]] <-  local_fw_MAIORANO[[nr]]$trophic.links[local_fw_MAIORANO[[nr]]$trophic.links$resource == "Lepus castroviejoi",]$consumer
  predators_lcastro_primary[[j]] <-  local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links$resource == "Lepus castroviejoi",]$consumer
  predators_lcastro_secondary[[j]] <-  local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links$resource == "Lepus castroviejoi",]$consumer
}

from_start_to_primary_lcastro <- list()
from_primary_to_secondary_lcastro <- list()
from_start_to_secondary_lcastro <- list()

for(i in 1:11){
  
from_start_to_primary_lcastro[[i]] <- setdiff(predators_lcastro_start[i][[1]], predators_lcastro_primary[i][[1]])
from_primary_to_secondary_lcastro[[i]] <- setdiff(predators_lcastro_primary[i][[1]], predators_lcastro_secondary[i][[1]])
from_start_to_secondary_lcastro[[i]] <- setdiff(predators_lcastro_start[i][[1]], predators_lcastro_secondary[i][[1]])

}

lcastro_predators <- data.frame(table(unlist(from_start_to_secondary_lcastro)))
#sum(lcastro_predators$Freq)

################################################################################

#Curruca hortensis predator release from which predators? ######################

#Where is this species
fw_with_chort <- c()
for(i in 1:length(local_fw_MAIORANO)) fw_with_chort[i] <- "Sylvia hortensis" %in% local_fw_MAIORANO[[i]]$nodes[,2]
#for(i in 1:length(local_fw_MAIORANO)) fw_with_chort[i] <- any(grepl("hortensis", local_fw_MAIORANO[[i]]$nodes[,2]))
#table(fw_with_chort)
fw_with_chort <- which(fw_with_chort == TRUE)

#where did it lose predators, and which predators
predators_chort_start <- list()
predators_chort_primary <- list()
predators_chort_secondary <- list()

for(j in 1:length(fw_with_chort)){
  
  nr <- fw_with_chort[j]
  
  predators_chort_start[[j]] <-  local_fw_MAIORANO[[nr]]$trophic.links[local_fw_MAIORANO[[nr]]$trophic.links$resource == "Sylvia hortensis",]$consumer
  predators_chort_primary[[j]] <-  local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links$resource == "Sylvia hortensis",]$consumer
  predators_chort_secondary[[j]] <-  local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links$resource == "Sylvia hortensis",]$consumer
}

from_start_to_primary_chort <- list()
from_primary_to_secondary_chort <- list()
from_start_to_secondary_chort <- list()

for(i in 1:364){
  
  from_start_to_primary_chort[[i]] <- setdiff(predators_chort_start[i][[1]], predators_chort_primary[i][[1]])
  from_primary_to_secondary_chort[[i]] <- setdiff(predators_chort_primary[i][[1]], predators_chort_secondary[i][[1]])
  from_start_to_secondary_chort[[i]] <- setdiff(predators_chort_start[i][[1]], predators_chort_secondary[i][[1]])
  
}

chort_predators <- data.frame(table(unlist(from_start_to_secondary_chort)))
#sum(chort_predators$Freq)

chort_predators <- data.frame(chort_predators, round((chort_predators$Freq * 100)/327, 2))
names(chort_predators) <- c("predator_species", "lost_interactions", "percentage")

chort_predators <- chort_predators[order(-chort_predators$percentage), ]
sum(chort_predators$lost_interactions)

################################################################################

#Aquila chrysaetos loses interactions with which prey species?

#Where is this species
fw_with_aqchrys <- c()
for(i in 1:length(local_fw_MAIORANO)) fw_with_aqchrys[i] <- "Aquila chrysaetos" %in% local_fw_MAIORANO[[i]]$nodes[,2]
#table(fw_with_aqchrys)
fw_with_aqchrys <- which(fw_with_aqchrys == TRUE)

#where did it lose predators, and which predators
prey_aqchrys_start <- list()
prey_aqchrys_primary <- list()
prey_aqchrys_secondary <- list()

for(j in 1:length(fw_with_aqchrys)){
  
  nr <- fw_with_aqchrys[j]
  
  prey_aqchrys_start[[j]] <-  local_fw_MAIORANO[[nr]]$trophic.links[local_fw_MAIORANO[[nr]]$trophic.links$consumer == "Aquila chrysaetos",]$resource
  prey_aqchrys_primary[[j]] <-  local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links$consumer == "Aquila chrysaetos",]$resource
  prey_aqchrys_secondary[[j]] <-  local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links$consumer == "Aquila chrysaetos",]$resource
}

from_start_to_primary_aqchrys <- list()
from_primary_to_secondary_aqchrys <- list()
from_start_to_secondary_aqchrys <- list()

for(i in 1:364){
  
  from_start_to_primary_aqchrys[[i]] <- setdiff(prey_aqchrys_start[i][[1]], prey_aqchrys_primary[i][[1]])
  from_primary_to_secondary_aqchrys[[i]] <- setdiff(prey_aqchrys_primary[i][[1]], prey_aqchrys_secondary[i][[1]])
  from_start_to_secondary_aqchrys[[i]] <- setdiff(prey_aqchrys_start[i][[1]], prey_aqchrys_secondary[i][[1]])
  
}

aqchrys_prey <- data.frame(table(unlist(from_start_to_secondary_aqchrys)))
#sum(aqchrys_prey$Freq)

aqchrys_prey <- data.frame(aqchrys_prey, round((aqchrys_prey$Freq * 100)/620, 2))
names(aqchrys_prey) <- c("prey_species", "lost_interactions", "percentage")

aqchrys_prey <- aqchrys_prey[order(-aqchrys_prey$percentage), ]
sum(aqchrys_prey$lost_interactions)

################################################################################

#Canis lupus  loses interactions with which prey species?

#Where is this species
fw_with_clupus <- c()
for(i in 1:length(local_fw_MAIORANO)) fw_with_clupus[i] <- "Canis lupus" %in% local_fw_MAIORANO[[i]]$nodes[,2]
#table(fw_with_clupus)
fw_with_clupus <- which(fw_with_clupus == TRUE)

#where did it lose predators, and which predators
prey_clupus_start <- list()
prey_clupus_primary <- list()
prey_clupus_secondary <- list()

for(j in 1:length(fw_with_clupus)){
  
  nr <- fw_with_clupus[j]
  
  prey_clupus_start[[j]] <-  local_fw_MAIORANO[[nr]]$trophic.links[local_fw_MAIORANO[[nr]]$trophic.links$consumer == "Canis lupus",]$resource
  prey_clupus_primary[[j]] <-  local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links$consumer == "Canis lupus",]$resource
  prey_clupus_secondary[[j]] <-  local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links$consumer == "Canis lupus",]$resource
}

from_start_to_primary_clupus <- list()
from_primary_to_secondary_clupus <- list()
from_start_to_secondary_clupus <- list()

for(i in 1:2881){
  
  from_start_to_primary_clupus[[i]] <- setdiff(prey_clupus_start[i][[1]], prey_clupus_primary[i][[1]])
  from_primary_to_secondary_clupus[[i]] <- setdiff(prey_clupus_primary[i][[1]], prey_clupus_secondary[i][[1]])
  from_start_to_secondary_clupus[[i]] <- setdiff(prey_clupus_start[i][[1]], prey_clupus_secondary[i][[1]])
  
}

clupus_prey <- data.frame(table(unlist(from_start_to_secondary_clupus)))
#sum(clupus_prey$Freq)

clupus_prey <- data.frame(clupus_prey, round((clupus_prey$Freq * 100)/6328, 2))
names(clupus_prey) <- c("prey_species", "lost_interactions", "percentage")

clupus_prey <- clupus_prey[order(-clupus_prey$percentage), ]
sum(clupus_prey$lost_interactions)

################################################################################

#Tyto alba

#Lost interactions as predator #################################################

#Where is this species
fw_with_talba <- c()
for(i in 1:length(local_fw_MAIORANO)) fw_with_talba[i] <- "Tyto alba" %in% local_fw_MAIORANO[[i]]$nodes[,2]
#table(fw_with_talba)
fw_with_talba <- which(fw_with_talba == TRUE)

#where did it lose predators, and which predators
prey_talba_start <- list()
prey_talba_primary <- list()
prey_talba_secondary <- list()

for(j in 1:length(fw_with_talba)){
  
  nr <- fw_with_talba[j]
  
  prey_talba_start[[j]] <-  local_fw_MAIORANO[[nr]]$trophic.links[local_fw_MAIORANO[[nr]]$trophic.links$consumer == "Tyto alba",]$resource
  prey_talba_primary[[j]] <-  local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links$consumer == "Tyto alba",]$resource
  prey_talba_secondary[[j]] <-  local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links$consumer == "Tyto alba",]$resource
}

from_start_to_primary_talba <- list()
from_primary_to_secondary_talba <- list()
from_start_to_secondary_talba <- list()

for(i in 1:1855){
  
  from_start_to_primary_talba[[i]] <- setdiff(prey_talba_start[i][[1]], prey_talba_primary[i][[1]])
  from_primary_to_secondary_talba[[i]] <- setdiff(prey_talba_primary[i][[1]], prey_talba_secondary[i][[1]])
  from_start_to_secondary_talba[[i]] <- setdiff(prey_talba_start[i][[1]], prey_talba_secondary[i][[1]])
  
}

talba_prey <- data.frame(table(unlist(from_start_to_secondary_talba)))
#sum(talba_prey$Freq)

talba_prey <- data.frame(talba_prey, round((talba_prey$Freq * 100)/16649, 2))
names(talba_prey) <- c("prey_species", "lost_interactions", "percentage")

talba_prey <- talba_prey[order(-talba_prey$percentage), ]
sum(talba_prey$lost_interactions)

#Lost interactions as prey #####################################################
#Where is this species? Getting, from above
fw_with_talba

#where did it lose predators, and which predators
predators_talba_start <- list()
predators_talba_primary <- list()
predators_talba_secondary <- list()

for(j in 1:length(fw_with_talba)){
  
  nr <- fw_with_talba[j]
  
  predators_talba_start[[j]] <-  local_fw_MAIORANO[[nr]]$trophic.links[local_fw_MAIORANO[[nr]]$trophic.links$resource == "Tyto alba",]$consumer
  predators_talba_primary[[j]] <-  local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links$resource == "Tyto alba",]$consumer
  predators_talba_secondary[[j]] <-  local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links$resource == "Tyto alba",]$consumer
}

from_start_to_primary_talba2 <- list()
from_primary_to_secondary_talba2 <- list()
from_start_to_secondary_talba2 <- list()

for(i in 1:1855){
  
  from_start_to_primary_talba2[[i]] <- setdiff(predators_talba_start[i][[1]], predators_talba_primary[i][[1]])
  from_primary_to_secondary_talba2[[i]] <- setdiff(predators_talba_primary[i][[1]], predators_talba_secondary[i][[1]])
  from_start_to_secondary_talba2[[i]] <- setdiff(predators_talba_start[i][[1]], predators_talba_secondary[i][[1]])
  
}

talba_predators <- data.frame(table(unlist(from_start_to_secondary_talba2)))
#sum(talba_predators$Freq)

talba_predators <- data.frame(talba_predators, round((talba_predators$Freq * 100)/20, 2))
names(talba_predators) <- c("predator_species", "lost_interactions", "percentage")

talba_predators <- talba_predators[order(-talba_predators$percentage), ]
sum(talba_predators$lost_interactions)

################################################################################

#Felis silvestris

#Lost interactions as predator #################################################

#Where is this species
fw_with_felis <- c()
for(i in 1:length(local_fw_MAIORANO)) fw_with_felis[i] <- "Felis silvestris" %in% local_fw_MAIORANO[[i]]$nodes[,2]
#table(fw_with_felis)
fw_with_felis <- which(fw_with_felis == TRUE)

#where did it lose prey, and which prey
prey_felis_start <- list()
prey_felis_primary <- list()
prey_felis_secondary <- list()

for(j in 1:length(fw_with_felis)){
  
  nr <- fw_with_felis[j]
  
  prey_felis_start[[j]] <-  local_fw_MAIORANO[[nr]]$trophic.links[local_fw_MAIORANO[[nr]]$trophic.links$consumer == "Felis silvestris",]$resource
  prey_felis_primary[[j]] <-  local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links$consumer == "Felis silvestris",]$resource
  prey_felis_secondary[[j]] <-  local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links$consumer == "Felis silvestris",]$resource
}

from_start_to_primary_felis <- list()
from_primary_to_secondary_felis <- list()
from_start_to_secondary_felis <- list()

for(i in 1:length(fw_with_felis)){
  
  from_start_to_primary_felis[[i]] <- setdiff(prey_felis_start[i][[1]], prey_felis_primary[i][[1]])
  from_primary_to_secondary_felis[[i]] <- setdiff(prey_felis_primary[i][[1]], prey_felis_secondary[i][[1]])
  from_start_to_secondary_felis[[i]] <- setdiff(prey_felis_start[i][[1]], prey_felis_secondary[i][[1]])
  
}

felis_prey <- data.frame(table(unlist(from_start_to_secondary_felis)))
#sum(felis_prey$Freq)

felis_prey <- data.frame(felis_prey, round((felis_prey$Freq * 100)/3916, 2))
names(felis_prey) <- c("prey_species", "lost_interactions", "percentage")

felis_prey <- felis_prey[order(-felis_prey$percentage), ]
sum(felis_prey$lost_interactions)

#Lost interactions as prey #####################################################
#Where is this species? Getting, from above
fw_with_felis

#where did it lose predators, and which predators
predators_felis_start <- list()
predators_felis_primary <- list()
predators_felis_secondary <- list()

for(j in 1:length(fw_with_felis)){
  
  nr <- fw_with_felis[j]
  
  predators_felis_start[[j]] <-  local_fw_MAIORANO[[nr]]$trophic.links[local_fw_MAIORANO[[nr]]$trophic.links$resource == "Felis silvestris",]$consumer
  predators_felis_primary[[j]] <-  local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[nr]]$trophic.links$resource == "Felis silvestris",]$consumer
  predators_felis_secondary[[j]] <-  local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links[local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[nr]]$trophic.links$resource == "Felis silvestris",]$consumer
}

from_start_to_primary_felis2 <- list()
from_primary_to_secondary_felis2 <- list()
from_start_to_secondary_felis2 <- list()

for(i in 1:length(fw_with_felis)){
  
  from_start_to_primary_felis2[[i]] <- setdiff(predators_felis_start[i][[1]], predators_felis_primary[i][[1]])
  from_primary_to_secondary_felis2[[i]] <- setdiff(predators_felis_primary[i][[1]], predators_felis_secondary[i][[1]])
  from_start_to_secondary_felis2[[i]] <- setdiff(predators_felis_start[i][[1]], predators_felis_secondary[i][[1]])
  
}

felis_predators <- data.frame(table(unlist(from_start_to_secondary_felis2)))
#sum(felis_predators$Freq)

felis_predators <- data.frame(felis_predators, round((felis_predators$Freq * 100)/4, 2))
names(felis_predators) <- c("predator_species", "lost_interactions", "percentage")

felis_predators <- felis_predators[order(-felis_predators$percentage), ]

################################################################################
################################################################################

library(cheddar)

local_fw_MAIORANO[[1]]
local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[1]]  
local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[1]]

ex1_0 <- cheddar::Community(nodes = local_fw_MAIORANO[[1]]$nodes,
                   properties = local_fw_MAIORANO[[1]]$properties,
                   trophic.links = local_fw_MAIORANO[[1]]$trophic.links)


################################################################################
################################################################################

'''
#Random code, used for ploting

#Load datasets
load("~/github/road_ecoloy_econetworks/local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS_METAWEB_TL_08NOV23.RData")
load("~/github/road_ecoloy_econetworks/local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS_METAWEB_TL_08NOV23.RData")
load("~/github/road_ecoloy_econetworks/local_fw_MAIORANO_with_metaweb_TL_08NOV23.RData")

#Lets use the grids BW39!
length(cheddar::NPS(local_fw_MAIORANO[["4458"]])$node)
length(cheddar::NPS(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["4458"]])$node)
length(cheddar::NPS(local_fw_MAIORANO_REMOVED[["4458"]])$node)
#
cheddar::TopLevelNodes(local_fw_MAIORANO[["4458"]])
cheddar::TopLevelNodes(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["4458"]])
cheddar::TopLevelNodes(local_fw_MAIORANO_REMOVED[["4458"]])
#
cheddar::BasalNodes(local_fw_MAIORANO[["4458"]])
cheddar::BasalNodes(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["4458"]])
cheddar::BasalNodes(local_fw_MAIORANO_REMOVED[["4458"]])

#
links <- cbind(TLPS(local_fw_MAIORANO[["4458"]]), colour="#c7c7c788")
links$colour["Buteo buteo" == links$resource] <- "red"
links$colour["Buteo buteo" == links$consumer] <- "blue"

#cheddar::plot.Community(network_list_cheddar[[116183]], node.labels="node", show.nodes.as="both", link.col=links$colour)
cheddar::plot.Community(local_fw_MAIORANO[["4458"]], link.col=links$colour)
#s

#Plot links going and coming from Circus cyaneus
links0 <- cbind(TLPS(local_fw_MAIORANO[["4458"]]), colour="#c7c7c788")
#
links0 <- cbind(TLPS(local_fw_MAIORANO[["4458"]]), colour="#ffffff00")
links0$colour["Hieraaetus pennatus" == links0$resource] <- "red"
links0$colour["Hieraaetus pennatus" == links0$consumer] <- "blue"

cheddar::plot.Community(local_fw_MAIORANO[["BW39"]], node.labels="node", show.nodes.as="both", link.col=links$colour)
cheddar::plot.Community(local_fw_MAIORANO[["BW39"]], link.col=links0$colour)

table(local_fw_MAIORANO[[1]]$trophic.links$consumer == "Felis silvestris")
table(local_fw_MAIORANO[[1]]$trophic.links$resource == "Felis silvestris")

length(local_fw_MAIORANO[[1]]$trophic.links$resource)

link_col <- rep("lightgrey", 1320)

link_col[which(local_fw_MAIORANO[[1]]$trophic.links$resource == "Felis silvestris")] <- "red"
link_col[which(local_fw_MAIORANO[[1]]$trophic.links$consumer == "Felis silvestris")] <- "green"


node_col <- rep("black", length(local_fw_MAIORANO[[1]]$nodes$node))
node_col[which(local_fw_MAIORANO[[1]]$nodes$node == "Felis silvestris")] <- "blue"
table(node_col)


?PlotWebByLevel
PlotWebByLevel(ex1_0, col = node_col, pch = 16, show.level.labels=FALSE, main=NULL)

'''

################################################################################
# Code for example figure
################################################################################

length(local_fw_MAIORANO)
length(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS)
length(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS)

#Wildcat predators species lost
head(predators_felis_secondary)

#Wildcat prey species lost
head(from_start_to_secondary_felis)
#Save csv files with sample data
write.csv(local_fw_MAIORANO[[4]]$trophic.links, file = "before_extinctions.csv")
write.csv(local_fw_MAIORANO_REMOVED_PRIMARY_EXTINCTIONS[[4]]$trophic.links, file = "after_primary.csv")
write.csv(local_fw_MAIORANO_REMOVED_SECONDARY_EXTINCTIONS[[4]]$trophic.links, file = "after_secondary.csv")
