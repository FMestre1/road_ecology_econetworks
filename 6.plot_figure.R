################################################################################
################################################################################
#                          SCRIPT 6 - Plot figure
################################################################################
################################################################################

#FMestre
#30-09-2023

#Load Packages
library(cheddar)
library(igraph)

nr_lost_interactions_sec
nr_lost_interactions_prim

#Chose a good network to show in the figure
View(data.frame(nr_lost_interactions_prim$grid, 
                nr_lost_interactions_prim$lost_interactions, 
                nr_lost_interactions_sec$lost_interactions, 
                nr_lost_interactions_prim$lost_interactions != nr_lost_interactions_sec$lost_interactions,
                as.numeric(nr_lost_interactions_sec$lost_interactions) - as.numeric(nr_lost_interactions_prim$lost_interactions)))

pair_pagenumber_pagename[pair_pagenumber_pagename$PageNumber =="4458",]

par(mfrow=c(1,3))
plot(local_fw_MAIORANO[["4458"]], main = "CU14")
plot(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["4458"]], main = "CU14")
plot(local_fw_MAIORANO_REMOVED[["4458"]], main = "CU14")
