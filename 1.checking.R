################################################################################
################################################################################
#                      SCRIPT 10 - CHECKING THE DATASET
################################################################################
################################################################################

#FMestre
#09-10-2023

#Look into a few networks
rede0 <- local_fw_MAIORANO[[6]] #initial network
rede1 <- local_fw_MAIORANO_REMOVED_PRIMARY_EX[[6]] #after primary extinctions
rede2 <- local_fw_MAIORANO_REMOVED[[6]] #after secondary extinctions

plot(local_fw_MAIORANO[[6]])
plot(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[6]])
plot(local_fw_MAIORANO_REMOVED[[6]])

length(rede0$nodes$node)
length(rede1$nodes$node)
length(rede2$nodes$node)

#Extintas em primary ext.
rede0$nodes$node[!(rede0$nodes$node %in% rede1$nodes$node)]

#Extintas em secondary ext.
rede1$nodes$node[!(rede1$nodes$node %in% rede2$nodes$node)]

#Espécies com threshold mais baixo
head(rede0$nodes[order(rede0$nodes$Median_MAXroad.RM.1000.), ], 5)


