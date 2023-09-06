#FMestre
#05-09-23


################################################################################
# Figure
################################################################################

#all_primary_secondary[all_primary_secondary$grid == "BW39",]
#
#local_fw_MAIORANO[["BW39"]]
#local_fw_MAIORANO_REMOVED_PRIMARY_EX[["BW39"]]
#local_fw_MAIORANO_REMOVED[["BW39"]]

par(mfrow=c(1,3))

plot(local_fw_MAIORANO[["BW39"]])
plot(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["BW39"]])
plot(local_fw_MAIORANO_REMOVED[["BW39"]])


local_fw_MAIORANO_IGRAPH <- vector(mode = "list", length = length(local_fw_MAIORANO))
local_fw_MAIORANO_REMOVED_PRIMARY_EX_IGRAPH <- vector(mode = "list", length = length(local_fw_MAIORANO_REMOVED_PRIMARY_EX))
local_fw_MAIORANO_REMOVED_IGRAPH <- vector(mode = "list", length = length(local_fw_MAIORANO_REMOVED))

#required function
ToIgraph <- function(community, weight=NULL)
{
  if(is.null(TLPS(community)))
  {
    stop('The community has no trophic links')
  }
  else
  {
    tlps <- TLPS(community, link.properties=weight)
    if(!is.null(weight))
    {
      tlps$weight <- tlps[,weight]
    }
    return (graph.data.frame(tlps,
                             vertices=NPS(community),
                             directed=TRUE))
  }
}

#COnvert to igraph
for(i in 1:length(local_fw_MAIORANO)) {
  
  if(unique(!is.na(local_fw_MAIORANO[[i]])) && cheddar::NumberOfTrophicLinks(local_fw_MAIORANO[[i]]) != 0) local_fw_MAIORANO_IGRAPH[[i]] <- ToIgraph(local_fw_MAIORANO[[i]])
  
  message(i)
  
  }

for(i in 1:length(local_fw_MAIORANO_REMOVED_PRIMARY_EX)) {
  
  if(unique(!is.na(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]])) && cheddar::NumberOfTrophicLinks(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]]) != 0) local_fw_MAIORANO_REMOVED_PRIMARY_EX_IGRAPH[[i]] <- ToIgraph(local_fw_MAIORANO_REMOVED_PRIMARY_EX[[i]])
  
  message(i)
  
}

for(i in 1:length(local_fw_MAIORANO_REMOVED)) {
  
  if(unique(!is.na(local_fw_MAIORANO_REMOVED[[i]])) && cheddar::NumberOfTrophicLinks(local_fw_MAIORANO_REMOVED[[i]]) != 0) local_fw_MAIORANO_REMOVED_IGRAPH[[i]] <- ToIgraph(local_fw_MAIORANO_REMOVED[[i]])
  
  message(i)
  
}

names(local_fw_MAIORANO_IGRAPH) <- names(local_fw_MAIORANO)
names(local_fw_MAIORANO_REMOVED_PRIMARY_EX_IGRAPH) <- names(local_fw_MAIORANO_REMOVED_PRIMARY_EX)
names(local_fw_MAIORANO_REMOVED_IGRAPH) <- names(local_fw_MAIORANO_REMOVED)

################################################################################
#                         Plotting FW for the figure
################################################################################

#Package
library(cheddar)
library(igraph)

#Which have rabbit and lynx? none?!

#is_rabbit_lynx <- data.frame(matrix(ncol = 3, nrow = length(local_fw_MAIORANO)))
#names(is_rabbit_lynx) <- c("LP", "OC", "VV")

#for(i in 1:length(local_fw_MAIORANO)){

#if(cheddar::is.Community(local_fw_MAIORANO[[i]]))
#{
#is_rabbit_lynx[i,1] <- ifelse("Lynx pardinus" %in% cheddar::NPS(local_fw_MAIORANO[[i]])$node, "present", "absent")
#is_rabbit_lynx[i,2] <- ifelse("Oryctolagus cuniculus" %in% cheddar::NPS(local_fw_MAIORANO[[i]])$node, "present", "absent") 
#is_rabbit_lynx[i,3] <- ifelse("Vulpes vulpes" %in% cheddar::NPS(local_fw_MAIORANO[[i]])$node, "present", "absent") 

#}else{
#  is_rabbit_lynx[i,1] <- NA 
#  is_rabbit_lynx[i,2] <- NA
#  is_rabbit_lynx[i,3] <- NA
#}
  
#message(i)

#}

#which(is_rabbit_lynx$LP == "present")
#local_fw_MAIORANO[[3660]]
#cheddar::plot.Community(local_fw_MAIORANO[[3660]], node.labels="node", show.nodes.as="both")

#is_rabbit_lynx[is_rabbit_lynx$VV == "present" & is_rabbit_lynx$OC == "present",]

#Lets use the grids BW39!
length(cheddar::NPS(local_fw_MAIORANO[["BW39"]])$node)
length(cheddar::NPS(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["BW39"]])$node)
length(cheddar::NPS(local_fw_MAIORANO_REMOVED[["BW39"]])$node)
#
cheddar::TopLevelNodes(local_fw_MAIORANO[["BW39"]])
cheddar::TopLevelNodes(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["BW39"]])
cheddar::TopLevelNodes(local_fw_MAIORANO_REMOVED[["BW39"]])
#
cheddar::BasalNodes(local_fw_MAIORANO[["BW39"]])
cheddar::BasalNodes(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["BW39"]])
cheddar::BasalNodes(local_fw_MAIORANO_REMOVED[["BW39"]])

#cheddar::plot.Community(local_fw_MAIORANO[["BW39"]], node.labels="node", show.nodes.as="both")
#cheddar::plot.Community(local_fw_MAIORANO_REMOVED_PRIMARY_EX[["BW39"]], node.labels="node", show.nodes.as="both")
#cheddar::plot.Community(local_fw_MAIORANO_REMOVED[["BW39"]], node.labels="node", show.nodes.as="both")

#Plot links going and coming from Vulpes vulpes
#links <- cbind(TLPS(local_fw_MAIORANO[["BW39"]]), colour="#c7c7c788")
#
links <- cbind(TLPS(local_fw_MAIORANO[["BW39"]]), colour="#c7c7c788")
links$colour["Buteo buteo" == links$resource] <- "red"
links$colour["Buteo buteo" == links$consumer] <- "blue"

#cheddar::plot.Community(network_list_cheddar[[116183]], node.labels="node", show.nodes.as="both", link.col=links$colour)
cheddar::plot.Community(local_fw_MAIORANO[["BW39"]], link.col=links$colour)
#

#Plot links going and coming from Circus cyaneus
links0 <- cbind(TLPS(local_fw_MAIORANO[["BW39"]]), colour="#c7c7c788")
#
links0 <- cbind(TLPS(local_fw_MAIORANO[["BW39"]]), colour="#ffffff00")
links0$colour["Circus cyaneus" == links0$resource] <- "red"
links0$colour["Circus cyaneus" == links0$consumer] <- "blue"

cheddar::plot.Community(local_fw_MAIORANO[["BW39"]], node.labels="node", show.nodes.as="both", link.col=links$colour)
cheddar::plot.Community(local_fw_MAIORANO[["BW39"]], link.col=links0$colour)