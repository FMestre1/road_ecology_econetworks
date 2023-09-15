################################################################################
#      What is the vulnerability of the species in each trophic level?
################################################################################
#FMestre
#06-09-2023

library(cheddar)
library(ggplot2)

vuln_tl_table <- data.frame(matrix(nrow = length(local_fw_MAIORANO), ncol = 3))
names(vuln_tl_table) <- c("top_vuln", "intermediate_vuln", "basal_vuln")
head(vuln_tl_table)

for(j in 1:length(local_fw_MAIORANO)){

if(unique(!is.na(local_fw_MAIORANO[[j]])))
  
  {
  
tp_level <- as.vector(cheddar::IsTopLevelNode(local_fw_MAIORANO[[j]]))
interm_level <- as.vector(cheddar::IsIntermediateNode(local_fw_MAIORANO[[j]]))
basal_level <- as.vector(cheddar::IsBasalNode(local_fw_MAIORANO[[j]]))

df1 <- data.frame(cheddar::NPS(local_fw_MAIORANO[[j]]), 
           tp_level,
           interm_level,
           basal_level)

df1$tp_level[df1$tp_level] <- "top"                  
df1$interm_level[df1$interm_level] <- "intermediate"
df1$basal_level[df1$basal_level] <- "basal"

tl <- rep(NA, nrow(df1))

for(i in 1:nrow(df1)){
  
  if(df1$tp_level[i] != FALSE) tl[i] <- "top"
  if(df1$interm_level[i] != FALSE) tl[i] <- "intermediate"
  if(df1$basal_level[i] != FALSE) tl[i] <- "basal"
  if(df1$tp_level[i] == FALSE & df1$interm_level[i] == FALSE & df1$basal_level[i] == FALSE) tl[i] <- "none"
  
  
}

df2 <- data.frame(cheddar::NPS(local_fw_MAIORANO[[j]]), tl)
  
df2$tl <- as.factor(df2$tl)

df2 <- df2[,c(1,2,7)]

df3 <- as.data.frame(df2 %>% group_by (tl) %>% summarise(Average=mean(Median_MAXroad.RM.1000.)))

vuln_tl_table$top_vuln[j] <- df3[3,2]
vuln_tl_table$intermediate_vuln[j] <- df3[2,2]
vuln_tl_table$basal_vuln[j] <- df3[1,2]

}

message(j)

}

nrow(vuln_tl_table)
str(vuln_tl_table)
head(vuln_tl_table)

top1 <- data.frame(vuln_tl_table$top_vuln, rep("top", nrow(vuln_tl_table)))
int1 <- data.frame(vuln_tl_table$intermediate_vuln, rep("intermediate", nrow(vuln_tl_table)))
bas1 <- data.frame(vuln_tl_table$basal_vuln, rep("basal", nrow(vuln_tl_table)))

names(top1) <- c("vulnerability", "tl")
names(int1) <- c("vulnerability", "tl")
names(bas1) <- c("vulnerability", "tl")

vuln_tl_table <- rbind(top1, int1, bas1)
vuln_tl_table$tl <- as.factor(vuln_tl_table$tl)

vuln_tl_table$tl <- factor(vuln_tl_table$tl, 
                           levels = c("top", "intermediate", "basal", "none")
)

tl_vuln <- ggplot(vuln_tl_table, aes(x = tl, y = vulnerability))

tl_vuln2 <- tl_vuln + geom_violin(aes(fill = tl),) +
  ylab("vulnerability") +
  xlab("trophic level") +
  scale_fill_manual(values = c("#E70F00", "#E69F00", "#1E811E"))
  

tl_vuln2 + labs(fill = "trophic level")

# Compute the analysis of variance
names(vuln_tl_table)

vulnerability_tl_aov <- aov(vulnerability ~ tl, data = vuln_tl_table)
# Summary of the analysis
summary(vulnerability_tl_aov)
