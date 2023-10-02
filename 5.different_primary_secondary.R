################################################################################
################################################################################
#   SCRIPT 5 - DIFFERENCES BETWEEN PRIMARY EXTINCTIONS AND CASCADING EFFECTS
################################################################################
################################################################################

#FMestre
#28-09-2023

local_fw_MAIORANO # original FW
local_fw_MAIORANO_REMOVED_PRIMARY_EX # primary extinctions FW
local_fw_MAIORANO_REMOVED # cascading effects FW

#save(local_fw_MAIORANO, file = "local_fw_MAIORANO_original.RData")
#save(local_fw_MAIORANO_REMOVED_PRIMARY_EX,  file = "local_fw_MAIORANO_REMOVED_PRIMARY_EX.RData")
#save(local_fw_MAIORANO_REMOVED,  file = "local_fw_MAIORANO_REMOVED_CASCADING.RData")

head(result_prim_ext)
head(result_sec_ext)

all_primary_secondary <- data.frame(result_prim_ext,
                                    result_sec_ext)

all_primary_secondary <- all_primary_secondary[,-5]

all_primary_secondary <- data.frame(all_primary_secondary, all_primary_secondary$species_loss_prim_ext - all_primary_secondary$species_loss)
names(all_primary_secondary) <- c("grid",
                                     "primary_sp_loss",
                                     "primary_connect",
                                     "primary_compart",
                                     "cascading_sp_loss",
                                     "cascading_connect",
                                     "cascading_compart",
                                     "diff_species_persistence")

#head(all_primary_secondary)
#View(all_primary_secondary)

#Save
#save(all_primary_secondary, file = "all_primary_secondary_30SET23.RData")

primary_and_cascading_effects <- merge(x=grids_grilo_shape, y=all_primary_secondary, by.x="PageNumber", by.y= "grid")
#terra::writeVector(primary_and_cascading_effects, "primary_and_cascading_effects30SET23.shp")
