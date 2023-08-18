################################################################################
#                                                                 02.08.2023   #
#                            Kinlan M.G. Jan                                   #  
#                           kinlan.jan@su.se                                   #
#                                                                              #
#                             Get the data                                     #
################################################################################
#                                                                              #
#                          ---- Zooplankton ----                               #
#                                                                              #
################################################################################
# Zooplankton data can be downloaded manually on
# Sharkweb (https://sharkweb.smhi.se/hamta-data/) by selecting
# Datatyp "Zooplankton" and Parametrar "Abundance"
# Another solution is to use the code below:
#dir.create("./Project2_Timeserie/Data/")
library(shaRk)
library(magrittr)
zooplankton <- getSHARK("Zooplankton") |> 
  addDyntaxa() |> 
  annotateSHARK()
saveRDS(zooplankton, "Data/zooplankton_02Aug23.rds")

################################################################################
#                                                                              #
#                         ---- Phytoplankton ----                              #
#                                                                              #
################################################################################
# Phytoplankton data can be downloaded manually on
# Sharkweb (https://sharkweb.smhi.se/hamta-data/) by selecting
# Datatyp "Phytoplankton" and Parametrar "Carbon concentration"
# Another solution is to use the code below:
phytoplankton <- getSHARK("Phytoplankton") |> 
  addDyntaxa() |> 
  annotateSHARK()
saveRDS(phytoplankton, "Data/phytoplankton_02Aug23.rds")

################################################################################
#                                                                              #
#                         ---- Phytoplankton ----                              #
#                                                                              #
################################################################################
# Physical data can be downloaded manually on
# Sharkweb (https://sharkweb.smhi.se/hamta-data/) by selecting
# Datatyp "Physical and Chemical"
# Another solution is to use the code below:
abiotic <- getSHARK("PhysicalChemical")
saveRDS(abiotic, ".Data/abiotic_02Aug23.rds")

sessionInfo()
