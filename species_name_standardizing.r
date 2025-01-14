# Species name standardization
# U.Taxonstand: An R package for standardizing scientifc names of plants and animals
# thesis: https://doi.org/10.1016/j.pld.2022.09.001
# https://blog.csdn.net/whitedrogen/article/details/129768029
# https://mp.weixin.qq.com/s/cDcXbo4LB1q_yOkclod_fg

# ---- packages ----

install_github("remotes")
install_github("ecoinfor/U.Taxonstand")

library("remotes")
library("U.Taxonstand")
library(openxlsx)

setwd("E:/phenology/data")
getwd()

# ---- Reference database ----

# downloaded from: https://github.com/nameMatch/Database 物种名数据库

dat1 <- read.xlsx("Plants_TPL_database_part1.xlsx")
dat2 <- read.xlsx("Plants_TPL_database_part2.xlsx")
dat3 <- read.xlsx("Plants_TPL_database_part3.xlsx")
database <- rbind(dat1, dat2, dat3)
rm(dat1, dat2, dat3)
write.csv(database, "Plants_TPL_database.csv", row.names = FALSE)

# ---- Read file ----

setwd("E:/Raw_Phenological_Data/species_name_standardized")

file_names <- list.files(pattern = "\\.csv$")

for (file_name in file_names) {
  data <- read.csv(file_name)
  variable_name <- sub("\\.csv$","",file_name)
  assign(variable_name, data)
}


# ---- Extract species names for each data ----

setwd("E:/Raw_Phenological_Data")

file_names <- c("CERN_LeafUnfolding_Raw.csv", "PEP725_LeafUnfolding_Raw.csv", 
                "Russia_LeafUnfolding_Raw.csv", "SND_LeafUnfolding_Raw.csv", 
                "USAnpn_LeafUnfolding_Raw.csv")

variable_names <- c("CERN_treetype", "PEP725_treetype", "Russia_treetype", 
                    "SND_treetype", "USAnpn_treetype")

unique_treetypes <- list()

for (i in 1:length(file_names)) {
  data <- read.csv(file_names[i])
  unique_treetype <- unique(data$treetype)
  unique_treetypes[[variable_names[i]]] <- unique_treetype
  write.csv(unique_treetype, paste0(variable_names[i], ".csv"), row.names = FALSE)
}


# ---- standardizing species name ----

setwd("E:/Raw_Phenological_Data/species_name_standardized")

# SND
SND_standardized_names <- nameMatch(spList = SND_treetype$x, 
                                spSource = database,  
                                author = FALSE, 
                                max.distance= 1)
SND_standardized_names <- SND_standardized_names[, c("Submitted_Name", "Name_in_database", 
                                                     "Accepted_SPNAME", "Genus_in_database", "Family")]
write.csv(SND_standardized_names,"SND_standardized_names.csv", row.names = FALSE)


# PEP725
# 46 names (38.02%) that need to do fuzzy matching
PEP725_treetype <- read.csv("PEP725_treetype.csv")
PEP725_treetype$treetype <- gsub("[-_()]", " ", PEP725_treetype$treetype)  
PEP725_standardized_names <- nameMatch(spList = PEP725_treetype$treetype, 
                                        spSource = database,  
                                        author = FALSE, 
                                        max.distance= 1)
PEP725_standardized_names <- PEP725_standardized_names[, c("Submitted_Name", "Name_in_database", 
                                                     "Accepted_SPNAME", "Genus_in_database", "Family")]
write.csv(PEP725_standardized_names,"PEP725_standardized_names.csv", row.names = FALSE)


# CERN 
# 52 names (15.66%) that need to do fuzzy matching
CERN_standardized_names <- nameMatch(spList = CERN_treetype$x, 
                                    spSource = database,  
                                    author = FALSE, 
                                    max.distance= 1)
CERN_standardized_names <- CERN_standardized_names[, c("Submitted_Name", "Name_in_database", 
                                                     "Accepted_SPNAME", "Genus_in_database", "Family")]
write.csv(CERN_standardized_names,"CERN_standardized_names.csv", row.names = FALSE)


# Russia
# 20 names (5.28%) that need to do fuzzy matching
Russia_standardized_names <- nameMatch(spList = Russia_treetype$x, 
                                     spSource = database,  
                                     author = FALSE, 
                                     max.distance= 1)
Russia_standardized_names <- Russia_standardized_names[, c("Submitted_Name", "Name_in_database", 
                                                       "Accepted_SPNAME", "Genus_in_database", "Family")]
write.csv(Russia_standardized_names,"Russia_standardized_names.csv", row.names = FALSE)


# USAnpn
# 27 names (2.54%) that need to do fuzzy matching
USAnpn_treetype$x <- gsub("[_()-]", " ", USAnpn_treetype$x) 
USAnpn_standardized_names <- nameMatch(spList = USAnpn_treetype$x, 
                                       spSource = database,  
                                       author = FALSE, 
                                       max.distance= 1)
USAnpn_standardized_names <- USAnpn_standardized_names[, c("Submitted_Name", "Name_in_database", 
                                                           "Accepted_SPNAME", "Genus_in_database", "Family")]
write.csv(USAnpn_standardized_names,"USAnpn_standardized_names.csv", row.names = FALSE)


# After performing the above operations, perform manual verification again
# Submitted_Name = Name_in_database, Correct
# Different, check manually in the database
# ambiguous, replace with NA


# ---- Replace species names in the original data with calibrated species names ----

setwd("E:/Raw_Phenological_Data/species_name_standardized")

leaf_unfolding <- read.csv("CERN_LeafUnfolding_Raw.csv", stringsAsFactors = FALSE)
standardized_names <- read.csv("CERN_standardized_names.csv", stringsAsFactors = FALSE)
output_path <- "CERN_LeafUnfolding_Raw_NameUpdated.csv"

merged_data <- merge(leaf_unfolding, standardized_names, 
                     by.x = "treetype", by.y = "Submitted_Name", 
                     all.x = TRUE)

merged_data$treetype <- merged_data$Name_in_database

merged_data <- merged_data[, !names(merged_data) %in% "Name_in_database"]

write.csv(merged_data, output_path, row.names = FALSE)

