###########################################################
# https://github.com/usa-npn
# https://github.com/usa-npn/rnpn?tab=readme-ov-file
# https://github.com/cran/rnpn
# https://github.com/genophenoenvo/NPN-Data/tree/main
# https://github.com/usa-npn/cales-thermal-calendars/tree/main
###########################################################

install.packages("rnpn")
install.packages("dplyr")
library(rnpn)
library(dplyr)

rm(list=ls())
options(mc.cores = parallel::detectCores())
setwd("E:/Raw_Phenological_Data/USA_npn/")

load("USA_npn.RData")
save.image(file = "USA_npn.RData")

# 1. about usanpn ####

citation(package = 'rnpn')

vignette(package = "rnpn")
vignette("II_status_intensity", package = "rnpn")
vignette("IV_site_phenometrics", package = "rnpn")  
vignette("VI_geospatial", package = "rnpn")  

species_list <- npn_species()
phenophases <- npn_phenophases()  

unique(species_list$functional_type)
# ******************************************************************
#  "Evergreen broadleaf"     "Semi-evergreen broadleaf"                                
#  "Deciduous broadleaf"   "Drought deciduous broadleaf"
#  "Evergreen conifer"      "Deciduous conifer"      "Pine"                    
# 
#  "Forb"  "Evergreen forb"  "Semi-evergreen forb" 
#  "Algae"   "Cactus"   "Graminoid"
# 
#  "Bird"  "Reptile"  "Mammal"  "Amphibian"  "Fish"    "Insect"   
# ******************************************************************

# 2. data download ####

# pheno_class_ids = 1
leaf <- npn_download_site_phenometrics(request_source='Heheer',
                                       years = c(2009:2024),
                                       num_days_quality_filter = "7",
                                       pheno_class_ids = 1,
                                       climate_data = TRUE )
write.csv(leaf, "leaf.csv", row.names = FALSE)

# ***************************
#  dim(leaf)    32151  48
# ***************************

# Evergreen broadleaf (but... no record output)
Ev_br_codes <- species_list %>% 
  filter(functional_type == "Evergreen broadleaf")
Evergreen_broadleaf <- npn_download_site_phenometrics(request_source = "Heheer", 
                                                      years = c(2008:2024), 
                                                      num_days_quality_filter = "7",
                                                      functional_types = unique(Ev_br_codes$functional_type),
                                                      climate_data = TRUE )
write.csv(Evergreen_broadleaf, "Evergreen_broadleaf.csv", row.names = FALSE)

# others' example (for reference)
dogwoood_codes <- npn_species() %>% 
  filter(genus == "Cornus")
dogwood_data <- npn_download_status_data(request_source = "Kristina Riemer", 
                                         years = c(2008:2020), 
                                         species_ids = c(dogwoood_codes$species_id), 
                                         states = c('AZ', 'IL'),
                                         climate_data = TRUE)
write.csv(dogwood_data, "raw_all_dogwood.csv", row.names = FALSE)
