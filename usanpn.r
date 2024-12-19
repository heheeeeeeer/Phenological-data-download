###########################################################
# https://github.com/usa-npn/rnpn?tab=readme-ov-file

install.packages("rnpn")
library(rnpn)

vignette(package = "rnpn")
vignette("II_status_intensity", package = "rnpn")

library(dplyr)

dogwoood_codes <- npn_species() %>% 
  filter(genus == "Cornus")

dogwood_data <- npn_download_status_data(request_source = "Kristina Riemer", 
                                         years = c(2008:2020), 
                                         species_ids = c(dogwoood_codes$species_id), 
                                         climate_data = TRUE)

write.csv(dogwood_data, "clonal_dogwoods/data/raw_all_dogwood.csv", row.names = FALSE)
