###########################################################
# https://github.com/usa-npn
# https://github.com/usa-npn/rnpn?tab=readme-ov-file
# https://github.com/cran/rnpn
# https://github.com/genophenoenvo/NPN-Data/tree/main
###########################################################

install.packages("rnpn")
library(rnpn)

vignette(package = "rnpn")
vignette("II_status_intensity", package = "rnpn")
citation(package = 'rnpn')

library(dplyr)

dogwoood_codes <- npn_species() %>% 
  filter(genus == "Cornus")

dogwood_data <- npn_download_status_data(request_source = "Kristina Riemer", 
                                         years = c(2008:2020), 
                                         species_ids = c(dogwoood_codes$species_id), 
                                         climate_data = TRUE)

some_data <- npn_download_status_data(
  request_source = 'Your Name or Org Here',
  years = c(2015),
  species_id = c(35),
  states = c('AZ', 'IL')
)

write.csv(dogwood_data, "clonal_dogwoods/data/raw_all_dogwood.csv", row.names = FALSE)
