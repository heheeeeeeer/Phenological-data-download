
###########################################################
# Phenology data: Leaf unfolding phenology, corresponding to BBCH=11.
# Each country's raw data recording format is different, requiring unified data preprocessing.
# dataset; station_ID; year; day; country; longitude; latitude; altitude; 
# phenology_phase; treetype; genus; species; Chinese_name; common_name; notes
###########################################################

rm(list=ls())

# ---- 1. PEP725_LeafUnfolding_Raw ----

library(data.table)

setwd("E:/Raw_Phenological_Data/Europe_PEP725")

bbch_data <- fread("BBCH/BBCH_11.csv")    
stations_data <- fread("stations_all.csv")  

PEP725_merged_data <- merge(
  bbch_data,
  stations_data[, .(PEP_ID, LON, LAT, ALT)], 
  by = "PEP_ID",
  all.x = TRUE 
)

PEP725_LeafUnfolding_Raw <- data.table(
  dataset = "PEP725",
  station_ID = PEP725_merged_data$PEP_ID,
  year = PEP725_merged_data$YEAR,
  day = PEP725_merged_data$DAY,
  country = PEP725_merged_data$country,
  longitude = PEP725_merged_data$LON,
  latitude = PEP725_merged_data$LAT,
  altitude = PEP725_merged_data$ALT,
  phenology_phase = "BBCH11",
  treetype = PEP725_merged_data$treetype,
  genus = NA,
  species = NA,
  Chinese_name = NA,
  common_name = NA,
  notes = NA
)

fwrite(PEP725_LeafUnfolding_Raw, "E:/Raw_Phenological_Data/PEP725_LeafUnfolding_Raw.csv")



# ---- 2. CERN_LeafUnfolding_Raw ----

library(readxl)
library(dplyr)
library(stringr)
library(lubridate) # date process
library(data.table)

setwd("E:/Raw_Phenological_Data/China_CERN")

# CERN_stations：from thesis 10.11922/csdata.180.2016.0110 (Omitted here)
raw_data <- "station_ID;lon;lat
            ALF;101.0281;24.5450..."

CERN_stations <- read_delim(raw_data, delim = ";")
write_csv(CERN_stations, "CERN_stations.csv")

# CERN_data: Copy the desired data directly from the original excel to csv
CERN_data <- fread("E:/Raw_Phenological_Data/China_CERN/CERN_data.csv")    
CERN_stations <- fread("E:/Raw_Phenological_Data/China_CERN/CERN_stations.csv")    
CERN_stations <- as.data.table(CERN_stations)

merged_data <- merge(
  CERN_data,
  CERN_stations[, .(station_ID, lon, lat)], 
  by = "station_ID",
  all.x = TRUE 
)

merged_data$date_parsed <- as.Date(merged_data$date, format = "%m/%d/%Y")
merged_data$date_parsed[is.na(merged_data$date_parsed)] <- as.Date(merged_data$date[is.na(merged_data$date_parsed)], format = "%Y/%m/%d")
merged_data$year <- format(merged_data$date_parsed, "%Y")
merged_data$day <- as.integer(format(merged_data$date_parsed, "%j")) 

CERN_LeafUnfolding_Raw <- data.table(
  dataset = "CERN",
  station_ID = merged_data$station_ID,
  year = merged_data$year,
  day = merged_data$day,
  country = "China",
  longitude = merged_data$lon,
  latitude = merged_data$lat,
  altitude = NA,
  phenology_phase = "leaf unfolding",
  treetype = merged_data$treetype,
  genus = NA,
  species = NA,
  Chinese_name = merged_data$Chinese_name,
  common_name = NA,
  notes = merged_data$notes
)

fwrite(CERN_LeafUnfolding_Raw, "E:/Raw_Phenological_Data/CERN_LeafUnfolding_Raw.csv")






# ---- 3. USA_LeafUnfolding_Raw ----

USAnpn_data <- fread("E:/Raw_Phenological_Data/USA_npn/leaf.csv")   
USAnpn_data$treetype <- paste(USAnpn_data$genus, USAnpn_data$species, sep = "_")

USAnpn_LeafUnfolding_Raw <- data.table(
  dataset = "USAnpn",
  station_ID = USAnpn_data$site_id,
  year = USAnpn_data$mean_first_yes_year,
  day = USAnpn_data$mean_first_yes_doy,
  country = USAnpn_data$state,
  longitude = USAnpn_data$longitude,
  latitude = USAnpn_data$latitude,
  altitude = USAnpn_data$elevation_in_meters,
  phenology_phase = USAnpn_data$phenophase_description,
  treetype = USAnpn_data$treetype,
  genus = USAnpn_data$genus,
  species = USAnpn_data$species,
  Chinese_name = NA,
  common_name = USAnpn_data$common_name,
  notes = NA )

fwrite(USAnpn_LeafUnfolding_Raw, "E:/Raw_Phenological_Data/USAnpn_LeafUnfolding_Raw.csv")

