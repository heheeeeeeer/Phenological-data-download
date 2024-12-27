# reference: https://github.com/bluegreen-labs/phenor/blob/5b95d79d85140fcc1ff3fbb0cc6dfb79f0f0090b/R/pr_dl_pep725.R

# ---- 1. functions ----
check_pep725_species <- function(species = NULL,
                                 list = FALSE){
  
  data_selection <- httr::GET("http://www.pep725.eu/data_download/data_selection.php")
  
  number <- xml2::read_html(data_selection) %>%
    rvest::html_nodes("form select") %>%
    rvest::html_children() %>%
    rvest::html_attr("value") %>%
    as.numeric()
  
  name <- xml2::read_html(data_selection) %>%
    rvest::html_nodes("form select") %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    as.character() %>%
    tolower()
  
  species_info <- data.frame(number, name)
  
  if(list){
    if (is.null(species)){
      return(species_info)
    } else {
      print(species_info,
            row.names = FALSE)
    }
  }
  
  if(is.character(species)){
    numbers <- species_info$number[grep(paste(tolower(species),collapse = "|"),
                                        species_info$name)]
    if(length(numbers)==0){
      stop("Species (number) not listed in PEP725 database!")
    } else {
      return(numbers)
    }
  }
  
  if (is.numeric(species) & all(species %in% species_info$number)){
    return(species)
  } else {
    stop("Species (number) not listed in PEP725 database!")
  }
}

# notice: pay attention to the download address
pr_dl_pep725 <- function(
    credentials = "E:/Raw_Phenological_Data/Europe_PEP725/credentials.txt",
    species = 115,
    path = "E:/Raw_Phenological_Data/Europe_PEP725/raw",
    internal = FALSE
){
  
  species_numbers = check_pep725_species(species = species)
  
  if(any(!file.exists(credentials) & missing(credentials))){
    stop("Credentials file not given or does not exist, check path !")
  } else {
    credentials = as.vector(unlist(utils::read.table(
      credentials, stringsAsFactors = FALSE)))
    email = credentials[1]
    password = credentials[2]
  }
  
  login <- list(
    email = email, pwd = password, submit = "Login")
  
  httr::POST("http://www.pep725.eu/login.php",
             body = login,
             encode = "form")
  
  all_pep_data = do.call("rbind",lapply(species_numbers, function(number){
    
    species_html = httr::POST(
      "http://www.pep725.eu/data_download/data_selection.php",
      body = list(
        plant = number,
        submit1 = "Submit"),
      encode = "form")
    
    species_links = xml2::read_html(species_html) %>%
      rvest::html_nodes("td a") %>%
      rvest::html_attr("href")
    
    do.call("rbind",lapply(species_links, function(link){
      
      tmp = tempfile()
      
      httr::GET(link, httr::write_disk(path = tmp,
                                       overwrite = TRUE))
      
      if (internal){
        pep_data = pr_merge_pep725(path = tmp)
        file.remove(tmp)
        return(pep_data)
      } else {
        file.copy(tmp, sprintf("%s/PEP725_%s.tar.gz",
                               path,
                               strsplit(link,"=")[[1]][2]),
                  overwrite = TRUE)
        file.remove(tmp)
      }
    }))
  }))
  
  if (internal){
    return(all_pep_data)
  }
}

# ---- 2. Extract tree species ----

# secies list (Manually delete/modify abnormal species)
# Citrus ? aurantium & Citrus ? limon: manually download from the website
# Picea abies (P.excelsa) & Quercus robur (Q.peduncula) : remove content in parentheses
# A total of 120 species

pacman::p_load(tidyverse)

input_file <- "E:/Raw_Phenological_Data/Europe_PEP725/species_unprocessed.txt" 
output_file <- "E:/Raw_Phenological_Data/Europe_PEP725/species.txt"

raw_data <- readLines(input_file)

cleaned_species <- raw_data %>%
  str_split("\\s*\\d+\\s*") %>% 
  unlist() %>%                  
  str_squish()                  

unique_sorted_species <- sort(unique(cleaned_species))
writeLines(unique_sorted_species, con = output_file)
cat("Species extraction and sorting complete! Results saved to:", output_file, "\n")

# ---- 3. data download ----

species_names <- readLines("E:/Raw_Phenological_Data/Europe_PEP725/species.txt")

for (species_name in species_names) {
  cat("Downloading data for species:", species_name, "\n")
  pr_dl_pep725(species = species_name)
}

# ---- 4. organize data ----

# Move the contents of folders into a common folder.

library(fs)

directory <- "E:/Raw_Phenological_Data/Europe_PEP725/data"
subdirs <- dir_ls(directory, type = "directory")

for (subdir in subdirs) {
  files <- dir_ls(subdir, type = "file")
  
  for (file in files) {
    target_path <- path(directory, path_file(file))
    
    if (file_exists(target_path)) {
      cat("File", path_file(file), "already exists, keeping one copy.\n")
    } else {
      file_move(file, target_path)
    }
  }
  dir_delete(subdir)
}

cat("All files have been consolidated into", directory, "\n")




# ---- 5. data combine ----
# combine all data to one csv file.

setwd("E:/Raw_Phenological_Data/Europe_PEP725/data")

# List of expected columns
expected_columns <- c("PEP_ID", "BBCH", "YEAR", "DAY")

file_list <- list.files(pattern = "*.csv")

all_data <- data.frame()

for (file_name in file_list) {
  
  data <- read.csv(file_name, sep = ";", header = TRUE)
  
  # Check for missing columns and add them if necessary
  missing_cols <- setdiff(expected_columns, names(data))
  
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      data[[col]] <- NA  # Add missing columns with NA values
    }
  }
  
  # Ensure columns are in the correct order
  data <- data[, expected_columns]
  
  # Extract country code and tree type from the file name
  file_parts <- strsplit(file_name, "_")[[1]]
  country_code <- file_parts[2]
  
  # Combine all parts from the third part onwards as the tree type
  treetype <- paste(file_parts[-(1:2)], collapse = "_")
  
  # Remove file extension if present
  treetype <- gsub("\\.csv$", "", treetype)
  
  # Add country and tree type columns
  data$country <- country_code
  data$treetype <- treetype
  
  # Combine with the main data frame
  all_data <- rbind(all_data, data)
}

# Rename column names if needed
colnames(all_data) <- c("PEP_ID", "BBCH", "YEAR", "DAY", "country", "treetype")

write.csv(all_data, "PEP725_Raw_Combined_Data.csv", row.names = FALSE)

print(unique(all_data$treetype))




# ---- 6. split by BBCH ----

bbch_groups <- split(all_data, all_data$BBCH)

output_folder <- "E:/Raw_Phenological_Data/Europe_PEP725/BBCH"

for(bbch in names(bbch_groups)) {
  
  file_name <- paste0(output_folder, "/BBCH_", bbch, ".csv")
  write.csv(bbch_groups[[bbch]], file_name, row.names = FALSE)
}

