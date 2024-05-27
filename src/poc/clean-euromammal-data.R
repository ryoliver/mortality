rm(list = ls())
library(tidyverse)
library(data.table)

### read in animal metadata ###

animals_lynx <- fread("././data/lynx_animals.csv") %>%
  mutate(mortality_code_new = rep(NA, n()))
animals_red_deer <- fread("././data/reddeer_animals.csv")
animals_roe_deer <- fread("././data/roe_animals.csv")
animals_wild_boar <- fread("././data/wildboar_animals.csv")
animals_wildcat <- fread("././data/wildcat_animals.csv") %>%
  mutate(mortality_code_new = rep(NA, n()))

### read in GPS data ###

gps_lynx <- fread("././data/lynx_gps.csv") 
gps_red_deer <- fread("././data/reddeer_gps.csv")
gps_roe_deer <- fread("././data/roe_gps.csv")
gps_wild_boar <- fread("././data/wildboar_gps.csv")
gps_wildcat <- fread("././data/wildcat_gps.csv")


### function to clean animal metadata to consistent format ###
#   - select matching columns

clean_animals_data <- function(animals_data, species_common_name){
  
  sp <- species_info %>%
    filter(common_name == species_common_name)
  
  scientific_name <- sp$scientific_name
  suffix <- sp$suffix
  
  animals_data_clean <- animals_data %>%
    select(animals_id, 
           sex, 
           year_birth, 
           year_birth_exact,
           mortality_code,
           mortality_code_new,
           death_date,
           death_time) %>%
    mutate(scientific_name = rep(scientific_name, n()),
           common_name = rep(species_common_name, n()),
           animals_id_suffix = rep(suffix, n())) %>%
    unite(animals_id_unique, animals_id, animals_id_suffix, sep = "-", remove = FALSE)
  
  return(animals_data_clean)
}

### function to clean GPS to consistent format ###
#   - remove missed fixes
#   - remove inaccurate fixes (DOP < 5)

clean_gps_data <- function(gps_data, species_common_name){
  
  sp <- species_info %>%
    filter(common_name == species_common_name)
  
  scientific_name <- sp$scientific_name
  suffix <- sp$suffix
  
  gps_clean <- gps_data %>%
    filter(dop < 5) %>%
    filter(!is.na(latitude)) %>%
    filter(!is.na(longitude)) %>%
    select(animals_id,
           latitude, 
           longitude,
           acquisition_time, 
           gps_validity_code,
           dop) %>%
    mutate(scientific_name = rep(scientific_name, n()),
           common_name = rep(species_common_name, n()),
           animals_id_suffix = rep(suffix, n())) %>%
    unite(animals_id_unique, animals_id, animals_id_suffix, sep = "-", remove = FALSE)
  
  return(gps_clean)
}


### clean animal metadata to consistent format ###

species_info <- data.frame(scientific_name = c("Lynx lynx",
                                               "Cervus elaphus",
                                               "Capreolus capreolus",
                                               "Sus scrofa",
                                               "Felis silvestris"),
                           common_name = c("lynx",
                                           "red deer",
                                           "roe deer",
                                           "wild boar",
                                           "wildcat"),
                           suffix = c("LX","RE","RO","WB","WC"))


animals_clean_lynx <- clean_animals_data(animals_lynx, "lynx")
animals_clean_red_deer <- clean_animals_data(animals_red_deer, "red deer") 
animals_clean_roe_deer <- clean_animals_data(animals_roe_deer, "roe deer") 
animals_clean_wild_boar <- clean_animals_data(animals_wild_boar, "wild boar")
animals_clean_wildcat <- clean_animals_data(animals_wildcat, "wildcat")

animals_clean <- rbind(animals_clean_lynx,
                       animals_clean_red_deer,
                       animals_clean_roe_deer,
                       animals_clean_wild_boar,
                       animals_clean_wildcat)

### clean GPS to consistent format ###

gps_clean_lynx <- clean_gps_data(gps_lynx, "lynx")
gps_clean_red_deer <- clean_gps_data(gps_red_deer, "red deer") 
gps_clean_roe_deer <- clean_gps_data(gps_roe_deer, "roe deer") 
gps_clean_wild_boar <- clean_gps_data(gps_wild_boar, "wild boar")
gps_clean_wildcat <- clean_gps_data(gps_wildcat, "wildcat")

gps_clean <- rbind(gps_clean_lynx,
                   gps_clean_red_deer,
                   gps_clean_roe_deer,
                   gps_clean_wild_boar,
                   gps_clean_wildcat)

### checks ###

# check that all rows in animal metadata represent unique animals
if(n_distinct(animals_clean$animals_id_unique) == nrow(animals_clean)) {
  print("animal metadata: all rows unique")
} else{
  print("animal metadata: rows not unique")
}

# check that all gps data has animal metadata
if(length(setdiff(gps_clean$animals_id_unique, animals_clean$animals_id_unique)) == 0){
  print("gps data: no animals missing metadata")
} else{
  print(paste0("gps data: ",
               length(setdiff(gps_clean$animals_id_unique, animals_clean$animals_id_unique)),
               " missing metadata"))
}

### write out cleaned data ##
fwrite(animals_clean, "././analysis/animals_clean.csv")
fwrite(gps_clean, "././analysis/gps_clean.csv")