rm(list = ls())
library(tidyverse)
library(data.table)
library(sf)


### read in animal metadata ###
# lynx animal data missing?
red_deer_animals <- fread("././data/red_deer_animals.txt", sep = ";")
roe_deer_animals <- read.csv("././data/roe_animals.csv")
wild_boar_animals <- read.csv("././data/wildboar_animals.csv")
wildcat_animals <- read.csv("././data/wildcat_animals.csv") %>%
  mutate(mortality_code_new = rep(NA, n()))

### read in GPS data ###
lynx_gps <- read.csv("././data/lynx_gps.csv") 
red_deer_gps <- read.csv("././data/red_animals.csv") # incorrectly named
# roe deer GPS data missing?
wild_boar_gps <- read.csv("././data/wildboar_gps.csv")
wildcat_gps <- read.csv("././data/wildcat_gps.csv") 

### clean animal metadata to consistent format ###
#   - select matching columns

clean_animals_data <- function(animals_data, scientific_name, common_name){
  
  animals_data_clean <- animals_data %>%
    select(animals_id, 
           sex, 
           year_birth, 
           year_birth_exact,
           mortality_code,
           mortality_code_new,
           death_date,
           death_time) %>%
    mutate(species = rep(scientific_name, n()),
           common_name = rep(common_name, n()))
  
  return(animals_data_clean)
}

#lynx_animals_clean <- clean_animals_data(lynx_animals, "Lynx lynx", "lynx")
red_deer_animals_clean <- clean_animals_data(red_deer_animals, "Cervus elaphus", "red deer") 
roe_deer_animals_clean <- clean_animals_data(roe_deer_animals, "Capreolus capreolus", "roe deer") 
wild_boar_animals_clean <- clean_animals_data(wild_boar_animals, "Sus scrofa", "wild boar")
wildcat_animals_clean <- clean_animals_data(wildcat_animals,"Felis silvestris", "wildcat")

animals_clean <- rbind(
                       #lynx_animals_clean,
                       red_deer_animals_clean,
                       #roe_deer_animals_clean,
                       wild_boar_animals_clean,
                       wildcat_animals_clean)

### clean GPS to consistent format ###
#   - remove missed fixes
#   - remove inaccurate fixes (DOP < 5)

clean_gps_data <- function(gps_data, scientific_name, common_name){
  
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
    mutate(species = rep(scientific_name, n()),
           common_name = rep(common_name, n()))
  
  return(gps_clean)
}


lynx_gps_clean <- clean_gps_data(lynx_gps, "Lynx lynx", "lynx")
red_deer_gps_clean <- clean_gps_data(red_deer_gps, "Cervus elaphus", "red deer") 
#roe_deer_gps_clean <- clean_gps_data(roe_deer_gps, "Capreolus capreolus", "roe deer") 
wild_boar_gps_clean <- clean_gps_data(wild_boar_gps, "Sus scrofa", "wild boar")
wildcat_gps_clean <- clean_gps_data(wildcat_gps,"Felis silvestris", "wildcat")

gps_clean <- rbind(
                  lynx_gps_clean,
                   red_deer_gps_clean,
                   #roe_deer_gps_clean,
                   wild_boar_gps_clean,
                   wildcat_gps_clean)


