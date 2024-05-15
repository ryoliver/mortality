rm(list = ls())
library(tidyverse)
library(data.table)
library(sf)

lookup_mortality_lynx <- fread("././data/lu_mortality_lynx.txt", sep = ";")
lookup_mortality_wildboar <- fread("././data/lu_mortality_wildboar.txt", sep = ";")

mortality_codes <- fread("././data/lu_mortality_new.txt", sep = ";")


# lynx animal data missing?
lynx_gps <- read.csv("././data/lynx_gps.csv") %>%
  filter(!is.na(latitude)) %>%
  filter(!is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) %>%
  select(animals_id, acquisition_time) %>%
  mutate(species = rep("Lynx lynx", n()))

red_deer_animals <- fread("././data/red_deer_animals.txt", sep = ";")
red_deer_gps <- read.csv("././data/red_animals.csv") %>% # incorrectly named
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(4326)) %>%
  select(specie, animals_id, acquisition_time)

  
roe_deer_animals <- read.csv("././data/roe_animals.csv")
# roe deer GPS data missing?

wildboar_animals <- read.csv("././data/wildboar_animals.csv")
wildboar_gps <- read.csv("././data/wildboar_gps.csv")

wildcat_animals <- read.csv("././data/wildcat_animals.csv")
wildcat_gps <- read.csv("././data/wildcat_gps.csv")

