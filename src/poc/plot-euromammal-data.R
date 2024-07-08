rm(list = ls())
library(tidyverse)
library(data.table)
library(here)
library(geosphere)
library(patchwork)
library(sf)

#---- Load data ----#

### read in animal and GPS data ###

print("reading in data...")

# find most recent cleaned data
animals_files <- data.frame(file_name = list.files(here::here("analysis"), "animals_*")) %>%
  arrange(desc(file_name)) %>%
  slice(1)

gps_files <- data.frame(file_name = list.files(here::here("analysis"), "gps_*")) %>%
  arrange(desc(file_name)) %>%
  slice(1)

print(paste0("cleaned animal metadata: ", animals_files$file_name))
print(paste0("cleaned GPS data: ", gps_files$file_name))


animals <- fread(here::here("analysis", animals_files$file_name))
gps <- fread(here::here("analysis", gps_files$file_name)) 

gps_test <- gps %>%
  mutate(longitude = as.numeric(longitude)) %>%
  filter(is.na(longitude))

%>%
  st_as_sf(coords = c("longitude", "latitude"))

# filter to animals with GPS data
animals <- animals %>%
  filter(animals_id_unique %in% gps$animals_id_unique)

animals_died_with_date <- animals %>%
  filter(!is.na(death_date))


animal_test <- animals_died_with_date[1,]
gps_test <- gps %>%
  filter(animals_id_unique == animal_test$animals_id_unique) 
