library(tidyverse)
library(here)
library(lubridate)

rm(list = ls())

#........................source functions........................

print("source functions...")

list.files(here("src", "funs","auto"), full.names = TRUE) %>%
  walk(source)

#..........................read in data..........................

print("read in data...")

# annotated gps data
gps_annotated_file <- select_annotated_file(here("analysis", "data_annotated"), "gps")
print(paste("reading in", gps_annotated_file))
gps_annotated <- read_csv(here("analysis", "data_annotated", gps_annotated_file))

# animal metadata
animals_file <- select_file(here("analysis", "data_cleaned"), "animals")
print(paste("reading in", animals_file))
animals <- read_csv(here("analysis", "data_cleaned", animals_file))

annotated_data <- gps_annotated %>%
  select(animals_id_unique, ghm) %>%
  left_join(., animals, by = "animals_id_unique") %>%
  mutate(dead = case_when(mortality_code_new > 0 ~ TRUE, 
                          mortality_code_new == 0 ~ FALSE))


  

