rm(list = ls())
library(tidyverse)
library(data.table)
library(here)
library(geosphere)
library(patchwork)

#---- Load data ----#

### read in animal and GPS data ###

print("reading in data...")

animals <- fread(here::here("analysis","animals_clean.csv"))
gps <- fread(here::here("analysis","gps_clean.csv"))

# filter to animals with GPS data
animals <- animals %>%
  filter(animals_id_unique %in% gps$animals_id_unique)