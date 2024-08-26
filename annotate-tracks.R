library(tidyverse)
library(here)
library(lubridate)
library(ARTofR)
library(sf)
library(terra)

rm(list = ls())

#........................source functions........................

list.files(here("src", "funs","auto"), full.names = TRUE) %>%
  walk(source)

#..........................read in data..........................

# animals metadata
animals_file <- select_file(here("analysis"), "animals")
animals <- read_csv(here("analysis", animals_file))

# gps data
gps_file <- select_file(here("analysis"), "gps")
gps <- read_csv(here("analysis", gps_file))

# global human modification
ghm <- terra::rast(here("data", "gHM.tif"))

#........................annotate tracks.........................

gps_sf <- st_as_sf(gps, coords = c("longitude", "latitude"), crs = 4326) 

gps_test_sf <- gps_sf[1:1000000,] %>%
  st_transform(crs = crs(ghm))

gps_test <- vect(gps_test_sf) 

gps_test_extract <- terra::extract(y = gps_test, x = ghm)

