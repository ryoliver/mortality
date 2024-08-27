library(tidyverse)
library(here)
library(lubridate)
library(sf)
library(terra)

rm(list = ls())

#........................source functions........................

print("source functions...")

list.files(here("src", "funs","auto"), full.names = TRUE) %>%
  walk(source)

#..........................read in data..........................

print("read in data...")

# gps data
gps_file <- select_file(here("analysis", "data_cleaned"), "gps")
gps <- read_csv(here("analysis", "data_cleaned", gps_file))

# global human modification
ghm <- terra::rast(here("data", "gHM.tif"))

#..................prep GPS data for annotation..................

print("prep GPS data for annotation...")

# convert to sf object
gps_sf <- st_as_sf(gps, coords = c("longitude", "latitude"), crs = 4326) 

# transform CRS to match GHM
gps_sf <- st_transform(gps_sf, crs = crs(ghm))

# convert to terra::SpatVector to use extract()
gps_spatvect <- vect(gps_sf) 

#........................annotate tracks.........................

print("annotate tracks...")

# annotate GPS data with GHM layer
gps_annotated_ghm <- terra::extract(y = gps_spatvect, x = ghm)

#........................clean up output.........................

print("clean up output...")

# bind annotations to original GPS data frame
gps_annotated <- cbind(gps, gps_annotated_ghm) %>%
  rename(ghm = gHM) %>%
  select(-ID)

#.........................write out data.........................

print("write out data...")

output_filename <- paste0(str_sub(gps_file, start = 1, end = -5),"_annotated.csv")
write_csv(x = gps_annotated, file = here("analysis", "data_annotated", output_filename))
