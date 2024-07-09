rm(list = ls())
library(tidyverse)
library(data.table)
library(here)
library(geosphere)
library(patchwork)
library(sf)
library(scales)

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


animals <- fread(here::here("analysis", animals_files$file_name)) %>%
  mutate(death_date = lubridate::date(death_date))
gps <- fread(here::here("analysis", gps_files$file_name)) %>%
  mutate(acquisition_time = lubridate::date(acquisition_time))

# convert GPS data to sf object
gps_sf <- gps %>%
  st_as_sf(coords = c("longitude", "latitude"))

# filter to animals with GPS data and known death date
animals_dead_with_date <- animals %>%
  filter(!is.na(death_date)) %>%
  filter(animals_id_unique %in% gps$animals_id_unique)



# select animal to display
animal_test <- animals_dead_with_date[1,] %>%
  mutate(death_time = "00:00:00") %>%
  unite(death_time, death_date, death_time, sep = " ") %>%
  mutate(death_time = as.POSIXct(death_time))

gps_test <- gps %>%
  filter(animals_id_unique == animal_test$animals_id_unique) 

gps_test_sf <- gps %>%
  filter(animals_id_unique == animal_test$animals_id_unique) 


ggplot() +
  geom_line(aes(x = c(animal_test$death_time, animal_test$death_time), 
                 y = c(min(gps_test$latitude),
                       max(gps_test$latitude))), 
            col = "black", lwd = 1) +
  geom_point(data = gps_test, aes(x = acquisition_time, y = latitude),
             alpha = 0.5) +
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y") +
  labs(x = " ", y = "Latitude")

ggplot() +
  geom_line(aes(x = c(animal_test$death_time, animal_test$death_time), 
                y = c(min(gps_test$longitude),
                      max(gps_test$longitude))), 
            col = "black", lwd = 1) +
  geom_point(data = gps_test, aes(x = acquisition_time, y = longitude),
             alpha = 0.5) +
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y") +
  labs(x = " ", y = "Longitude")


