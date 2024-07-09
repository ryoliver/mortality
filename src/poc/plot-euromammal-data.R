rm(list = ls())
library(tidyverse)
library(data.table)
library(here)
library(geosphere)
library(patchwork)
library(sf)
library(scales)
library(gridE)

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
  mutate(acquisition_time = as.POSIXct(acquisition_time))


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
  filter(animals_id_unique == animal_test$animals_id_unique) %>%
  arrange(acquisition_time) %>%
  mutate(lag_longitude = dplyr::lag(longitude, 1),
         lag_latitude = dplyr::lag(latitude, 1),
         sl = distGeo(cbind(longitude,latitude), cbind(lag_longitude, lag_latitude)))

date_difference <- round(animal_test$death_time - gps_test[nrow(gps_test),]$acquisition_time, 0)


p1 <- ggplot() +
  geom_line(aes(x = c(animal_test$death_time, animal_test$death_time), 
                 y = c(min(gps_test$latitude),
                       max(gps_test$latitude))), 
            col = "#EF6F6C", lwd = 1) +
  geom_point(data = gps_test, aes(x = acquisition_time, y = latitude),
             col = "#7C73E8",alpha = 0.5) +
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y") +
  labs(x = " ", y = "latitude",
       title = paste0(animal_test$common_name,": ",animal_test$animals_id_unique),
       subtitle = paste0("data gap? ", date_difference, " days"))

p2 <- ggplot() +
  geom_line(aes(x = c(animal_test$death_time, animal_test$death_time), 
                y = c(min(gps_test$longitude),
                      max(gps_test$longitude))), 
            col = "#EF6F6C", lwd = 1) +
  geom_point(data = gps_test, aes(x = acquisition_time, y = longitude),
             col = "#7C73E8",alpha = 0.5) +
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y") +
  labs(x = " ", y = "longitude")

p3 <- ggplot() +
  geom_line(aes(x = c(animal_test$death_time, animal_test$death_time), 
                y = c(min(gps_test$sl, na.rm = TRUE),
                      max(gps_test$sl, na.rm = TRUE))), 
            col = "#EF6F6C", lwd = 1) +
  geom_point(data = gps_test, aes(x = acquisition_time, y = sl),
             col = "#7C73E8",alpha = 0.5) +
  scale_x_datetime(date_breaks = "1 month", date_labels =  "%b %Y") +
  labs(x = " ", y = "step length")

p <- p1 / p2 / p3

ggsave(here::here("out","plot-euromammal-data",paste0(animal_test$animals_id_unique,".pdf")),p)
