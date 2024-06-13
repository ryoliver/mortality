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

# animals with known death date
animals_dead_withdate <- animals %>%
  filter(mortality_code > 0) %>%
  filter(!is.na(death_date)) 

# find date of last GPS signal for animals with known mortality
gps_dead <- gps %>%
  filter(animals_id_unique %in% animals_dead_withdate$animals_id_unique) %>%
  group_by(animals_id_unique) %>%
  arrange(desc(acquisition_time)) %>%
  slice_head() %>%
  select(animals_id_unique, acquisition_time) %>%
  rename("final_gps_date_time" = acquisition_time)

# test whether mortality data is within GPS record
animals_check <- left_join(animals_dead_withdate, gps_dead, 
                           by = "animals_id_unique") %>%
  unite("death_date_time", death_date, death_time, sep = " ", remove = FALSE) %>%
  mutate(death_date_time = as_datetime(death_date_time),
         death_date = as_date(death_date),
         final_gps_date = as_date(final_gps_date_time),
         mortality_date_gap = death_date - final_gps_date)

# filter to animals within GPS record
animals_dead_inside_GPS_record <- animals_check %>%
  filter(mortality_date_gap <= 0) %>%
  select(animals_id_unique) %>%
  mutate(status = "died inside GPS record")

animals_dead_outside_GPS_record <- animals_check %>%
  filter(mortality_date_gap > 0) %>%
  select(animals_id_unique) %>%
  mutate(status = "died outside GPS record")

animals_dead <- rbind(animals_dead_inside_GPS_record,
                      animals_dead_outside_GPS_record)

# find step lengths
gps <- gps %>%
  filter(!is.na(acquisition_time)) %>%
  filter(abs(latitude) < 90) %>%
  filter(abs(longitude) < 180) %>%
  arrange(animals_id_unique, acquisition_time) %>%
  mutate(lag_longitude = dplyr::lag(longitude, 1),
         lag_latitude = dplyr::lag(latitude, 1),
         sl = distGeo(cbind(longitude,latitude), cbind(lag_longitude, lag_latitude)),
         year = year(acquisition_time),
         doy = day(acquisition_time)) %>%
  left_join(., animals_dead, by = "animals_id_unique") %>%
  mutate(status = ifelse(is.na(status), "alive", status))

test <- gps %>%
  group_by(animals_id_unique) %>%
  filter(year == max(year, na.rm = T)) %>%
  filter(doy >= (max(doy, na.rm = T) - 7)) %>%
  ungroup()


ggplot(test) +
  facet_wrap(~common_name, scales = "free") +
  geom_histogram(aes(x = sl, fill = status),
                 position = "identity",
                 alpha = 0.5) +
  scale_x_log10()

ggplot(test) +
  facet_wrap(~common_name, scales = "free") +
  geom_line(aes(x = acquisition_time, y = sl,
                group = animals_id_unique,
                colour = status))
  
