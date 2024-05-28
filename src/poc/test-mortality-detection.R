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

#---- Perform analysis ----#

### check that death dates fall within GPS record ###

print("summarize mortality data...")

# animals known to have died
animals_dead <- animals %>%
  filter(mortality_code > 0)

# animals with known death date
animals_dead_withdate <- animals_dead %>%
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
animals_dead_filtered <- animals_check %>%
  filter(mortality_date_gap <= 0) 
  
# print summary counts of individuals
print(paste0("individuals total (n): ", nrow(animals)))
print(paste0("individuals with known mortality (n): ", nrow(animals_dead)))
print(paste0("individuals with known mortality date (n): ", nrow(animals_dead_withdate)))
print(paste0("individuals with known mortality within GPS record (n): ", nrow(animals_dead_filtered)))


### detect mortality from GPS data ###

detect_dead_individuals <- function(gps_data, time_window, sl_threshold){
  
  # get mean step length per individual over last portion of track
  step_length <- gps_data %>%
    group_by(animals_id_unique) %>%
    filter(year == max(year, na.rm = T)) %>%
    filter(doy >= (max(doy, na.rm = T) - time_window)) %>%
    summarize(mean_sl = mean(sl, na.rm = T))
  
  # extract putative dead animals based on step length threshold
  deads <- step_length %>% 
    filter(mean_sl < sl_threshold) 
  
  # find the number of false positives
  false_positive <- length(setdiff(deads$animals_id_unique, animals_dead$animals_id_unique))
  
  # export results as data frame
  results <- data.frame(time_window = time_window,
                        sl_threshold = sl_threshold,
                        n_dead = nrow(deads),
                        false_positive = false_positive)
                
  return(results)
}

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
         doy = day(acquisition_time)) 

# detect mortality based on range of averaging windows and step length thresholds

print("test morality detection...")

test <- rbind(detect_dead_individuals(gps, 1, 10),
              detect_dead_individuals(gps, 1, 20),
              detect_dead_individuals(gps, 1, 30),
              detect_dead_individuals(gps, 1, 40),
              detect_dead_individuals(gps, 1, 50),
              detect_dead_individuals(gps, 1, 60),
              detect_dead_individuals(gps, 1, 70),
              detect_dead_individuals(gps, 1, 80),
              detect_dead_individuals(gps, 1, 90),
              detect_dead_individuals(gps, 1, 100),
              detect_dead_individuals(gps, 3, 10),
              detect_dead_individuals(gps, 3, 20),
              detect_dead_individuals(gps, 3, 30),
              detect_dead_individuals(gps, 3, 40),
              detect_dead_individuals(gps, 3, 50),
              detect_dead_individuals(gps, 3, 60),
              detect_dead_individuals(gps, 3, 70),
              detect_dead_individuals(gps, 3, 80),
              detect_dead_individuals(gps, 3, 90),
              detect_dead_individuals(gps, 3, 100),
              detect_dead_individuals(gps, 5, 10),
              detect_dead_individuals(gps, 5, 20),
              detect_dead_individuals(gps, 5, 30),
              detect_dead_individuals(gps, 5, 40),
              detect_dead_individuals(gps, 5, 50),
              detect_dead_individuals(gps, 5, 60),
              detect_dead_individuals(gps, 5, 70),
              detect_dead_individuals(gps, 5, 80),
              detect_dead_individuals(gps, 5, 90),
              detect_dead_individuals(gps, 5, 100),
              detect_dead_individuals(gps, 7, 10),
              detect_dead_individuals(gps, 7, 20),
              detect_dead_individuals(gps, 7, 30),
              detect_dead_individuals(gps, 7, 40),
              detect_dead_individuals(gps, 7, 50),
              detect_dead_individuals(gps, 7, 60),
              detect_dead_individuals(gps, 7, 70),
              detect_dead_individuals(gps, 7, 80),
              detect_dead_individuals(gps, 7, 90),
              detect_dead_individuals(gps, 7, 100))

# compute false positive and negative rates
test <- test %>%
  mutate(proportion_false_positive = false_positive/n_dead)

# plot results
p1 <- ggplot(test) +
  geom_point(aes(x = time_window, y = n_dead,
                 color = sl_threshold)) +
  scale_color_viridis_c() +
  labs(x = "Averaging window (days)",
       y = "Estimated dead (n)",
       color = "Mean step length threshold") +
  theme(legend.position="bottom")

p2 <- ggplot(test) +
  geom_point(aes(x = time_window, y = proportion_false_positive*100,
                 color = sl_threshold)) +
  scale_color_viridis_c() +
  labs(x = "Averaging window (days)",
       y = "False positive (%)",
       color = "Mean step length threshold") +
  theme(legend.position="bottom")


p3 <- ggplot(test) +
  geom_point(aes(x = n_dead, y = proportion_false_positive*100,
                 color = sl_threshold,
                 size = time_window),
             alpha = 0.5) +
  geom_vline(xintercept = nrow(animals_dead_filtered)) +
  scale_color_viridis_c() +
  labs(x = "Estimated dead (n)",
       y = "False positive (%)",
       color = "Mean step length threshold",
       size = "Averaging window (days)") +
  theme(legend.position="bottom", legend.box = "vertical")

#---- Save output ---#

p <- (p1 | p2)/(p3)

ggsave(p, file = here::here("out","test-mortatlity-dection.pdf"),
       width = 8, height = 8)
