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
  
  species <- gps_data[1,]$common_name
  
  animals_dead_species <- animals_dead %>%
    filter(common_name == species)
  
  false_positive <- length(setdiff(deads$animals_id_unique, animals_dead_species$animals_id_unique))
  
  # export results as data frame
  results <- data.frame(time_window = time_window,
                        sl_threshold = sl_threshold,
                        n_dead_detected = nrow(deads),
                        false_positive = false_positive) %>%
            mutate(proportion_dead_detected_false_positive = false_positive/n_dead_detected,
                   n_dead_true = rep(nrow(animals_dead_species),n()),
                   false_negative = n_dead_true - n_dead_detected,
                   proportion_dead_true_false_negative = false_negative/n_dead_true)
                
  return(results)
}


find_species_mortality <- function(gps_data, species){
  
  gps_species <- gps_data %>%
    filter(common_name == species)
  
  test <- rbind(detect_dead_individuals(gps_species, 1, 10),
                detect_dead_individuals(gps_species, 1, 20),
                detect_dead_individuals(gps_species, 1, 30),
                detect_dead_individuals(gps_species, 1, 40),
                detect_dead_individuals(gps_species, 1, 50),
                detect_dead_individuals(gps_species, 1, 60),
                detect_dead_individuals(gps_species, 1, 70),
                detect_dead_individuals(gps_species, 1, 80),
                detect_dead_individuals(gps_species, 1, 90),
                detect_dead_individuals(gps_species, 1, 100),
                detect_dead_individuals(gps_species, 3, 10),
                detect_dead_individuals(gps_species, 3, 20),
                detect_dead_individuals(gps_species, 3, 30),
                detect_dead_individuals(gps_species, 3, 40),
                detect_dead_individuals(gps_species, 3, 50),
                detect_dead_individuals(gps_species, 3, 60),
                detect_dead_individuals(gps_species, 3, 70),
                detect_dead_individuals(gps_species, 3, 80),
                detect_dead_individuals(gps_species, 3, 90),
                detect_dead_individuals(gps_species, 3, 100),
                detect_dead_individuals(gps_species, 5, 10),
                detect_dead_individuals(gps_species, 5, 20),
                detect_dead_individuals(gps_species, 5, 30),
                detect_dead_individuals(gps_species, 5, 40),
                detect_dead_individuals(gps_species, 5, 50),
                detect_dead_individuals(gps_species, 5, 60),
                detect_dead_individuals(gps_species, 5, 70),
                detect_dead_individuals(gps_species, 5, 80),
                detect_dead_individuals(gps_species, 5, 90),
                detect_dead_individuals(gps_species, 5, 100),
                detect_dead_individuals(gps_species, 7, 10),
                detect_dead_individuals(gps_species, 7, 20),
                detect_dead_individuals(gps_species, 7, 30),
                detect_dead_individuals(gps_species, 7, 40),
                detect_dead_individuals(gps_species, 7, 50),
                detect_dead_individuals(gps_species, 7, 60),
                detect_dead_individuals(gps_species, 7, 70),
                detect_dead_individuals(gps_species, 7, 80),
                detect_dead_individuals(gps_species, 7, 90),
                detect_dead_individuals(gps_species, 7, 100))
  
  
  
  results <- test %>%
    mutate(common_name = rep(species, n()))
  
  return(results)
}

# detect mortality based on range of averaging windows and step length thresholds

print("test morality detection...")

test_mortality_lynx <- find_species_mortality(gps, "lynx")
test_mortality_red_deer <- find_species_mortality(gps, "red deer")
test_mortality_roe_deer <- find_species_mortality(gps, "roe deer")
test_mortality_wild_boar <- find_species_mortality(gps, "wild boar")
test_mortality_wildcat <- find_species_mortality(gps, "wildcat")

test_mortality <- rbind(test_mortality_lynx,
                        test_mortality_red_deer,
                        test_mortality_roe_deer,
                        test_mortality_wild_boar,
                        test_mortality_wildcat)

#---- Plot results ---#


# plot results
p1 <- ggplot(data = test_mortality) +
  facet_grid(~common_name) +
  geom_line(aes(x = time_window, y = n_dead_true)) +
  geom_point(aes(x = time_window, y = n_dead_detected,
                 color = sl_threshold)) +
  scale_color_viridis_c(option = "magma") +
  labs(x = "Averaging window (days)",
       y = "Estimated dead (n)",
       color = "Mean step length threshold") +
  theme(legend.position="none")

p2 <- ggplot(data = test_mortality) +
  facet_grid(~common_name) +
  geom_point(aes(x = time_window, y = proportion_dead_detected_false_positive*100,
                 color = sl_threshold)) +
  scale_color_viridis_c(option = "magma") +
  labs(x = "Averaging window (days)",
       y = "False positive (%)",
       color = "Mean step length threshold") +
  theme(legend.position="none")

p3 <- ggplot(data = test_mortality) +
  facet_grid(~common_name) +
  geom_point(aes(x = time_window, y = proportion_dead_true_false_negative*100,
                 color = sl_threshold)) +
  scale_color_viridis_c(option = "magma") +
  labs(x = "Averaging window (days)",
       y = "False negative (%)",
       color = "Mean step length threshold") +
  theme(legend.position="none")

p4 <- ggplot(test_mortality) +
  facet_grid(~common_name) +
  geom_point(aes(x = proportion_dead_true_false_negative*100, y = proportion_dead_detected_false_positive*100,
                 color = sl_threshold,
                 size = time_window),
             alpha = 0.5) +
  scale_color_viridis_c(option = "magma") +
  labs(x = "False negative (%)",
       y = "False positive (%)",
       color = "Mean step length threshold",
       size = "Averaging window (days)") +
  theme(legend.position="bottom", legend.box = "vertical")

#---- Save output ---#

p <- (p1)/(p2)/(p3)/(p4)

ggsave(p, file = here::here("out","test-mortatlity-dection.pdf"),
       width = 10, height = 12)
