rm(list = ls())
library(tidyverse)
library(data.table)
library(here)
library(geosphere)
library(patchwork)
library(sf)
library(scales)

source(here::here("src","funs","plot_individual_summary.R"))

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
  mutate(acquisition_time = as.POSIXct(acquisition_time)) %>%
  filter(abs(longitude) < 180) %>%
  filter(abs(latitude) < 90)

#---- Analysis ---#

gps_final_location <- gps %>%
  arrange(animals_id_unique, desc(acquisition_time)) %>%
  group_by(animals_id_unique) %>%
  slice_head() %>%
  select(animals_id_unique, acquisition_time)


# find gap between GPS data and death date
animals_dead_with_date <- animals %>%
  filter(!is.na(death_date)) %>% # filter to animals with death date
  filter(animals_id_unique %in% gps$animals_id_unique) %>% # filter to animals with GPS data
  left_join(., gps_final_location, by = c("animals_id_unique")) %>% # join with date of final location
  mutate(death_date = as.POSIXct(death_date),
         acquisition_date = lubridate::as_date(as.POSIXct(acquisition_time))) %>%
  mutate(data_gap = difftime(death_date, acquisition_date, units = "days")) %>% # find gap between locations and death date
  filter(abs(data_gap) < 365)

#---- Print data gap summary ---#

p1 <- ggplot(data = animals_dead_with_date) +
  facet_wrap(~ common_name, scales = "free") +
  geom_histogram(aes(x = data_gap)) +
  labs(x = "data gap (days)",
       title = "Filtered to 1 year")


p2 <- ggplot(data = subset(animals_dead_with_date, 
                     abs(data_gap) < 10)) +
  facet_wrap(~ common_name, scales = "free") +
  geom_histogram(aes(x = data_gap)) +
  labs(x = "data gap (days)",
       title = "Filtered to 10 days")

p <- p1 / p2

ggsave(here::here("out","plot-euromammal-data","summary-data-gap.pdf"), p)


#---- Print random output ---#

# randomly select animals to plot
animals_to_plot_random <- animals_dead_with_date %>%
  sample_n(100)

for(i in 1:nrow(animals_to_plot)){
  
  print(paste0(round(i/nrow(animals_to_plot_random)*100),"%"))
  
  filepath <- here::here("out","plot-euromammal-data","random-individuals",
                         paste0(animal_test$animals_id_unique,".pdf"))
  
  plot_individual_summary(animals_to_plot_random[i,], gps, filepath)
}

#---- Print subset output ---#

# select animals with less than a 3 day gap between GPS data and mortality
animals_dead_subset_window <- animals_dead_with_date %>%
  filter(abs(data_gap) < 3)

for(i in 1:nrow(animals_dead_subset_window)){

  print(paste0(round(i/nrow(animals_dead_subset_window)*100),"%"))
  
  filepath <- here::here("out","plot-euromammal-data","subset-individuals",
                         paste0(animal_test$animals_id_unique,".pdf"))
  
  plot_individual_summary(animals_dead_subset_window[i,], gps, filepath)
}

print("done!")
