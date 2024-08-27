library(tidyverse)
library(here)
library(lubridate)
library(mgcv)

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

#....................prep data for modelling.....................

annotated_data <- gps_annotated %>%
  left_join(., animals, by = c("animals_id_unique", "scientific_name", "common_name")) %>%
  mutate(week = week(acquisition_time)) %>%
  mutate(dead = case_when(mortality_code_new > 0 ~ TRUE, 
                          mortality_code_new == 0 ~ FALSE)) %>%
  mutate(dead = as.factor(dead)) %>%
  select(animals_id_unique, scientific_name, common_name, sex, week,
         death_date, gap_death_gps_date, mortality_code_new,
         dead, ghm)

#........................model mortality.........................

m1 <- gam(ghm ~ dead, data = annotated_data)

m2 <- gam(ghm ~ dead + s(week, by = dead, bs = "cc"), data = annotated_data)

m3 <- gam(ghm ~ dead + s(week, by = dead, bs = "cc") + s(animals_id_unique, bs = "re"), data = annotated_data)

m4 <- gam(ghm ~ dead + s(week, by = dead)+s(animals_id_unique, by = scientific_name, bs = "re"), data = annotated_data)

#..........................plot summary..........................

p1 <- ggplot(data = annotated_data) +
  geom_density(aes(x = ghm, group = dead, color = dead, fill = dead), alpha = 0.3) +
  theme_classic() +
  facet_wrap(~common_name)

ggsave(p1, file = here("out", "ghm_distribution.png"))

p2 <- ggplot(data = annotated_data) +
  #geom_smooth(aes(x = week, y = ghm, color = dead, fill = dead), method = "gam")+
  geom_point(aes(x = week, y = ghm, color = dead)) +
  theme_classic() +
  facet_wrap(~common_name)

ggsave(p2, file = here("out", "ghm_week.png"))


