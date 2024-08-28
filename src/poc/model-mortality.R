library(tidyverse)
library(here)
library(lubridate)
library(stringr)
library(mgcv)
library(tidymv)


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
  mutate(doy = yday(acquisition_time),
         death_date_doy = yday(death_date)) %>%
  mutate(dead = case_when(mortality_code_new > 0 ~ TRUE, 
                          mortality_code_new == 0 ~ FALSE)) %>%
  mutate(dead = as.factor(dead),
         animals_id_unique = as.factor(animals_id_unique),
         scientific_name = as.factor(scientific_name)) %>%
  select(animals_id_unique, scientific_name, common_name, sex, doy,
         death_date, death_date_doy, gap_death_gps_date, mortality_code_new,
         dead, ghm)

#........................model mortality.........................

species_gam <- function(species) {
  
  # filter annotated GPS data
  data_species <- annotated_data %>%
    filter(scientific_name == species) %>%
    filter(!is.na(ghm))
  
  # filter animals metadata
  animals_species <- animals %>%
    filter(scientific_name == species) %>%
    mutate(death_date_doy = yday(death_date))
  
  # count number of alive individuals
  n_alive <- animals_species %>%
    filter(mortality_code_new == 0) %>%
    nrow(.)
  
  # count number of dead individuals
  n_dead <- animals_species %>%
    filter(mortality_code_new > 0) %>%
    nrow(.)
  
  species_common_name <- animals_species[1,]$common_name
  
  m <- bam(scale(ghm) ~ dead +
             s(doy, by = dead, bs = "cc") +
             s(animals_id_unique, bs = "re"), 
           data = data_species)
  
  p <- tidymv::plot_smooths(m, doy, dead) +
    scale_fill_manual(values = c("#FA9500", "#3685B5"), name = NULL, labels = c("Alive", "Dead")) +
    scale_color_manual(values = c("#FA9500", "#3685B5"), name = NULL, labels = c("Alive", "Dead")) +
    scale_linetype_manual(values = c(1, 1), name = NULL, labels = c("Alive", "Dead")) +
    geom_rug(data = animals_species, aes(x = death_date_doy, y = NULL)) +
    theme_minimal() +
    labs(title = str_to_title(species_common_name),
         subtitle = paste0(n_alive, " alive individuals; ", n_dead, " dead individuals"),
         x = "Day of Year",
         y = "Global Human Modification")
  
  ggsave(p, file = here("out",paste0("gam-summary-",snakecase::to_snake_case(species_common_name),".png")))
  return(p)
}


animals %>%
  distinct(scientific_name)

p_lynx <- species_gam("Lynx lynx")
p_red_deer <- species_gam("Cervus elaphus")
p_roe_deer <- species_gam("Capreolus capreolus")
p_wild_boar <- species_gam("Sus scrofa")
p_wildcat <- species_gam("Felis silvestris")



#bam(ghm ~ dead + s(doy, bs = "cc") + s(day_from_death, by = dead), data = data_species)

#..........................plot summary..........................



