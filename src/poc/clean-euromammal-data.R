rm(list = ls())
library(tidyverse)
library(data.table)
library(here)
library(geosphere)

#---- Load data ----#

### read in animal metadata ###

print("reading in animal metadata...")

print("...lynx")
animals_lynx <- fread(here::here("data", "euromammal-data","lynx_animals.csv")) %>%
  mutate(mortality_code_new = rep(NA, n())) %>%
  mutate(mortality_code_new = case_when(mortality_code == "0" ~ "0",
                                        mortality_code == "1" ~ "11",
                                        mortality_code == "2" ~ "12",
                                        mortality_code == "3" ~ "1",
                                        mortality_code == "4" ~ "31",
                                        mortality_code == "5" ~ "2",
                                        mortality_code == "6" ~ "7",
                                        mortality_code == "7" ~ "9",
                                        mortality_code == "8" ~ "25",
                                        mortality_code == "9" ~ "13",
                                        mortality_code == "10" ~ "91",
                                        mortality_code == "11" ~ "21",
                                        mortality_code == "12" ~ "9",
                                        mortality_code == "13" ~ "51",
                                        mortality_code == "14" ~ "52",
                                        mortality_code == "15" ~ "53",
                                        mortality_code == "16" ~ "5",
                                        mortality_code == "21" ~ "6102",
                                        mortality_code == "22" ~ "6103",
                                        mortality_code == "23" ~ "6104",
                                        mortality_code == "24" ~ "6105",
                                        mortality_code == "26" ~ "6",
                                        mortality_code == "31" ~ "81",
                                        mortality_code == "32" ~ "82",
                                        mortality_code == "33" ~ "83",
                                        mortality_code == "34" ~ "84",
                                        mortality_code == "35" ~ "8",
                                        mortality_code == "40" ~ "35"))

print("...red deer")
animals_red_deer <- fread(here::here("data","euromammal-data","reddeer_animals.csv"))

print("...roe deer")
animals_roe_deer <- fread(here::here("data","euromammal-data","roe_animals.csv"))

print("...slovenian roe deer")
animals_roe_deer_slovenia <- fread(here::here("data","euromammal-data", "Jelovica_Slovenia_ROEDEER_data","Animals_Jelovica_Slovenia.txt"))

print("...wild boar")
animals_wild_boar <- fread(here::here("data","euromammal-data","wildboar_animals.csv")) %>%
  mutate(mortality_code_new = rep(NA, n())) %>%
  mutate(mortality_code_new = case_when(mortality_code == "0" ~ "0",
                                        mortality_code == "1" ~ "61",
                                        mortality_code == "2" ~ "11",
                                        mortality_code == "3" ~ "12",
                                        mortality_code == "4" ~ "1",
                                        mortality_code == "5" ~ "31",
                                        mortality_code == "6" ~ "2",
                                        mortality_code == "7" ~ "7",
                                        mortality_code == "8" ~ "9",
                                        mortality_code == "9" ~ "8",
                                        mortality_code == "10" ~ "25",
                                        mortality_code == "11" ~ "13",
                                        mortality_code == "12" ~ "54",
                                        mortality_code == "13" ~ "91",
                                        mortality_code == "14" ~ "9",
                                        mortality_code == "15" ~ "5",
                                        mortality_code == "21" ~ "6101",
                                        mortality_code == "22" ~ "6102",
                                        mortality_code == "23" ~ "6103",
                                        mortality_code == "24" ~ "6104",
                                        mortality_code == "25" ~ "6105",
                                        mortality_code == "26" ~ "6106"))

print("...wild cat")
animals_wildcat <- fread(here::here("data","euromammal-data","wildcat_animals.csv")) %>%
  mutate(mortality_code_new = rep(NA, n())) %>%
  mutate(mortality_code_new = case_when(mortality_code == "0" ~ "0",
                                        mortality_code != "0" ~ "9"))


### read in GPS data ###
print("reading in GPS data...")

print("...lynx")
gps_lynx <- fread(here::here("data","euromammal-data","lynx_gps.csv")) 

print("...red deer")
gps_red_deer <- fread(here::here("data","euromammal-data","reddeer_gps.csv")) 

print("...roe deer")
gps_roe_deer <- fread(here::here("data","euromammal-data","roe_gps.csv"))

print("...slovenian roe deer")
suppressWarnings(
gps_roe_deer_slovenia <- fread(here::here("data","euromammal-data", "Jelovica_Slovenia_ROEDEER_data","GPS_Jelovica_Slovenia.txt")) %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))
)

print("...wild boar")
gps_wild_boar <- fread(here::here("data","euromammal-data","wildboar_gps.csv")) 

print("...wild cat")
gps_wildcat <- fread(here::here("data","euromammal-data","wildcat_gps.csv")) 

### species info ###
# - scientific name
# - common name
# - species prefix to append to animal IDs

species_info <- data.frame(scientific_name = c("Lynx lynx",
                                               "Cervus elaphus",
                                               "Capreolus capreolus",
                                               "Sus scrofa",
                                               "Felis silvestris"),
                           common_name = c("lynx",
                                           "red deer",
                                           "roe deer",
                                           "wild boar",
                                           "wildcat"),
                           prefix = c("LX","RE","RO","WB","WC"))

#---- Perform analysis ----#

### function to clean animal metadata to consistent format ###
#   - select matching columns

clean_animals_data <- function(animals_data, species_common_name){
  
  sp <- species_info %>%
    filter(common_name == species_common_name)
  
  scientific_name <- sp$scientific_name
  prefix <- sp$prefix
  
  animals_data_clean <- animals_data %>%
    select(animals_id, 
           sex, 
           year_birth, 
           year_birth_exact,
           mortality_code,
           mortality_code_new,
           death_date,
           death_time) %>%
    unite(death_date, death_date, death_time) %>%
    mutate(scientific_name = rep(scientific_name, n()),
           common_name = rep(species_common_name, n()),
           animals_id_prefix = rep(prefix, n()),
           death_date = as_date(death_date)) %>%
    unite(animals_id_unique, animals_id_prefix, animals_id, sep = "-", remove = FALSE)
  
  return(animals_data_clean)
}

### function to clean GPS to consistent format ###
#   - remove missed fixes
#   - remove inaccurate fixes (DOP < 5)

clean_gps_data <- function(gps_data, species_common_name){
  
  sp <- species_info %>%
    filter(common_name == species_common_name)
  
  scientific_name <- sp$scientific_name
  prefix <- sp$prefix
  
  gps_clean <- gps_data %>%
    filter(dop < 5) %>%
    # filter NAs and error values
    filter(!is.na(latitude)) %>% 
    filter(!is.na(longitude)) %>%
    filter(abs(longitude) < 180) %>%
    filter(abs(latitude) < 90) %>%
    select(animals_id,
           latitude, 
           longitude,
           acquisition_time, 
           gps_validity_code,
           dop) %>%
    # add unique animal ID
    mutate(scientific_name = rep(scientific_name, n()),
           common_name = rep(species_common_name, n()),
           animals_id_prefix = rep(prefix, n())) %>%
    unite(animals_id_unique, animals_id_prefix, animals_id, sep = "-", remove = FALSE) %>%
    # find step length, turning angle, and bearing
    arrange(animals_id_unique, acquisition_time) %>%
    group_by(animals_id_unique) %>%
    mutate(lag_longitude = dplyr::lag(longitude, 1),
           lag_latitude = dplyr::lag(latitude, 1),
           sl = distGeo(cbind(longitude,latitude), cbind(lag_longitude, lag_latitude)),
           bearing = bearing(cbind(longitude,latitude), cbind(lag_longitude, lag_latitude)),
           ta = 180-abs(180 - abs(bearing - dplyr::lag(bearing, 1)) %% 360))
  
  # calculate quantile-based cutoffs
  cuts <- gps_clean %>% 
    group_by(animals_id_unique) %>% 
    summarize(
      qta = quantile(ta, probs = 0.95, na.rm = T, names = F),
      qsl = quantile(sl, probs = 0.95, na.rm = T, names = F)) 
  
  # filter outliers
  gps_clean <- gps_clean %>% 
    # join the cutpoints back to the dataset
    left_join(., cuts, by = "animals_id_unique") %>% 
    # conservative outlier threshold, must be past 95% quant for either sl and ta
    filter(sl < qsl & ta < qta) %>% 
    ungroup()
  
  return(gps_clean)
}


### clean animal metadata to consistent format ###

print("cleaning animal metadata...")

suppressWarnings(
animals_clean_lynx <- clean_animals_data(animals_lynx, "lynx")
)
suppressWarnings(
animals_clean_red_deer <- clean_animals_data(animals_red_deer, "red deer")
)
suppressWarnings(
animals_clean_roe_deer <- clean_animals_data(animals_roe_deer, "roe deer")
)
suppressWarnings(
animals_clean_roe_deer_slovenia <- clean_animals_data(animals_roe_deer_slovenia, "roe deer")
)
suppressWarnings(
animals_clean_wild_boar <- clean_animals_data(animals_wild_boar, "wild boar")
)
suppressWarnings(
animals_clean_wildcat <- clean_animals_data(animals_wildcat, "wildcat")
)

if (sum(animals_clean_roe_deer_slovenia$animals_id_unique %in% animals_clean_roe_deer$animals_id_unique) != 0) {
  stop("non-unique animal IDs for roe deer")
}


animals_clean <- rbind(animals_clean_lynx,
                       animals_clean_red_deer,
                       animals_clean_roe_deer,
                       animals_clean_roe_deer_slovenia,
                       animals_clean_wild_boar,
                       animals_clean_wildcat) %>%
  mutate(mortality_code_new_level1 = substr(mortality_code_new, 1,1),
         mortality_code_new_level2 = substr(mortality_code_new, 2,2),
         mortality_code_new_level3 = substr(mortality_code_new, 3,3),
         mortality_code_new_level4 = substr(mortality_code_new, 4,4),
         mortality_code_new_n_levels = str_length(mortality_code_new)) %>%
  select(-mortality_code)

### clean GPS to consistent format ###
print("cleaning GPS data...")

gps_clean_lynx <- clean_gps_data(gps_lynx, "lynx")
gps_clean_red_deer <- clean_gps_data(gps_red_deer, "red deer") 
gps_clean_roe_deer <- clean_gps_data(gps_roe_deer, "roe deer")
gps_clean_roe_deer_slovenia <- clean_gps_data(gps_roe_deer_slovenia, "roe deer")
gps_clean_wild_boar <- clean_gps_data(gps_wild_boar, "wild boar")
gps_clean_wildcat <- clean_gps_data(gps_wildcat, "wildcat")

gps_clean <- rbind(gps_clean_lynx,
                   gps_clean_red_deer,
                   gps_clean_roe_deer,
                   gps_clean_roe_deer_slovenia,
                   gps_clean_wild_boar,
                   gps_clean_wildcat)

# remove any GPS data without animal metadata
gps_clean <- gps_clean %>%
  filter(animals_id_unique %in% animals_clean$animals_id_unique)

### clear variables
rm(gps_lynx,
   gps_red_deer,
   gps_roe_deer,
   gps_roe_deer_slovenia,
   gps_wild_boar,
   gps_wildcat,
   animals_lynx,
   animals_red_deer,
   animals_roe_deer,
   animals_roe_deer_slovenia,
   animals_wild_boar,
   animals_wildcat)

rm(gps_clean_lynx,
   gps_clean_red_deer,
   gps_clean_roe_deer,
   gps_clean_roe_deer_slovenia,
   gps_clean_wild_boar,
   gps_clean_wildcat,
   animals_clean_lynx,
   animals_clean_red_deer,
   animals_clean_roe_deer,
   animals_clean_roe_deer_slovenia,
   animals_clean_wild_boar,
   animals_clean_wildcat)

### add date of final GPS location to animal metadata

# find date of final GPS location
gps_final_location <- gps_clean %>%
  arrange(animals_id_unique, desc(acquisition_time)) %>%
  group_by(animals_id_unique) %>%
  slice_head() %>%
  select(animals_id_unique, acquisition_time) %>%
  rename(final_gps_location_datetime = acquisition_time) %>%
  mutate(final_gps_location_date = date(final_gps_location_datetime))

# link date of final GPS location to animal metadata
animals_clean <- animals_clean %>%
  filter(animals_id_unique %in% gps_clean$animals_id_unique) %>%
  left_join(., gps_final_location, by = "animals_id_unique") %>%
  # find gap between final GPS location and death date
  mutate(gap_death_gps_date = difftime(death_date, final_gps_location_date, units = "days")) %>%
  select(animals_id_unique, scientific_name, common_name,
         sex, year_birth, year_birth_exact,
         mortality_code_new, mortality_code_new_level1, 
         mortality_code_new_level2, mortality_code_new_level3,
         mortality_code_new_level4, mortality_code_new_n_levels,
         death_date, final_gps_location_datetime, final_gps_location_date, gap_death_gps_date)

### filter animals with possible errors in mortality information

# find animals with mortality code 0 (alive), but with a death date
animals_filtered <- animals_clean %>%
  filter(mortality_code_new == "0") %>%
  filter(!is.na(death_date))
  
fwrite(animals_filtered, here::here("analysis",paste0("animals_filtered_",Sys.Date(),".csv")))

animals_clean <- animals_clean %>%
  filter(!animals_id_unique %in% animals_filtered$animals_id_unique)

gps_clean <- gps_clean %>%
  filter(!animals_id_unique %in% animals_filtered$animals_id_unique)
  

#---- Save output ---#

### checks ###

# check that all rows in animal metadata represent unique animals
if(n_distinct(animals_clean$animals_id_unique) == nrow(animals_clean)) {
  print("animal metadata: all rows unique")
} else{
  stop("animal metadata: rows not unique")
}

# check that all GPS data has animal metadata
if(length(setdiff(gps_clean$animals_id_unique, animals_clean$animals_id_unique)) == 0){
  print("gps data: no animals missing metadata")
} else{
  stop(paste0("gps data: ",
               length(setdiff(gps_clean$animals_id_unique, animals_clean$animals_id_unique)),
               " missing metadata"))
}

### write out cleaned data ##

print("writing out cleaned data...")

print(paste0(round(nrow(gps_clean)/1000000,2), " million locations"))

fwrite(animals_clean, here::here("analysis",paste0("animals_clean_",Sys.Date(),".csv")))
fwrite(gps_clean, here::here("analysis",paste0("gps_clean_",Sys.Date(),".csv")))

#---- Finalize script ----#
print("done!")
