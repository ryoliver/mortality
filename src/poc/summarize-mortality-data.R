rm(list = ls())
library(tidyverse)
library(data.table)
library(here)
library(patchwork)
library(ggpubr)


#---- Load data ----#

### read in animal and GPS data ###

print("reading in data...")

# find most recent cleaned data
animals_files <- data.frame(file_name = list.files(here::here("analysis"), "animals_clean_*")) %>%
  arrange(desc(file_name)) %>%
  slice(1)

gps_files <- data.frame(file_name = list.files(here::here("analysis"), "gps_*")) %>%
  arrange(desc(file_name)) %>%
  slice(1)

print(paste0("cleaned animal metadata: ", animals_files$file_name))
print(paste0("cleaned GPS data: ", gps_files$file_name))

# load data
animals <- fread(here::here("analysis", animals_files$file_name)) %>%
  mutate(death_date = lubridate::date(death_date))

mortality_codes <- read_delim(here::here("data","lu_mortality_new.txt"))

n_alive <- animals %>%
  filter(mortality_code_new == 0) %>%
  nrow()

n_dead <- animals %>%
  filter(mortality_code_new > 0) %>%
  nrow()

n_dead_death_date <- animals %>%
  filter(mortality_code_new > 0) %>%
  filter(!is.na(death_date)) %>%
  nrow()

n_dead_no_date <- animals %>%
  filter(mortality_code_new > 0) %>%
  filter(is.na(death_date)) %>%
  nrow()

summary_individuals <- data.frame(status = c("alive",
                 "dead",
                 "dead with date",
                 "dead without date",
                 "total"),
           n_individuals = c(n_alive, n_dead, n_dead_death_date, n_dead_no_date, nrow(animals)))



p1 <- ggplot(data = animals) +
  facet_wrap(~ common_name, ncol = 1, scales = "free_y") +
  geom_bar(aes(x = as.character(mortality_code_new_level1),
               fill = as.character(mortality_code_new_level1))) +
  scale_fill_manual(values = c("#020887","#647AA3","#7A9CC6",
                               "#B3D2B2","#BDE4A7","#FFFD98",
                               "#FFE19C","#FEB95F","#FFD0C2","#FF784F"
                               )) +
  theme(legend.position = "none") +
  labs(x = "", y = "Individuals (n)",
       title = "All individuals")

p2 <- ggplot(data = subset(animals, !is.na(death_date))) +
  facet_wrap(~ common_name, ncol = 1, scales = "free_y") +
  geom_bar(aes(x = as.character(mortality_code_new_level1),
               fill = as.character(mortality_code_new_level1))) +
  scale_fill_manual(values = c("#647AA3","#7A9CC6",
                               "#B3D2B2","#BDE4A7","#FFFD98",
                               "#FFE19C","#FEB95F","#FFD0C2","#FF784F"
  )) +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  labs(x = "", y = "Individuals (n)",
       title = "Individuals with death date")

p3 <- ggplot(data = subset(animals, mortality_code_new_level1 > 0 &
                       is.na(death_date))) +
  facet_wrap(~ common_name, ncol = 1, scales = "free_y") +
  geom_bar(aes(x = as.character(mortality_code_new_level1),
               fill = as.character(mortality_code_new_level1))) +
  scale_fill_manual(values = c("#647AA3","#7A9CC6",
                               "#B3D2B2","#BDE4A7",
                               "#FFE19C","#FEB95F","#FFD0C2","#FF784F"
  )) +
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  labs(x = "", 
       title = "Individuals without death date")

mortality_codes_table <- mortality_codes %>%
  filter(mortality_code <= 9) %>%
  select(mortality_code, mortality_name)

mortality_codes_table[mortality_codes_table$mortality_code == 9,]$mortality_name <- "unknown death"

t1 <- ggtexttable(summary_individuals, rows = NULL, 
            cols = c("status", "individuals (n)"),
            theme = ttheme("blank")) %>%
  tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>%
  tab_add_hline(at.row = nrow(summary_individuals), 
                row.side = "bottom", linewidth = 3, linetype = 1)

t2 <- ggtexttable(mortality_codes_table,
              rows = NULL, cols = c("code", "mortality status"),
              theme = ttheme("blank")) %>%
    tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2) %>%
    tab_add_hline(at.row = nrow(mortality_codes_table) + 1, 
                  row.side = "bottom", linewidth = 3, linetype = 1)
  
p <- p1 | p2 | p3 | (t1 / t2) 


ggsave(here::here("out","summarize-mortality.pdf"), p,
       width = 12, height = 8)



gps <- fread(here::here("analysis", gps_files$file_name)) %>%
  mutate(acquisition_time = as.POSIXct(acquisition_time))





# animals with known death date
animals_dead_withdate <- animals %>%
  filter(!is.na(death_date)) 

animals_dead_withdate %>%
  filter(mortality_code_new == "0")

ggplot() +
  facet_wrap(~ common_name) +
  geom_bar(data = animals_dead_withdate,
                 aes(x = as.character(mortality_code_new_level1)))



animals_dead_withdate %>%
  filter(mortality_code_new_level1 == "N")

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
  mutate(doy_relative = max(doy, na.rm = T) - doy) %>%
  ungroup()


ggplot(test) +
  facet_wrap(~common_name, scales = "free") +
  geom_histogram(aes(x = sl, fill = status),
                 position = "identity",
                 alpha = 0.5) +
  scale_x_log10()

ggplot(test) +
  facet_wrap(~common_name, scales = "free") +
  geom_point(aes(x = doy_relative, y = sl,
                colour = status),
            alpha = 0.5) +
  scale_y_log10()
  
