library(tidyverse)
library(here)

rm(list = ls())

#........................source functions........................

print("source functions...")

list.files(here("src", "funs","auto"), full.names = TRUE) %>%
  walk(source)

#..........................read in data..........................

print("read in data...")

# gps data
animals_file <- select_file(here("analysis", "data_cleaned"), "animals")
animals <- read_csv(here("analysis", "data_cleaned", animals_file))

animals_summary <- animals %>%
  mutate(status = case_when(mortality_code_new > 0 ~ "dead", 
                          mortality_code_new == 0 ~ "alive")) %>% 
  group_by(common_name, status) %>% 
  summarise(n_individual = n())


p <- ggplot(data = animals_summary, 
       aes(y = fct_reorder(common_name, n_individual), x = n_individual)) +
  geom_col(aes(fill = status)) +
  scale_fill_manual(values = c("#FA9500", "#3685B5"), name = NULL, labels = c("Alive", "Dead")) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.text = element_text(size = 10, color = "black")) +
  labs(x = "Individuals (n)")

ggsave(here("out","animals-summary.png"), p, height = 3, width = 6)
