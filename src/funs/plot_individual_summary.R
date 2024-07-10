plot_individual_summary <- function(animal_to_plot, gps_to_plot, filepath){
  
  animal_test <- animal_to_plot %>%
    mutate(death_time = "00:00:00") %>%
    unite(death_time, death_date, death_time, sep = " ") %>%
    mutate(death_time = as.POSIXct(death_time))
  
  gps_test <- gps_to_plot %>%
    filter(animals_id_unique == animal_test$animals_id_unique) %>%
    arrange(acquisition_time) %>%
    mutate(lag_longitude = dplyr::lag(longitude, 1),
           lag_latitude = dplyr::lag(latitude, 1),
           sl = distGeo(cbind(longitude,latitude), cbind(lag_longitude, lag_latitude)))
  
  date_difference <- animal_test$data_gap
  
  p1 <- ggplot() +
    geom_line(aes(x = c(animal_test$death_time, animal_test$death_time), 
                  y = c(min(gps_test$latitude),
                        max(gps_test$latitude))), 
              col = "#EF6F6C", lwd = 1) +
    geom_point(data = gps_test, aes(x = acquisition_time, y = latitude),
               col = "#7C73E8",alpha = 0.5) +
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
    labs(x = " ", y = "longitude")
  
  p3 <- ggplot() +
    geom_line(aes(x = c(animal_test$death_time, animal_test$death_time), 
                  y = c(min(gps_test$sl, na.rm = TRUE),
                        max(gps_test$sl, na.rm = TRUE))), 
              col = "#EF6F6C", lwd = 1) +
    geom_point(data = gps_test, aes(x = acquisition_time, y = sl),
               col = "#7C73E8",alpha = 0.5) +
    labs(x = " ", y = "step length")
  
  p <- p1 / p2 / p3
  
  
  ggsave(filepath, p)
}