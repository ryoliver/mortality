select_file <- function(file_path, data_type) {
  
  if (data_type %in% c("gps", "animals")){
    
    file_name <- data.frame(file_name = list.files(file_path)) %>%
      separate(file_name, into = c("type", NA, "date"), sep = "_", remove = FALSE) %>%
      mutate(date = str_remove(date, pattern = ".csv")) %>%
      mutate(date = lubridate::ymd(date)) %>%
      filter(type == data_type) %>%
      arrange(desc(date)) %>%
      slice_head(n = 1)
    
    file_name <- file_name[1]$file_name
    return(file_name)
    
  } else {
    stop("data type should be 'animals' or 'gps'")
  }
  
}