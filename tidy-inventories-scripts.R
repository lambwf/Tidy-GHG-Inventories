
get_latest_version <- function(files) {
  
  latest_file <- data.frame(file=files) %>% 
    filter(grepl("^data_crts_v.*\\.RData$",file)) %>% 
    mutate(version = str_extract(file, "(?<=data_crts_v)[0-9.]+")) %>%
    separate(version, c("major","minor"), "\\.", fill="right") %>%
    mutate(across(c(major,minor), ~as.integer(replace_na(.x,"0")))) %>%
    arrange(desc(major), desc(minor)) %>%
    slice(1)
  
  return(paste0("data/",latest_file$file))
}
