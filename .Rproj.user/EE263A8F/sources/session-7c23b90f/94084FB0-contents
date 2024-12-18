library(openxlsx)
library(tidyverse)
library(countrycode)


folders <- data.frame(dir=list.files(path = "sources/CRTs/"))
files_crts = data.frame(dir=NA,files=NA,country=NA,file_year=NA)

## this takes long

for (i in 1:length(folders$dir)) {
  
  files <- data.frame(files=list.files(path = paste0("sources/CRTs/",folders$dir[i])),file_year=NA,country=NA) %>% 
    mutate(dir=folders$dir[i])
  
  for (j in 1:length(files$files)) {
    
    file_info <- read.xlsx(xlsxFile = paste0("sources/CRTs/",folders$dir[i],"/",files$files[j]),
                           sheet = paste0("Summary1"),rows = 1:3,colNames = FALSE)
    
    files$file_year[j] <- str_extract(file_info$X2[1], "\\d{4}")
    files$country[j] <- file_info$X2[3]
    
  }
  
  files_crts <- rbind(files_crts,files)
  
}

files_crts <- files_crts %>% 
  filter(!is.na(dir)) %>% 
  mutate(iso=countrycode(country,"country.name","iso3c")) %>% 
  mutate(country=countrycode(iso,"iso3c","country.name")) %>% 
  select(dir,files,file_year,country,iso)

save(files_crts,file="data/data_crt_files.RData")
