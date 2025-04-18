rm(list=ls())
library(openxlsx)
library(tidyverse)
library(countrycode)

folders <- data.frame(dir=list.files(path = "sources/CRTs/"))

## comment these lines out to read all folders, instead of new ones (takes long)
load("data/data_crt_files.RData")
excluded_folders <- files_crts %>% select(dir) %>% distinct() %>% mutate(exclude=1)
folders <- folders %>% 
  left_join(.,excluded_folders,by="dir") %>% 
  filter(is.na(exclude)) %>% 
  select(-exclude)
##


#files_crts = data.frame(dir=NA,files=NA,country=NA,file_year=NA)

for (i in 1:length(folders$dir)) {
  
  files <- data.frame(dir=folders$dir[i],
                      files=list.files(path = paste0("sources/CRTs/",folders$dir[i])),
                      file_year=NA,
                      country=NA,
                      iso=NA)
  
  for (j in 1:length(files$files)) {
    
    file_info <- read.xlsx(xlsxFile = paste0("sources/CRTs/",folders$dir[i],"/",files$files[j]),
                           sheet = paste0("Table1"),rows = 1:3,colNames = FALSE)
    
    files$file_year[j] <- str_extract(file_info$X2[1], "\\d{4}")
    files$country[j] <- file_info$X2[3]
    
  }
  
  files <- files %>% mutate(iso=countrycode(country,"country.name","iso3c"))
  files_crts <- rbind(files_crts,files)
  
}

files_crts <- files_crts %>% 
  filter(!is.na(dir)) %>% 
  mutate(country=countrycode(iso,"iso3c","country.name"))

save(files_crts,file="data/data_crt_files.RData")


