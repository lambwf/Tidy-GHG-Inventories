rm(list=ls())
library(openxlsx)
library(tidyverse)
load("data/data_crt_files.RData")

## the inventories have different sector aggregations in different places
## this is an attempt to create a single consistent hierarchy to be used across the Tidy GHG Inventories activity dataset

## we will use Tables 1:6 in the USA CRT to start with
files_crts <- files_crts %>% 
  filter(iso=="USA") %>% #
  filter(file_year==2022)


table_1 <- read.xlsx(xlsxFile = paste0("sources/CRTs/",files_crts$dir[1],"/",files_crts$files[1]),
                     sheet = paste0("Table1"),
                     startRow = 8) %>% 
  select(1)


table_2 <- read.xlsx(xlsxFile = paste0("sources/CRTs/",files_crts$dir[1],"/",files_crts$files[1]),
                     sheet = paste0("Table2(I)"),
                     startRow = 8) %>%
  select(1)


table_3 <- read.xlsx(xlsxFile = paste0("sources/CRTs/",files_crts$dir[1],"/",files_crts$files[1]),
                     sheet = paste0("Table3"),
                     startRow = 8) %>%
  select(1)


table_4 <- read.xlsx(xlsxFile = paste0("sources/CRTs/",files_crts$dir[1],"/",files_crts$files[1]),
                     sheet = paste0("Table4"),
                     startRow = 8) %>% 
  select(1)


table_5 <- read.xlsx(xlsxFile = paste0("sources/CRTs/",files_crts$dir[1],"/",files_crts$files[1]),
                     sheet = paste0("Table5"),
                     startRow = 8) %>% 
  select(1)

names(table_1) <- "sector"
names(table_2) <- "sector"
names(table_3) <- "sector"
names(table_4) <- "sector"
names(table_5) <- "sector"

table <- bind_rows(table_1,table_2,table_3,table_4,table_5)


## extract sector codes
table <- table %>% 
  mutate(sector=ifelse(sector=="Total Energy","1. Energy",sector)) %>% 
  mutate(sector=ifelse(sector=="2. Total industrial processes","2. Industrial process and product use",sector)) %>% 
  mutate(sector=ifelse(sector=="3. Total agriculture","3. Agriculture",sector)) %>% 
  mutate(sector=ifelse(sector=="4. Total LULUCF","4. Land use, land use change and forestry",sector)) %>% 
  mutate(sector=ifelse(sector=="5. Total waste ","5. Waste",sector)) %>% 
  add_row(sector="5.F. Memo items") %>% 
  mutate(sector=ifelse(sector=="1.D.1.b.Navigation","1.D.1.b. Navigation",sector)) %>% 
  mutate(sector_code=sub("^(\\S+)\\s.*", "\\1", sector)) %>% 
  mutate(sector=sub("^\\S+\\s(.*)", "\\1", sector)) %>% 
  arrange(sector_code)


## remove non-sector rows
table <- table %>% 
  filter(str_detect(sector_code, "^[0-9]")) %>% 
  filter(sector_code!="2.A.3:")


## remove spaces at the beginning of categories, rename memo items
table$sector <- gsub("^\\s+", "", table$sector)
table$sector <- gsub("Memo items:","Memo items",table$sector)


## extract hierarchy
table <- table %>%
  mutate(sector_level = map_int(sector_code, ~ sum(str_split(.x, "\\.")[[1]] != ""))) %>% 
  select(sector_code,sector_description=sector,sector_level)


## remove trailing spaces in sector descriptions, text within round brackets
table$sector_code <- gsub("\\s+$", "", table$sector_code) 
table$sector_description <- gsub("\\(.*?\\)", "", table$sector_description)
table$sector_description <- gsub("\\s+$", "", table$sector_description) 
table$sector_description[table$sector_description=="Land use, land-use change and forestry "] <-
  "Land use, land-use change and forestry"


## propagate sector_lv1
table <- table %>% 
  mutate(match = substr(sector_code,1,1)) %>% 
  left_join(.,table %>% 
              filter(sector_level==1) %>%
              mutate(match=substr(sector_code,1,1)) %>% 
              select(match,sector_lv1=sector_description),
            by = join_by(match)) %>% 
  select(-match)


## propagate sector_lv2
table <- table %>% 
  mutate(match = substr(sector_code,1,3)) %>% 
  left_join(.,table %>% 
              filter(sector_level==2) %>%
              mutate(match=substr(sector_code,1,3)) %>% 
              select(match,sector_lv2=sector_description),
            by = join_by(match)) %>% 
  select(-match)


## propagate sector_lv3
table <- table %>% 
  mutate(match = substr(sector_code,1,6)) %>% 
  left_join(.,table %>% 
              filter(sector_level==3) %>%
              mutate(match=substr(sector_code,1,6)) %>% 
              select(match,sector_lv3=sector_description),
            by = join_by(match)) %>% 
  select(-match)


# customise levels and reduce some detail
table <- table %>%
  filter(!sector_code %in% c("2.F.1.a","2.F.1.b","2.F.2.a","2.F.2.b","2.F.3","2.F.1.d")) %>% 
  filter(!sector_code %in% c("3.A.1.a.","3.A.1.a.iv.","3.B.1.a.","3.B.1.a.iv.")) %>% 
  filter(!sector_code %in% c("3.D.1.a.","3.D.1.b.","3.D.1.c.","3.D.1.d.","3.D.1.e.","3.D.1.f.","3.D.1.g.")) %>% 
  filter(!(sector_lv2=="Agricultural soils" & sector_level==3)) %>% 
  filter(!(sector_lv1=="Waste" & sector_level==3)) %>% 
  filter(!(sector_lv1=="Land use, land use change and forestry" & sector_level==3)) %>% 
  filter(!(sector_lv1=="Energy" & sector_lv3=="Other" & sector_level==4)) %>% 
  filter(!(sector_lv1=="Industrial process and product use" & sector_lv2=="Other" & sector_level==3))



## identify leaf nodes
table <- table %>%
  mutate(is_leaf = !sapply(sector_code, function(code) {
    any(str_detect(setdiff(sector_code, code), paste0("^", str_replace_all(code, "\\.", "\\\\."))))
  }))


## propagate sector levels 3 (if not already there) and 4 
table <- table %>% 
  mutate(sector_lv3=ifelse(is_leaf==TRUE & is.na(sector_lv3),sector_description,sector_lv3)) %>% 
  mutate(sector_lv4=ifelse(is_leaf=="TRUE",sector_description,NA)) 


## make a summary column showing the hierarchy
table <- table %>% 
  mutate(sector=paste0(sector_code," ",sector_description)) %>% 
  mutate(sector=ifelse(sector_level>1,paste0("   ",sector),sector)) %>% 
  mutate(sector=ifelse(sector_level>2,paste0("   ",sector),sector)) %>% 
  mutate(sector=ifelse(sector_level>3,paste0("   ",sector),sector)) %>% 
  mutate(sector=ifelse(sector_level>4,paste0("   ",sector),sector))


## tidy up
table <- table %>% 
  select(sector,sector_code,sector_level,sector_description,everything())

## remove memo items etc.
# table <- table %>% 
#   filter(!sector_code %in% c("Memo","Indirect","Total")) %>% 
#   filter(!grepl("1.D.",sector_code)) %>% 
#   filter(!grepl("5.F.1.",sector_code))


## save to a workbook and R file
cc_sectors <- table
save(cc_sectors,file="data/cc_sectors.RData")
wb <- createWorkbook()
addWorksheet(wb,"sectors")
writeData(wb, sheet = "sectors", table, colNames = T, rowNames = F)
setColWidths(wb, "sectors", cols = 1:ncol(table), widths = "auto")
for (row in which(table$is_leaf == FALSE)) {
  addStyle(wb, "sectors", createStyle(textDecoration = "bold"), rows = row + 1, cols=which(names(table)=="sector"), gridExpand = TRUE)
}
saveWorkbook(wb,"data/cc_inventory_sectors.xlsx",overwrite=T)
