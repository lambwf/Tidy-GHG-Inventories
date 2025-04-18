rm(list=ls())
library(rvest)
library(httr)
library(countrycode)
library(tidyverse)

# Function to fetch and parse the webpage
fetch_webpage <- function(url) {
  tryCatch({
    page <- GET(url)
    if (status_code(page) == 200) {
      content <- read_html(page)
      return(content)
    } else {
      stop("Failed to fetch the webpage. HTTP status code: ", status_code(page))
    }
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })
}

# Function to extract links with title "CRT"
extract_crt_links <- function(webpage) {
  tryCatch({
    # Select anchor (<a>) elements where title contains "CRT"
    elements <- webpage %>%
      html_elements("a[title*='CRT']")  # Matches any <a> tag with 'CRT' in the title
    
    # Extract href (link) and title
    links <- elements %>% html_attr("href")
    titles <- elements %>% html_attr("title")
    
    if (length(links) == 0) {
      message("No CRT links found on the page.")
      return(NULL)
    } else {
      # Prepend the base URL to form complete links
      base_url <- "https://unfccc.int"
      full_links <- sapply(links, function(link) {
        if (startsWith(link, "/")) {
          paste0(base_url, link)
        } else {
          link
        }
      })
      
      # Extract country names (assume the first word(s) before the first period in the title)
      country_names <- sapply(titles, function(title) {
        strsplit(title, "\\.", fixed = FALSE)[[1]][1]
      })
      
      # Combine country names and links into a data frame
      result <- data.frame(country = country_names, link = full_links, stringsAsFactors = FALSE)
      return(result)
    }
  }, error = function(e) {
    message("Error during link extraction: ", e$message)
    return(NULL)
  })
}


## 
webpage <- fetch_webpage("https://unfccc.int/first-biennial-transparency-reports")
crt_links <- extract_crt_links(webpage)
crt_links <- crt_links %>% 
  mutate(iso=countrycode(country,'country.name','iso3c'))

load("data/data_crt_files.RData")

crt_links <- left_join(crt_links,files_crts %>% select(dir,iso) %>% distinct(),by="iso")
crt_links <- crt_links %>% filter(is.na(dir))
