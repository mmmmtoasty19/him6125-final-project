#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------

folder_path <- "./data-unshared/raw/cdc-wonder"


import_data <- function(folder){
  # browser()
  files <- list.files(folder,full.names = TRUE)
  dto <- list()
  
  for(item_i in seq_along(files)){
    item_name <- files[[item_i]] %>% basename() %>% str_remove(".txt") %>% 
      janitor::make_clean_names()
    item_year <- str_sub(item_name,-4)
    dto[[item_name]] <- read_tsv(files[[item_i]]) %>% 
      drop_na(Gender) %>%
      select(-Notes
             ,-`Age Group Code`
             ,-`Race Code`
             ,-`Gender Code`
             ,-`Ethnicity Code` 
      ) %>% 
      mutate(across(County, ~str_remove_all(.,"County, NC"))
             ,across(`Age Group`, ~str_remove_all(.,"years"))
             ,year = as.integer(item_year)) %>% 
      janitor::clean_names()
      
  }
  return(dto)
}


ds0 <- import_data(folder_path) %>%  bind_rows()


# ---- save-to-disk ----

ds0 %>% write_rds("./data-public/derived/cdc-wonder-agegroup.rds", compress = "gz")



# ---- single-file test ----

# file_path <- "./data-unshared/raw/cdc-wonder/Bridged-Race Population Estimates 2006.txt"
#   
# 
# ds0 <- read_tsv(paste(file_path))
# 
# ds1 <- ds0 %>% drop_na(Gender)
# 
# ds2 <- ds1 %>% select(-Notes
#                       ,-`Age Group Code`
#                       ,-`Race Code`
#                       ,-`Gender Code`
#                       ,-`Ethnicity Code` 
#                       )
# ds3 <- ds2 %>% 
#   mutate(across(County, ~str_remove_all(.,"County, NC"))
#          ,across(`Age Group`, ~str_remove_all(.,"years"))) %>% 
#   janitor::clean_names()
