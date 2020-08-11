#' ---
#' author: Kyle Belanger
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#' 
#' ---

#+ include = FALSE
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 


# ---- knitr-opts --------------------------------------------------------------
#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
knitr::opts_knit$set(root.dir = "../")

# ---- load-sources ------------------------------------------------------------

#' # Load Packages
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: 
# http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

#' # Declare Globals
# ---- declare-globals ---------------------------------------------------------

folder <- "./data-unshared/raw/us-physical-inactivity-data"

import_data <- function(folder){
  files <- list.files(folder,full.names = TRUE)
  output_list <- list()
  for(item_i in seq_along(files)){
    item_path <- files[item_i]
    item_name <- item_path %>% basename() %>% 
      stringr::str_replace(".csv","") %>% tolower() 
    item_year <- item_name %>% str_sub(-4)
    
    d_raw <- readr::read_csv(item_path,col_names = TRUE, skip = 2) 
    
    d <- d_raw %>% 
      drop_na() %>% 
      janitor::clean_names() %>% 
      filter(!str_detect(state,"Puerto Rico")) %>% 
      mutate(across(c("percentage","lower_limit","upper_limit"),as.numeric)
             ,across(where(is.character),tolower)
             ,year = as.integer(item_year)) %>% 
      rename_with(.cols = c("percentage","lower_limit","upper_limit")
                  ,.fn = ~paste0("inactivity_",.)
                  ) %>% 
      relocate(year, .after = state)
      
    output_list[[item_name]] <- d
  }
  return(output_list)
}

#' # Load Data
# ---- load-data --------------------------------------------------------------

ds_inactivity_raw <- import_data(folder)

#' # Merge Data
# ---- merge-data -------------------------------------------------------------

ds_inactivity <- bind_rows(ds_obesity_raw) %>% 
  select(-county, -state)

#' # Save to Disk
# ---- save-data --------------------------------------------------------------

ds_inactivity %>% write_rds(
  "./data-public/derived/us-inactivity-data.rds"
  ,compress = 'gz'
  )
# ds_obesity %>% write_csv("./data-public/derived/us-obesity-data.csv")
