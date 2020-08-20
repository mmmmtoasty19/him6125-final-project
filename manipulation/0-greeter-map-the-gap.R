#' ---
#' author: Kyle Belanger
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#' 
#' ---

#+ include = FALSE
# These first few lines run only when the file is run in RStudio, 
# !!NOT when an Rmd/Rnw file calls it!!
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

# ---- declare-globals ---------------------------------------------------------

folder <- "./data-unshared/raw/map_the_gap"

#' # Load Data
# ---- load-data ---------------------------------------------------------------

files <- list.files(folder, full.names = TRUE)

ds_list <- list()

for(item_i in seq_along(files)){
  
  item_path <- files[item_i]
  item_name <- item_path %>% basename() %>% str_remove_all(".xlsx")
  
  
  d_raw <- item_path %>% readxl::read_xlsx(sheet = 1)
  
  ds_list[[item_name]] <- d_raw
  
}


  