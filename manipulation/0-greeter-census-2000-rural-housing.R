#' ---
#' author: Kyle Belanger
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#' 
#' ---

#+ include = FALSE
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 
knitr::opts_knit$set(root.dir = "../")
knitr::opts_chunk$set(warning = F, message = F)
# ---- load-sources ------------------------------------------------------------

#' # Load Packages
# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions 
# don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

# ---- declare-globals ---------------------------------------------------------

#' # Load Data
# ---- load-data ---------------------------------------------------------------

ds_rural_housing_raw <- read_csv(
  "./data-unshared/raw/2000-census-rural-urban-counts.csv"
  ,skip = 1
  ) 

#' # Tweak Data
# ---- tweak-data -------------------------------------------------------------

ds_rural_housing <- ds_rural_housing_raw %>% 
  janitor::clean_names() %>% 
  filter(!str_detect(label_for_geo_id,"Puerto Rico")) %>% 
  select(
    -total_urban_inside_urbanized_areas
    ,-total_urban_inside_urban_clusters
    ,-total_filler
    ,-label_for_geo_id
    ) %>% 
   mutate(
     across(id, ~str_sub(.,-5))
     ,pct_rural = round((total_rural/total)*100,2)
     ,rural     = case_when(
       pct_rural == 100 ~ "Rural"
       ,pct_rural >= 50 ~ "Mostly Rural"
       ,TRUE ~ "Mostly Urban"
       )
     ) %>% 
  select(
    id
    ,pct_rural  
    ,rural 
    ) %>% rename(
    county_fips = id
  )


#' # Save to Disk
# ---- save-to-disk -----------------------------------------------------------

ds_rural_housing %>% write_rds("./data-public/derived/percentage-rural-2000.rds"
                               ,compress = "gz")
ds_rural_housing %>% write_csv("./data-public/derived/percentage-rural-2000.csv")



