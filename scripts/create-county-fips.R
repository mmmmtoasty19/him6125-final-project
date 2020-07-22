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

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: 
# http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------

geocodes_raw <- readxl::read_excel(
  "./data-unshared/raw/all-geocodes-v2017.xlsx", skip = 4)

state_fips_raw <- readxl::read_excel(
  "./data-unshared/raw/state_fips.xlsx"
)



# ---- tweak-data --------------------------------------------------------------

ds_fips <- geocodes_raw %>% 
  janitor::clean_names() %>% 
  filter(summary_level == "050") %>% 
  select(
    -county_subdivision_code_fips
    ,-place_code_fips
    ,-consolidtated_city_code_fips
    ,-summary_level
    ) %>% 
  left_join(state_fips_raw, by = c("state_code_fips" = "state_fips")) %>% 
  unite("county_fips", state_code_fips:county_code_fips, sep = "") %>% 
  rename(area_name = area_name_including_legal_statistical_area_description)

# ---- save-data ---------------------------------------------------------------

ds_fips %>% write_csv("./data-public/metadata/county_fips.csv")
