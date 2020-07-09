#' ---
#' author: Kyle Belanger
#' date: "`r format(Sys.Date(), '%m/%d/%Y')`"
#' 
#' ---

#+ include = FALSE
#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path

library(tidyverse)

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------

ds_rural_housing_raw <- read_csv(
  "./data-unshared/raw/2010-census-rural-urban-counts.csv"
  ,skip = 1
  ) 

state_names <- read_csv("./data-public/metadata/state-abb.csv") %>% 
  mutate(across(.fn = tolower))

# ---- tweak-data -------------------------------------------------------------

ds_rural_housing <- ds_rural_housing_raw %>% 
  janitor::clean_names() %>% 
  select(
    -id
    ,-total_urban_inside_urbanized_areas
    ,-total_urban_inside_urban_clusters
    ,-total_not_defined_for_this_file
    ) %>% 
  filter(!str_detect(geographic_area_name,"Puerto Rico")) %>% 
  separate(geographic_area_name
           ,into = c("county", "state")
           ,sep = ",") %>% 
   mutate(
     across(where(is.character), tolower)
     ,across(where(is.character), trimws)
     ,pct_rural = round((total_rural/total)*100,2)
     ,rural     = case_when(
       pct_rural == 100 ~ "Rural"
       ,pct_rural >= 50 ~ "Mostly Rural"
       ,TRUE ~ "Mostly Urban"
       )
     ) %>% 
  left_join(state_names) %>% 
  relocate(c("abb", "region"), .after = state) %>% 
  select(
    -total
    ,-total_urban
    ,-total_rural
  )

# ---- save-to-disk -----------------------------------------------------------

ds_rural_housing %>% write_rds("./data-public/derived/percentage-rural.rds"
                               ,compress = "gz")
ds_rural_housing %>% write_csv("./data-public/derived/percentage-rural.csv")



# ---- testing-thresholds ------------------------------------------------------


# comparing pct-rural to Office of Rural Health Policy list of rural counties

# rural_county <- read_csv('./data-public/metadata/rural-counties.csv') %>% pull() %>% tolower()
# 
# ds_test <- ds_rural_housing %>% 
#   mutate(across(where(is.character),trimws)) %>% 
#   filter(state == "north carolina") %>% 
#   mutate(across(county, ~str_remove_all(.,"county"))
#          ,across(county, trimws)) %>% 
#   mutate(rural = county %in% rural_county)
# 
# 
# ds_test %>% ggplot(aes(x = pct_rural)) +
#   geom_histogram()



