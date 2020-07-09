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

ds_estimates_raw <- readxl::read_excel("./data-unshared/raw/us-county-popuulation-estimates-2010-2019.xlsx", skip = 3)

state_names <- tibble(state.abb,state.name) %>% mutate(across(state.name, tolower))

# ---- tweak-data -------------------------------------------------------------

ds_estimates <- ds_estimates_raw %>% 
  select(-Census, -`Estimates Base`) %>% 
  rename(county = ...1) %>% 
  filter(county != "United States") %>% 
  drop_na() %>% 
  mutate(across(county, ~str_remove_all(.,"\\."))
         ,across(where(is.character), tolower)) %>% 
  separate(
    col   = county
    ,into = c("county","state")
    ,sep  = ','
    ) %>% 
  mutate(across(state, trimws)) %>% 
  left_join(state_names, by = c("state" = "state.name")) %>% 
  relocate(state.abb, .after = state) %>% 
  rename(state_abb = `state.abb`) %>% 
  pivot_longer(
    cols = c(`2010`:`2019`)
    ,names_to = "year"
    ,values_to = "total_population"
  )

# ---- save-to-disk -----------------------------------------------------------

ds_estimates %>% write_rds("./data-public/derived/us-county-population-estimates.rds", compress = "gz")
ds_estimates %>% write_csv("./data-public/derived/us-county-population-estimates.csv")
