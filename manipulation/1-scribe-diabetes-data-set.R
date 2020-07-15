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

ds_rural_housing <- read_rds("./data-public/derived/percentage-rural.rds")
ds_risk_factors  <- read_rds(
  "./data-public/derived/national-diabetes-risk-factors-2010-2020.rds")
ds_population    <- read_rds(
  "./data-public/derived/us-county-population-estimates-v2.rds")
ds_us_diabetes   <- read_rds("./data-public/derived/us-diabetes-data.rds")

# ---- filter-states -----------------------------------------------------------

ds_list <- list(ds_rural_housing,ds_risk_factors,ds_population,ds_us_diabetes)

for(item in seq_along(ds_list)){
  d <- ds_list[[item]]
  
  d_out <- d %>% 
    filter(!str_detect(state, "alaska|hawaii"))
  
  ds_list[[item]] <- d_out
}


ds_list %>% map(~filter(.,!str_detect(state, "alaska|hawaii")))



# ---- county-names ------------------------------------------------------------

rural_names <- ds_rural_housing %>% pull(county)
risk_names <- ds_risk_factors %>% filter(release_year == 2014) %>% pull(name)
pop_names <- ds_population %>% filter(year == 2014,age_group == "20-24") %>%  
  pull(county_name)
diabetes_names <- ds_us_diabetes %>% filter(year == 2014) %>% pull(county)
